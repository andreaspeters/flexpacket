#!/usr/bin/env bash

function priv_lazbuild
(
    if ! command -v lazbuild >/dev/null 2>&1; then
        source /etc/os-release

        case "${ID:?}" in
            debian|ubuntu)
                sudo apt-get update
                sudo apt-get install -y \
                    wget \
                    ca-certificates \
                    libgtk2.0-dev

                declare -r FPC_VERSION="3.2.2-210709"
                declare -r LAZ_VERSION="4.4"
                declare -r LAZ_BASE_URL="https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%20${LAZ_VERSION}"
                declare -r QT6_URL="https://github.com/davidbannon/libqt6pas/releases/download/v6.2.10/"

                declare -A PKG=(
                    [fpc]="fpc-laz_${FPC_VERSION}_amd64.deb"
                    [src]="fpc-src_${FPC_VERSION}_amd64.deb"
                    [laz]="lazarus-project_${LAZ_VERSION}.0-0_amd64.deb"
                )

                declare -A QT6=(
                    [dev]="libqt6pas6-dev_6.2.10-1_amd64.deb"
                    [non]="libqt6pas6_6.2.10-1_amd64.deb"
                )

                TMPDIR="build"
                pushd "${TMPDIR}" >/dev/null

                for p in "${QT6[@]}"; do
                    wget -nc -L "${QT6_URL}/${p}" -O ${p} || true
                    sudo apt install -y ./${p}
                done

                for p in "${PKG[@]}"; do
                    wget -nc -L "${LAZ_BASE_URL}/${p}/download" -O ${p} || true
                    sudo apt install -y ./${p}
                done

                popd >/dev/null
                ;;
            *)
                printf 'Unsupported OS: %s\n' "${ID}" >&2
                exit 1
                ;;
        esac
    fi

    if [[ -f '.gitmodules' ]]; then
        git submodule update --init --recursive --force --remote &
    fi
    wait

    declare -rA VAR=(
        [src]='src'
        [use]='use'
        [pkg]='use/components.txt'
    )

    if [[ -d "${VAR[use]}" ]]; then
        if [[ -f "${VAR[pkg]}" ]]; then
            while read -r; do
                if [[ -n "${REPLY}" ]] &&
                   ! [[ -d "${VAR[use]}/${REPLY}" ]] &&
                   ! lazbuild --verbose-pkgsearch "${REPLY}" &&
                   ! lazbuild --add-package "${REPLY}"; then
                    (
                        declare -A TMP=(
                            [url]="https://packages.lazarus-ide.org/${REPLY}.zip"
                            [dir]="${VAR[use]}/${REPLY}"
                            [out]=$(mktemp)
                        )
                        wget --quiet --output-document "${TMP[out]}" "${TMP[url]}"
                        unzip -o "${TMP[out]}" -d "${TMP[dir]}"
                        rm --verbose "${TMP[out]}"
                    ) &
                fi
            done < "${VAR[pkg]}"
            wait
        fi

        find "${VAR[use]}" -type f -name '*.lpk' -printf '\033[32m\tadd package link\t%p\033[0m\n' -exec \
            lazbuild --add-package-link {} + 1>&2
    fi

    declare -i errors=0

    while read -r; do
        TMP_OUT="$(mktemp)"
        if lazbuild --build-all --verbose --recursive \
            --no-write-project \
            --build-mode='release' \
            --widgetset='qt6' \
            "${REPLY}" >"${TMP_OUT}"; then
            printf '\x1b[32m\t[%s]\t%s\x1b[0m\n' "$?" "${REPLY}"
            grep --color='always' 'Linking' "${TMP_OUT}"
        else
            printf '\x1b[31m\t[%s]\t%s\x1b[0m\n' "$?" "${REPLY}"
            grep --color='always' -E '(Error|Fatal):' "${TMP_OUT}"
            ((errors+=1))
        fi 1>&2
        rm -f "${TMP_OUT}"
    done < <(find "${VAR[src]}" -type f -name '*.lpi' | sort)

    exit "${errors}"
)


function priv_main
(
    set -euo pipefail
    if ((${#})); then
        case ${1} in
            build) priv_lazbuild ;;
            *) priv_clippit ;;
        esac
    else
        priv_clippit
    fi
)

if [ -d "src/backup" ]; then
	mv src/backup /tmp/backup
fi

priv_main "${@}"

if [ -d "/tmp/backup" ]; then
	mv /tmp/backup src/backup
fi
