#!/usr/bin/env bash

function priv_clippit
(
    cat <<EOF
Usage: bash ${0} [OPTIONS]
Options:
    build   Build program
EOF
)

function priv_lazbuild
(
    if ! (command -v lazbuild); then
        source '/etc/os-release'
        case ${ID:?} in
            debian | ubuntu)
                sudo apt-get update
                sudo apt-get install -y lazarus{-ide-qt5,} &              
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
                    ! (lazbuild --verbose-pkgsearch "${REPLY}") &&
                    ! (lazbuild --add-package "${REPLY}"); then
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
        find "${VAR[use]}" -type 'f' -name '*.lpk' -printf '\033[32m\tadd package link\t%p\033[0m\n' -exec \
            lazbuild --add-package-link {} + 1>&2
    fi
    declare -i errors=0
    while read -r; do
        declare -A TMP=(
            [out]=$(mktemp)
        )
        if (lazbuild --build-all --verbose --recursive --no-write-project --build-mode='release' --widgetset='qt5' "${REPLY}" > "${TMP[out]}"); then
            printf '\x1b[32m\t[%s]\t%s\x1b[0m\n' "${?}" "${REPLY}"
            grep --color='always' 'Linking' "${TMP[out]}"
        else
            printf '\x1b[31m\t[%s]\t%s\x1b[0m\n' "${?}" "${REPLY}"
            grep --color='always' --extended-regexp '(Error|Fatal):' "${TMP[out]}"
            ((errors+=1))
        fi 1>&2
        rm "${TMP[out]}"
    done < <(find "${VAR[src]}" -type 'f' -name '*.lpi' | sort)
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

priv_main "${@}" 2>/dev/null

if [ -d "/tmp/backup" ]; then
	mv /tmp/backup src/backup
fi
