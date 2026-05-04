# This Makefile is only usable on my systems

.PHONY: build nix

all: build

nix:
	-bash make.sh build || true
	lazbuild --build-all --verbose --recursive --no-write-project --build-mode='release' --widgetset='qt6' --lazarusdir=${LAZARUS_DIR}  src/flexpacket.lpi 

build: debian12 debian13 ubuntu22 ubuntu24

debian12:
	@debian.sh 12 -s "bash make.sh build"
	mv src/flexpacket src/flexpacket_debian12_amd64

debian13:
	@debian.sh 13 -s "bash make.sh build"
	mv src/flexpacket src/flexpacket_debian13_amd64

ubuntu22:
	@ubuntu.sh 22 -s "bash make.sh build"
	mv src/flexpacket src/flexpacket_ubuntu2204_amd64

ubuntu24:
	@ubuntu.sh 24 -s "bash make.sh build"
	mv src/flexpacket src/flexpacket_ubuntu2404_amd64

