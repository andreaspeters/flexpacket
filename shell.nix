with import <nixpkgs> {};

stdenv.mkDerivation {
name = "tmp-env";

buildInputs = [
  unzip
	sudo
	lazarus-qt6
];

SOURCE_DATE_EPOCH = 315532800;
PROJDIR = "/tmp/tmp-dev";
S_IMAGE="localhost:5000/debian_build:13";

shellHook = ''
    '';
}
