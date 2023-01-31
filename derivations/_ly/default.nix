pkgs:

let
    std = pkgs.lib;

    pname = "ly";
    version = "0.5.3";
in
pkgs.stdenv.mkDerivation {
    inherit pname version;

    src = pkgs.fetchFromGitHub {
        owner = "fairyglade";
        repo = pname;
        rev = "v${version}";
        sha256 = "sha256-hQ1G69ljV5yrxMHMl7G3bDDC5ic0+MvV8t8BhCuI+hA=";
        fetchSubmodules = true;
    };

    buildInputs = with pkgs; [];
}
