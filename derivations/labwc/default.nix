pkgs:

let
    version = "0.6.1";
in
pkgs.labwc.overrideAttrs (oldAttrs: {
    inherit version;

    src = pkgs.fetchFromGitHub {
        owner = "labwc";
        repo = "labwc";
        rev = version;
        sha256 = "sha256-PfvtNbSAz1vt0+ko4zRPyRRN+lhQoA2kJ2xoJy5o4So=";
    };
})
