pkgs:

pkgs.pastel.overrideAttrs (oldAttrs: {
    src = pkgs.fetchFromGitHub {
        owner = "veryaery";
        repo = "pastel";
        rev = "31b3ba8116a144b8d8b0780c16e4165ed7b670a1";
        sha256 = "sha256-3xLsUcccyK0kRA1Nylh4hNCS9+bTisXm/3ER/mRZXNE=";
    };
})
