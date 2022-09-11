pkgs:

let
    version = "2.20.2";
in
pkgs.vimPlugins.indent-blankline-nvim.overrideAttrs (oldAttrs: {
    inherit version;

    src = pkgs.fetchFromGitHub {
        owner = "lukas-reineke";
        repo = "indent-blankline.nvim";
        rev = "v${version}";
        sha256 = "sha256-d+G+tLdvD8OxENIRobAv0ATl4gGQdoz3RPuBrPA3PuU=";
    };
})
