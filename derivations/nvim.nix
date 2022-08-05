pkgs:

{
    # bin :: [ derivation ]
    bin
}:

let
    path = pkgs.lib.makeBinPath bin;
in
pkgs.writeScriptBin "nvim"
''
#!${pkgs.fish}/bin/fish

set -x PATH ${path}
set -x NIXOSCFG_THEME_NAME $NIXOSCFG_THEME_NAME
${pkgs.neovim}/bin/nvim $argv
''
