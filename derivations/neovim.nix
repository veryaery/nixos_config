pkgs:

{
    bin
}:

let
    path = pkgs.lib.makeBinPath bin;
in
pkgs.runCommand
"neovim"
{
    buildInputs = with pkgs; [ makeWrapper ];
}
''
    makeWrapper ${pkgs.neovim}/bin/nvim $out/bin/nvim \
        --set PATH ${path} 
''
