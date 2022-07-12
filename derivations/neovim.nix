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
    makeWrapper ${pkgs.neovim}/bin/neovim $out/bin/neovim \
        --set PATH ${path} 
''