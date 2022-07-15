pkgs:

{
    # bin :: [ derivation ]
    bin
}:

let
    path = pkgs.lib.makeBinPath bin;
in
pkgs.runCommandLocal
"neovim"
{
    nativeBuildInputs = with pkgs; [ makeWrapper ];
}
''
makeWrapper ${pkgs.neovim}/bin/nvim $out/bin/nvim \
    --set PATH ${path} 
''
