pkgs:

{
    # themes :: Map string Theme
    themes
}:

let
    xmonad = pkgs.xmonad-with-packages.override {
        packages = haskellPackages: with haskellPackages; [ xmonad-contrib ];
    };

    path =
        pkgs.lib.makeBinPath
        (with pkgs; [
            (xmobar { inherit themes; })
        ]);
in
pkgs.runCommand "xmonad"
{ nativeBuildInputs = with pkgs; [ makeWrapper ]; }
''
makeWrapper ${xmonad}/bin/xmonad $out/bin/xmonad \
    --prefix PATH : ${path}
''
