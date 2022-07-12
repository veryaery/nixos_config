pkgs:

let
    xmonad = pkgs.xmonad-with-packages.override {
        packages = haskellPackages: with haskellPackages; [ xmonad-contrib ];
    };

    path = pkgs.lib.makeBinPath (with pkgs; [ xmobar ]);
in
pkgs.runCommand "xmonad"
{ buildInputs = with pkgs; [ makeWrapper ]; }
''
makeWrapper ${xmonad}/bin/xmonad $out/bin/xmonad \
    --prefix PATH ${path}
''
