pkgs:

let
    ghc = pkgs.ghc.withPackages (haskellPackages: with haskellPackages; [
        xmonad
        xmonad-contrib
    ]);
in
pkgs.runCommand
"xmonad"
{
    nativeBuildInputs = [
        ghc
        pkgs.haskellPackages.xmonad
    ];
    buildInputs = [ pkgs.makeWrapper ];
}
''
    install -D \
        -t $out/share/man/man1 \
        ${pkgs.haskellPackages.xmonad}/share/man/man1/xmonad.1.gz
    makeWrapper ${pkgs.haskellPackages.xmonad}/bin/xmonad $out/bin/xmonad \
        --set NIX_GHC ${ghc}/bin/ghc
''