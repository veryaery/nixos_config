pkgs:

{
    dotfiles,
    theme
}:

let
    ghc = pkgs.ghc.withPackages (haskellPackages: with haskellPackages; [
        xmonad
        xmonad-contrib
    ]);
in
pkgs.stdenv.mkDerivation {
    name = "xmonad";

    src = dotfiles + /.xmonad; 
    buildInputs = [
        ghc
        pkgs.haskellPackages.xmonad
        pkgs.makeWrapper
        pkgs.tree
    ];
    preferLocalBuild = true;
    allowSubstitutes = false;

    unpackPhase = ''
        for file in $(find $src -type f ! -path "*/lib/Theme.hs"); do
            install -D -T $file ''${file#$src/}
        done
    '';

    buildPhase = ''
        mkdir -p lib
        cat ${theme} > lib/Theme.hs
        
        ghc xmonad.hs -i:lib
    '';

    installPhase = ''
        install -D \
            -t $out/share/man/man1 \
            ${pkgs.haskellPackages.xmonad}/share/man/man1/xmonad.1.gz
        install -D -t $out/bin xmonad
    '';
}
