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
        pkgs.tree
        pkgs.haskellPackages.xmonad
    ];
    preferLocalBuild = true;
    allowSubstitutes = false;

    unpackPhase = ''
        ${pkgs.tree}/bin/tree $src
        for file in $(find $src -type f ! -path "*/lib/Theme.hs"); do
            echo "install -D -T $file ''${file#$src/}"
            install -D -T $file ''${file#$src/}
        done
    '';

    buildPhase = ''
        # Replace Theme.hs in the lib directory.
        mkdir -p lib
        cat ${theme} > lib/Theme.hs
        ${pkgs.tree}/bin/tree        

        ghc xmonad.hs -v -ilib
    '';

    installPhase = ''
        install -D -t $out/bin xmonad
        install -D \
            -t $out/share/man/man1 \
            ${pkgs.haskellPackages.xmonad}/share/man/man1/xmonad.1.gz
    '';
}
