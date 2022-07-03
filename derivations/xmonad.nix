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
        theme
        pkgs.haskellPackages.xmonad
    ];
    preferLocalBuild = true;
    allowSubstitutes = false;

    buildPhase = ''
        # Replace Theme.hs in the lib directory.
        install -D ${theme} -t lib

        ghc xmonad.hs
    '';

    installPhase = ''
        install -D xmonad -t $out/bin
        install -D \
            ${pkgs.haskellPackages.xmonad}/share/man/man1/xmonad.1.gz \
            -t $out/share/man/man1
    '';
}