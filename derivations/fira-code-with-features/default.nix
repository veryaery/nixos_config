pkgs:

let
    std = pkgs.lib;

    inherit (builtins)
        concatStringsSep;

    inherit (std.strings)
        escapeShellArg;
in

pkgs.lib.makeOverridable
({ features }:

let
    version = "6.2";
in

pkgs.stdenv.mkDerivation {
    inherit version;
    pname = "fira-code-with-features";

    src = (pkgs.fetchzip {
        url = "https://github.com/tonsky/FiraCode/releases/download/${version}/Fira_Code_v${version}.zip";
        sha256 = "sha256-3oHmVu9XyAxQx1umtnEbG+ZnNosmShM387uyzTn2DMU=";
    }).overrideAttrs (oldAttrs: {
        postFetch = ''
            mkdir $out
            unzip -j "$downloadedFile" "ttf/*" -d $out
        '';
    });

    nativeBuildInputs = with pkgs; [
        fontfreeze-cli
        nerd-font-patcher
    ];

    featdir = "feat";
    patchdir = "patch";

    buildPhase = ''
        # Freeze font features.
        mkdir $featdir
        for srcfile in $src/*; do
            file=''${srcfile#$src/}
            suffix=''${file#FiraCode}
            featfile=$featdir/FiraCode-Features$suffix

            echo $srcfile $featfile

            fontfreeze-cli -i $srcfile -o $featfile -f ${escapeShellArg (concatStringsSep "," features)}
        done

        # Patch nerd font.
        mkdir $patchdir
        for featfile in $featdir/*; do
            echo $featfile

            nerd-font-patcher -out $patchdir $featfile \
                --fontawesome --fontawesomeextension \
                --fontlogos \
                --octicons \
                --codicons \
                --powersymbols \
                --pomicons \
                --powerlineextra \
                --material \
                --weather
        done
    '';

    installPhase = ''
        outdir=$out/share/fonts/truetype
        mkdir -p $outdir
        cp -r $patchdir/. $outdir/
    '';
}

)
{ features = []; }
