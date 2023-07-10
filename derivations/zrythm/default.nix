pkgs:

let
    std = pkgs.lib;

    inherit (builtins)
        concatStringsSep
        filter
        head
        split;

    inherit (std.lists)
        last;

    inherit (std.strings)
        escapeShellArg;
in

std.makeOverridable
({ lv2Path, vst3Path, clapPath }:

let
    version = "1.0.0-beta.4.5.62";

    removeLine = regex: s:
        let lst = split regex s;
        in (head lst) + (last lst);

    carla = pkgs.carla.overrideAttrs (oldAttrs: {
        version = "2.6.0-alpha1";
        src = pkgs.fetchFromGitHub {
            owner = "falkTX";
            repo = "carla";
            rev = "515af8ac30ac64deed8ae2fb89038266f0845da9";
            sha256 = "sha256-WGHtEuWtqlRBZnE7xyqxC9Nixxjj5YwzzI8mqBNR//g=";
        };
        postFixup = removeLine "^.*carla_database\.py.*$" oldAttrs.postFixup;
    });

    zix = pkgs.stdenv.mkDerivation {
        name = "zix";
        src = pkgs.fetchFromGitHub {
            owner = "drobilla";
            repo = "zix";
            rev = "18e62483cb7173c6604f7dd2097299e47c2a4c0c";
            sha256 = "sha256-IQIMbguqdTuYThzSD3Fx1qOFk3C4QmWPqdIZDlwRcyI=";
        };
        mesonFlags = [
            "-Dbenchmarks=disabled"
            "-Ddocs=disabled"
            "-Dtests=disabled"
        ];
        preConfigure = ''
            export LIBRARY_PATH=$LIBRARY_PATH:${pkgs.glib}/lib
        '';
        nativeBuildInputs = with pkgs; [ meson ninja ];
        buildInputs = [ pkgs.glib ];
    };

    zrythm = pkgs.zrythm.overrideAttrs (oldAttrs: {
        inherit version;
    
        src = pkgs.fetchFromGitHub {
            owner = "zrythm";
            repo = "zrythm";
            rev = "v${version}";
            sha256 = "sha256-K93Y4Adh9TqoetSn7nrbbruIri1MKYoSGzoRBGHwbPA=";
        };
    
        preConfigure = ''
            export LIBRARY_PATH=$LIBRARY_PATH:${pkgs.fftw}/lib:${pkgs.fftwFloat}/lib
        '';
    
        buildInputs = (filter (p: p.pname != "carla") oldAttrs.buildInputs) ++ ([
            pkgs.libpanel
            carla
            zix
            pkgs.boost
        ]);
    });

    lv2PathWithBuiltin = [ "${zrythm}/lib/zrythm/lv2" ] ++ lv2Path;
in

pkgs.runCommand "zrythm" { nativeBuildInputs = [ pkgs.makeWrapper ]; } ''
    makeWrapper ${zrythm}/bin/zrythm $out/bin/zrythm \
        --prefix LV2_PATH : ${escapeShellArg (concatStringsSep ":" lv2PathWithBuiltin)} \
        --prefix VST3_PATH : ${escapeShellArg (concatStringsSep ":" vst3Path)} \
        --prefix CLAP_PATH : ${escapeShellArg (concatStringsSep ":" clapPath)}
''

)
{ lv2Path = []; vst3Path = []; clapPath = []; }
