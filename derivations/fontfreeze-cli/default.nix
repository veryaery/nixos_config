pkgs:

let
    python = pkgs.python3.withPackages (ps: [ ps.fonttools ]);

    fontfreeze = pkgs.stdenv.mkDerivation {
        name = "fontfreeze";

        src = pkgs.fetchFromGitHub {
            owner = "mutsuntsai";
            repo = "fontfreeze";
            rev = "5758c7ea4bd1162692356ec646f214d9bd850faf";
            sha256 = "sha256-NXte555gcnyRGUk+tnhi29rC+WVEqU2od0J+4sZy1q0=";
        };

        installPhase = ''
            mkdir $out
            cp $src/src/main.py $out/fontfreeze.py
        '';            
    };
in
pkgs.writeShellScriptBin "fontfreeze-cli" ''
    export PYTHONPATH=$PYTHONPATH:${fontfreeze}
    exec ${python}/bin/python ${./main.py} "$@"
''
