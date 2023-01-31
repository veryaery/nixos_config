pkgs:

let
    version = "0.7.1";
in
pkgs.stdenv.mkDerivation
{
    inherit version;
    pname = "fundle";

    src = pkgs.fetchFromGitHub {
        owner = "danhper";
        repo = "fundle";
        rev = "v${version}";
        sha256 = "sha256-H//6xQLWk3pfiugLV9kOu41M3tcVmfqUOsMdLZPBo8c=";
    };

    dontBuild = true;

    installPhase = ''
        mkdir $out
        cp -r $src/functions/ $src/completions/ $out/
    '';
}
