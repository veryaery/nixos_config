pkgs:

{
    lib,
    dirPath,
    subs
}:

let
    std = pkgs.lib;

    inherit (builtins)
        concatStringsSep;

    inherit (std.attrsets)
        mapAttrsToList;

    inherit (lib)
        sedCommand;
    
    commands =
        mapAttrsToList
        (path: sub: "${sedCommand sub} -i ${path} > $out/${path}")
        subs;
in
pkgs.stdenv.mkDerivation {
    name = "dotfiles";

    src = dirPath; 
    preferLocalBuild = true;
    allowSubstitutes = false;

    unpackPhase = ''
        cp -r -t . $src
    '';

    buildPhase = ''
        mkdir -p $out
        cp -r -t $out $src
        ${concatStringsSep "\n" commands}
    '';

    installPhase = "";
}
