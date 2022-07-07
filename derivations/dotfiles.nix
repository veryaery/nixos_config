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
        sedScript;
    
    commands =
        mapAttrsToList
        (path: sub: "sed -i ${sedScript sub} ${path}")
        subs;
in
pkgs.stdenv.mkDerivation {
    name = "dotfiles";

    src = dirPath;
    buildInputs = with pkgs; [
        tree
    ];
    preferLocalBuild = true;
    allowSubstitutes = false;

    unpackPhase = ''
        cp -r -t . $src
    '';

    buildPhase = ''
        ${pkgs.tree}/bin/tree
        ${concatStringsSep "\n" commands}
    '';

    installPhase = ''
        mkdir -p $out
        cp -r -t $out .
        ${pkgs.tree}/bin/tree $out
    '';
}
