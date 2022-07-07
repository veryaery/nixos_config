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
        (path: sub:
            ''
                mkdir -p $(dirname $TMP/${path})
                sed ${sedScript sub} dotfiles/${path} > $TMP/${path}
            ''
        )
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
        mkdir dotfiles 
        cp -r $src/. dotfiles
    '';

    buildPhase = ''
        ${pkgs.tree}/bin/tree -a
        ${concatStringsSep "\n" commands}
        ${pkgs.tree}/bin/tree -a $TMP
    '';

    installPhase = ''
        mkdir -p $out
        cp -r . $out
        ${pkgs.tree}/bin/tree $out -a
    '';
}
