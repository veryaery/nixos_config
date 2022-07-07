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
                mkdir -p $(dirname sed/${path})
                sed ${sedScript sub} dotfiles/${path} > sed/${path}
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

    buildPhase = concatStringsSep "\n" commands;

    installPhase = ''
        mkdir -p $out
        cp -r sed/. $out
        for path in $(find dotfiles -type f); do
            shortPath=$(echo $path | sed "s/^dotfiles\///")
            outPath=$out/$shortPath
            if [ ! -e $outPath ]; then
                mkdir -p $(dirname outPath)
                cp $path $outPath
            fi
        done
    '';
}
