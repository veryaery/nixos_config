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
                mkdir -p $(dirname out/${path})
                sed ${sedScript sub} dotfiles/${path} > out/${path}
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
        ${concatStringsSep "\n" commands}
        ${pkgs.tree}/bin/tree -a
    '';

    installPhase = ''
        mkdir -p $out
        cp -r out/. $out
        for path in $(find -type f ./dotfiles); do
            shortPath=$(echo $path | sed "s/^\.\/dotfiles\///")
            outPath=$out/$shortPath
            if [ ! -e $outPath ]; then
                mkdir -p $(dirname outPath)
                cp $path $outPath
            fi
        done
        ${pkgs.tree}/bin/tree $out -a
    '';
}
