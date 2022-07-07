pkgs:

{
    lib,
    dirPath,
    subs
}:

let
    std = pkgs.lib;

    inherit (builtins)
        concatStringsSep
        toString;

    inherit (std.attrsets)
        mapAttrsToList;

    inherit (lib)
        escapeBREScript
        escapeSEDScript
        sedScript;
    
    commands =
        mapAttrsToList
        (path: sub:
            ''
                srcPath=$src/${path}
                outPath=$out/${path}
                mkdir -p $(dirname $outPath)
                sed ${sedScript sub} $srcPath > $outPath
            ''
        )
        subs;
in
pkgs.runCommandLocal
"dotfiles"
{
    src = dirPath;
}
''
    ${concatStringsSep "\n" commands}

    mkdir -p $out
    echo ${escapeBREScript}
    echo ${escapeSEDScript}
    escapeSrc=$(echo $src | sed ${escapeBREScript} | sed ${escapeSEDScript})
    for srcPath in $(find $src -type f); do
        outPath=$out/$(echo $srcPath | sed "s/^$escapeSrc\///")
        echo srcPath = $srcPath
        echo outPath = $outPath
        echo sed 's/^'$escapeSrc'\///'
        if [ ! -e $outPath ]; then
            mkdir -p $(dirname outPath)
            cp $srcPath $outPath
        fi
    done
''