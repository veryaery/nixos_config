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
        escapeBREScriptBash
        escapeSEDScriptBash
        sedScript;
    
    commands =
        mapAttrsToList
        (file: sub:
            ''
                srcfile=$src/${file}
                outfile=$out/${file}
                mkdir -p $(dirname $outfile)
                sed ${sedScript sub} $srcfile > $outfile
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

    files=$(find $src -type f)
    for srcfile in $files; do
        file=''${srcfile#$src/}
        outfile=$out/$file
        if [ ! -e $outfile ]; then
            mkdir -p $(dirname $outfile)
            cp $srcfile $outfile
        fi
    done
''
