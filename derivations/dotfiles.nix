pkgs:

{
    lib,

    # src :: derivation
    src,

    # filesFn :: string -> Theme -> Map string { variant? :: string; subs :: attrset }
    filesFn
}:

# string -> Theme -> derivation
themeName: theme:

let
    std = pkgs.lib;

    inherit (builtins)
        baseNameOf
        concatStringsSep
        dirOf
        toString;

    inherit (std.attrsets)
        mapAttrsToList;

    inherit (std.strings)
        optionalString;

    inherit (lib)
        sedScript;

    commands =
        let
            files = filesFn themeName theme;
        in
            mapAttrsToList
            (file: dotfile:
                let
                    srcfile =
                        if dotfile ? "variant"
                        then
                            let
                                dir = dirOf file;
                            in
                                src +
                                (
                                    (optionalString (dir != ".") "/${dir}") +
                                    "/${dotfile.variant}.${baseNameOf file}"
                                )
                        else src + "/${file}";
                in ''
                    outfile=$out/${file}
                    mkdir -p $(dirname $outfile)
                    sed ${sedScript dotfile.subs} ${srcfile} > $outfile
                ''
            )
            files;
in
pkgs.runCommandLocal "dotfiles"
{}
''
${concatStringsSep "\n" commands}

mkdir -p $out

files=$(find ${src} -type f)
for srcfile in $files; do
    file=''${srcfile#${src}/}

    outfile=$out/$file
    outdir=$(dirname $outfile)

    basename=$(basename $file)
    varoutbasename=$(echo $basename | sed "s/^[^.]*\.//")
    varoutfile=$outdir/$varoutbasename

    if [ -e $varoutfile ]; then
        continue
    fi

    if [ ! -e $outfile ]; then
        mkdir -p $(dirname $outfile)
        cp $srcfile $outfile
    fi
done
''
