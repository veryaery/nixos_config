pkgs:

{
    lib,
}:

# string -> path -> Map string { variant? :: string; subs :: attrset }
name: src: subs:

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
        mapAttrsToList
        (file: sub:
            let
                srcfile =
                    if sub ? "variant"
                    then
                        let
                            dir = dirOf file;
                        in
                            src +
                            (
                                (optionalString (dir != ".") "/${dir}") +
                                "/${sub.variant}.${baseNameOf file}"
                            )
                    else src + "/${file}";
            in ''
                outfile=$out/${file}
                mkdir -p $(dirname $outfile)
                sed ${sedScript sub.subs} ${srcfile} > $outfile
            ''
        )
        subs;
in
pkgs.runCommandLocal name
{}
''
mkdir -p $out

${concatStringsSep "\n" commands}

# Copy files which were not written by a substitution command.
files=$(find ${src} -type f)
for srcfile in $files; do
    file=''${srcfile#${src}/}

    outfile=$out/$file
    outdir=$(dirname $outfile)

    basename=$(basename $file)
    variant=$(echo $basename | sed "s/^[^.]*\.//")
    outfile_variant=$outdir/$variant # outfile without variant prefix.
    
    # Skip srcfile if another variant of it already exists.
    if [ -e $outfile_variant ]; then
        continue
    fi

    # Skip srcfile if it has already been written by a substitution command.
    if [ ! -e $outfile ]; then
        mkdir -p $outdir
        cp $srcfile $outfile
    fi
done
''
