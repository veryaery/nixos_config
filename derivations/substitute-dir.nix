pkgs:

{
    lib
}:

#   FileSubstitution = {
#       variant? :: string;
#       subs :: substitutionMap;
#   }
#
#   FileSubstitutionMap = Map string FileSubstitution

# string -> path -> FileSubstitutionMap -> derivation
name: src: fileSubMap:

let
    std = pkgs.lib;

    inherit (builtins)
        baseNameOf
        concatStringsSep
        dirOf;

    inherit (std.strings)
        optionalString;

    inherit (std.attrsets)
        mapAttrsToList;

    cmds =
        mapAttrsToList
        (file: fileSub:
            let
                srcfile =
                    if fileSub ? "variant"
                    then
                        let
                            dir = dirOf file;
                        in
                        "${src}${optionalString (dir != ".") "/${dir}"}/${fileSub.variant}.${baseNameOf file}"
                    else "${src}/${file}";
                subsJson = pkgs.substitution-json { inherit lib; } fileSub.subs;
            in ''
                outfile=$out/${file}
                outdir=$(dirname $outfile)

                mkdir -p $outdir

                mustache ${subsJson} ${srcfile} > $outfile

                if [ -x ${srcfile} ]; then
                    chmod +x $outfile
                fi
            ''
        )
        fileSubMap;
in
pkgs.runCommand name
{
    nativeBuildInputs = with pkgs; [ mustache-go ];
}
''
    mkdir -p $out

    ${concatStringsSep "\n" cmds}

    srcfiles=$(find ${src} -type f)
    for srcfile in $srcfiles; do
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

        # Skip srcfile if it already exits.
        if [ -e $outfile ]; then
            continue
        fi

        mkdir -p $outdir
        cp $srcfile $outfile
        if [ -x $srcfile ]; then
            chmod +x $outfile
        fi
    done
''
