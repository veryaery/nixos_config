pkgs:

{
    lib,

    # src :: derivation
    src,

    # files :: Map string { src? :: string -> derivation, subs :: Theme -> attrset }
    #                       ^^^^ src is optional
    files
}:

# string -> Theme -> derivation
themeName: theme:

let
    std = pkgs.lib;

    inherit (builtins)
        concatStringsSep
        toString;

    inherit (std.attrsets)
        mapAttrsToList;

    inherit (lib)
        sedScript;
    
    commands =
        mapAttrsToList
        (file: themedDotfile:
            let
                _src =
                    if themedDotfile ? "src"
                    then themedDotfile.src
                    else src + "/${file}";
                subs = themedDotfile.subs theme; 
            in ''
                outfile=$out/${file}
                mkdir -p $(dirname $outfile)
                sed ${sedScript subs} ${_src} > $outfile
            ''
        )
        files;
in
pkgs.runCommandLocal "dotfiles"
{}
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