pkgs:

{
    lib,
    themes
}:

let
    inherit (lib)
        escapeBREScriptFish
        escapeSEDScriptFish;
in
pkgs.writeScriptBin
"installtheme"
''
    #!${pkgs.fish}/bin/fish

    set -l theme $argv[1]
    set -l file ${themes}/$theme
    if [ ! -e $file ]
        echo $theme: Invalid theme.
        return 1
    end
    set -l dotfiles $(cat $file)

    set -l files $(find $dotfiles -type f)
    set -l escapeDotfiles $(echo $dotfiles | sed ${escapeBREScriptFish} | sed ${escapeSEDScriptFish})

    for file in $files
        set -l shortPath $(echo $file | sed "s/^$escapeDotfiles\///")
        echo $shortPath
    end
''