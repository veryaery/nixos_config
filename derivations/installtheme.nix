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
    set -l escapeDotfiles $(echo $dotfiles | sed ${escapeBREScriptFish} | sed ${escapeSEDScriptFish})

    if [ -e ~/.prevtheme ]
        set -l prevFiles $(cat ~/.prevtheme)
        for file in $prevFiles
            set -l dir $(dirname ~/$file)
            rm ~/$file
            echo Removed regular file $file
            if [ $dir != ~ ] && [ -z "$(ls -A $dir)" ]
                rm -r $dir
                echo Removed directory $dir
            end
        end

        truncate -s 0 ~/.prevtheme
    end

    set -l files $(find $dotfiles -type f)
    for file in $files
        set -l shortPath $(echo $file | sed "s/^$escapeDotfiles\///")
        mkdir -p $(dirname ~/$shortPath)
        ln -s $file ~/$shortPath
        echo $shortPath >> ~/.prevtheme
        echo Made symlink $shortPath
    end

    echo $theme: Theme installed.
''
