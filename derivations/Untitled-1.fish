#!${pkgs.fish}/bin/fish

set -l theme $argv[1]
set -l file ${themes}/$theme
if [ ! -e $file ]
    echo $theme: Invalid theme.
    return 1
end
set -l dotfiles $(cat $file)
# set -l escapeDotfiles $(echo $dotfiles | sed ${escapeBREScriptFish} | sed ${escapeSEDScriptFish})

# function remove_file_and_parents
#     set -l file $argv[1]

#     rm $file

#     set -l segments $(string split / $(dirname $file))
#     set -l i $(count $segments)
#     while [ $i -gt 0 ]
#         set -l path $(string join / $segments[$(seq $i)])
#         if [ -z "$(ls -A $path)" ]
#             rm -r $path
#             echo Removed directory $path
#         end

#         set i $(math $i - 1)
#     end
# end

set -l prevtheme ~/.prevtheme

if [ -e $prevtheme ]
    set -l files $(cat $prevtheme)
    for file in $files
        set -l dotfile $dotfiles/$file
        set -l homefile ~/$file

        if [ -e $dotfile ]
            continue
        end
        # file is no longer in dotfiles

        if [ -f $homefile ]
            # file is a regular file
            rm $homefile
            echo Removed regular file $file
        end
        if [ -d $homefile ] && [ -z "$(ls -A $homefile)" ]
            # file is an empty directory
            rm $homefile
            echo Removed empty directory $file
        end
    end

    truncate -s 0 $prevtheme
end

set -l files $(find $dotfiles -type f)
for dotfile in $files
    set -l file $(string split -n -m1 $dotfiles $dotfile)
    set -l homefile ~/$file

    set -l segments $(string split / $(dirname $file))
    set -l segments_count $(count $segments)
    set -l i 1
    set -l created_dirs
    
    while [ $i -le $segments_count ]
        set -l dir $(string join / $segments[$(seq $i)])
        set -l homedir ~/$dir

        if [ ! -e $homedir ]
            mkdir $homedir
            set -p created_dirs $dir
            echo Created directory $dir
        end

        set i $(math $i + 1)
    end

    for dir in $created_dirs
        echo $dir >> $prevtheme
    end

    if [ -e $homefile ]
        truncate -s 0 $homefile
        cat $dotfile >> $homefile
        echo Updated regular file $file
    else
        cat $dotfile >> $homefile
        echo Created regular file $file
    end
end

echo $theme: Theme installed.