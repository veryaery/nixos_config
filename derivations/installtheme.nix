pkgs:

{
    # themes :: derivation
    themes,

    # postinstall :: derivation
    postinstall
}:

pkgs.writeScriptBin "installtheme"
''
#!${pkgs.fish}/bin/fish

set -l theme_name $argv[1]
set -l themefile ${themes}/$theme_name
if [ ! -f $themefile ]
    echo $theme_name: Invalid theme.
    return 1
end
set -l themedir $(cat $themefile)

set -l themelogfile ~/.themelog

# TODO: Keep looping until all directories which do not exist in dotfiles have files or have been deleted and files ahve not been deleted.

if [ -f $themelogfile ]
    set -l themelog $(cat $themelogfile)

    set -l has_rm true
    while [ $has_rm = true ]
        set -l newthemelog

        set has_rm false

        for file in $themelog
            set -l homefile ~/$file
            set -l themefile $themedir/$file
            
            if [ -e $themefile ]
                set -a newthemelog $file # Keep theme log entry.
                continue
            end

            if [ -f $homefile ]
                rm $homefile
                set has_rm true
                echo Removed regular file $file
            end

            if [ -d $homefile ] && [ -z "$(ls -A $homefile)" ]
                rm $homefile
                set keep_loop true
                echo Removed directory $file
            end
        end

        truncate -s 0 $themelog
        for entry in $newthemelog
            echo $entry >> $themelog
        end
    end
end

set -l themefiles $(find $themedir -type f)
for themefile in $themefiles
    set -l file $(string split -n -m1 $themedir/ $themefile)
    set -l homefile ~/$file

    set -l segments $(string split / $(dirname $file))
    set -l segments_count $(count $segments)
    set -l i 1
    while [ $i -le $segments_count ]
        set -l dir $(string join / $segments[$(seq $i)])
        set -l homedir ~/$dir

        if [ ! -e $homedir ]
            mkdir $homedir
            echo $dir >> $themelog
            echo Created directory $dir
        end

        set i $(math $i + 1)
    end

    cp $themefile $homefile
    if [ -x $themefile ]
        chmod +x $homefile
    else
        chmod -x $homefile
    end

    if [ -e $homefile ]
        echo Updated regular file $file
    else
        echo $file >> $themelog
        echo Created regular file $file
    end
end

set -U NIXOSCFG_THEME_NAME $theme

${postinstall}

echo $theme_name: Theme installed
''
