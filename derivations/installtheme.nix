pkgs:

{
    themes,
    postinstall
}:

pkgs.writeScriptBin "installtheme"
''
#!${pkgs.fish}/bin/fish

set -l theme $argv[1]
set -l file ${themes}/$theme
if [ ! -e $file ]
    echo $theme: Invalid theme.
    return 1
end
set -l dotfiles $(cat $file)

set -l themelog ~/.themelog

if [ -e $themelog ]
    set -l newlog

    set -l files $(cat $themelog)
    for file in $files
        set -l dotfile $dotfiles/$file
        set -l homefile ~/$file

        if [ -e $dotfile ]
            # file is still in dotfiles.
            set -a newlog $file # keep the log entry.
            continue
        end
        # file is no longer in dotfiles.

        if [ -f $homefile ]
            # file is a regular file.
            rm $homefile
            echo Removed regular file $file
        end
        if [ -d $homefile ] && [ -z "$(ls -A $homefile)" ]
            # file is an empty directory.
            rm $homefile
            echo Removed empty directory $file
        end
    end

    truncate -s 0 $themelog
    for entry in $newlog
        echo $entry >> $themelog
    end
end

set -l files $(find $dotfiles -type f)
for dotfile in $files
    set -l file $(string split -n -m1 $dotfiles/ $dotfile)
    set -l homefile ~/$file

    set -l dirlog
    
    set -l segments $(string split / $(dirname $file))
    set -l segments_count $(count $segments)
    set -l i 1
    while [ $i -le $segments_count ]
        set -l dir $(string join / $segments[$(seq $i)])
        set -l homedir ~/$dir

        if [ ! -e $homedir ]
            mkdir $homedir
            set -p dirlog $dir
            echo Created directory $dir
        end

        set i $(math $i + 1)
    end

    for entry in $dirlog
        echo $entry >> $themelog
    end

    if [ -e $homefile ]
        truncate -s 0 $homefile
        cat $dotfile >> $homefile
        echo Updated regular file $file
    else
        cat $dotfile >> $homefile
        echo $file >> $themelog
        echo Created regular file $file
    end

end

set -U NIXOSCFG_THEME $theme

${postinstall}

echo $theme: Theme installed
''
