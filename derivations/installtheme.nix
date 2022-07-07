pkgs:

{
    lib,
    themes
}:

let
    std = pkgs.lib;

    inherit (builtins)
        concatStringsSep;

    inherit (std.attrsets)
        mapAttrsToList;
    
    inherit (lib)
        escapeBREScriptFish
        escapeSEDScriptFish;

    commands =
        mapAttrsToList
        (theme: dotfiles: "set -l __theme_${theme} ${dotfiles}")
        themes;
in
pkgs.writeScriptBin
"installtheme"
''
    #!${pkgs.fish}/bin/fish

    ${concatStringsSep "\n" commands}

    set -l theme $argv[1]
    set -l var __theme_$theme
    set -l dotfiles $$var

    set -l files $(find $dotfiles -type f)
    set -l escapeDotfiles $(echo $dotfiles | sed ${escapeBREScriptFish} | sed ${escapeSEDScriptFish})

    for file in $files
        set -l shortPath $(echo $file | sed "s/^$escapeDotfiles\///")
        echo $shortPath
    end
''