pkgs:

{
    themes
}:

let
    std = pkgs.lib;

    inherit (builtins)
        concatStringsSep;

    inherit (std.attrsets)
        mapAttrsToList;

    commands =
        mapAttrsToList
        (theme: dotfiles: "echo ${dotfiles} > $out/${theme}")
        themes;
in
pkgs.runCommandLocal
"themes"
{}
''
    mkdir -p $out
    ${concatStringsSep "\n" commands}
''