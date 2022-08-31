pkgs:

# Map string Theme -> (string -> Theme -> derivation) -> derivation
themes: drvFn:

let
    std = pkgs.lib;

    inherit (builtins)
        concatStringsSep;

    inherit (std.attrsets)
        mapAttrsToList;

    commands =
        mapAttrsToList
        (themeName: theme:
            let
                drv = drvFn themeName theme;
            in "echo ${drv} > $out/${themeName}"
        )
        themes;
in
pkgs.runCommandLocal
"themes"
{}
''
    mkdir -p $out
    ${concatStringsSep "\n" commands}
''
