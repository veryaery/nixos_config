std:

let
    inherit (builtins)
        attrNames
        baseNameOf
        concatStringsSep
        elemAt
        map
        pathExists
        readDir
        stringLength
        substring;

    inherit (std.lists)
        flatten
        foldr
        init;

    inherit (std.strings)
        hasPrefix
        splitString
        toLower;

    # cartesianProduct :: [ a ] -> [ b ] -> [[ a b ]]
    cartesianProduct = lsta: lstb:
        foldr
        (a: z:
            let x =
                map
                (b:
                    [ a b ]
                )
                lstb;
            in z ++ x
        )
        []
        lsta;

    # basenameWithoutExtention :: string -> string 
    basenameWithoutExtention = s:
        concatStringsSep "." (init (splitString "." (baseNameOf s)));
in
{
    # Identity function.
    # id :: a -> a
    id = a: a;

    # A list containing the path if the path exists.
    # Otherwise, an empty list.
    # optionalPath :: path -> [ path ]
    optionalPath = path: if pathExists path then [ path ] else [];

    # Reads the host directory
    # then returns an attrset with an attr for each combination of theme and host.
    # Each attrset key is the form of "<theme>.<host>".
    # Each attrset valye is the returned value of f theme host.
    # attrsetFromEachThemeEachHost :: attrset -> path -> string -> string -> a -> Map string a
    attrsetFromEachThemeEachHost = themes: hostDirPath: f:
        let
            themeNameList = attrNames themes;
            hostList = attrNames (readDir hostDirPath);

            combinations = cartesianProduct themeNameList hostList;
        in
            foldr
            (combination: z:
                let
                    themeName = elemAt combination 0;
                    host = elemAt combination 1;

                    a = f themeName host;
                    x = { "${themeName}.${host}" = a; };
                in z // x
            )
            {}
            combinations;
    
    # The corresponding fish terminal color for theme terminal color.
    # fishTerminalColor :: string -> string
    fishTerminalColor = color:
        if hasPrefix "bright" color then
            "br" + toLower (substring 6 (stringLength color) color)
        else
            color;

    # readThemes :: path -> Map string Theme
    readThemes = themeDirPath:
        let
            files = attrNames (readDir themeDirPath);
        in
            foldr
            (file: z:
                let
                    themeName = basenameWithoutExtention file;
                    theme = import (themeDirPath + "/${file}");

                    x = { "${themeName}" = theme; };
                in z // x
            )
            {}
            files;
}
