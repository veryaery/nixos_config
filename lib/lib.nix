std:

let
    inherit (builtins)
        attrNames
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

    # withoutFileExtension :: string -> string
    withoutFileExtension = s: concatStringsSep "." (init (splitString "." s));
in
{
    # A list containing the path if the path exists.
    # Otherwise, an empty list.
    # optionalPath :: path -> [ path ]
    optionalPath = path: if pathExists path then [ path ] else [];

    # Reads the theme and host directories
    # then returns an attrset with an attr for each combination of theme and host.
    # Each attrset key is the form of "<theme>.<host>".
    # Each attrset valye is the returned value of f theme host.
    # attrsetFromEachThemeEachHost :: path -> path -> string -> string -> a -> Map string a
    attrsetFromEachThemeEachHost = themeDirPath: hostDirPath: f:
        let
            themeList = map withoutFileExtension (attrNames (readDir themeDirPath));
            hostList = attrNames (readDir hostDirPath);

            combinations = cartesianProduct themeList hostList;
        in
            foldr
            (combination: z:
                let
                    theme = elemAt combination 0;
                    host = elemAt combination 1;

                    a = f os theme host;
                    x = { "${os}.${theme}.${host}" = a; };
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
}