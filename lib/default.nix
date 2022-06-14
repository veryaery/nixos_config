std:

let
    inherit (builtins)
        attrNames
        elemAt
        map
        pathExists
        readDir;
    
    inherit (std.lists)
        foldr;

    # combineWithoutPermutations :: [ a ] -> [ b ] -> [[ a b ]]
    combineWithoutPermutations = lsta: lstb:
        foldr
        (a: xs:
            let x =
                map
                (b:
                    [ a b ]
                )
                lstb;
            in xs ++ x
        )
        []
        lsta;
in
{
    # A list containing the path if the path exists.
    # Otherwise, an empty list.
    # listWithPathIfPathExists :: path -> [ path ]
    listWithPathIfPathExists = path: if pathExists path then [ path ] else [];

    # Reads the os, theme, and host directories
    # then returns an attrset with an attr for each combination of os, theme, and host.
    # Each attrset key is the form of "<os>.<theme>.<host>".
    # Each attrset valye is the returned value of f os theme host.
    # attrsetFromEachOSEachThemeEachHost :: path -> path -> path -> string -> string -> string -> a -> Map string a
    attrsetFromEachOSEachThemeEachHost = osDirPath: themeDirPath: hostDirPath: f:
        let
            osFiles = attrNames (readDir osDirPath);
            themeFiles = attrNames (readDir themeDirPath);
            hostFiles = attrNames (readDir hostDirPath);

            combinations = flatten
                (
                    combineWithoutPermutations osFiles
                    (
                        combineWithoutPermutations themeFiles hostFiles
                    )
                );
        in
            foldr
            (combination: xs:
                let
                    os = elemAt combination 0;
                    theme = elemAt combination 1;
                    host = elemAt combination 2;

                    a = f os theme host;
                    x = { "${os}.${theme}.${host}" };
                in xs // x
            )
            {}
            combinations;
}