let
    lib = (import <nixpkgs> {}).lib;

    inherit (builtins)
        pathExists
        readDir
        map
        elemAt;
    
    inherit (lib.lists)
        foldr;

    # [ a ] -> [ b ] -> [[ a b ]]
    combineWithoutPermutations = lsta: lstb:
        foldr
        (xs: a:
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
    # Return a list containing the path if the path exists.
    # Otherwise, returns an empty list.
    # path -> [ path ]
    listWithPathIfPathExists = path: if pathExists path then [ path ] else [];

    # Returns an attrset with an attr for each combination of os and host.
    # The key is in the form of "<os>.<host>".
    # The value is the value of f for os and host.
    # path -> path -> string -> string -> a -> { string = a }
    attrsetFromEachOSEachHost = osDirPath: hostDirPath: f:
        let
            osList = attrNames (readDir osDirPath);
            hostList = attrNames (readDir hostDirPath);
            combinations = combineWithoutPermutations osList hostList;
        in
            foldr
            (xs: combination:
                let
                    os = elemAt combination 0;
                    host = elemAt combination 1;
                    a = f os host;
                    x = { "${os}.${host}" = a; };
                in xs // x
            )
            {}
            combinations;
}