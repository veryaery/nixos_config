std:

let
    inherit (builtins)
        attrNames
        concatStringsSep
        isAttrs
        map
        replaceStrings;

    inherit (std.attrsets)
        mapAttrsToList;
    
    inherit (std.lists)
        foldr;

    flattenAttrset' = path: attrset:
        foldr
        (name: z:
            let
                _path = if path == null then name else "${path}.${name}"; 
                value = attrset."${name}";
                x =
                    if isAttrs value
                    then flattenAttrset' _path value
                    else { "${_path}" = value; };
            in z // x
        )
        {}
        (attrNames attrset);

    flattenAttrset = flattenAttrset' null;

    # https://unix.stackexchange.com/questions/32907/what-characters-do-i-need-to-escape-when-using-sed-in-a-sh-script
    breEscape = 
        replaceStrings
        [ "$"   "."   "*"   "^"   "["   "\\"   ]
        [ "\\$" "\\." "\\*" "\\^" "\\[" "\\\\" ];
    
    sedEscape = replaceStrings [ "/" ] [ "\\/" ];

    fishEscape =
        replaceStrings
        # \"         "      \$      $     \
        # \\\"       \"     \\$     \$    \\
        [ "\\\""     "\""   "\\$"   "$"   "\\"   ]
        [ "\\\\\\\"" "\\\"" "\\\\$" "\\$" "\\\\" ];
    
    fishString = s: "\"${fishEscape s}\"";

    attrNamesValues = attrset:
        foldr
        (name: z:
            let value = attrset."${name}";
            in {
                names = z.names ++ [ name ];
                values = z.values ++ [ value ];
            }
        )
        {
            names = [];
            values = [];
        }
        (attrNames attrset);
in
{
    replace = attrset:
        let
            flatNamesValues = attrNamesValues (flattenAttrset attrset);
            froms = map (name: "<${name}>") flatNamesValues.names;
        in replaceStrings froms flatNamesValues.values;
    
    sedCommand = attrset:
        let
            flat = flattenAttrset attrset;
            commands =
                mapAttrsToList
                (from: to:
                    let
                        escapeFrom = sedEscape (breEscape "<${from}>");
                        escapeTo = sedEscape to;
                    in "s/${escapeFrom}/${escapeTo}/g"
                )
                flat;
            script = concatStringsSep " ; " commands;
        in "sed ${fishString script}";
}