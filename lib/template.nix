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

    inherit (import ./escape.nix std)
        bashString
        breEscape
        sedEscape;

    flattenAttrset' = path: attrset:
        foldr
        (name: z:
            let
                _path = if path == null then name else "${path}.${name}"; 
                value = attrset.${name};
                x =
                    if isAttrs value
                    then flattenAttrset' _path value
                    else { ${_path} = value; };
            in z // x
        )
        {}
        (attrNames attrset);

    # flattenAttrset :: attrset -> attrset
    flattenAttrset = flattenAttrset' null;

    # attrNamesValues :: attrset -> { names :: [ string ]; values :: [ any ] }
    attrNamesValues = attrset:
        foldr
        (name: z:
            let value = attrset.${name};
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
    # substitute :: attrset -> (string -> string)
    substitute = attrset:
        let
            flatNamesValues = attrNamesValues (flattenAttrset attrset);
            froms = map (name: "<${name}>") flatNamesValues.names;
        in replaceStrings froms flatNamesValues.values;
    
    # sedScript :: attrset -> string
    sedScript = attrset:
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
        in bashString script;
}
