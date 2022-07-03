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
            x = attrNamesValues attrset;
            names = map (name: "<${name}>") x.names;
        in replaceStrings names x.values;
}
