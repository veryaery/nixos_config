std:

let
    inherit (builtins)
        attrNames
        attrValues
        isAttrs;

    inherit (std.lists)
        foldr;

    # hasAttrset :: any -> bool
    hasAttrset = x:
        (isAttrs x) && (
            foldr
            (value: z: (isAttrs value) || z)
            false
            (attrValues x)
        );

    # parseSubstitutionStruct' :: (string | null) -> SubstitutionMap -> SubstitutionStruct
    parseSubstitutionStruct' = parent: attrset:
        foldr
        (name: z:
            let
                path = if parent == null then name else "${parent}.${name}";
                value = attrset.${name};
            in
                if hasAttrset value then
                    let
                        parsedValue = parseSubstitutionStruct' path value;
                    in
                        {
                            strs = (
                                if parsedValue.strs == {}
                                then z.strs
                                else z.strs // { ${name} = parsedValue.strs; }
                            );
                            cmds = z.cmds // parsedValue.cmds;
                        }
                else
                    if value ? "str" then
                        z // { strs = z.strs // { ${name} = value.str; }; }
                    else if value ? "cmd" then
                        z // { cmds = z.cmds // { ${path} = value.cmd; }; }
                    else
                        throw "Substitution ${path} is missing either a str or a cmd."
        )
        {
            strs = {};
            cmds = {};
        }
        (attrNames attrset);
in
{
    #   Substitution = {
    #       str? :: string; 
    #       cmd? :: string;
    #   }
    #
    #   SubstitutionMap = Map string (SubstitutionMap | Substitution)
    #
    #   SubstitutionStructStrs = Map string (SubstitutionStructStrs | string)
    #
    #   SubstitutionStruct = {
    #       strs :: SubstitutionStructStrs;
    #       cmds :: Map string string;
    #   }

    # parseSubstitutionStruct :: SubstitutionMap -> SubstitutionStruct
    parseSubstitutionStruct = attrset: parseSubstitutionStruct' null attrset;
} 
