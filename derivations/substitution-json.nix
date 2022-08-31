pkgs:

{
    lib
}:

# SubstitutionMap -> derivation
substitutionMap:

let
    std = pkgs.lib;

    inherit (builtins)
        toJSON;

    inherit (std)
        escapeShellArg;

    inherit (std.attrsets)
        mapAttrsToList;

    inherit (lib)
        parseSubstitutionStruct;
    
    substitutionStruct = parseSubstitutionStruct substitutionMap;
    strsJson = pkgs.writeTextFile {
        name = "substitution.json";
        text = toJSON substitutionStruct.strs;
    };
    cmds =
        mapAttrsToList
        (path: cmd:
         "${cmd} | yq -n --arg path ${escapeShellArg path} '[ inputs ] as $inputs | $inputs[0] | setpath($path | split(\".\"); $inputs[1])' $out - > tmp && mv tmp $out"
        )
        substitutionStruct.cmds;
in
pkgs.runCommand "substitution.json"
{
    nativeBuildInputs = with pkgs; [ yq ];
}
''
    cat ${strsJson} > $out
    ${pkgs.lib.strings.concatStringsSep "\n" cmds}
''
