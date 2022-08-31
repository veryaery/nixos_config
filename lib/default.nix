std:

{
    inherit (import ./lib.nix std)
        attrsetToStrSubstitutionMap
        fishTerminalColor
        id
        mapThemeHostToAttrset
        optionalPath
        overlayFromImports
        readThemes;

    inherit (import ./substitution.nix std)
        parseSubstitutionStruct;
}
