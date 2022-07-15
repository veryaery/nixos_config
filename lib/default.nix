std:

{
    inherit (import ./lib.nix std)
        attrsetFromEachThemeEachHost
        fishTerminalColor
        id
        optionalPath
        overlayFromImports
        readThemes;

    inherit (import ./escape.nix std)
        bashEscape
        bashString
        breEscape
        escapeBREScriptBash
        escapeBREScriptFish
        escapeSEDScriptBash
        escapeSEDScriptFish
        fishEscape
        fishString
        sedEscape;

    inherit (import ./template.nix std)
        replace
        sedScript;
}
