std:

let
    lib = import ./lib.nix std;
    template = import ./template.nix std;
in
{
    inherit (lib)
        attrsetFromEachThemeEachHost
        fishTerminalColor
        optionalPath;

    inherit (template)
        replace
        sedScript;
}