std:

let
    inherit (std.attrsets)
        mapAttrsRecursive;

    inherit (std.strings)
        hasPrefix
        removePrefix
        toLower;

    strAttrs = mapAttrsRecursive (_: value: { str = value; });

    fishColor = color:
        if hasPrefix "bright" color
        then "br${toLower (removePrefix "bright")}"
        else color;
in
{
    inherit
        strAttrs
        fishColor;
}
