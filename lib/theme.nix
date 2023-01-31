std:

let
    inherit (std.attrsets)
        mapAttrsRecursive;

    strAttrs = mapAttrsRecursive (_: value: { str = value; });
in
{
    inherit strAttrs;
}
