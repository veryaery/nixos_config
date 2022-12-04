std:

let
    inherit (builtins)
        baseNameOf
        concatStringsSep
        map
        mapAttrs
        readDir;

    inherit (std.strings)
        splitString;

    inherit (std.attrsets)
        mapAttrs'
        mapAttrsToList
        nameValuePair;

    inherit (std.lists)
        foldr
        init;

    baseNameOfExtentionless = s: concatStringsSep "." (init (splitString "." (baseNameOf s)));

    cartesianEachHost = hostsPath: f:
        let
            hosts =
                mapAttrs'
                (file: _:
                    let
                        hostName = baseNameOfExtentionless file;
                    in
                        nameValuePair
                        hostName
                        (import (hostsPath + "/${file}"))
                )
                (readDir hostsPath);
            hostAttrs =
                mapAttrs
                (hostName: host: f { inherit hosts hostName host; })
                hosts;
        in
            foldr
            (x: z: z // x)
            {}
            (
                mapAttrsToList
                (hostName: attrs:
                    mapAttrs'
                    (name: nameValuePair "${hostName}.${name}")
                    attrs
                )
                hostAttrs
            );
in
{
    inherit cartesianEachHost;
}
