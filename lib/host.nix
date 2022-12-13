std:

let
    inherit (builtins)
        mapAttrs
        readDir;

    inherit (std.attrsets)
        mapAttrs'
        mapAttrsToList
        nameValuePair;

    inherit (std.lists)
        foldr;

    cartesianEachHost = hostsPath: f:
        let
            hosts =
                mapAttrs'
                (file: _:
	            nameValuePair
                    file
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
