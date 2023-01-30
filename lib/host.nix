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

    eachHost = hostsPath: f:
        let
            hosts =
                mapAttrs'
                (file: _:
	            nameValuePair
                    file
                    (import (hostsPath + "/${file}"))
                )
                (readDir hostsPath);
        in
            mapAttrs
            (hostName: host: f { inherit hosts hostName host; })
            hosts;

    cartesianEachHost = hostsPath: f:
        let
            hostAttrs = eachHost hostsPath f;
        in
            foldr
            (x: z: z // x)
            {}
            (
                mapAttrsToList
                (hostName:
                    mapAttrs'
                    (name: nameValuePair "${hostName}.${name}")
                )
                hostAttrs
            );

in
{
    inherit
        eachHost
	cartesianEachHost;
}
