pkgs:

{
    scripts
}:

let
    std = pkgs.lib;

    inherit (builtins)
        concatStringsSep;

    inherit (std.attrsets)
        mapAttrsToList;

    _scripts = mapAttrsToList
        (name: script:
            ''
                # ${name}
                ${script}
            ''    
        )
        scripts;
in
pkgs.writeShellScript "postinstall"
(concatStringsSep "\n\n" _scripts)