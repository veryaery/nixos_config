pkgs:

# Map string string -> derivation
scripts:

let
    std = pkgs.lib;

    inherit (builtins)
        concatStringsSep;

    inherit (std.attrsets)
        mapAttrsToList;

    _scripts = mapAttrsToList
        (name: script:
            ''
                # --- Post install ${name} ---
                echo --- Post install ${name} ---
                ${script}
                echo ---
                # ---
            ''    
        )
        scripts;
in
pkgs.writeShellScript "postinstall"
(concatStringsSep "\n\n" _scripts)
