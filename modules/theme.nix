{
    pkgs,
    config,
    themeName,
    themes,
    flakeRoot,
    ...
}@args:

let
    std = args.lib;
    lib = import (flakeRoot + /lib) std;

    inherit (builtins)
        mapAttrs;

    inherit (std)
        mkOption
        types;
    
    dotfilesThemes =
        pkgs.themes
        {
            inherit themes;
            drvFn = cfg.dotfiles;
        };
    
    postinstall =
        pkgs.postinstall
        { scripts = cfg.postInstallScripts; };
    
    installtheme =
        pkgs.installtheme
        {
            inherit postinstall;
            themes = dotfilesThemes;
        };
    
    lstheme =
        pkgs.lstheme
        { themes = dotfilesThemes; };
    
    cfg = config.theme;
in
{
    options.theme = {
        dotfilesFn = mkOption {
            default = {};
            description = ''
                dotfilesFn :: string -> Theme -> derivation
            '';
            type = types.functionTo types.package;
        };

        postInstallScripts = mkOption {
            default = {};
            description = ''
                Attrset of scripts to run after theme installation.

                postInstallScripts :: Map string string
            '';
            type = types.attrsOf types.lines;
        };
    };

    config = {
        environment.systemPackages = [
            lstheme
            installtheme
        ];

        system.userActivationScripts.theme.text = ''
            ${installtheme}/bin/installtheme ${themeName}
        '';
    };
}
