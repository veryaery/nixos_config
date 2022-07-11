{
    pkgs,
    config,
    theme,
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

    evalSubs = themeExpr:
        mapAttrs
        (_: subFn: subFn themeExpr)
        cfg.subs;
    
    evalDotfiles = themeExpr:
        pkgs.dotfiles
        {
            inherit lib;
            dirPath = flakeRoot + /dotfiles;
            subs = evalSubs themeExpr;
        };
    
    _themes =
        pkgs.themes
        {
            themes =
                mapAttrs
                (_: evalDotfiles)
                themes;
        };
    
    postinstall =
        pkgs.postinstall
        { scripts = cfg.postInstallScripts; };
    
    installtheme =
        pkgs.installtheme
        {
            inherit postinstall;
            themes = _themes;
        };
    
    lstheme =
        pkgs.lstheme
        { themes = _themes; };
    
    cfg = config.theme;
in
{
    options.theme = {
        subs = mkOption {
            default = {};
            description = ''
                Attrset of string substitutions.
            '';
            type = types.attrsOf (types.functionTo types.attrs);
        };

        postInstallScripts = mkOption {
            default = {};
            description = ''
                Attrset of scripts to run after theme installation.
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
            ${installtheme}/bin/installtheme ${theme}
        '';
    };
}
