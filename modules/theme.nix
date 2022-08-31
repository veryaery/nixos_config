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

    dotfilesDrvFn = themeName: theme:
        pkgs.substitute-dir
        { inherit lib; }
        "dotfiles"
        (flakeRoot + /dotfiles)
        (cfg.dotfilesSubs themeName theme);
    
    dotfilesThemes = pkgs.themes themes dotfilesDrvFn;
    
    postinstall = pkgs.postinstall cfg.postInstallScripts;
    
    installtheme =
        pkgs.installtheme
        {
            inherit postinstall;
            themes = dotfilesThemes;
        };
    
    lstheme = pkgs.lstheme dotfilesThemes;
    
    cfg = config.theme;
in
{
    options.theme = {
        dotfilesSubs = mkOption {
            default = {};
            description = ''
                Function which is evaluated for each theme and returns substitutions for dotfiles.

                dotfilesFn :: string -> Theme -> FileSubstitutionMap
                '';
            type = types.functionTo (types.functionTo types.attrs);
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
