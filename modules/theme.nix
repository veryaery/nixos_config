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
        mapAttrs
        toString;

    inherit (std)
        mkOption
        types;

    inherit (lib)
        fishTerminalColor;

    font = "Fira Code";

    themeSubs = themeExpr:
        {
            ".config/alacritty/alacritty.yml" =
                themeExpr //
                {
                    inherit font; 
                    fish = toString pkgs.fish;
                };
            ".config/fish/config.fish" = {
                primary = fishTerminalColor themeExpr.primaryTerminalColor;
            };
            ".xmonad/lib/Theme.hs" = themeExpr;
            ".config/xmobar/.xmobarrc" = themeExpr;
        };
    
    themeDotfiles = themeExpr:
        pkgs.dotfiles
        {
            inherit lib;
            dirPath = flakeRoot + /dotfiles;
            subs = themeSubs themeExpr;
        };
    
    _themes =
        pkgs.themes
        {
            themes =
                mapAttrs
                (_: themeDotfiles)
                themes;
        };
    
    postinstall =
        pkgs.postinstall
        {
            scripts = cfg.postInstallScripts;
        };
    
    installtheme =
        pkgs.installtheme
        {
            inherit postinstall;
            themes = _themes;
        };
    
    lstheme =
        pkgs.writeScriptBin "lstheme"
        ''
            #!${pkgs.fish}/bin/fish

            ls -1 ${themes}
        '';
    
    cfg = config.theme;
in
{
    options.theme = {
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
