{
    pkgs,
    theme,
    poopthemes,
    flakeRoot,
    ...
}@args:

let
    std = args.lib;
    lib = import (flakeRoot + /lib) std;

    inherit (builtins)
        mapAttrs
        toString;

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
                (name: value: themeDotfiles value)
                poopthemes;
        };
    
    installtheme =
        pkgs.installtheme
        {
            inherit lib;
            themes = _themes;
        };
in
{
    environment.systemPackages = [ installtheme ];

    system.userActivationScripts.theme.text = ''
        ${installtheme}/bin/installtheme ${theme}
    '';
}
