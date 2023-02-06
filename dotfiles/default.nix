{
    hostName, host,
    themeName, theme,
    flakeRoot,
    pkgs
}:

let
    std = pkgs.lib;
    lib = import (flakeRoot + /lib) std;

    inherit (std.attrsets)
        mapAttrsRecursive;

    inherit (std.strings)
        escapeShellArg;

    inherit (lib.theme)
        strAttrs
        fishColor;

    pastel = "${pkgs.pastel}/bin/pastel";
    fmt = "${pastel} format hex";
    textColors = map escapeShellArg (with theme; [ foreground background ]);
    textColor = bg: "${pastel} sort-by-binary-operation contrast ${escapeShellArg bg} ${toString textColors} | ${fmt} | tail -n1";
    darken = amount: c: "${pastel} darken ${toString amount} ${escapeShellArg c} | ${fmt}";

    themeSubs = (strAttrs theme) // {
        primary_foreground.cmd = textColor theme.primary;
    };
    fishColors = mapAttrsRecursive (_: color: { str = fishColor color; }) theme.shell;
in

{
    files = _: {
        ".config/sway/config" = {};
        ".config/fish/fish_plugins" = {};
        ".config/fish/config.fish".subs = fishColors // {
            pkgs.fundle.str = toString pkgs.fundle;
        };
        ".config/waybar/config" = {};
        ".config/waybar/style.css".subs = themeSubs;
        ".config/labwc/rc.xml" = {};
        ".config/labwc/environment" = {};
        ".config/labwc/autostart" = {};
        ".config/labwc/themerc-override".subs = themeSubs // {
            foreground_1.cmd = darken 0.15 theme.foreground;
            primary_1.cmd = darken 0.15 theme.primary;
        };
        ".config/kitty/kitty.conf".subs = themeSubs;
    };

    postInstallScripts = {
        installthemeTimestamp = ''
            date +%s000 > ''${XDG_DATA_DIR:-$HOME/.local/share}/themenix/timestamp
        '';
    };
}
