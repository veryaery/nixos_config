# https://www.nordtheme.com/docs/colors-and-palettes

let
    foreground = "#d8dee9"; # nord4
    background = "#2e3440"; # nord0

    white = "#e5e9f0"; # nord5
    black = "#000000";
    red = "#bf616a";
    green = "#a3be8c";
    blue = "#81a1c1";
    yellow = "#ebcb8b";
    cyan = "#88c0d0";
    magenta = "#b48ead";
    brightWhite = "#eceff4"; # nord6
    brightBlack = "#4c566a"; # nord3
    brightRed = "#bf616a";
    brightGreen = "#a3be8c";
    brightBlue = "#81a1c1"; 
    brightYellow = "#ebcb8b";
    brightCyan = "#8fbcbb";
    brightMagenta = "#b48ead";
in
{
    inherit foreground background;

    primary = cyan;
    primaryTerminalColor = "cyan";

    terminalColors = {
        inherit
            black brightBlack
            blue brightBlue
            green brightGreen
            cyan brightCyan
            red brightRed
            magenta brightMagenta
            yellow brightYellow
            white brightWhite;
    };
}