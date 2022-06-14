# https://www.nordtheme.com/docs/colors-and-palettes

let
    foreground = "#d8dee9"; # nord4
    background = "#2e3440"; # nord0

    black = "#000000";
    blue = "#81a1c1";
    green = "#a3be8c";
    cyan = "#88c0d0";
    red = "#bf616a";
    magenta = "#b48ead";
    yellow = "#ebcb8b";
    white = foreground;
    brightBlack = "#4c566a"; # nord3
    brightBlue = "#81a1c1"; 
    brightGreen = "#a3be8c";
    brightCyan = "#8fbcbb";
    brightRed = "#bf616a";
    brightMagenta = "#b48ead";
    brightYellow = "#ebcb8b";
    brightWhite = foreground;
in
{
    inherit foreground background;

    primary = cyan;

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