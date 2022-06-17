# https://github.com/morhetz/gruvbox

let
    foreground = "#ebdbb2"; # bg0
    background = "#282828"; # fg1

    white = "#a89984";
    black = "#000000";
    red = "#cc241d";
    green = "#98971a";
    blue = "#458588";
    yellow = "#d79921";
    cyan = "#689d6a";
    magenta = "#b16286";
    brightWhite = foreground;
    brightBlack = "#928374";
    brightRed = "#fb4934";
    brightGreen = "#b8bb26";
    brightBlue = "#83a598";
    brightYellow = "#fabd2f";
    brightCyan = "#8ec07c";
    brightMagenta = "#d3869b";
in
{
    inherit foreground background;

    primary = brightRed;
    primaryTerminalColor = "brightRed";

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