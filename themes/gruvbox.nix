# https://github.com/morhetz/gruvbox

let
    foreground = "#ebdbb2"; # bg0
    background = "#282828"; # fg1

    black = "#000000";
    blue = "#458588";
    green = "#98971a";
    cyan = "#689d6a";
    red = "#cc241d";
    magenta = "#b16286";
    yellow = "#d79921";
    white = "#a89984";
    brightBlack = "#928374";
    brightBlue = "#83a598";
    brightGreen = "#b8bb26";
    brightCyan = "#8ec07c";
    brightRed = "#fb4934";
    brightMagenta = "#d3869b";
    brightYellow = "#fabd2f";
    brightWhite = foreground;
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