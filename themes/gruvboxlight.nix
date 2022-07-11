# https://github.com/morhetz/gruvbox

let
    foreground = "#3c3836"; # fg1
    background = "#fbf1c7"; # bg0

    white = foreground;
    black = background;
    red = "#9d0006";
    green = "#79740e";
    blue = "#076678";
    yellow = "#b57614";
    cyan = "#427b58";
    magenta = "#8f3f71";
    brightWhite =  "#7c6f64";
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