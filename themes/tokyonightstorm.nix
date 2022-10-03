# https://github.com/enkia/tokyo-night-vscode-theme

let
    foreground = "#a9b1d6";
    background = "#24283b";

    white = foreground;
    black = "#414868";
    red = "#f7768e";
    green = "#9ece6a";
    blue = "#7aa2f7";
    yellow = "#e0af68";
    cyan = "#2ac3de";
    magenta = "#bb9af7";
    brightWhite = "#c0caf5";
    brightBlack = "#565f89";
    brightRed = "#f7768e";
    brightGreen = "#9ece6a";
    brightBlue = "#7dcfff";
    brightYellow = "#ff9e64";
    brightCyan = "#73daca";
    brightMagenta = "#bb9af7";
in
{
    inherit foreground background;

    primary = foreground;
    primaryTerminalColor = "white";

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
