# https://github.com/NLKNguyen/papercolor-theme

let
    foreground = "#444444"; # 7
    background = "#eeeeee"; # 0

    white = foreground;
    black = background;
    red = "#0087af"; # 4
    green = "#008700"; # 2
    blue = "#0087af"; # 1
    yellow = "#005f87"; # 6
    cyan = "#5f8700"; # 3
    magenta = "#878787"; # 5
    brightWhite = "005f87"; # 15
    brightBlack = "#bcbcbc"; # 8
    brightRed = "#d75f00"; # 12
    brightGreen = "#d70087"; # 10
    brightBlue = "#d70000"; # 9
    brightYellow = "#005faf"; # 14
    brightCyan = "#8700af"; # 11
    brightMagenta = "#d75f00"; # 13
in
{
    inherit foreground background;

    primary = red;
    primaryTerminalColor = "red";

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