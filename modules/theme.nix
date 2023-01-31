{
    hostName, host,
    themeName, theme,
    flakeRoot,
    pkgs,
    ...
} @ args:

let
    dotfilesAttrs = import (flakeRoot + /dotfiles) {
        inherit
            hostName host
            themeName theme
            flakeRoot
            pkgs;
    };
in
{
    theme = {
        inherit (dotfilesAttrs)
            files postInstallScripts;

        enable = true;
        src = flakeRoot + /dotfiles;
    };
}
