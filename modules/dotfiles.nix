{
    hostName, host,
    themeName, theme,
    flakeRoot,
    pkgs,
    ...
} @ args:

{
    theme.enable = true;
    theme.src = flakeRoot + /dotfiles;
    theme.files = import (flakeRoot + /files.nix) {
        inherit
            hostName host
            themeName theme
            flakeRoot
            pkgs;
    };
    theme.postInstallScripts = {};
}
