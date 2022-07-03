{
    pkgs,
    flakeRoot,
    dotfiles,
    themeExpr,
    ...
}@args:

let
    std = args.lib;
    lib = import (flakeRoot + /lib) std;

    inherit (buildtins)
        readFile;

    inherit (std)
        mkEnableOption
        mkIf
        mkOption;
    
    inherit (lib)
        replace;

    cfg = config.serives.xserver.windowManager._xmonad;
in
{
    options.services.xserver.windowManager._xmonad = {
        enable = mkEnableOption "xmonad";

        theme = mkOption {
            default = null;
            description = "The Theme.hs file.";
            type = with std.types; nullOr package;
        };
    };

    config = mkIf cfg.enable (
    let
        xmonad = import (flakeRoot + /derivations/xmonad.nix) pkgs cfg.theme;
    in
    {
        environment.systemPackages = [ xmonad ];

        services.xserver.windowManager.session = [
            {
                manage = "window";
                name = "xmonad";
                start = ''
                    ${xmonad}/bin/xmonad &
                    waitPID=$!
                '';
            }
        ];

        system.userActivationScripts = {
            xmonad = {
                text = ''
                    if [[ -v DISPLAY ]]; then
                        ${xmonad}/bin/xmonad --restart
                    fi
                '';
                deps = [ "display-manager.service" ];
            };
        };
    }
    );
}