{
    pkgs,
    config,
    flakeRoot,
    themeExpr,
    ...
}@args:

let
    std = args.lib;
    lib = import (flakeRoot + /lib) std;

    inherit (std)
        mkEnableOption
        mkIf
        mkOption;

    cfg = config.services.xserver.windowManager._xmonad;
in
{
    options.services.xserver.windowManager._xmonad = {
        enable = mkEnableOption "xmonad";
    };

    config = mkIf cfg.enable ( {
        environment.systemPackages = with pkgs; [ xmonad ];

        services.xserver.windowManager.session = [
            {
                manage = "window";
                name = "xmonad";
                start = ''
                    systemd-cat -t xmonad -- ${pkgs.xmonad}/bin/xmonad &
                    waitPID=$!
                '';
            }
        ];
    }
    );
}
