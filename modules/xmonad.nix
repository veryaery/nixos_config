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

    xmonad = pkgs.xmonad-with-packages.override {
        packages = haskellPackages: with haskellPackages; [ xmonad-contrib ];
    };
    cfg = config.services.xserver.windowManager._xmonad;
in
{
    options.services.xserver.windowManager._xmonad = {
        enable = mkEnableOption "xmonad";
    };

    config = mkIf cfg.enable ( {
        environment.systemPackages = [ xmonad ];

        services.xserver.windowManager.session = [
            {
                manage = "window";
                name = "xmonad";
                start = ''
                    systemd-cat -t xmonad -- ${xmonad}/bin/xmonad &
                    waitPID=$!
                '';
            }
        ];
    }
    );
}
