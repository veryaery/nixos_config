{ pkgs, config, ... } @ args:

let
    std = args.lib;

    inherit (std.modules)
        mkIf;

    inherit (std.options)
        mkEnableOption;

    inherit (std.lists)
        singleton;

    cfg = config.services.xserver.windowManager._herbstluftwm;
in
{
    options.services.xserver.windowManager._herbstluftwm = {
        enable = mkEnableOption "herbstluftwm";
    };

    config = mkIf cfg.enable {
        environment.systemPackages = singleton pkgs.herbstluftwm;

        services.xserver.windowManager.session = singleton {
            manage = "window";
            name = "herbstluftwm";
            start = ''
                systemd-cat -t herbstluftwm -- ${pkgs.herbstluftwm}/bin/herbstluftwm &
                waitPID=$!
            '';
        };

        theme.postInstallScripts.herbstluftwm = "${pkgs.herbstluftwm}/bin/herbstclient reload";
    };
}
