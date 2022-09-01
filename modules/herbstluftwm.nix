{
    pkgs,
    config,
    ...
}@args:

let
    std = args.lib;

    inherit (std)
        mkEnableOption
        mkIf
        mkOption;

    cfg = config.services.xserver.windowManager._herbstluftwm;
in
{
    options.services.xserver.windowManager._herbstluftwm = {
        enable = mkEnableOption "herbstluftwm";
    };

    config = mkIf cfg.enable {
        environment.systemPackages = with pkgs; [ herbstluftwm ];

        services.xserver.windowManager.session = [
            {
                manage = "window";
                name = "herbstluftwm";
                start = ''
                    systemd-cat -t herbstluftwm -- ${pkgs.herbstluftwm}/bin/herbstluftwm &
                    waitPID=$!
                '';
            }
        ];

        theme.postInstallScripts = {
            herbstluftwm = ''
                echo Reloading herbstluftwm
                ${pkgs.herbstluftwm}/bin/herbstclient reload
            '';
        };
    };
}
