{ config, pkgs, ... } @ args:

let
    std = args.lib;

    inherit (std.options)
        mkEnableOption
        mkOption;

    inherit (std.modules)
        mkIf;

    cfg = config.programs._labwc;
in
{
    options.programs._labwc = {
        enable = mkEnableOption "labwc";
    };

    config = mkIf cfg.enable (
        let
            sessionPkg = (pkgs.writeTextFile {
                name = "labwc.desktop";
                text = ''
                    [Desktop Entry]
                    Name=LabWC
                    Comment=A wayland stacking compositor
                    Exec=systemd-cat -t labwc -- ${pkgs.labwc}/bin/labwc
                    Type=Application
                    DesktopNames=wlroots
                '';
                destination = "/share/wayland-sessions/labwc.desktop";
            }).overrideAttrs (oldAttrs: { passthru.providedSessions = [ "labwc" ]; });
        in
        {
            environment.systemPackages = [ pkgs.labwc ];
            
            services.xserver.displayManager.sessionPackages = [ sessionPkg ];
        }
    );
}
