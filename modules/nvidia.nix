{
    hostOptions,
    ...
}@args:

let
    std = args.lib;

    inherit (builtins)
        elem;

    inherit (std)
        mkIf;
in
{
    config = mkIf (elem "nvidia" hostOptions.roles) {
        services.xserver.videoDrivers = [ "nvidia" ];

        hardware.opengl.enable = true;
    };
}
