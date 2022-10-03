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
        services.xserver = {
            videoDrivers = [ "nvidia" ];

            screenSection = ''
                Option "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
                Option "AllowIndirectGLXProtocol" "off"
                Option "TripleBuffer" "on"
            '';
        };

        hardware.opengl.enable = true;
    };
}
