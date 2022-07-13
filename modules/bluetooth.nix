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
    config = mkIf (elem "bluetooth" hostOptions.roles) {
        hardware.bluetooth.enable = true;

        services.blueman.enable = true;
    };
}
