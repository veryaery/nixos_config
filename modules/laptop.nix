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
    # Enable wireless networking if the host has the "laptop" role.
    config = mkIf (elem "laptop" hostOptions.roles) {
        networking = {
            wireless.iwd.enable = true;

            networkmanager = {
                enable = true;
                wifi.backend = "iwd";
            };
        };
    };
}
