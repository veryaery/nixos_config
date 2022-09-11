{
    pkgs,
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
    config = mkIf (elem "desktop" hostOptions.roles) {
        environment.systemPackages = with pkgs; [
            discord
            audacity
            gimp
        ];

        services.autorandr.enable = true;
    };
}
