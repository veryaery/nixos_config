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
        services.autorandr.enable = true;

        environment.systemPackages = with pkgs; [
            discord
            audacity
            gimp
            noisetorch
            godot
            v4l-utils
            qbittorrent
            vivaldi
            vlc
        ];
    };
}
