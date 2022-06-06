{ pkgs, ... }:

{
    nix = {
        package = pkgs.nixFlakes;
        extraOptions = ''
            experimental-features = nix-command flakes
        '';
    };

    users.users = {
        aery = {
            isNormalUser = true;

            createHome = true;
            home = "/home/aery";

            extraGroups = [ "wheel" ];
        }
    };

    services.xserver.layout = "se";
}