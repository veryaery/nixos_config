{ ... }:

{
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