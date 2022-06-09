{ pkgs, ... }:

{
    users.users = {
        aery = {
            isNormalUser = true;

            # Explicit home.
            createHome = true;
            home = "/home/aery";

            extraGroups = [ "wheel" ];
        };
    };
    
    # Installing Nix flakes system-wide.
    # https://nixos.wiki/wiki/Flakes
    nix = {
        package = pkgs.nixFlakes;
        extraOptions = ''
            experimental-features = nix-command flakes
        '';
    };
}