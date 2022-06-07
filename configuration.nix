{ pgks, ... }:

{
    boot.loader.grub = {
        enable = true;
        version = 2;
        device = "/dev/sda";
    };

    networking.interfaces = {
        enp0s3 = {
            useDHCP = true;
        };
    };

    services.xserver = {
        enable = true;

        displayManager.gdm.enable = true;
        desktopManager.gnome.enable = true;

        layout = "se";
    };

    users.users = {
        aery = {
            isNormalUser = true;

            # Explicit home.
            createHome = true;
            home = "/home/aery";

            extraGroups = [ "wheel" ];
        };
    };

    enviroment.systemPackages = with pkgs; [
        git

        # Fun command line toys.
        neofetch
        figlet
        tty-clock
        fortune
        cowsay
        lolcat
        cbonsai
        cmatrix
        pipes
    ];

    # Installing Nix flakes system-wide.
    # https://nixos.wiki/wiki/Flakes
    nix = {
        package = pgks.nixFlakes;
        extraOptions = ''
            experimental-features = nix-command flakes
        '';
    };

    system.stateVersion = "21.11";
}