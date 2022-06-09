{ pkgs, ... }:

{
    services.xserver = {
        enable = true;

        displayManager.gdm.enable = true;
        desktopManager.gnome.enable = true;

        layout = "se";
    };

    environment.systemPackages = with pkgs; [
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
}