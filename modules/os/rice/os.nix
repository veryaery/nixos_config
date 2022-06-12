{ pkgs, ... }:

{
    services.xserver = {
        enable = true;

        displayManager.gdm.enable = true;
        desktopManager.xmonad.enable = true;

        layout = "se";
    };

    home-manager.users = {
        aery = {
            home = {
                username = "aery";
                homeDirectory = "/home/aery";

                packages = with pkgs; [
                    git
                    alacritty

                    # Rice command line utilities.
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

                file = {
                    ".config/xmonad" = {
                        source = ../../../dotfiles/.config/xmonad;
                        recursive = true;
                    };
                };

                stateVersion = "22.11";
            };

            programs = {
                home-manager.enable = true;
            };
        };
    };
}