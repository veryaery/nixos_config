{ pkgs, ... }:

{
    services.xserver = {
        enable = true;

        displayManager.gdm.enable = true;
        windowManager.xmonad = {
            enable = true;
            
            extraPackages = haskellPackages: with haskellPackages; [
                xmonad-contrib
            ];
        };

        layout = "se";
    };

    environment.systemPackages = with pkgs; [
        git
    ];

    home-manager.users = {
        aery = {
            home = {
                username = "aery";
                homeDirectory = "/home/aery";

                packages = with pkgs; [
                    alacritty

                    tree

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
                    ".xmonad" = {
                        source = ../../../dotfiles/.xmonad;
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