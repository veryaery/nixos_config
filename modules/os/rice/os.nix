{ pkgs, themeExpr, ... }:

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
                    firefox
                    tree
                    killall

                    xmobar
                    feh

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
                    ".xmonad/xmonad.hs" = {
                        source = ../../../dotfiles/.xmonad/xmonad.hs;
                    };

                    ".xmonad/lib/Theme.hs" = {
                        text = ''
                            module Theme where

                            themeForeground :: String
                            themeForeground = ${themeExpr.foreground}

                            themeBackground :: String
                            themeBackground = ${themeExpr.background}

                            themePrimary :: String
                            themePrimary = ${themeExpr.primary}
                        '';
                    };

                    ".config/xmobar/xmobarrc" = {
                        text = ''
                            Config {
                                bgColor = "${themeExpr.background}",
                                fgColor = "${themeExpr.foreground}",

                                commands = [
                                    Run XMonadLog,

                                    Run Cpu [ "-t", "<total>%" ] 10,
                                    Run Memory [ "-t", "<used>/<total> MB <usedratio>%" ] 10,
                                    Run Date "%H:%M" "time" 10
                                ],

                                template = "%XMonadLog% }{ cpu: %cpu% mem: %memory% %time%"
                            }
                        '';
                    };

                    ".config/alacritty" = {
                        source = ../../../dotfiles/.config/alacritty;
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