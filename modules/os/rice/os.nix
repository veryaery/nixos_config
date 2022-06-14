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
                            themeForeground = "${themeExpr.foreground}"

                            themeBackground :: String
                            themeBackground = "${themeExpr.background}"

                            themePrimary :: String
                            themePrimary = "${themeExpr.primary}"
                        '';
                    };

                    ".config/xmobar/xmobarrc" = {
                        text = ''
                            Config {
                                fgColor = "${themeExpr.foreground}",
                                bgColor = "${themeExpr.background}",

                                position = TopSize C 25 32,
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

                    ".config/alacritty/alacritty.yml" = {
                        text = ''
                            colors:
                                primary:
                                    foreground: "${themeExpr.foreground}"
                                    background: "${themeExpr.background}"
                                
                                normal:
                                    black: "${themeExpr.black}"
                                    blue: "${themeExpr.blue}"
                                    green: "${themeExpr.green}"
                                    cyan: "${themeExpr.cyan}"
                                    red: "${themeExpr.red}"
                                    magenta: "${themeExpr.magenta}"
                                    yellow: "${themeExpr.yellow}"
                                    white: "${themeExpr.white}"
                                
                                bright:
                                    black: "${themeExpr.brightBlack}"
                                    blue: "${themeExpr.brightBlue}"
                                    green: "${themeExpr.brightGreen}"
                                    cyan: "${themeExpr.brightCyan}"
                                    red: "${themeExpr.brightRed}"
                                    magenta: "${themeExpr.brightMagenta}"
                                    yellow: "${themeExpr.brightYellow}"
                                    white: "${themeExpr.brightWhite}"

                            window:
                                padding:
                                    x: 8
                                    y: 8

                                cursor:
                                    style:
                                        blinking: On
                        '';
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