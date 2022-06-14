{ pkgs, home-managerLib, themeExpr, ... }:

{
    services.xserver = {
        enable = true;

        displayManager.gdm.enable = true;
        windowManager.xmonad = {
            enable = true;
            xmonadCliArgs = [ "--replace" ];
            
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
                    firefox
                    tree
                    killall

                    xmobar
                    feh

                    alacritty
                    fish
                    starship

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

                file =
                    let dotfiles = ../../../dotfiles;
                    in {
                        ".xmonad/xmonad.hs" = {
                            source = dotfiles + /.xmonad/xmonad.hs;
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

                                    position = TopSize C 40 32,
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
                            text =
                                let fish = pkgs.fish;
                                in ''
                                    colors:
                                        primary:
                                            foreground: "${themeExpr.foreground}"
                                            background: "${themeExpr.background}"
                                        
                                        normal:
                                            black: "${themeExpr.terminalColors.black}"
                                            blue: "${themeExpr.terminalColors.blue}"
                                            green: "${themeExpr.terminalColors.green}"
                                            cyan: "${themeExpr.terminalColors.cyan}"
                                            red: "${themeExpr.terminalColors.red}"
                                            magenta: "${themeExpr.terminalColors.magenta}"
                                            yellow: "${themeExpr.terminalColors.yellow}"
                                            white: "${themeExpr.terminalColors.white}"
                                        
                                        bright:
                                            black: "${themeExpr.terminalColors.brightBlack}"
                                            blue: "${themeExpr.terminalColors.brightBlue}"
                                            green: "${themeExpr.terminalColors.brightGreen}"
                                            cyan: "${themeExpr.terminalColors.brightCyan}"
                                            red: "${themeExpr.terminalColors.brightRed}"
                                            magenta: "${themeExpr.terminalColors.brightMagenta}"
                                            yellow: "${themeExpr.terminalColors.brightYellow}"
                                            white: "${themeExpr.terminalColors.brightWhite}"

                                    shell:
                                        program: "${fish}/bin/fish"

                                    window:
                                        padding:
                                            x: 8
                                            y: 8

                                        cursor:
                                            style:
                                                blinking: On
                                '';
                        };

                        ".config/fish/config.fish" = {
                            source = dotfiles + /.config/fish/config.fish;
                        };

                        ".config/starship.toml" = {
                            source = dotfiles + /.config/starship.toml;
                        };

                        ".xinitrc" = {
                            source = dotfiles + /.xinitrc;
                        };
                    };
                
                # activation = {
                #     xmonadRecompileRestart = 
                #         let
                #             xmonad = pkgs.xmonad-with-packages;
                #             procps = pkgs.procps;
                #         in
                #         home-managerLib.hm.dag.entryAfter [ "writeBoundary" ] ''
                #             if ${procps}/bin/pgrep "^xmonad.*" > /dev/null; then
                #                 echo "WTF?" > /home/aery/hi
                #                 ${procps}/bin/pgrep "^xmonad.*" > /home/aery/hello
                #             fi
                #         '';

                #         # ${xmonad}/bin/xmonad --recompile
                #         # ${xmonad}/bin/xmonad --restart
                # };

                stateVersion = "22.11";
            };
        };
    };
}