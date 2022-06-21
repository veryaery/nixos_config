{ pkgs, themeExpr, ... }@args:

let
    flakeRoot = ../../..;
    dotfiles = flakeRoot + /dotfiles;
    std = args.lib;
    lib = import (flakeRoot + /lib) std;

    inherit (lib)
        fishTerminalColor;
    
    font = "Fira Code";
in
{
    services.xserver = {
        enable = true;

        displayManager = {
            session = [{
                manage = "desktop";
                name = "home-manager";
                start = ''
                    ${pkgs.runtimeShell} $HOME/.xsession &
                    waitPID=$!
                '';
            }];

            defaultSession = "home-manager";

            autoLogin = {
                enable = true;
                user = "aery";
            };
        };

        layout = "se";
    };

    environment.systemPackages = with pkgs; [
        git
    ];

    home-manager.users = {
        aery = home-managerArgs: {
            xsession = {
                enable = true;
                
                # Restore feh wallpaper
                initExtra = ''
                    ${pkgs.runtimeShell} $HOME/.fehbg &
                '';

                windowManager.xmonad = {
                    enable = true;

                    config = dotfiles + /.xmonad/xmonad.hs;
                    libFiles = {
                        "Theme.hs" = pkgs.writeText "Theme.hs" ''
                            module Theme where

                            themeForeground :: String
                            themeForeground = "${themeExpr.foreground}"

                            themeBackground :: String
                            themeBackground = "${themeExpr.background}"

                            themePrimary :: String
                            themePrimary = "${themeExpr.primary}"
                        '';
                    };
                    extraPackages = haskellPackages: with haskellPackages; [
                        xmonad-contrib
                    ];
                };
            };

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

                    # Fonts
                    font-awesome
                    (fira-code-with-features { features = [
                        "cv01"
                        "cv02"
                        "cv06"
                        "cv11"
                        "ss05"
                        "ss03"
                    ]; })

                    # Neovim plugins.
                    vimPlugins.vim-nix
                ];

                file = {
                    ".config/fish/config.fish" = {
                        text = ''
                            # Remove greeting.
                            set -U fish_greeting

                            alias .. "cd .."

                            function fish_prompt
                                printf "\n"
                                printf "%s\n" (prompt_pwd -d 0)
                                printf "%sλ%s " \
                                    (set_color -o ${fishTerminalColor themeExpr.primaryTerminalColor}) \
                                    (set_color normal)
                            end

                            function fish_right_prompt
                                printf $status

                                set -l s (math "floor($CMD_DURATION / 1000)")
                                set -l ms (math "$CMD_DURATION % 1000")

                                [ $s -gt 0 ]
                                    or [ $ms -gt 0 ]
                                set -l hasDuration $status

                                [ $hasDuration = 0 ]; and printf " ("
                                if [ $s -gt 0 ]
                                    printf "%s$s s%s" \
                                        (set_color ${fishTerminalColor themeExpr.primaryTerminalColor}) \
                                        (set_color normal)
                                    [ $ms -gt 0 ]; and printf " "
                                end
                                [ $ms -gt 0 ]; and printf "$ms ms"
                                [ $hasDuration = 0 ]; and printf ")"
                            end
                        '';
                    };

                    ".config/xmobar/xmobarrc" = {
                        text =
                            let
                                primaryBox = s: "<box type=Bottom width=2 color=${themeExpr.primary}>${s}</box>";
                                primary = s: "<fc=${themeExpr.primary}>${s}</fc>";
                            in ''
                                Config {
                                    font = "xft:${font}:pixelsize=12",
                                    additionalFonts = [
                                        "xft:Font Awesome 6 Free Solid:pixelsize=12"
                                    ],
                                    fgColor = "${themeExpr.foreground}",
                                    bgColor = "${themeExpr.background}",

                                    position = TopH 24,
                                    commands = [
                                        Run XMonadLog,

                                        Run Cpu [ "-t", "${primaryBox "${primary "<fn=1></fn>"} <total>%"}" ] 10,
                                        Run Memory [ "-t", "${primaryBox "${primary "<fn=1></fn>"} <used>/<total> MB <usedratio>%"}" ] 10
                                    ],
                                    template = "%XMonadLog% }{ %cpu% %memory%"
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
                                
                                font:
                                    normal:
                                        family: "${font}"
                                    bold:
                                        family: "${font}"
                                    italic:
                                        family: "${font}"
                                    bold_italic:
                                        family: "${font}"

                                window:
                                    padding:
                                        x: 8
                                        y: 8

                                    cursor:
                                        style:
                                            blinking: On
                            '';
                    };

                    # ".config/nvim/init.lua" = {
                    #     source = dotfiles + /.config/nvim/init.lua;
                    # };

                    # ".local/share/nvim/site/pack/all/start/vim-nix" = {
                    #     source = home-managerArgs.config.lib.file.mkOutOfStoreSymlink pkgs.vimPlugins.vim-nix;
                    # };
                };

                stateVersion = "22.11";
            };

            programs.neovim = {
                enable = true;

                plugins = with pkgs.vimPlugins; [
                    vim-nix
                ];
            };

            fonts.fontconfig.enable = true;
        };
    };
}