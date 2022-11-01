{
    pkgs,
    themeName,
    flakeRoot,
    hostOptions,
    ...
}@args:

let
    std = args.lib;
    lib = import (flakeRoot + /lib) std;

    inherit (builtins)
        elem
        readFile
        replaceStrings
        toString;

    inherit (std)
        escapeShellArg;

    inherit (lib)
        attrsetToStrSubstitutionMap
        fishTerminalColor
        id
        replace;
    
    font = "FiraCode Nerd Font";
in
{
    imports = [
        ./theme.nix
        ./herbstluftwm.nix

        # Roles.
        ./desktop.nix
        ./laptop.nix
        ./bluetooth.nix
        ./nvidia.nix
    ];

    console.keyMap = "sv-latin1";

    security = {
        sudo.enable = false;
        doas.enable = true;
    };

    sound.enable = true;

    services = {
        xserver = {
            enable = true;
            
            layout = "se";
            xkbOptions = "caps:escape";

            windowManager._herbstluftwm = {
                enable = true;
            };
        };

        pipewire = {
            enable = true;
            alsa.enable = true;
            pulse.enable = true;
        };

        # Required by vscode.
        gnome.gnome-keyring.enable = true;
    };

    environment.systemPackages = with pkgs; [
        git
        gcc
        gnumake
        binutils
        tree
        firefox
        feh
        alacritty
        fish
        pavucontrol
        unzip
        flameshot
        imagemagick
        obs-studio
        obsidian
        postgresql
        nodejs
        rustc
        cargo
        bloomrpc
        bitwarden
        pulseaudio
        qpwgraph
        spotify
        yt-dlp
        mpv
        htop
        rsync
        zip
        (vscode-with-extensions.override {
            vscodeExtensions = with pkgs.vscode-extensions; [
                vscodevim.vim
                ms-vsliveshare.vsliveshare

                (pkgs.vscode-utils.extensionFromVscodeMarketplace {
                    name = "glassit";
                    publisher = "s-nlf-fh";
                    version = "0.2.4";
                    sha256 = "sha256-YmohKiypAl9sbnmg3JKtvcGnyNnmHvLKK1ifl4SmyQY=";
                })
                (pkgs.vscode-utils.extensionFromVscodeMarketplace {
                    name = "tokyo-night";
                    publisher = "enkia";
                    version = "0.9.4";
                    sha256 = "sha256-pKokB6446SR6LsTHyJtQ+FEA07A0W9UAI+byqtGeMGw=";
                })
                
                # Rust
                matklad.rust-analyzer
            ];
        })
        (nvim { bin = with pkgs; [
            # Clipboard dependencies.
            xclip

            # nvim-tresitter dependencies:
            coreutils
            gnutar
            gzip
            curl
            git
            gcc

            # TypeScript language server dependencies.
            nodePackages.typescript
            nodePackages.typescript-language-server

            # Rust language server dependencies.
            rust-analyzer
            rustc
            cargo
        ]; })
    ];

    fonts.fonts = with pkgs; [
        font-awesome
        (fira-code-with-features { features = [
            "cv01"
            "cv02"
            "cv06"
            "cv11"
            "ss05"
            "ss03"
        ]; })
    ];

    virtualisation.docker.enable = true;

    programs = {
        ssh = {
            startAgent = true;
        };
    };

    users.users = {
        aery = {
            isNormalUser = true;

            # Explicitly define home.
            createHome = true;
            home = "/home/aery";

            packages = with pkgs; [
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

            extraGroups = [
                "wheel"
                "networkmanager"
            ];
        };
    };

    theme.dotfilesSubs = themeName: theme:
        let
            themeSubMap = attrsetToStrSubstitutionMap theme;
            pastel = "${pkgs.pastel}/bin/pastel";

            quote = pkgs.writeShellScriptBin "quote" "sed 's/^/\"/ ; s/$/\"/'";
            rgba = a: pkgs.writeShellScriptBin "rgba" "${pastel} format rgb | sed 's/^.\\{3\\}/rgba/ ; s/)$/, ${replaceStrings [ "." ] [ "\\." ] (toString a)})/'";

            formatCmd = "${pastel} format hex | ${quote}/bin/quote";
            foregroundCmd = color: "${pastel} maxcontrast ${escapeShellArg color} ${escapeShellArg theme.foreground} ${escapeShellArg theme.background} | ${formatCmd}";

            primaryMix.cmd = "${pastel} mix -f 0.75 ${escapeShellArg theme.primary} ${escapeShellArg theme.background} | ${formatCmd}";
            foregroundMix.cmd = "${pastel} mix -f 0.75 ${escapeShellArg theme.foreground} ${escapeShellArg theme.background} | ${formatCmd}";
            backgroundMix.cmd = "${pastel} mix -f 0.75 ${escapeShellArg theme.background} ${escapeShellArg theme.primary} | ${formatCmd}";

            redForeground.cmd = foregroundCmd theme.terminalColors.red;
            greenForeground.cmd = foregroundCmd theme.terminalColors.green;
            blueForeground.cmd = foregroundCmd theme.terminalColors.blue;
            magentaForeground.cmd = foregroundCmd theme.terminalColors.magenta;
        in
        {
            ".config/alacritty/alacritty.yml".subs =
                let
                    size =
                        if (elem "desktop" hostOptions.roles) then 8
                        else if (elem "laptop" hostOptions.roles) then 6
                        else 11;
                in
                    themeSubMap //
                    {
                        font.str = font;
                        fish.str = toString pkgs.fish;
                        size.str = toString size;
                    };

            ".config/fish/config.fish".subs =
                { primary.str = fishTerminalColor theme.primaryTerminalColor; };

            ".config/nvim/init.lua".subs =
                let
                    neovim-pack =
                        pkgs.neovim-pack
                        {
                            start = with pkgs.vimPlugins; [
                                nvim-lspconfig
                                nvim-treesitter
                                nvim-ts-rainbow
                                nvim-autopairs
                                pkgs.indent-blankline-nvim
                                nvim-tree-lua
                                nvim-web-devicons
                                gitsigns-nvim
                                comment-nvim
                                plenary-nvim
                                nvim-cmp
                                luasnip
                                cmp-buffer
                                cmp-nvim-lsp
                                telescope-nvim

                                # Themes
                                papercolor-theme
                                tokyonight-nvim
                                nord-nvim
                            ];
                        };
                in
                {
                    packpath.str = toString neovim-pack;
                    pkgs = {
                        typescript.str = toString pkgs.nodePackages.typescript;
                    };
                };
            
            ".config/herbstluftwm/autostart".subs =
                themeSubMap //
                {
                    inherit primaryMix backgroundMix;

                    roles.str = toString hostOptions.roles;
                    font.str = font;
                    size.str = toString 12;

                    pkgs = with pkgs; {
                        fish.str = toString fish;
                        autorandr.str = toString autorandr;
                        rofi.str = toString rofi;
                        flameshot.str = toString flameshot;
                        polybar.str = toString polybar;
                        picom.str = toString picom-jonaburg;
                    };
                };

            ".config/rofi/config.rasi".subs =
                themeSubMap //
                {
                    inherit foregroundMix;

                    font.str = font;
                    transparentBackground.cmd = "echo ${escapeShellArg theme.background} | ${rgba 0.5}/bin/rgba";
                };

            ".config/polybar/config.ini" = {
                variant =
                    if (elem "laptop" hostOptions.roles)
                    then "laptop"
                    else "default";

                subs = 
                    themeSubMap //
                    {
                        inherit
                            redForeground
                            greenForeground
                            blueForeground
                            magentaForeground;

                        font.str = font;
                    };
            };

            ".config/polybar/scripts/tags/event_loop.sh".subs =
                { runtimeShell.str = toString pkgs.runtimeShell; };

            ".config/polybar/scripts/tags/on_hook.fish".subs =
                themeSubMap //
                {
                    inherit primaryMix backgroundMix;

                    pkgs = with pkgs; {
                        fish.str = toString fish;
                    };
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
