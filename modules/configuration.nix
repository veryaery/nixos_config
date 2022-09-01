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
    };

    environment.systemPackages = with pkgs; [
        git
        gcc
        gnumake
        binutils
        tree
        killall
        bright
        firefox
        feh
        alacritty
        fish
        pavucontrol
        unzip
        flameshot
        vscode
        imagemagick
        obs-studio
        obsidian
        postgresql
        nodejs
        buf # TODO: Should be removed and moved to a Nix shell.
        bloomrpc
        bitwarden
        pulseaudio
        (nvim { bin = with pkgs; [
            # Clipboard dependencies
            xclip

            # nvim-tresitter dependencies
            coreutils
            gnutar
            gzip
            curl
            git
            gcc

            # TypeScript language server dependencies
            nodePackages.typescript
            nodePackages.typescript-language-server
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
            quote = pkgs.writeShellScriptBin "quote" "sed 's/^/\"/ ; s/$/\"/'";
            rgba = a: pkgs.writeShellScriptBin "rgba" "${pkgs.pastel}/bin/pastel format rgb | sed 's/^.\\{3\\}/rgba/ ; s/)$/, ${replaceStrings [ "." ] [ "\\." ] (toString a)})/'";

            primaryMix.cmd = "${pkgs.pastel}/bin/pastel mix -f 0.75 ${escapeShellArg theme.primary} ${escapeShellArg theme.background} | ${pkgs.pastel}/bin/pastel format hex | ${quote}/bin/quote";
            foregroundMix.cmd = "${pkgs.pastel}/bin/pastel mix -f 0.75 ${escapeShellArg theme.foreground} ${escapeShellArg theme.background} | ${pkgs.pastel}/bin/pastel format hex | ${quote}/bin/quote";
            backgroundMix.cmd = "${pkgs.pastel}/bin/pastel mix -f 0.75 ${escapeShellArg theme.background} ${escapeShellArg theme.primary} | ${pkgs.pastel}/bin/pastel format hex | ${quote}/bin/quote";
        in
        {
            "debug".subs =
                themeSubMap;

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

            # ".xmonad/lib/Theme.hs".subs =
            #     theme //
            #     { inherit font; };

            # ".config/xmobar/.xmobarrc" = {
            #     variant =
            #         if (elem "laptop" hostOptions.roles)
            #         then "laptop"
            #         else "default";
            #     subs =
            #         theme //
            #         { inherit font; };
            # };

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
                                indent-blankline-nvim
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

                    font.str = font;
                    size.str = toString 12;

                    pkgs = with pkgs; {
                        fish.str = toString fish;
                        autorandr.str = toString autorandr;
                        rofi.str = toString rofi;
                        flameshot.str = toString flameshot;
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
