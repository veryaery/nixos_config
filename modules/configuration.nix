{
    pkgs,
    flakeRoot,
    hostOptions,
    ...
}@args:

let
    std = args.lib;
    lib = import (flakeRoot + /lib) std;

    inherit (builtins)
        elem
        toString
        readFile;

    inherit (lib)
        fishTerminalColor
        id
        replace;
    
    font = "Fira Code";
in
{
    imports = [
        ./xmonad.nix
        ./laptop.nix
        ./theme.nix
    ];

    console.keyMap = "sv-latin1";

    security = {
        sudo.enable = false;
        doas.enable = true;
    };

    services.xserver = {
        enable = true;
        
        layout = "se";

        windowManager._xmonad = {
            enable = true;
        };
    };

    environment.systemPackages = with pkgs; [
        git
        gcc
        tree
        killall
        neovim
        firefox
        xmobar
        feh
        alacritty
        fish
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

    theme.subs =
        let
            neovim-pack =
                pkgs.neovim-pack
                {
                    start = with pkgs.vimPlugins; [
                        nvim-treesitter
                        nvim-autopairs
                    ];
                };
        in
        {
            ".config/alacritty/alacritty.yml" = themeExpr:
                let
                    size =
                        if (elem "laptop" hostOptions.roles)
                        then 6
                        else 11;
                in
                    themeExpr //
                    {
                        inherit font; 
                        fish = toString pkgs.fish;
                        size = toString size;
                    };

            ".config/fish/config.fish" = themeExpr:
                { primary = fishTerminalColor themeExpr.primaryTerminalColor; };

            ".xmonad/lib/Theme.hs" = id;

            ".config/xmobar/.xmobarrc" = id;

            ".config/nvim/init.lua" = _:
                { packpath = toString neovim-pack; };
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
