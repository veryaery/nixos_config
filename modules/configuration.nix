{
    pkgs,
    flakeRoot,
    themeExpr,
    hostOptions,
    ...
}@args:

let
    std = args.lib;
    lib = import (flakeRoot + /lib) std;

    inherit (builtins)
        readFile;

    inherit (lib)
        fishTerminalColor
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

    services.xserver = {
        enable = true;
        
        layout = "se";

        windowManager._xmonad = {
            enable = true;
        };
    };

    environment.systemPackages = with pkgs; [
        git
        tree
        killall
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

    # Installing Nix flakes system-wide.
    # https://nixos.wiki/wiki/Flakes
    nix = {
        package = pkgs.nixFlakes;
        extraOptions = ''
            experimental-features = nix-command flakes
        '';
    };
}
