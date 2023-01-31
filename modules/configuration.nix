{ pkgs, config, ... } @ args:

{
    imports = [
        ./theme.nix
        ./labwc.nix
    ];

    nixpkgs.config.allowUnfree = true;

    services.openssh = {
        enable = true;
        ports = [ 32168 ];
        openFirewall = true;
    };

    security.sudo.enable = false;
    security.doas.enable = true;
    security.doas.wheelNeedsPassword = false;

    environment.systemPackages = with pkgs; [
        tree
        htop
        git
        vim
        nodejs
        firefox
        pavucontrol
        waybar
        kitty
        timg
        libreoffice-qt
        jq
        flameshot
        grim # flameshot dependency.
        gimp
        wl-clipboard
        emacs
        exa
    ];
    environment.shells = [ pkgs.fish ];

    networking.networkmanager.enable = true;

    time.timeZone = "Europe/Stockholm";

    i18n.defaultLocale = "en_US.UTF-8";

    i18n.extraLocaleSettings = {
        LC_ADDRESS = "sv_SE.UTF-8";
        LC_IDENTIFICATION = "sv_SE.UTF-8";
        LC_MEASUREMENT = "sv_SE.UTF-8";
        LC_MONETARY = "sv_SE.UTF-8";
        LC_NAME = "sv_SE.UTF-8";
        LC_NUMERIC = "sv_SE.UTF-8";
        LC_PAPER = "sv_SE.UTF-8";
        LC_TELEPHONE = "sv_SE.UTF-8";
        LC_TIME = "sv_SE.UTF-8";
    };
  
    services.greetd = {
        enable = true;
        vt = 7;
        settings.default_session = {
	    command =
	        let sessions = config.services.xserver.displayManager.sessionData.desktops;
	        in "${pkgs.greetd.tuigreet}/bin/tuigreet --sessions ${sessions}/share/wayland-sessions --asterisks";
	    user = "greeter";
        };
    };

    xdg.portal = {
        enable = true;
        wlr.enable = true;
        extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    };

    programs.sway = {
        enable = true;
        wrapperFeatures.gtk = true;
    };

    programs._labwc.enable = true;
    
    console.keyMap = "sv-latin1";
 
    sound.enable = true; 
    security.rtkit.enable = true;
    services.pipewire = {
        enable = true;
        alsa.enable = true;
        pulse.enable = true;
    };

    services.gnome.gnome-keyring.enable = true;

    services.emacs.enable = true;
 
    users.defaultUserShell = pkgs.fish; 
    users.users.aery = {
        isNormalUser = true;
        createHome = true;
        home = "/home/aery";
        description = "aery";
        extraGroups = [ "networkmanager" "wheel" ];
        packages = with pkgs; [
            bitwarden
        ];
    };
  
    programs.ssh.startAgent = true;

    programs.fish.enable = true;

    # Enable nix-command and flakes.
    nix.settings.experimental-features = [ "nix-command" "flakes" ];
}
