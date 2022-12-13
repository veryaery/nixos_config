{} @ args:

{
    nixpkgs.config.allowUnfree = true;

    environment.systemPackages = with pkgs; [
        git
        vim
        nodejs
    ];

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
  
    services.xserver.enable = true;
  
    services.xserver.displayManager.lightdm.enable = true;
    services.xserver.desktopManager.cinnamon.enable = true;
    # services.xserver.windowManager._herbstluftwm.enable = true;
  
    services.xserver = {
        layout = "se";
        xkbVariant = "";
        xkbOptions = "caps:escape";
    };
  
    console.keyMap = "sv-latin1";
  
    sound.enable = true;
    hardware.pulseaudio.enable = false;
    security.rtkit.enable = true;
    services.pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
        media-session.enable = true;
    };
  
    users.users.aery = {
        isNormalUser = true;
        createHome = true;
        home = "/home/aery";
        description = "aery";
        extraGroups = [ "networkmanager" "wheel" ];
        packages = with pkgs; [
            firefox
            bitwarden
            pavucontrol
        ];
    };
  
    programs.ssh.startAgent = true;
}
