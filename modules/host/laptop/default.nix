{
    options = {
        system = "x86_64-linux";
        roles = [
            "laptop"
            "nvidia"
            "bluetooth"
        ];
    };

    module = {
        imports = [ ./hardware-configuration.nix ];

        boot.loader = {
            efi = {
                canTouchEfiVariables = true;
                efiSysMountPoint = "/boot/efi";
            };

            grub = {
                enable = true;
                version = 2;
                device = "nodev";
                efiSupport = true;
            };
        };

        time.timeZone = "Europe/Stockholm";

        system.stateVersion = "22.05";
    };
}
