{
    options = {
        system = "x86_64-linux";
        roles = [
            "desktop"
            "nvidia"
        ];
    };

    module = {
        imports = [ ./hardware-configuration.nix ];

        boot.loader = {
            efi.efiSysMountPoint = "/boot/efi";

            grub = {
                enable = true;
                version = 2;
                device = "nodev";
                efiSupport = true;
                efiInstallAsRemovable = true;
            };
        };

        time.timeZone = "Europe/Stockholm";

        system.stateVersion = "22.05";
    };
}
