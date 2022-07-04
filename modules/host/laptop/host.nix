{
    options = {
        system = "x86_64-linux";
        roles = [ "laptop" ];
    };

    module = {
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

        system.stateVersion = "22.05";
    };
}
