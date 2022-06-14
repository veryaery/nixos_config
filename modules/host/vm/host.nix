{
    options = {
        system = "x86_64-linux";
        roles = [ "laptop" ];
    };

    module = {
        boot.loader.grub = {
            enable = true;
            version = 2;
            device = "/dev/sda";
        };

        networking.interfaces = {
            enp0s3 = {
                useDHCP = true;
            };
        };

        system.stateVersion = "21.11";
    };
}