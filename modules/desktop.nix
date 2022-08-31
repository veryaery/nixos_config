{
    pkgs,
    hostOptions,
    ...
}@args:

let
    std = args.lib;

    inherit (builtins)
        elem;

    inherit (std)
        mkIf;
in
{
    config = mkIf (elem "desktop" hostOptions.roles) {
        environment.systemPackages = with pkgs; [
            discord
        ];

        services.autorandr = {
            enable = true;
            
            profiles = {
                desktop = {
                    fingerprint = {
                        DP-4 = "00ffffffffffff001e6d7f5bafc205000a1d0104b53c22789f8cb5af4f43ab260e5054254b007140818081c0a9c0b300d1c08100d1cf28de0050a0a038500830080455502100001a000000fd003090e6e63c010a202020202020000000fc003237474c3835300a2020202020000000ff003931304e54565342333531390a018b02031a7123090607e305c000e606050160592846100403011f13565e00a0a0a029503020350055502100001a909b0050a0a046500820880c555021000000b8bc0050a0a055500838f80c55502100001a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001a";
                        HDMI-0 = "00ffffffffffff0004721602954130542b19010380351e78cabb04a159559e280d5054bfef80714f8140818081c081009500b300d1c0023a801871382d40582c4500132b2100001e000000fd00384c1f5311000a202020202020000000fc0053323432484c0a202020202020000000ff004c52393044303231383537370a01eb020324f14f01020304050607901112131415161f230907078301000067030c001000382d023a801871382d40582c4500132b2100001f011d8018711c1620582c2500132b2100009f011d007251d01e206e285500132b2100001e8c0ad08a20e02d10103e9600132b21000018000000000000000000000000000000000000007e";
                    };

                    config = {
                        DP-4 = {
                            enable = true;
                            primary = true;
                            mode = "2560x1440";
                            position = "0x0";
                        };

                        HDMI-0 = {
                            enable = true;
                            mode = "1920x1080";
                            position = "2560x360";
                        };
                    };
                };
            };
        };
    };
}
