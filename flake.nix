{
    inputs = {
        nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    };

    outputs = { nixpkgs, ... }@inputs:
        let
            std = nixpkgs.lib;

            inherit (std)
                importJSON
                nixosSystem
                mkForce;
            
            inherit (import ./lib std)
                attrsetFromEachOSEachHost
                listWithPathIfPathExists;
            
            commonDirPath = ./modules/common;
            osDirPath = ./modules/os;
            hostDirPath = ./modules/host;
        in
        {
            # Make a NixOS configuration for each combination of os and host.
            # The attrset keys are in the form of "<os>.<host>" (As defined by attrsetFromEachOSEachHost).
            nixosConfigurations =
                attrsetFromEachOSEachHost
                osDirPath
                hostDirPath
                (os: host:
                    let
                        osPath = osDirPath + "/${os}";
                        hostPath = hostDirPath + "/${host}";

                        inherit (importJSON hostPath + "/host.json")
                            system;

                        localSystem = { inherit system; };
                        pkgs = import nixpkgs { inherit localSystem; };
                        
                        modules =
                            [
                                (commonDirPath + "/common.nix")
                                (osPath + "/os.nix")
                            ] ++
                            (listWithPathIfPathExists (hostPath + "/${host}/host.nix")) ++
                            (listWithPathIfPathExists (hostPath + "/${host}/hardware-configuration.nix"));
                    in nixosSystem {
                        modules = [{
                            imports = modules;

                            # Explicitly define localSystem and pkgs.
                            nixpkgs = { inherit localSystem; };
                            _module.args.pkgs = mkForce pkgs;
                        }];
                    }
                );
        };
}