{
    inputs = {
        nixpkgs.url = "nixpkgs/nixpkgs-unstable";

        home-manager = {
            url = "github:nix-community/home-manager";
            inputs.nixpkgs.follows = "nixpkgs";
        };
    };

    outputs = { nixpkgs, home-manager, ... }@inputs:
        let
            std = nixpkgs.lib;
            lib = import ./lib std;
            home-managerLib = home-manager.lib;

            inherit (std)
                importJSON
                mkForce
                nixosSystem;
            
            inherit (lib)
                attrsetFromEachOSEachThemeEachHost
                listWithPathIfPathExists;
            
            commonDirPath = ./modules/common;
            osDirPath = ./modules/os;
            themeDirPath = ./themes;
            hostDirPath = ./modules/host;
        in
        {
            # Make a NixOS configuration for each combination of os, theme, and host
            # so that the appropriate combination may be selected from nixos-rebuild
            # e.g. nixos-rebuild --flake .#<os>.<theme>.<host>
            #
            # This is possible becuase the attrset keys are in the form of "<os>.<theme>.<host>"
            # as defined by attrsetFromEachOSEachThemeEachHost.
            nixosConfigurations = attrsetFromEachOSEachThemeEachHost osDirPath themeDirPath hostDirPath
                # This function is evaluated for each combination of os, theme, and host.
                # The function will return an appropriate NixOS configuration for that combination.
                (os: theme: host:
                    let
                        osPath = osDirPath + "/${os}";
                        hostPath = hostDirPath + "/${host}";
                        
                        themeExpr = import (themeDirPath + "/${theme}.nix");

                        hostExpr = import (hostPath + /host.nix);
                        hostOptions = hostExpr.options;
                        hostModule = hostExpr.module;

                        localSystem = { inherit (hostOptions) system; };
                        pkgs = import nixpkgs
                            {
                                inherit localSystem;

                                config = {
                                    allowUnfree = true;
                                };
                            };
                        
                        modules =
                            [
                                (commonDirPath + /common.nix)
                                (osPath + /os.nix)
                                hostModule
                            ]
                            # Import hardware-configuration.nix if it exists.
                            ++ (listWithPathIfPathExists (hostPath + /hardware-configuration.nix));
                    in nixosSystem
                    {
                        modules = [{
                            imports =
                                [
                                    # Import the home-manager NixOS module.
                                    home-manager.nixosModules.home-manager
                                    # Define home-manager options.
                                    {
                                        home-manager = {
                                            useGlobalPkgs = true;
                                            useUserPackages = true;
                                        };
                                    }
                                ] ++
                                modules;

                            # Explicitly define localSystem.
                            nixpkgs = { inherit localSystem; };

                            _module.args = {
                                # Explicitly define pkgs.
                                pkgs = mkForce pkgs;

                                inherit
                                    os theme host
                                    hostOptions themeExpr
                                    home-managerLib;
                            };
                        }];
                    }
                );
        };
}