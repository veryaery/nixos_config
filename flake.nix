{
    inputs = {
        nixpkgs.url = "nixpkgs/nixpkgs-unstable";

        home-manager = {
            url = "github:nix-community/home-manager";
            inputs.nixpkgs.follows = "nixpkgs";
        };

        hyprland = {
            url = "github:hyprwm/hyprland";
            inputs.nixpkgs.follows = "nixpkgs";
        };
    };

    outputs =
    {
        nixpkgs,
        home-manager,
        hyprland,
        ...
    }@inputs:
    let
        std = nixpkgs.lib;
        lib = import ./lib std;
        hmlib= home-manager.lib;

        inherit (std)
            importJSON
            mkForce
            nixosSystem;
        
        inherit (lib)
            attrsetFromEachThemeEachHost
            optionalPath;
        
        derivationsDirPath = ./derivations;
        overlays = [
            (self: super:
                {
                    opentype-feature-freezer =
                        import (derivationsDirPath + /opentype-feature-freezer.nix) self;
                }
            )

            (self: super:
                {
                    fira-code-with-features =
                        import (derivationsDirPath + /fira-code-with-features.nix) super;
                }
            )
        ];

        themeDirPath = ./themes;
        hostDirPath = ./modules/host;
        flakeRoot = ./.;
        dotfiles = flakeRoot + /dotfiles;
    in
    {
        # Make a NixOS configuration for each combination of theme and host
        # so that the appropriate combination may be selected from nixos-rebuild
        # e.g. nixos-rebuild --flake .#<theme>.<host>
        #
        # This is possible becuase the attrset keys are in the form of "<theme>.<host>"
        # as defined by attrsetFromEachThemeEachHost.
        nixosConfigurations = attrsetFromEachThemeEachHost themeDirPath hostDirPath
            # This function is evaluated for each combination of theme and host.
            # The function will return an appropriate NixOS configuration for that combination.
            (theme: host:
                let
                    hostPath = hostDirPath + "/${host}";
                    
                    themeExpr = import (themeDirPath + "/${theme}.nix");

                    hostExpr = import (hostPath + /host.nix);
                    hostOptions = hostExpr.options;
                    hostModule = hostExpr.module;

                    localSystem = { inherit (hostOptions) system; };
                    pkgs = import nixpkgs
                        {
                            inherit overlays localSystem;

                            config = {
                                allowUnfree = true;
                            };
                        };
                    
                    imports =
                        [
                            ./modules/configuration.nix
                            hostModule
                        ]
                        # Import hardware-configuration.nix if it exists.
                        ++ (optionalPath (hostPath + /hardware-configuration.nix));
                in
                nixosSystem {
                    modules = [
                        home-manager.nixosModules.home-manager
                        hyprland.nixosModules.default
                        {
                            inherit imports;

                            # Explicitly define localSystem.
                            nixpkgs = { inherit localSystem; };

                            # Define home-manager options.
                            home-manager = {
                                useGlobalPkgs = true;
                                useUserPackages = true;
                            };

                            _module.args = {
                                # Explicitly define pkgs.
                                pkgs = mkForce pkgs;

                                inherit
                                    theme host
                                    flakeRoot dotfiles
                                    hostOptions themeExpr
                                    hmlib;
                            };
                        }
                    ];
                }
            );
    };
}
