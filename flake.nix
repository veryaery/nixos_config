{
    inputs = {
        nixpkgs.url = "nixpkgs/nixpkgs-unstable";

        home-manager = {
            url = "github:nix-community/home-manager";
            inputs.nixpkgs.follows = "nixpkgs";
        };
    };

    outputs =
    {
        nixpkgs,
        home-manager,
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
            optionalPath
            overlayFromImports
            readThemes;
        
        drvDirPath = ./derivations;
        overlays = [
            (overlayFromImports drvDirPath [
                "themes"
            ])
            (overlayFromImports drvDirPath [
                "opentype-feature-freezer"
                "xmobar"
                "picom-jonaburg"
            ])
            (overlayFromImports drvDirPath [
                "dotfiles"
                "installtheme"
                "postinstall"
                "lstheme"
                "neovim"
                "neovim-pack"
                "fira-code-with-features"
                "xmonad"
            ])
        ];

        hostDirPath = ./modules/host;
        flakeRoot = ./.;

        themes = readThemes ./themes;
    in
    {
        # Make a NixOS configuration for each combination of theme and host
        # so that the appropriate combination may be selected from nixos-rebuild
        # e.g. nixos-rebuild --flake .#<theme>.<host>
        #
        # This is possible becuase the attrset keys are in the form of "<theme>.<host>"
        # as defined by attrsetFromEachThemeEachHost.
        nixosConfigurations = attrsetFromEachThemeEachHost themes hostDirPath
            # This function is evaluated for each combination of theme and host.
            # The function will return an appropriate NixOS configuration for that combination.
            (themeName: host:
                let
                    hostPath = hostDirPath + "/${host}";

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
                                    themes themeName
                                    host
                                    flakeRoot
                                    hostOptions
                                    hmlib;
                            };
                        }
                    ];
                }
            );
    };
}
