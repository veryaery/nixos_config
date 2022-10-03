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
        hmlib = home-manager.lib;

        inherit (std)
            importJSON
            mkForce
            nixosSystem;
        
        inherit (lib)
            mapThemeHostToAttrset
            optionalPath
            overlayFromImports
            readThemes;
        
        drvDirPath = ./derivations;
        overlays = [
            (overlayFromImports drvDirPath [
                "opentype-feature-freezer"
                "substitution-json"
            ])
            (overlayFromImports drvDirPath [
                "themes"
                "picom-jonaburg"
                "substitute-dir"
                "installtheme"
                "postinstall"
                "lstheme"
                "nvim"
                "neovim-pack"
                "fira-code-with-features"
                "bright"
                "discord"
                "pastel"
                "indent-blankline-nvim"
            ])
        ];

        hostDirPath = ./modules/host;
        flakeRoot = ./.;

        themes = readThemes ./themes;
    in
    {
        # nixosConfigurations is set to an attrset with an attr for each combination of theme and host
        # where the key is the form of "<theme>.<host>"
        # and where the value is the respective NixOS configuration for the combination.
        #
        # This is so that the appropriate theme and host can be selected from nixos-rebuild:
        # λ nixos-rebuild switch --flake .#<theme>.<host>
        nixosConfigurations = mapThemeHostToAttrset themes hostDirPath (themeName: host:
            # This function is evaluated for each combination of themeName and host.
            # This function should return the respective NixOS configuration for that combination.
            let
                hostPath = hostDirPath + "/${host}";

                hostExpr = import hostPath;
                hostOptions = hostExpr.options;
                hostModule = hostExpr.module;

                localSystem = { inherit (hostOptions) system; };
                config = { allowUnfree = true; };
                pkgs = import nixpkgs
                    { inherit overlays localSystem config; };
                
                imports =
                    [
                        ./modules/configuration.nix
                        hostModule
                    ];
            in
            nixosSystem {
                modules = [
                    home-manager.nixosModules.home-manager
                    {
                        inherit imports;

                        # Explicitly define localSystem.
                        nixpkgs = { inherit localSystem config; };

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
            });
    };
}
