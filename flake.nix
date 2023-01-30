{
    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
        themenix = {
            url = "github:veryaery/themenix";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        flake-utils.url = "github:numtide/flake-utils";
    };

    outputs = { nixpkgs, themenix, flake-utils, ... } @ inputs:
    let
        std = nixpkgs.lib;
        lib = import ./lib std;

        inherit (std.strings)
            escapeShellArg;

        inherit (lib.host)
            cartesianEachHost
            eachHost;

        inherit (themenix.lib.helper)
            eachTheme;
    in
    {
        nixosConfigurations = cartesianEachHost ./hosts ({ hostName, host, ... }:
            eachTheme ./themes {} ({ themeName, theme, module, ... }:
                std.nixosSystem {
                    modules = [
                        module
                        {
                            imports = [
                                host.module
                            ];

                            _module.args = {
                                inherit
                                    hostName host
                                    themeName theme;

                                flakeRoot = ./.;
                            };
                        }
                    ];
                }
            )
        );
    }
    //
    flake-utils.lib.eachDefaultSystem (system:
        let
            pkgs = import nixpkgs { localSystem = { inherit system; }; };
        in
        {
            apps = eachHost ./hosts ({ hostName, host, ... }:
                eachTheme ./themes {} ({ themes, themeName, theme, ... }:
                    let
                        files = import ./files.nix {
                            inherit
                                hostName host
                                themeName theme
                                pkgs;

                            flakeRoot = ./.;
                        };
                        themenixPkg = themenix.packages.${system}.default.override {
                            inherit themes files; src = ./dotfiles;
                        };
                        installthemeWrapper = pkgs.writeShellScript "installtheme-wrapper" ''
                            exec ${themenixPkg}/bin/installtheme ${escapeShellArg themeName}
                        '';

                    in
                    {
                        type = "app"; 
                        program = toString installthemeWrapper;
                    }
                )
            );
        }
    );
}
