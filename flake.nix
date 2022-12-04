{
    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
        themenix.url = "github:veryaery/themenix";
    };

    outputs = { nixpkgs, themenix, ... } @ inputs:
    let
        std = nixpkgs.lib;
        lib = import ./lib std;

        inherit (lib.host)
            cartesianEachHost;

        inherit (themenix.lib.helper)
            eachTheme;
    in
    {
        nixosConfigurations = cartesianEachHost ./hosts ({ hostName, host, ... }:
            eachTheme ./themes {} ({ themeName, theme, module, ... }:
                let
                    hostOptions = host.options;
                    imports = [
                        host.module
                    ];
                in
                std.nixosSystem {
                    modules = [
                        module
                        {
                            nixpkgs = {
                                localSystem = { inherit (hostOptions) system; };
                            };

                            _module.args = {
                                inherit
                                    hostName
                                    themeName theme
                                    hostOptions;
                            };
                        }
                    ];
                }
            )
        );
    };
}
