{
    inputs = {
        nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    };

    outputs = { nixpkgs, ... }@inputs:
        let
            localSystem = "x86_64-linux";
            pkgs = import nixpkgs { inherit localSystem; };
            lib = nixpkgs.lib;
        {
            nixosConfigurations.test = lib.nixosSystem {
                modules = [
                    {
                        imports = [
                            ./configuration.nix
                        ];

                        # Explicit localSystem and pkgs.
                        config = {
                            nixpkgs.localSystem = localSystem;
                            _module.args.pkgs = pkgs;
                        }
                    }
                ];
            };
        };
}