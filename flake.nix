{
    inputs = {
        nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    };

    outputs = { nixpkgs, ... }@inputs:
    let
        inherit (nixpkgs.lib)
            nixosSystem
            mkForce;

        system = "x86_64-linux";
        localSystem = { inherit system; };
        pkgs = import nixpkgs { inherit localSystem; };
    in
    {
        nixosConfigurations.test = nixosSystem {
            modules = [
                {
                    imports = [
                        ./configuration.nix
                        ./nixos-generate-config_output/hardware-configuration.nix
                    ];

                    # Explicit system and pkgs.
                    config = {
                        nixpkgs = { inherit localSystem; };
                        _module.args.pkgs = mkForce pkgs;
                    };
                }
            ];
        };
    };
}