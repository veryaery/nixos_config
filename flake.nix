let
    lib = import ./lib.nix;
    options = lib.options;
    # nixosModules = lib.nixosModules options.os;
in
{
    inputs =
        # nixosModules.inputs //
        {
            nixpkgs.url = "nixpkgs/nixpkgs-unstable";
            module.url = "path:/modules/common";
        };

    outputs = { nixpkgs, ... }@inputs:
        {
            nixosConfigurations.vm = {
                inherit (options)
                    system;
            
                modules = [
                    inputs.module
                ];
            };
        };
}