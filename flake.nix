{
    inputs = {
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