{
    inputs = {
        nixpkgs.url = "nixpkgs/nixpkgs-unstable";
        module.url = "path:./modules/common#nixosModule";
    };

    outputs = { nixpkgs, ... }@inputs:
        {
            nixosConfigurations.vm = {
                system = "x86_64-linux";
            
                modules = [
                    inputs.module
                ];
            };
        };
}