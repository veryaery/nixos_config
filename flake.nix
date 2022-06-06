{
    inputs = {
        nixpgs.url = "nixpkgs";
    };
    outputs = { nixpgs, ... }@inputs:
    let
        configuration = import ./configuration.nix;
        lib = (import ./lib.nix) nixpgs;
    in
    {
        nixosConfigurations = 
            let os = lib.env_or "NIXOS_CONFIGURATION_OS" configuration.os;
            in lib.nixos_configurations_for configuration.system os;
    };
}