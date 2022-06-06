{
    inputs = {
        nixpkgs.url = "nixpkgs";
    };
    outputs = { nixpkgs, ... }@inputs:
    let
        configuration = import ./configuration.nix;
        lib = (import ./lib.nix) nixpkgs;
    in
    {
        nixosConfigurations = 
            let os = lib.env_or "NIXOS_CONFIGURATION_OS" configuration.os;
            in lib.nixos_configurations_for configuration.system os;
    };
}