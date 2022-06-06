let
    std = (import <nixpkgs>).lib;
in
{
    inherit (import ./options.nix std)
        options;
    
    inherit (import ./modules.nix std)
        nixosModules;
}