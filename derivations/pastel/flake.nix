{
    inputs = {
        nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    };

    outputs = { nixpkgs, ... }@inputs:
    let
        system = "x86_64-linux";
        localSystem = { inherit system; };
        pkgs = import nixpkgs { inherit localSystem; };

        std = pkgs.lib;
    in
    {
        packages.${system}.default = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
                rustc
                cargo
            ];
        };
    };
}
