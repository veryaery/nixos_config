{
    inputs = {
        nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    };
    outputs = attrs: {
        nixosModule = { pkgs, ... }:
            {
                nix = {
                    package = pkgs.nixFlakes;
                    extraOptions = ''
                        experimental-features = nix-command flakes
                    '';
                };
            };
    };
}