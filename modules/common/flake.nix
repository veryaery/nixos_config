{
    outputs = {
        nixosModule = { pkgs, ... }:
            {
                nix = {
                    package = pkgs.nixFlakes;
                    extraOptions = ''
                        experimental-features = nix-command flakes
                    '';
                };
            }
    };
}