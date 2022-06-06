nixpgs:

let
    inherit (builtins)
        map
        readDir
        attrNames
        getEnv;
    
    inherit (nixpkgs.lib)
        nixosSystem;
    
    inherit (nixpkgs.lib.lists)
        foldr;

    hsc_dir_path = ./host-specific_configuration;
    osc_dir_path = ./os-specific_configuration;

    modules'' = {
        "common_configuration.nix" = ./common_configuration.nix;
    };
in
{
    env_or = env_var: default:
        let env_var_val = getEnv env_var;
        in
            if env_var_val == ""
            then default
            else env_var_val;

    nixos_configurations_for = system: os:
        let
            modules' = modules'' // {
                "os-specific_configuration.nix" = osc_dir_path + "/${os}/os-specific_configuration.nix";
            };
            hsc_files = attrNames (readDir hsc_dir_path);
        in
            foldr
                (nixos_configurations: hsc_file:
                    let
                        hsc_file_path = hsc_dir_path + "/${hsc_file}";
                        modules = modules' // {
                            "hardware-configuration.nix" = hsc_file_path + "/generated/hardware-configuration.nix";
                            "host-specific_configuration.nix" = hsc_file_path + "/host-specific_configuration.nix";
                        };
                        nixos_configuration = nixosSystem {
                            inherit system;

                            modules = [
                                modules."hardware-configuration.nix"
                                modules."host-specific_configuration.nix"
                                modules."os-specific_configuration.nix"
                                modules."common_configuration.nix"
                            ];
                        };
                    in
                        nixos_configurations // { "${hsc_file}" = nixos_configuration; }
                )
                {}
                hsc_files;
}