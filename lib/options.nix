std:

let
    inherit (builtins)
        getEnv;
    
    getEnvOr = envVar: defaultVal:
        let envVarVal = getEnv envVar;
        in
            if envVarVal == ""
            then defaultVal
            else envVarVal;
in
{
    options =
        with import ../config.nix;
        {
            inherit system;
            
            os = getEnvOr "NIXOS_CONFIG_OS" os;
        }
}