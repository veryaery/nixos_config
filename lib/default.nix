std:

let
    host = import ./host.nix std;
    theme = import ./theme.nix std;
in
{
    inherit
        host
        theme;
}
