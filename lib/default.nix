std:

let
    host = import ./host.nix std;
in
{
    inherit host;
}
