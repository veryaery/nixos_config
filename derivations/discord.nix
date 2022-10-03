pkgs:

let
    version = "0.0.20";
in
pkgs.discord.overrideAttrs (oldAttrs: {
    inherit version;

    src = builtins.fetchTarball {
        url = "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
        sha256 = "0qaczvp79b4gzzafgc5ynp6h4nd2ppvndmj6pcs1zys3c0hrabpv";
    };
})
