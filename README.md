# NixOS Configuration (WIP)

# Installing

```
nixos-rebuild switch --flake .#<os>.<theme>.<host>
```

# Modules

## OS

`./modules/os/<os>/`:  
* `os.nix` — OS-specific NixOS module option definitions.

## Host

`./modules/host/<host>/`:  
* `host.nix` — Host options and host-specific NixOS module option definitions.
* `hardware-configuration.nix` (Optional) — Hardware NixOS module option definitions. Generate this file using `nixos-generate-config` or copy it from your own NixOS configuration.  

**Hosts are expected to define a [`boot.loader`](https://search.nixos.org/options?query=boot.loader) and define [`system.stateVersion`](https://search.nixos.org/options?query=system.stateVersion).**

### `host.nix` Schema

```nix
{
    options = {
        # The host's Nix platform type e.g. "x86_64-linux" "aarch64-linux".
        # system :: string

        # Available roles:
        # * "laptop" — The host is a laptop.
        # roles :: [ string ]
    };

    # module :: NixOS Module
}
```

# Themes

### `./themes/<theme>.nix` Schema

```nix
{
    # Colors should be 6 digit hexadecimal RGB prefixed with "#".

    # foreground :: string
    # background :: string

    # primary :: string
    # Should be a key of terminalColors.
    # primaryTerminalColor :: string

    terminalColors = {
        # black :: string
        # blue :: string
        # green :: string
        # cyan :: string
        # red :: string
        # magenta :: string
        # yellow :: string
        # white :: string
        # brightBlack :: string
        # brightBlue :: string
        # brightGreen :: string
        # brightCyan :: string
        # brightRed :: string
        # brightMagenta :: string
        # brightYellow :: string
        # brightWhite :: string
    };
}
```