# NixOS Configuration (WIP)

My NixOS configuration is split up by host and OS.

# Installing

```
nixos-rebuild switch --flake .#<os>.<host>
``` 

# Modules

## OS

`./modules/os/<os>/`  
* `os.nix` — OS-specific NixOS module option definitions.

## Host

`./modules/host/<host>/`  
* `host.json`
* `host.nix` (Optional) — Host-specific NixOS module option definitions.
* `hardware-configuration.nix` (Optional) — Hardware NixOS module option definitions. Generate this file using `nixos-generate-config` or copy it from your own NixOS configuration.  

**Hosts are expected to define a [`boot.loader`](https://search.nixos.org/options?query=boot.loader) and define [`system.stateVersion`](https://search.nixos.org/options?query=system.stateVersion).**

### `host.json` Schema

```ts
{
    // The host's Nix platform type e.g. "x86_64-linux" "aarch64-linux".
    "system": string
}
```