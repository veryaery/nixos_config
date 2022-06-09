# NixOS Configuration (WIP)

My NixOS configuration is split up by host and OS.

# Installing

```
nixos-rebuild switch --flake .#<os>.<host>
``` 

## OS

Directory in ./modules/os  
Files:
* `os.nix` — Contains OS-specific NixOS module option definitions.

## Host

Directory in ./modules/host  
Files:  
* `host.json`
* `host.nix` (Optional) — Contains host-specific NixOS module option definitions.
* `hardware-configuration.nix` (Optional) — Contains hardware related NixOS module option definitions. Generate this file using `nixos-generate-config` or copy it from your own NixOS configuration.  

**Host-specific configurations are expected to define a `boot.loader` and define `system.stateVersion`.**

### `host.json` Schema

```ts
{
    // The host's Nix platform type e.g. "x86_64-linux" "aarch64-linux".
    "system": string
}
```