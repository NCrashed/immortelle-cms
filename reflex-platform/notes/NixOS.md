### Enabling the binary cache on NixOS

When using Nix on NixOS, only root can add binary caches to the system.  This will force ghcjs-setup to rebuild GHCJS from scratch, which takes hours.  To enable the binary cache, you can add the following lines to your `/etc/nixos/configuration.nix`:

```
  nix.trustedBinaryCaches = [ "https://ryantrinkle.com:5443" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
```

If you already have these options set up, just add `"https://ryantrinkle.com:5443/"` and `"ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="` to the existing lists.

Once it's been added, run `sudo -i nixos-rebuild switch` to make the change take effect, then run try-reflex as normal.
