## NixOS flake-based configuration

This repo contains the NixOS configuration for all my hosts. This is based on the still experimental concept of [flakes](https://gist.github.com/edolstra/40da6e3a4d4ee8fd019395365e0772e7).

Given that you're using [direnv](https://direnv.net/) and of course [NixOS](https://nixos.org) (because that's what this is all about) - here's some commands you could use:

```
world help
  Hello, world! Here's some things to do:

    help                                 -  this help output
    build <host>                         -  build the given host (you'll find the result in ./result)
    update                               -  update the current host (first build it, then update the profile and switch to new config)
    update-remote <host> [reboot]        -  update the given remote host (first build it, then update the profile remotely and switch to new config and maybe reboot)
    package <package>                    -  build a package available under nixpkgs (includes overlays, you'll find the result in ./result)
    update-pkgs                          -  update all packages (except those outside flake control)
    update-bin-pkg <pkgpath> <repo> <dlname> [version]
                                         -  updates a binary package, eg. one that is precompiled generally,
                                            only works with github releases
```

I do have an old repo for this here [NixOS Configuration](https://github.com/johnae/nixos-configuration).

Also of note is that there's a private repo referenced in [flake.nix](flake.nix) with the name "secrets" though the repo is called "nixos-metadata". This is private and contains encrypted secrets. So clearly, you won't be able to just clone this repo and use an existing host.