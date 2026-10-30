{
  withSystem,
  lib,
  self,
  ...
}: let
  inherit (lib) mapAttrsToList filterAttrs hasPrefix hasSuffix filter elem;
  defaultSkip = [
    "container-"
    "container-processes"
    "container-shell"
    "devenv-up"
  ];
in {
  flake = {
    github-actions-package-matrix-x86-64-linux = withSystem "x86_64-linux" (ctx @ {pkgs, ...}: let
      skip =
        (mapAttrsToList (name: _: name) (filterAttrs (name: _: hasPrefix "images/" name) pkgs))
        ++ defaultSkip;
    in {
      os = ["ubuntu-latest"];
      pkg = filter (item: !(elem item skip)) (mapAttrsToList (name: _: name) ctx.config.packages);
    });

    github-actions-package-matrix-aarch64-linux = withSystem "aarch64-linux" (ctx @ {pkgs, ...}: let
      skip =
        (mapAttrsToList (name: _: name) (filterAttrs (name: _: hasPrefix "images/" name) pkgs))
        ++ defaultSkip
        ++ [
          "wezterm"
        ];
    in {
      os = ["ubuntu-latest"];
      pkg = filter (item: !(elem item skip)) (mapAttrsToList (name: _: name) ctx.config.packages);
    });

    # Packages are defined for every system whether they build there or not,
    # so only the ones whose platforms include darwin are kept. Disk images
    # and cluster manifests are Linux artifacts, and a few packages are
    # `hello` stand-ins on darwin.
    github-actions-package-matrix-aarch64-darwin = withSystem "aarch64-darwin" (ctx @ {pkgs, ...}: let
      buildsHere = name: drv:
        !(hasPrefix "images/" name)
        && !(hasSuffix "-diskformat" name)
        && !(hasPrefix "kubevirt-" name)
        && !(elem name (defaultSkip ++ ["noctalia" "persway" "zwift"]))
        && (builtins.tryEval (lib.meta.availableOn pkgs.stdenv.hostPlatform drv)).value;
    in {
      os = ["macos-latest"];
      pkg = mapAttrsToList (name: _: name) (filterAttrs buildsHere ctx.config.packages);
    });

    github-actions-host-matrix-x86-64-linux = {
      os = ["ubuntu-latest"];
      host = mapAttrsToList (name: _: name) (filterAttrs (_: config: config.pkgs.stdenv.hostPlatform.system == "x86_64-linux") self.nixosConfigurations);
    };

    github-actions-host-matrix-aarch64-linux = {
      os = ["ubuntu-latest"];
      host = mapAttrsToList (name: _: name) (filterAttrs (_: config: config.pkgs.stdenv.hostPlatform.system == "aarch64-linux") self.nixosConfigurations);
    };
  };
}
