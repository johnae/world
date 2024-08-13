{
  inputs,
  self,
  ...
}: {
  perSystem = {
    pkgs,
    lib,
    system,
    ...
  }: let
    inherit (lib // builtins) filterAttrs mapAttrs readDir mapAttrs';
    locallyDefinedPackages = mapAttrs (
      name: _: (pkgs.callPackage (../packages + "/${name}") {inherit inputs;})
    ) (filterAttrs (_filename: type: type == "directory") (readDir ../packages));

    tofuProvider = provider:
      provider.override (oldArgs: {
        provider-source-address =
          lib.replaceStrings
          ["https://registry.terraform.io/providers"]
          ["registry.opentofu.org"]
          oldArgs.homepage;
      });
    kexec-installer = nixpkgs: modules: (nixpkgs.legacyPackages.${system}.nixos (modules ++ [inputs.nixos-images.nixosModules.kexec-installer])).config.system.build.kexecTarball;
  in {
    packages =
      (
        mapAttrs' (hostname: config: {
          name = "${hostname}-diskformat";
          value = pkgs.callPackage ../utils/diskformat.nix {
            inherit config;
            inherit lib;
          };
        })
        self.nixosConfigurations
      )
      // locallyDefinedPackages
      // {
        world = pkgs.writeShellApplication {
          name = "world";
          runtimeInputs = with pkgs; [just nushell statix deadnix];
          text = ''
            just -f ${../Justfile} -d "$(pwd)" "$@"
          '';
        };
        tofuWithPlugins = pkgs.opentofu.withPlugins (
          p:
            map tofuProvider [p.null p.external p.hcloud p.cloudflare p.random]
        );
        rbw = pkgs.runCommand "rbw" {} ''
          mkdir -p $out/bin
          tar -zxf ${inputs.rbw-static-linux-amd64} -C $out/bin
        '';
        kexec-installer-nixos-unstable-noninteractive = kexec-installer inputs.nixpkgs-tmp [
          ({
            lib,
            pkgs,
            modulesPath,
            ...
          }: {
            disabledModules = [
              # This module adds values to multiple lists (systemPackages, supportedFilesystems)
              # which are impossible/unpractical to remove, so we disable the entire module.
              "profiles/base.nix"
            ];

            imports = [
              # reduce closure size by removing perl
              "${modulesPath}/profiles/perlless.nix"
              # FIXME: we still are left with nixos-generate-config due to nixos-install-tools
              {system.forbiddenDependenciesRegexes = lib.mkForce [];}
            ];

            # among others, this prevents carrying a stdenv with gcc in the image
            system.extraDependencies = lib.mkForce [];

            # prevents shipping nixpkgs, unnecessary if system is evaluated externally
            nix.registry = lib.mkForce {};

            # would pull in nano
            programs.nano.enable = false;

            # prevents strace
            environment.defaultPackages = lib.mkForce [pkgs.rsync pkgs.parted pkgs.gptfdisk];

            # normal users are not allowed with sys-users
            # see https://github.com/NixOS/nixpkgs/pull/328926
            users.users.nixos = {
              isSystemUser = true;
              isNormalUser = lib.mkForce false;
              group = "nixos";
            };
            users.groups.nixos = {};

            # we are missing this from base.nix
            boot.supportedFilesystems = [
              "btrfs"
              # probably not needed but does not seem to increase closure size
              "cifs"
              "f2fs"
              ## anyone still using this over ext4?
              #"jfs"
              "ntfs"
              ## no longer seems to be maintained, anyone still using it?
              #"reiserfs"
              "vfat"
              "xfs"
            ];
            boot.kernelModules = [
              # we have to explicitly enable this, otherwise it is not loaded even when creating a raid:
              # https://github.com/nix-community/nixos-anywhere/issues/249
              "dm-raid"
            ];
          })
          {
            system.kexec-installer.name = "nixos-kexec-installer-noninteractive";
            boot.kernelPackages = lib.mkForce pkgs.linuxPackages_latest;
          }
        ];
        zjstatus = inputs.zjstatus.packages.${system}.default;
        zwift = inputs.zwift.packages.${system}.default;
        helix-latest = inputs.helix.packages.${system}.helix;
        wezterm = inputs.wezterm.packages.${system}.default;
      };
  };
}
