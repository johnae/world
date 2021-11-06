{
  description = "John's NixOS configurations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nur.url = "github:nix-community/NUR";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-misc = {
      url = "github:johnae/nix-misc";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    packages = {
      url = "path:./packages";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.fenix.follows = "fenix";
      inputs.nix-misc.follows = "nix-misc";
    };
    agenix = {
     url = "github:ryantm/agenix";
     inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ... } @ inputs:
    let
      inherit (nixpkgs.lib) genAttrs filterAttrs mkOverride makeOverridable mkIf
        hasSuffix mapAttrs mapAttrs' removeSuffix nameValuePair nixosSystem
        mkForce mapAttrsToList splitString concatStringsSep last hasAttr recursiveUpdate;
      inherit (builtins) replaceStrings attrNames functionArgs substring pathExists fromTOML readFile readDir listToAttrs filter;

      supportedSystems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = genAttrs supportedSystems;
      pkgs = forAllSystems (system: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          inputs.nix-misc.overlay
          inputs.nur.overlay
          inputs.agenix.overlay
        ] ++ mapAttrsToList (_: value: value) inputs.packages.overlays;
      });

      hostConfigs = mapAttrs' (f: _:
        let hostname = replaceStrings [".toml"] [""] f;
        in { name = hostname; value = fromTOML (readFile (./hosts + "/${f}")); }
      ) (readDir ./hosts);

      hosts = mapAttrs (_: config:
        let
          arch = if hasAttr "arch" config then config.arch else "x86_64-linux";
          profiles = config.profiles;
          cfg = builtins.removeAttrs config [ "profiles" "arch" ];
        in
        {
        specialArgs.system = arch;
        specialArgs.hostConfig = cfg;
        specialArgs.hostConfigs = hostConfigs;
        configuration.imports = (map (item:
          if pathExists (toString (./. + "/${item}")) then
            (./. + "/${item}")
          else (./. + "/${item}.nix")
        ) profiles) ++ [ ./modules ./profiles/disable-users-groups-dry-run.nix ]; ## disable-users-groups-dry-run is a temp fix
      }) hostConfigs;

      toNixosConfig = hostName: host:
        let
          inherit (host.specialArgs) system;
        in
        makeOverridable nixosSystem {
          inherit system;
          specialArgs = {
            pkgs = pkgs.${system};
            inherit hostName inputs;
            userProfiles = import ./users/profiles.nix { lib = inputs.nixpkgs.lib; };
          } // host.specialArgs;
          modules = [
            { system.configurationRevision = mkIf (self ? rev) self.rev; }
            { system.nixos.versionSuffix = mkForce "git.${substring 0 11 nixpkgs.rev}"; }
            { nixpkgs = { pkgs = pkgs.${system}; }; }
            inputs.nixpkgs.nixosModules.notDetected
            inputs.home-manager.nixosModules.home-manager
            inputs.agenix.nixosModules.age
            host.configuration
          ];
        };

      toPxeBootSystemConfig = hostName: system:
        let
          bootSystem = makeOverridable nixosSystem {
            inherit system;
            specialArgs = {
              pkgs = pkgs.${system};
              inherit hostName inputs;
            };
            modules = [
              { system.configurationRevision = mkIf (self ? rev) self.rev; }
              { system.nixos.versionSuffix = mkForce "git.${substring 0 11 nixpkgs.rev}"; }
              { nixpkgs = { pkgs = pkgs.${system}; }; }
              inputs.nixpkgs.nixosModules.notDetected
              ({ modulesPath, pkgs, lib, ... }: {
                 imports = [
                   "${modulesPath}/installer/netboot/netboot-minimal.nix"
                   ./cachix.nix
                 ];
                 nix = {
                   trustedUsers = [ "root" ];
                   extraOptions = ''
                     experimental-features = nix-command flakes ca-references
                   '';
                   package = pkgs.nixUnstable;
                 };
                 environment.systemPackages = with pkgs; [
                   git curl jq skim
                 ];
                 boot.supportedFilesystems = lib.mkForce [ "btrfs" "vfat" ];
                 boot.kernelPackages = pkgs.linuxPackages_latest;
                 services.getty.autologinUser = mkForce "root";
                 hardware.video.hidpi.enable = true;
                 # Enable sshd which gets disabled by netboot-minimal.nix
                 systemd.services.sshd.wantedBy = mkOverride 0 [ "multi-user.target" ];
                 users.users.root.openssh.authorizedKeys.keys = [
                   "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCyjMuNOFrZBi7CrTyu71X+aRKyzvTmwCEkomhB0dEhENiQ3PTGVVWBi1Ta9E9fqbqTW0HmNL5pjGV+BU8j9mSi6VxLzJVUweuwQuvqgAi0chAJVPe0FSzft9M7mJoEq5DajuSiL7dSjXpqNFDk/WCDUBE9pELw+TXvxyQpFO9KZwiYCCNRQY6dCjrPJxGwG+JzX6l900GFrgOXQ3KYGk8vzep2Qp+iuH1yTgEowUICkb/9CmZhHQXSvq2gAtoOsGTd9DTyLOeVwZFJkTL/QW0AJNRszckGtYdA3ftCUNsTLSP/VqYN9EjxcMHQe4PGjkK7VLb59DQJFyRQqvPXiUyxNloHcu/sDuiKHIk/0qDLHlVn2xc5zkvzSqoQxoXx+P4dDbje1KHLY8E96gLe2Csu0ti+qsM5KEvgYgwWwm2g3IBlaWwgAtC0UWEzIuBPrAgPd5vi+V50ITIaIk6KIV7JPOubLUXaLS5KW77pWyi9PqAGOXj+DgTWoB3QeeZh7CGhPL5fAecYN7Pw734cULZpnw10Bi/jp4Nlq1AJDk8BwLUJbzZ8aexwMf78syjkHJBBrTOAxADUE02nWBQd0w4K5tl/a3UnBYWGyX8TD44046Swl/RY/69PxFvYcVRuF4eARI6OWojs1uhoR9WkO8eGgEsuxxECwNpWxR5gjKcgJQ=="
                   "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIJY3QSBIiRKN8/B3nHgCBDpauQBOftphOeuF2TaBHGQSAAAABHNzaDo="
                   "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIAwJWtQ5ZU9U0szWzJ+/GH2uvXZ15u9lL0RdcHdsXM0VAAAABHNzaDo="
                 ];

                 environment.etc."profile.local".text = ''
                   attempt=0
                   max_attempts=5
                   wait_secs=3
                   while ! curl -sf --connect-timeout 5 --max-time 5 http://www.google.com > /dev/null; do
                     if [ "$attempt" -ge "$max_attempts" ]; then
                       echo Fail - no internet, tried "$max_attempts" times over $((max_attempts * wait_secs)) seconds
                       exit 1
                     fi
                     sleep "$wait_secs"
                   done
                   if [ -z "$_INSTALLER_HAS_RUN" ]; then
                     _INSTALLER_HAS_RUN=y
                     export _INSTALLER_HAS_RUN
                     git clone https://github.com/johnae/world /tmp/world
                     cd /tmp/world
                     echo 'Which config should be installed?'
                     host="$(nix eval --apply builtins.attrNames .#nixosConfigurations --json | jq -r '.[]' | sk)"
                     nix build .#"$host"-diskformat
                     ./result/bin/diskformat | tee -a diskformat.log
                     mount
                     nixos-install --flake .#"$host" --no-root-passwd --impure | tee -a nixos-install.log
                   else
                     echo installer has already been run
                   fi
                   bash
                 '';
              })
            ];
          };
          in
           pkgs.${system}.symlinkJoin {
             name = "netboot";
             paths = with bootSystem.config.system.build; [
               netbootRamdisk
               kernel
               netbootIpxeScript
             ];
             preferLocalBuild = true;
          };

      toDiskFormatter = hostName: config: system:
        inputs.nixpkgs.lib.nameValuePair "${hostName}-diskformat" (
          pkgs.${system}.callPackage ./utils/diskformat.nix {
            inherit hostName config;
          }
        );

      worldUtils = forAllSystems (system:
        let
          f = import ./utils/world.nix;
          args = listToAttrs (map (name: { inherit name; value = pkgs.${system}.${name}; }) (attrNames (functionArgs f)));
        in f args
      );

      nixosConfigurations = mapAttrs toNixosConfig hosts;

      diskFormatters = forAllSystems (system:
        (mapAttrs' (hostName: config: toDiskFormatter hostName config system) nixosConfigurations)
      );

      exportedPackages = forAllSystems (system:
        (mapAttrs (name: _: pkgs.${system}.${name})
          (filterAttrs (name: _: (hasAttr name pkgs.${system}) && nixpkgs.lib.isDerivation pkgs.${system}.${name}) inputs.packages.overlays)
        ) // {
          pxebooter = toPxeBootSystemConfig "pxebooter" system;
        }
      );

    in
    {
      devShell = forAllSystems (system:
        pkgs.${system}.callPackage ./devshell.nix {
          worldUtils = worldUtils.${system};
          agenix = pkgs.${system}.agenix.override { nix = pkgs.${system}.nixUnstable; };
          mkDevShell = pkgs.${system}.callPackage inputs.nix-misc.lib.mkSimpleShell { };
        }
      );

      inherit nixosConfigurations hostConfigs;

      packages = recursiveUpdate (recursiveUpdate diskFormatters exportedPackages) worldUtils;

      github-actions-package-matrix-x86-64-linux = {
        os = [ "ubuntu-latest" ];
        pkg = mapAttrsToList (name: _:  name) exportedPackages.x86_64-linux;
      };

      github-actions-package-matrix-aarch64-linux = let
        skip = [
          "age-plugin-yubikey"
          "blur"
          "fire"
          "git-branchless"
          "grim"
          "innernet"
          "meson-0591"
          "my-emacs"
          "my-emacs-config"
          "netns-dbus-proxy"
          "netns-exec"
          "nixpkgs-fmt"
          "persway"
          "pueue"
          "rofi-wayland"
          "slurp"
          "slurp"
          "sway"
          "sway-unwrapped"
          "swaybg"
          "swayidle"
          "swaylock"
          "swaylock-dope"
          "wayland-protocols-master"
          "wf-cliboard"
          "wf-cliboard-x11"
          "wf-recorder"
          "wlroots"
          "xdg-desktop-portal-wlr"
        ];
      in
        {
        os = [ "ubuntu-latest" ];
        pkg = filter (elem: !(builtins.elem elem skip)) (mapAttrsToList (name: _:  name) exportedPackages.aarch64-linux);
      };

      github-actions-host-matrix = {
        os = [ "ubuntu-latest" ];
        host = mapAttrsToList (name: _:  name) nixosConfigurations;
      };
    };
}
