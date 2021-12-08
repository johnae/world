{
  description = "John's NixOS configurations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixlib.url = "github:nix-community/nixpkgs.lib";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nur.url = "github:nix-community/NUR";

    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-misc = {
      url = "github:johnae/nix-misc";
      inputs.nixlib.follows = "nixlib";
    };

    devshell.url = "github:johnae/devshell";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agenix = {
     url = "github:ryantm/agenix";
     inputs.nixpkgs.follows = "nixpkgs";
     inputs.nixlib.follows = "nixlib";
    };
    notracking = {
     url = "github:notracking/hosts-blocklists";
     flake = false;
    };

    ######################## packages ########################
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    spotnix = {
      url = "github:johnae/spotnix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.fenix.follows = "fenix";
    };
    persway = {
      url = "github:johnae/persway";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.fenix.follows = "fenix";
    };

    ## non flakes
    rofi-wayland = { url = "github:lbonn/rofi/wayland"; flake = false; };
    age-plugin-yubikey = { url = "github:str4d/age-plugin-yubikey"; flake = false; };
    blur = { url = "github:johnae/blur"; flake = false; };
    fire = { url = "github:johnae/fire"; flake = false; };
    fish-kubectl-completions = { url = "github:evanlucas/fish-kubectl-completions"; flake = false; };
    google-cloud-sdk-fish-completion = { url = "github:Doctusoft/google-cloud-sdk-fish-completion"; flake = false; };
    grim = { url = "github:emersion/grim"; flake = false; };
    nixpkgs-fmt = { url = "github:nix-community/nixpkgs-fmt"; flake = false; };
    netns-exec = { url = "github:johnae/netns-exec"; flake = false; };
    slurp = { url = "github:emersion/slurp"; flake = false; };
    spotifyd = { url = "github:spotifyd/spotifyd"; flake = false; };
    wayland-protocols-master = { url = "git+https://gitlab.freedesktop.org/wayland/wayland-protocols?ref=main"; flake = false; };
    sway = { url = "github:swaywm/sway"; flake = false; };
    swaybg = { url = "github:swaywm/swaybg"; flake = false; };
    swayidle = { url = "github:swaywm/swayidle"; flake = false; };
    swaylock = { url = "github:swaywm/swaylock"; flake = false; };
    wlroots = { url = "git+https://gitlab.freedesktop.org/wlroots/wlroots?ref=master"; flake = false; };
    wf-recorder = { url = "github:ammen99/wf-recorder"; flake = false; };
    wl-clipboard = { url = "github:bugaevc/wl-clipboard"; flake = false; };
    xdg-desktop-portal-wlr = { url = "github:emersion/xdg-desktop-portal-wlr/v0.5.0"; flake = false; };
    git-branchless = { url = "github:arxanas/git-branchless"; flake = false; };
    pueue = { url = "github:Nukesor/pueue"; flake = false; };
    ####################### end packages #######################
  };

  outputs = { self, nixpkgs, ... } @ inputs:
    let

      inherit (nixpkgs.lib) genAttrs filterAttrs mkOverride makeOverridable mkIf
        hasSuffix mapAttrs mapAttrs' removeSuffix nameValuePair nixosSystem
        mkForce mapAttrsToList splitString concatStringsSep last hasAttr recursiveUpdate;

      inherit (builtins) replaceStrings attrNames functionArgs substring pathExists
        fromTOML readFile readDir listToAttrs filter removeAttrs;

      supportedSystems = [ "x86_64-linux" "aarch64-linux" ];

      packageOverlays = import ./packages/overlays.nix { inherit inputs; lib = nixpkgs.lib; };

      overlays = [
        inputs.nix-misc.overlay
        inputs.devshell.overlay
        inputs.nur.overlay
        inputs.agenix.overlay
        inputs.spotnix.overlay
        inputs.persway.overlay
        inputs.emacs-overlay.overlay
        inputs.fenix.overlay
        (final: prev: { nix-direnv = prev.nix-direnv.overrideAttrs (oldAttrs:
          {
            postPatch = ''
            ${oldAttrs.postPatch}sed -i "s|sed.*shellHook.*||g" direnvrc
            '';
          }
        );})
      ] ++ mapAttrsToList (_: value: value) packageOverlays;

      worldOverlay = (final: prev: {
        world = prev.callPackage ./utils/world.nix { };
      });

      forAllSystems = f: genAttrs supportedSystems (system: f (import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        inherit overlays;
      }));

      hostConfigs = mapAttrs' (f: _:
        let hostname = replaceStrings [".toml"] [""] f;
        in { name = hostname; value = fromTOML (readFile (./hosts + "/${f}")); }
      ) (readDir ./hosts);

      hosts = mapAttrs (_: config:
        let
          arch = if hasAttr "arch" config then config.arch else "x86_64-linux";
          profiles = config.profiles;
          cfg = removeAttrs config [ "profiles" "arch" ];
        in
        {
        specialArgs.system = arch;
        specialArgs.hostConfig = cfg;
        specialArgs.hostConfigs = hostConfigs;
        configuration.imports = (map (item:
          if pathExists (toString (./. + "/${item}")) then
            (./. + "/${item}")
          else (./. + "/${item}.nix")
        ) profiles) ++ [ ./modules ];
      }) hostConfigs;

      toNixosConfig = hostName: host:
        makeOverridable nixosSystem {
          inherit (host.specialArgs) system;
          specialArgs = {
            inherit hostName inputs;
            userProfiles = import ./users/profiles.nix { lib = inputs.nixpkgs.lib; };
          } // host.specialArgs;
          modules = [
            {
              system.configurationRevision = mkIf (self ? rev) self.rev;
              system.nixos.versionSuffix = mkForce "git.${substring 0 11 nixpkgs.rev}";
              nixpkgs.overlays = overlays;
            }
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
              inherit hostName inputs;
            };
            modules = [
              {
                system.configurationRevision = mkIf (self ? rev) self.rev;
                system.nixos.versionSuffix = mkForce "git.${substring 0 11 nixpkgs.rev}";
                nixpkgs.overlays = overlays;
              }
              inputs.nixpkgs.nixosModules.notDetected
              ({ modulesPath, pkgs, lib, ... }: {
                 imports = [
                   "${modulesPath}/installer/netboot/netboot-minimal.nix"
                   ./cachix.nix
                 ];
                 nix = {
                   trustedUsers = [ "root" ];
                   extraOptions = ''
                     experimental-features = nix-command flakes
                   '';
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
           bootSystem.pkgs.symlinkJoin {
             name = "netboot";
             paths = with bootSystem.config.system.build; [
               netbootRamdisk
               kernel
               netbootIpxeScript
             ];
             preferLocalBuild = true;
          };

      toDiskFormatter = hostName: config: pkgs:
        inputs.nixpkgs.lib.nameValuePair "${hostName}-diskformat" (
          pkgs.callPackage ./utils/diskformat.nix {
            inherit hostName config;
          }
        );

      nixosConfigurations = mapAttrs toNixosConfig hosts;

      diskFormatters = forAllSystems (pkgs:
        (mapAttrs' (hostName: config: toDiskFormatter hostName config pkgs) nixosConfigurations)
      );

      exportedPackages = forAllSystems (pkgs:
        (mapAttrs (name: _: pkgs.${name})
          (filterAttrs (name: _: (hasAttr name pkgs) && nixpkgs.lib.isDerivation pkgs.${name}) packageOverlays)
        ) // {
          pxebooter = toPxeBootSystemConfig "pxebooter" pkgs.system;
        }
      );

      worldUtils = forAllSystems (pkgs:
        {
          inherit (pkgs.extend worldOverlay) world;
        }
      );

    in
    {
      devShell = forAllSystems (pkgs:
        let
          extendedPkgs = pkgs.extend worldOverlay;
        in
        extendedPkgs.devshell.mkShell {
          imports = [
            (extendedPkgs.devshell.importTOML ./devshell.toml)
          ];
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
          "pxebooter"
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

      github-actions-host-matrix-x86-64-linux = {
        os = [ "ubuntu-latest" ];
        host = mapAttrsToList (name: _:  name) (filterAttrs (_: config: config.specialArgs.system == "x86_64-linux") hosts);
      };

      github-actions-host-matrix-aarch64-linux = {
        os = [ "ubuntu-latest" ];
        host = mapAttrsToList (name: _:  name) (filterAttrs (_: config: config.specialArgs.system == "aarch64-linux") hosts);
      };
    };
}
