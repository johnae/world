{
  description = "John's Nixos Configurations, potions, packages and magical incantations";

  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://insane.cachix.org"
      "https://cachix.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "insane.cachix.org-1:cLCCoYQKkmEb/M88UIssfg2FiSDUL4PUjYj9tdo4P8o="
      "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
    ];
  };

  inputs = {
    ## flakes
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    devshell.url = "github:johnae/devshell";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixlib.url = "github:nix-community/nixpkgs.lib";
    nur.url = "github:nix-community/NUR";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-misc = {
      url = "github:johnae/nix-misc";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixlib.follows = "nixlib";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixlib.follows = "nixlib";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

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
    notracking = {
      url = "github:notracking/hosts-blocklists";
      flake = false;
    };

    rofi-wayland = {
      url = "github:lbonn/rofi/wayland";
      flake = false;
    };

    kile = {
      url = "gitlab:snakedye/kile";
      flake = false;
    };

    matrix-conduit = {
      url = "gitlab:famedly/conduit";
      flake = false;
    };

    ristate = {
      url = "gitlab:snakedye/ristate";
      flake = false;
    };

    age-plugin-yubikey = {
      url = "github:str4d/age-plugin-yubikey";
      flake = false;
    };
    blur = {
      url = "github:johnae/blur";
      flake = false;
    };
    fire = {
      url = "github:johnae/fire";
      flake = false;
    };
    fish-kubectl-completions = {
      url = "github:evanlucas/fish-kubectl-completions";
      flake = false;
    };
    google-cloud-sdk-fish-completion = {
      url = "github:Doctusoft/google-cloud-sdk-fish-completion";
      flake = false;
    };
    grim = {
      url = "github:emersion/grim";
      flake = false;
    };
    netns-exec = {
      url = "github:johnae/netns-exec";
      flake = false;
    };
    slurp = {
      url = "github:emersion/slurp";
      flake = false;
    };
    spotifyd = {
      url = "github:spotifyd/spotifyd";
      flake = false;
    };
    wayland-protocols-master = {
      url = "git+https://gitlab.freedesktop.org/wayland/wayland-protocols.git?ref=main";
      flake = false;
    };
    sway = {
      url = "github:swaywm/sway";
      flake = false;
    };
    swaybg = {
      url = "github:swaywm/swaybg";
      flake = false;
    };
    swayidle = {
      url = "github:swaywm/swayidle";
      flake = false;
    };
    swaylock = {
      url = "github:swaywm/swaylock";
      flake = false;
    };
    wlroots = {
      url = "git+https://gitlab.freedesktop.org/wlroots/wlroots.git?ref=master";
      flake = false;
    };
    wl-clipboard = {
      url = "github:bugaevc/wl-clipboard";
      flake = false;
    };
    xdg-desktop-portal-wlr = {
      url = "github:emersion/xdg-desktop-portal-wlr/v0.5.0"; # gh-release-update
      flake = false;
    };
    git-branchless = {
      url = "github:arxanas/git-branchless";
      flake = false;
    };
    kured = {
      url = "github:weaveworks/kured";
      flake = false;
    };
    fluxcd-install = {
      url = "https://github.com/fluxcd/flux2/releases/download/v0.31.1/install.yaml"; # gh-release-update
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  } @ inputs: let
    l = nixpkgs.lib // builtins;

    inherit
      (l)
      attrByPath
      elem
      filter
      filterAttrs
      filterAttrsRecursive
      fromTOML
      hasAttr
      hasPrefix
      isDerivation
      makeOverridable
      mapAttrs
      mapAttrs'
      mapAttrsToList
      mkForce
      mkIf
      mkOverride
      nameValuePair
      nixosSystem
      pathExists
      readDir
      readFile
      recursiveUpdate
      replaceStrings
      substring
      ;

    packageOverlays = import ./packages/overlays.nix {
      inherit inputs;
      inherit (nixpkgs) lib;
    };

    worldOverlays = {
      pixieboot = final: prev: {inherit (prev.callPackage ./utils/world.nix {}) pixieboot;};
      lint = final: prev: {inherit (prev.callPackage ./utils/world.nix {}) lint;};
    };

    overlays =
      [
        inputs.agenix.overlay
        inputs.devshell.overlay
        inputs.emacs-overlay.overlay
        inputs.fenix.overlay
        inputs.nix-misc.overlay
        inputs.nur.overlay
        inputs.persway.overlay
        inputs.spotnix.overlay
        (
          final: prev: {
            notracking = prev.runCommand "notracking" {} ''
              mkdir -p "$out"/{dnsmasq,dnscrypt-proxy}
              grep -v upsales ${inputs.notracking}/dnsmasq/dnsmasq.blacklist.txt > "$out"/dnsmasq/dnsmasq.blacklist.txt
              grep -v upsales ${inputs.notracking}/dnscrypt-proxy/dnscrypt-proxy.blacklist.txt > "$out"/dnscrypt-proxy/dnscrypt-proxy.blacklist.txt
            '';
          }
        )
        (import ./containers/overlay.nix {inherit self inputs;})
        (import ./kubernetes/overlay.nix {inherit inputs;})
        (
          final: prev: let
            default_flake = "github:johnae/world";
            flags = "--use-remote-sudo -L";
          in {
            nixos-upgrade = prev.writeStrictShellScriptBin "nixos-upgrade" ''
              echo Clearing fetcher cache
              echo rm -rf ~/.cache/nix/fetcher-cache-v1.sqlite*
              rm -rf ~/.cache/nix/fetcher-cache-v1.sqlite*
              flake=''${1:-${default_flake}}
              echo nixos-rebuild boot --flake "$flake" ${flags}
              nixos-rebuild boot --flake "$flake" ${flags}
              booted="$(readlink /run/booted-system/{initrd,kernel,kernel-modules})"
              built="$(readlink /nix/var/nix/profiles/system/{initrd,kernel,kernel-modules})"
              if [ "$booted" = "$built" ]; then
                echo nixos-rebuild switch --flake "$flake" ${flags}
                nixos-rebuild switch --flake "$flake" ${flags}
              else
                cat<<MSG
                The system must be rebooted for the changes to take effect
                this is because either all of or some of the kernel, the kernel
                modules or initrd were updated
              MSG
              fi
            '';
          }
        )
      ]
      ++ mapAttrsToList (_: value: value) (packageOverlays // worldOverlays);

    pkgsFor = system:
      import nixpkgs {
        inherit system overlays;
      };

    forAllNixosSystems = fn:
      flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-linux"]
      (system: fn system (pkgsFor system));

    forAllDefaultSystems = fn:
      flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-linux"]
      (system: fn system (pkgsFor system));

    hostConfigurations = mapAttrs' (
      filename: _: let
        name = replaceStrings [".toml"] [""] filename;
      in {
        inherit name;
        value = fromTOML (readFile (./hosts + "/${filename}"));
      }
    ) (readDir ./hosts);

    nixosConfig = hostName: config: let
      fileOrDir = path: let
        basePath = toString (./. + "/${path}");
      in
        if pathExists basePath
        then basePath
        else "${basePath}.nix";

      hostConf = config.config;
      profiles = map fileOrDir hostConf.profiles;

      ## Somewhat hacky way of making it seem as if we're
      ## giving home-manager the profiles to load - that
      ## can't actually happen within a module though. I.e
      ## in a module you can't add imports coming from an option.
      userProfiles = mapAttrs (
        _: user: let
          profiles = attrByPath ["profiles"] {} user;
        in
          map fileOrDir profiles
      ) (attrByPath ["home-manager" "users"] {} hostConf);

      modules = [./modules];

      inherit (config) system;

      cfg =
        filterAttrsRecursive (
          name: _:
            name != "profiles"
        )
        hostConf;
    in
      makeOverridable nixosSystem {
        inherit system;
        specialArgs = {
          inherit hostName inputs userProfiles;
          hostConfiguration = cfg;
          hostConfigurations = mapAttrs (_: conf: conf.config) hostConfigurations;
        };
        modules = [
          {
            system.configurationRevision = mkIf (self ? rev) self.rev;
            system.nixos.versionSuffix = mkForce "git.${substring 0 11 nixpkgs.rev}";
            nixpkgs.overlays = overlays;
          }
          (
            {pkgs, ...}: {
              environment.systemPackages = [pkgs.nixos-upgrade];
            }
          )
          inputs.nixpkgs.nixosModules.notDetected
          inputs.home-manager.nixosModules.home-manager
          inputs.agenix.nixosModules.age
          {
            imports = modules ++ profiles;
          }
        ];
      };

    nixosConfigurations = mapAttrs nixosConfig hostConfigurations;

    exportedPackages = forAllDefaultSystems (
      system: pkgs: let
        pkgFilter = name: _:
          hasAttr name pkgs
          && isDerivation pkgs.${name}
          && elem system (attrByPath ["meta" "platforms"] [system] pkgs.${name});
      in {
        packages =
          mapAttrs (name: _: pkgs.${name})
          (filterAttrs pkgFilter (packageOverlays
            // (filterAttrs (name: _: hasPrefix "images/" name) pkgs)
            // worldOverlays
            // {
              spotnix = true;
              persway = true;
              fluxcd-yaml = true;
              kured-yaml = true;
              notracking = true;
            }));
      }
    );

    nixosPackages = forAllNixosSystems (system: _: let
      bootSystem = makeOverridable nixosSystem {
        inherit system;
        modules = [
          {
            system.configurationRevision = mkIf (self ? rev) self.rev;
            system.nixos.versionSuffix = mkForce "git.${substring 0 11 nixpkgs.rev}";
            nixpkgs.overlays = overlays;
          }
          inputs.nixpkgs.nixosModules.notDetected
          ({
            modulesPath,
            pkgs,
            lib,
            ...
          }: {
            imports = [
              "${modulesPath}/installer/netboot/netboot-minimal.nix"
              ./cachix.nix
            ];
            nix = {
              settings.trusted-users = ["root"];
              extraOptions = ''
                experimental-features = nix-command flakes
                accept-flake-config = true
              '';
            };
            environment.systemPackages = with pkgs; [git curl jq skim];
            boot.supportedFilesystems = lib.mkForce ["btrfs" "vfat"];
            boot.kernelPackages = pkgs.linuxPackages_latest;
            services.getty.autologinUser = mkForce "root";
            hardware.video.hidpi.enable = true;
            # Enable sshd which gets disabled by netboot-minimal.nix
            systemd.services.sshd.wantedBy = mkOverride 0 ["multi-user.target"];
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
                ./result/bin/diskformat 2>&1 | tee -a diskformat.log
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
    in {
      packages.pxebooter = bootSystem.pkgs.symlinkJoin {
        name = "netboot";
        paths = with bootSystem.config.system.build; [
          netbootRamdisk
          kernel
          netbootIpxeScript
        ];
        preferLocalBuild = true;
      };
    });

    diskFormatters = forAllNixosSystems (
      _: pkgs: {packages = mapAttrs' (hostName: config: diskFormatter hostName config pkgs) nixosConfigurations;}
    );

    diskFormatter = hostName: config: pkgs:
      nameValuePair "${hostName}-diskformat" (
        pkgs.callPackage ./utils/diskformat.nix {
          inherit hostName config;
        }
      );
  in
    (forAllDefaultSystems (
      _: pkgs: {
        apps =
          mapAttrs (
            name: drv: {
              type = "app";
              program = "${drv}/bin/${name}";
            }
          ) {
            inherit (pkgs) pixieboot;
            inherit (pkgs) lint;
            inherit (pkgs) nixos-upgrade;
            update-github-release-flake-inputs = pkgs.world-updaters;
            update-cargo-vendor-sha = pkgs.world-updaters;
            update-all-cargo-vendor-shas = pkgs.world-updaters;
            update-fixed-output-derivation-sha = pkgs.world-updaters;
            update-all-fixed-output-derivation-shas = pkgs.world-updaters;
          };
        devShells.default = pkgs.devshell.mkShell {
          imports = [
            (pkgs.devshell.importTOML ./devshell.toml)
          ];
        };
      }
    ))
    // (
      forAllDefaultSystems (
        _: pkgs: {
          formatter = pkgs.alejandra;
        }
      )
    )
    // {
      inherit nixosConfigurations hostConfigurations;

      packages = recursiveUpdate (recursiveUpdate nixosPackages.packages exportedPackages.packages) diskFormatters.packages;

      overlays =
        packageOverlays
        // worldOverlays
        // {
          spotnix = inputs.spotnix.overlay;
          persway = inputs.persway.overlay;
        };

      github-actions-package-matrix-x86-64-linux = let
        pkgs = pkgsFor "x86_64-linux";
        skip = mapAttrsToList (name: _: name) (filterAttrs (name: _: hasPrefix "images/" name) pkgs);
      in {
        os = ["ubuntu-latest"];
        pkg = filter (item: !(elem item skip)) (mapAttrsToList (name: _: name) exportedPackages.packages.x86_64-linux);
      };

      github-actions-package-matrix-aarch64-linux = let
        pkgs = pkgsFor "x86_64-linux";
        skip =
          (mapAttrsToList (name: _: name) (filterAttrs (name: _: hasPrefix "images/" name) pkgs))
          ++ [
            "age-plugin-yubikey"
            "blur"
            "fire"
            "git-branchless"
            "grim"
            "kile"
            "libdrm24109"
            "matrix-conduit"
            "meson-061"
            "my-emacs"
            "my-emacs-config"
            "mynerdfonts"
            "netns-dbus-proxy"
            "netns-exec"
            "nixpkgs-fmt"
            "persway"
            "pixieboot"
            "pxebooter"
            "ristate"
            "rofi-wayland"
            "rust-analyzer-bin"
            "scripts"
            "slurp"
            "spotifyd"
            "spotnix"
            "sway"
            "sway-unwrapped"
            "swaybg"
            "swayidle"
            "swaylock"
            "swaylock-dope"
            "wayland-protocols-master"
            "wayland120"
            "wl-cliboard"
            "wl-cliboard-x11"
            "wlroots-master"
            "xdg-desktop-portal-wlr"
          ];
      in {
        os = ["ubuntu-latest"];
        pkg = filter (item: !(elem item skip)) (mapAttrsToList (name: _: name) exportedPackages.packages.aarch64-linux);
      };

      github-actions-host-matrix-x86-64-linux = {
        os = ["ubuntu-latest"];
        host = mapAttrsToList (name: _: name) (filterAttrs (_: config: config.system == "x86_64-linux") hostConfigurations);
      };

      github-actions-host-matrix-aarch64-linux = {
        os = ["ubuntu-latest"];
        host = mapAttrsToList (name: _: name) (filterAttrs (_: config: config.system == "aarch64-linux") hostConfigurations);
      };
    };
}
