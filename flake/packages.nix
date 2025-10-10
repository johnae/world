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

    # tofuProvider = provider:
    #   provider.override (oldArgs: {
    #     provider-source-address =
    #       lib.replaceStrings
    #       ["https://registry.terraform.io/providers"]
    #       ["registry.opentofu.org"]
    #       oldArgs.homepage;
    #   });
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
          runtimeInputs = with pkgs; [just nushell statix deadnix cachix];
          text = ''
            if [ -e files/Justfile ] && [ -e flake.nix ]; then
              just -f ./files/Justfile -d "$(pwd)" "$@"
            else
              just -f ${../files/Justfile} -d "$(pwd)" "$@"
            fi
          '';
        };
        rbw-atomic-unlock = pkgs.writeShellApplication {
          name = "rbw-atomic-unlock";
          runtimeInputs = [pkgs.rbw pkgs.util-linux];
          text = ''
            rbw_lock="/tmp/rbw-unlock.lock"
            exec 200>"$rbw_lock"
            flock -w 15 200
            rbw unlocked || rbw unlock
          '';
        };
        # tofuWithPlugins = pkgs.opentofu.withPlugins (
        #   p:
        #     map tofuProvider [p.null p.external p.hcloud p.cloudflare p.random p.tailscale]
        #);
        kexec-installer-nixos-unstable-noninteractive = kexec-installer inputs.nixpkgs [
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
            system.stateVersion = lib.mkForce "25.05";
            boot.kernelPackages = lib.mkForce pkgs.linuxPackages_latest;
          }
        ];
        helix-latest = inputs.helix-editor.packages.${system}.default;
        # zjstatus = inputs.zjstatus.packages.${system}.default.overrideAttrs (oa: {
        #   patches = [../files/zjstatus_cargo_patch.patch];
        # });
        ## fix this one on darwin
        zwift =
          if pkgs.stdenv.isLinux
          then inputs.zwift.packages.${system}.default
          else pkgs.hello;
        persway =
          if pkgs.stdenv.isLinux
          then inputs.persway.packages.${system}.default
          else pkgs.hello;
        wezterm =
          if pkgs.stdenv.isLinux
          then
            inputs.wezterm.packages.${system}.default.overrideAttrs (oa: {
              patches = [../files/7034-wezterm.patch];
            })
          else
            inputs.wezterm.packages.${system}.default.overrideAttrs (oa: {
              buildInputs = oa.buildInputs ++ [pkgs.openssl];
            });

        victoriametrics-metrics-datasource-plugin = pkgs.stdenvNoCC.mkDerivation {
          pname = "victoriametrics-metrics-datasource-plugin";
          version = "latest";
          src = inputs.victoriametrics-metrics-datasource-plugin;
          installPhase = ''
            cp -R "." "$out"
            chmod -R a-w "$out"
            chmod u+w "$out"
          '';
        };

        victoriametrics-logs-datasource-plugin = pkgs.stdenvNoCC.mkDerivation {
          pname = "victoriametrics-logs-datasource-plugin";
          version = "latest";
          src = inputs.victoriametrics-logs-datasource-plugin;
          installPhase = ''
            cp -R "." "$out"
            chmod -R a-w "$out"
            chmod u+w "$out"
          '';
        };

        victorialogs = pkgs.stdenvNoCC.mkDerivation {
          pname = "victorialogs";
          version = "latest";
          src = inputs.victorialogs;
          installPhase = ''
            mkdir -p "$out"/bin
            cp victoria-logs-prod "$out"/bin/victoria-logs
            chmod +x "$out"/bin/victoria-logs
          '';
        };

        buildkite-agent-patched = pkgs.buildkite-agent.overrideAttrs (oa: {
          postPatch = ''
            ${oa.postPatch}
            substituteInPlace version/version.go --replace 'buildNumber = "x"' 'buildNumber = "123"'
          '';
        });

        tuwunel-latest = inputs.tuwunel.packages.${system}.default;

        inherit
          (inputs.hyprland.packages.${system})
          hyprland
          hyprland-unwrapped
          xdg-desktop-portal-hyprland
          ;

        unlockremote = pkgs.writeShellApplication {
          name = "unlockremote";
          runtimeInputs = with pkgs; [coreutils openssh];
          text = ''
            check_required_vars() {
                for var in CLOUD_DISK_PASSWORD IP SSH_KEY; do
                    if [ -z "''${!var}" ]; then
                        echo "Missing environment variable $var"
                        exit 1
                    fi
                done
            }

            setup_ssh_key() {
                if [ ! -e "$SSH_KEY" ]; then
                    SSH_KEY_PATH="$(mktemp ~/sshkey.XXXXXX)"
                    echo "$SSH_KEY" | base64 -d > "$SSH_KEY_PATH"
                    SSH_KEY="$SSH_KEY_PATH"
                fi
                chmod 0600 "$SSH_KEY"
                trap 'rm -f $SSH_KEY' EXIT
            }

            probe_host() {
                echo "Probing host $REMOTE_IP on port 2222"
                timeout 5 bash -c "</dev/tcp/$REMOTE_IP/2222"
            }

            ssh_unlock() {
                echo "Host $REMOTE_IP is up, unlocking"
                echo "$CLOUD_DISK_PASSWORD" | ssh -oStrictHostKeyChecking=no \
                    -oUserKnownHostsFile=/dev/null \
                    -i "$SSH_KEY" \
                    -p 2222 \
                    "root@$REMOTE_IP"
            }

            unlock() {
                local retries=5
                while (( retries > 0 )); do
                    if probe_host; then
                        ssh_unlock
                        return 0
                    else
                        echo "Host $REMOTE_IP is down, retrying unlock later"
                        retries=$((retries - 1))
                        if (( retries < 1 )); then
                            echo "Failed to unlock host"
                            exit 1
                        fi
                        echo "Waiting 5 seconds..."
                        sleep 5
                    fi
                done
            }

            main() {
                REMOTE_IP="''${REMOTE_IP:-}"
                CLOUD_DISK_PASSWORD="''${CLOUD_DISK_PASSWORD:-}"
                SSH_KEY="''${SSH_KEY:-}"

                check_required_vars
                setup_ssh_key
                unlock
            }

            main
          '';
        };
      };
  };
}
