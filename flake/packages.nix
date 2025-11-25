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
        helix-latest = inputs.helix-editor.packages.${system}.default;
        zjstatus = inputs.zjstatus.packages.${system}.default;
        zj-which-key = inputs.zj-which-key.packages.${system}.default;
        noctalia = inputs.noctalia.packages.${system}.default;
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
