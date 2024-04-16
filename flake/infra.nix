{inputs, ...}: {
  perSystem = {
    pkgs,
    lib,
    system,
    ...
  }: let
    inherit (lib) listToAttrs flatten mapAttrs' mapAttrsToList;
    packages =
      mapAttrs' (name: _: {
        name = "terranix-${name}";
        value = inputs.terranix.lib.terranixConfiguration {
          inherit system;
          modules = [../infra/${name}/config.nix];
        };
      })
      (builtins.readDir ../infra);
  in {
    inherit packages;
    apps = listToAttrs (flatten (mapAttrsToList (
      name: _: let
        cfg = packages."terranix-${name}";
      in [
        {
          name = "terranix-${name}-apply";
          value = {
            type = "app";
            program = toString (pkgs.writers.writeBash "apply" ''
              rm -f config.tf.json
              cp ${cfg} config.tf.json
              echo -- Tofu Init --
              ${pkgs.tofuWithPlugins}/bin/tofu init
              echo -- Tofu Apply --
              ${pkgs.tofuWithPlugins}/bin/tofu apply
              sleep 10

              unlocked_all=1

              function unlock() {
                for server in $(${pkgs.hcloud}/bin/hcloud server list -o noheader | awk '{print $4}'); do
                  echo "Probing host $server on strPort 2222"
                  if timeout 5 bash -c "</dev/tcp/$server/2222"; then
                    echo "Host $server is up, unlocking"
                    ${pkgs.rbw}/bin/rbw get hetzner -- cloud_disk_password | ssh -oStrictHostKeyChecking=no -oUserKnownHostsFile=/dev/null -i /home/john/.ssh/id_ed25519_alt -p 2222 "root@$server"
                  else
                    if timeout 5 bash -c "</dev/tcp/$server/22"; then
                      echo "Host $server is already unlocked"
                    else
                      unlocked_all=0
                      echo "Host $server is down, retry later"
                    fi
                  fi
                done
              }

              unlock
              if [[ $unlocked_all -eq 0 ]]; then
                echo "retrying unlocking in 10 seconds"
                sleep 10
                unlock
              fi
            '');
          };
        }
        {
          name = "terranix-${name}-destroy";
          value = {
            type = "app";
            program = toString (pkgs.writers.writeBash "destroy" ''
              rm -f config.tf.json
              cp ${cfg} config.tf.json
              echo -- Tofu Init --
              ${pkgs.tofuWithPlugins}/bin/tofu init
              echo -- Tofu Destroy --
              ${pkgs.tofuWithPlugins}/bin/tofu destroy
            '');
          };
        }

        {
          name = "terranix-${name}-plan";
          value = {
            type = "app";
            program = toString (pkgs.writers.writeBash "plan" ''
              rm -f config.tf.json
              cp ${cfg} config.tf.json
              echo -- Tofu Init --
              ${pkgs.tofuWithPlugins}/bin/tofu init
              echo -- Tofu Plan --
              ${pkgs.tofuWithPlugins}/bin/tofu plan
            '');
          };
        }
      ]
    ) (builtins.readDir ../infra)));
    # apps = foldl' (apps: _:
    #   apps
    #   // (mapAttrs' (name: _: let
    #       cfg = packages."terranix-${name}";
    #     in {
    #       name = "terranix-${name}-apply";
    #       value = {
    #         type = "app";
    #         program = toString (pkgs.writers.writeBash "apply" ''
    #           rm -f config.tf.json
    #           cp ${cfg} config.tf.json
    #           ${pkgs.tofuWithPlugins}/bin/tofu init
    #           ${pkgs.tofuWithPlugins}/bin/tofu apply
    #         '');
    #       };
    #     })
    #     builtins.readDir
    #     ../infra)) {};
  };
}
