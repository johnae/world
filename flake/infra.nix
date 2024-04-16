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
              ${pkgs.tofuWithPlugins}/bin/tofu init
              ${pkgs.tofuWithPlugins}/bin/tofu apply
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
              ${pkgs.tofuWithPlugins}/bin/tofu init
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
              ${pkgs.tofuWithPlugins}/bin/tofu init
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
