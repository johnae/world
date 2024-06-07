{
  pkgs,
  config,
  lib,
  ...
}: let
  yamlFormat = pkgs.formats.yaml {};
  l = lib // builtins;
  inherit
    (l)
    literalExpression
    mkEnableOption
    mkIf
    mkMerge
    mkOption
    types
    ;
  cfg = config.programs.kubie;
in {
  options = {
    programs.kubie = {
      enable = mkEnableOption "Kubie";
      package = mkOption {
        type = types.package;
        default = pkgs.kubie;
        defaultText = literalExpression "pkgs.kubie";
        description = "The Kubie package to install";
      };
      settings = mkOption {
        inherit (yamlFormat) type;
        default = {};
        example = literalExpression ''
          {
            # Force kubie to use a particular shell, if unset detect shell currently in use.
            # Possible values: bash, dash, fish, xonsh, zsh
            # Default: unset
            # shell = "bash";

            # Configure where to look for kubernetes config files.
            configs = {
              # Include these globs.
              # Default: values listed below.
              include = [
                "~/.kube/config"
                "~/.kube/*.yml"
                "~/.kube/*.yaml"
                "~/.kube/configs/*.yml"
                "~/.kube/configs/*.yaml"
                "~/.kube/kubie/*.yml"
                "~/.kube/kubie/*.yaml"
              ];
              # Exclude these globs.
              # Default: values listed below.
              # Note: kubie's own config file is always excluded.
              exclude = [
                "~/.kube/kubie.yaml"
              ];
            };

            # Prompt settings.
            prompt = {
              # Disable kubie's custom prompt inside of a kubie shell. This is useful
              # when you already have a prompt displaying kubernetes information.
              # Default: false
              disable = true;

              # When using recursive contexts, show depth when larger than 1.
              # Default: true
              show_depth = true;

              # When using zsh, show context and namespace on the right-hand side using RPS1.
              # Default: false
              zsh_use_rps1 = false;

              # When using fish, show context and namespace on the right-hand side.
              # Default: false
              fish_use_rprompt = false;

              # When using xonsh, show context and namespace on the right-hand side.
              # Default: false
              xonsh_use_right_prompt = false;
            };

            # Behavior
            behavior = {
              # Make sure the namespace exists with `kubectl get namespaces` when switching
              # namespaces. If you do not have the right to list namespaces, disable this.
              # Default: true
              validate_namespaces = true;

              # Enable or disable the printing of the 'CONTEXT => ...' headers when running
              # `kubie exec`.
              # Valid values:
              #   auto:   Prints context headers only if stdout is a TTY. Piping/redirecting
              #           kubie output will auto-disable context headers.
              #   always: Always prints context headers, even if stdout is not a TTY.
              #   never:  Never prints context headers.
              # Default: auto
              print_context_in_exec = true;
            };
          }
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home.packages = [cfg.package];
      home.file.".kube/kubie.yaml" = mkIf (cfg.settings != {}) {
        source = yamlFormat.generate "alacritty.yml" cfg.settings;
      };
    })
  ];
}
