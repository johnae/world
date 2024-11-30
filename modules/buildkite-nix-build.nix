{
  config,
  lib,
  ...
}: let
  inherit (lib) types;
  cfg = config.services.buildkite-nix-builder;
in {
  options.services.buildkite-nix-builder = {
    enable = lib.mkEnableOption "Enable buildkite nix builder agents";
    runtimePackages = lib.mkOption {
      type = types.listOf types.package;
      default = [];
    };
    tags = lib.mkOption {
      type = types.attrsOf types.str;
      default = {};
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge (builtins.genList (id: let
      agentName = "nix-build-${toString id}";
      owner = "buildkite-agent-${agentName}";
      secrets = {
        "buildkite-agent-${agentName}-token" = {
          file = ../secrets/buildkite-token.age;
          inherit owner;
        };
        "buildkite-agent-${agentName}-ssh-key" = {
          file = ../secrets/buildkite-ssh-key.age;
          inherit owner;
        };
        "buildkite-agent-${agentName}-cachix-signing-key" = {
          file = ../secrets/cachix-signing-key.age;
          inherit owner;
        };
        "buildkite-agent-${agentName}-github-app-auth-key" = {
          file = ../secrets/github-app-bk-auth.age;
          inherit owner;
        };
      };
    in {
      nix.settings.trusted-users = ["buildkite-agent-${agentName}"];
      age.secrets = secrets;
      services.buildkite-agents."nix-build-${toString id}" = let
        inherit (config.services.buildkite-agents."nix-build-${toString id}") dataDir;
      in {
        inherit (cfg) tags runtimePackages;
        tokenPath = config.age.secrets."buildkite-agent-${agentName}-token".path;
        privateSshKeyPath = config.age.secrets."buildkite-agent-${agentName}-ssh-key".path;
        extraConfig = ''
          plugins-path=${dataDir}/plugins
        '';
        hooks = {
          environment = ''
            CACHIX_SIGNING_KEY="$(head -1 ${config.age.secrets."buildkite-agent-${agentName}-cachix-signing-key".path})"
            CACHE_NAME=insane
            GITHUB_APP_RSA_KEY_FILE="${config.age.secrets."buildkite-agent-${agentName}-github-app-auth-key".path}"
            export CACHIX_SIGNING_KEY CACHE_NAME GITHUB_APP_RSA_KEY_FILE
          '';
          pre-command = ''
            #!/usr/bin/env bash
            cachix use "$CACHE_NAME"
          '';
          command = ''
            #!/usr/bin/env bash
            cachix --verbose watch-exec "$CACHE_NAME" -- bash -c "$BUILDKITE_COMMAND"
          '';
        };
      };
    })
    4));
}
