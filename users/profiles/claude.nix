{pkgs, ...}: let
  claude-code = pkgs.writeShellApplication {
    name = "claude";
    text = ''
      if [ -e "$HOME/.claude/secrets.sh" ]; then
        # shellcheck disable=SC1091
        source "$HOME/.claude/secrets.sh"
      fi
      exec ${pkgs.claude-code}/bin/claude "$@"
    '';
  };
in {
  home.file.".claude/shared/base.md".source = ./claude/base.md;
  home.file.".claude/env.sh" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      if command -v direnv >/dev/null; then
        if [ -e .envrc ]; then
          eval "$(direnv export bash 2>/dev/null)"
          exit
        fi
      fi
      if command -v devenv >/dev/null; then
        if devenv --version | grep -q '1\.1*'; then
          eval "$(devenv print-dev-env)"
        else
          eval "$(devenv print-dev-env --no-tui)"
        fi
      fi
    '';
  };

  programs.claude-code = {
    enable = true;
    package = claude-code;
    enableMcpIntegration = true;
    rulesDir = ./claude/rules;
    commandsDir = ./claude/commands;
    agentsDir = ./claude/agents;
  };
}
