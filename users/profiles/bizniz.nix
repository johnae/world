{
  config,
  pkgs,
  ...
}: {
  age.secrets.exa-api-key.rekeyFile = ../../secrets/exa-api-key.age;
  age.secrets.context7-api-key.rekeyFile = ../../secrets/context7-api-key.age;
  age.secrets.gitlab-api-url.rekeyFile = ../../secrets/gitlab-api-url.age;
  age.secrets.gitlab-personal-access-token.rekeyFile = ../../secrets/gitlab-personal-access-token.age;
  age.secrets.jira-url.rekeyFile = ../../secrets/jira-url.age;
  age.secrets.jira-username.rekeyFile = ../../secrets/jira-username.age;
  age.secrets.jira-api-token.rekeyFile = ../../secrets/jira-api-token.age;

  # Work-only MCP servers (merge with common servers from mcp.nix)
  programs.mcp.servers = {
    gitlab = {
      command = "${pkgs.nodejs}/bin/npx";
      args = ["-y" "@zereight/mcp-gitlab"];
      env = {
        GITLAB_READ_ONLY_MODE = "false";
        USE_GITLAB_WIKI = "true";
        USE_MILESTONE = "true";
        USE_PIPELINE = "true";
      };
    };
    mcp-atlassian = {
      command = "docker";
      args = [
        "run"
        "-i"
        "--rm"
        "-e"
        "JIRA_URL"
        "-e"
        "JIRA_USERNAME"
        "-e"
        "JIRA_API_TOKEN"
        "ghcr.io/sooperset/mcp-atlassian:latest"
      ];
    };
  };

  programs.claude-code.memory.text = ''
    Address me as **Dr. Bizniz**. When you can't figure something out, just say so.

    @~/.claude/shared/base.md
    @~/.claude/shared/extras.md
  '';

  home.file.".claude/secrets.sh" = {
    executable = true;
    text = ''
      export EXA_API_KEY="$(cat ${config.age.secrets.exa-api-key.path})"
      export CONTEXT7_API_KEY="$(cat ${config.age.secrets.context7-api-key.path})"
      export GITLAB_API_URL="$(cat ${config.age.secrets.gitlab-api-url.path})"
      export GITLAB_PERSONAL_ACCESS_TOKEN="$(cat ${config.age.secrets.gitlab-personal-access-token.path})"
      export JIRA_URL="$(cat ${config.age.secrets.jira-url.path})"
      export JIRA_USERNAME="$(cat ${config.age.secrets.jira-username.path})"
      export JIRA_API_TOKEN="$(cat ${config.age.secrets.jira-api-token.path})"
    '';
  };
}
