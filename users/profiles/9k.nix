{config, ...}: {
  age.secrets.exa-api-key.rekeyFile = ../../secrets/exa-api-key.age;
  age.secrets.context7-api-key.rekeyFile = ../../secrets/context7-api-key.age;
  age.secrets.ghcli-token.rekeyFile = ../../secrets/ghcli_token.age;

  # GitHub token for gh CLI and nix. Exporting NIX_CONFIG raises GitHub API
  # rate limits during `world` flake operations (eval runs as this user). Read
  # at shell startup so the token never lands in the nix store; guarded so
  # machines without the secret are unaffected.
  programs.nushell.extraConfig = ''
    # agenix decrypts this under $XDG_RUNTIME_DIR at runtime; build the path in
    # nushell since the home-manager secret path embeds a literal
    # ''${XDG_RUNTIME_DIR} that nushell would not expand.
    let ghTokenFile = $"($env.XDG_RUNTIME_DIR)/agenix/ghcli-token"
    if ($ghTokenFile | path exists) {
      let ghToken = (open --raw $ghTokenFile | str trim)
      $env.GH_TOKEN = $ghToken
      let nixLine = $"access-tokens = github.com=($ghToken)"
      let existing = ($env.NIX_CONFIG? | default "")
      $env.NIX_CONFIG = (if ($existing | is-empty) { $nixLine } else { $"($existing)\n($nixLine)" })
    }
  '';

  programs.claude-code.context = ''
    Address me as **Mr 9k**. When you can't figure something out, just say so.

    @~/.claude/shared/base.md
    @~/.claude/shared/extras.md
  '';

  home.file.".claude/secrets.sh" = {
    executable = true;
    text = ''
      export EXA_API_KEY="$(cat ${config.age.secrets.exa-api-key.path})"
      export CONTEXT7_API_KEY="$(cat ${config.age.secrets.context7-api-key.path})"
    '';
  };
}
