{config, ...}: {
  age.secrets.exa-api-key.rekeyFile = ../../secrets/exa-api-key.age;
  age.secrets.context7-api-key.rekeyFile = ../../secrets/context7-api-key.age;

  programs.claude-code.memory.text = ''
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
