{
  config,
  pkgs,
  ...
}: let
  aider = pkgs.writeShellApplication {
    name = "aider";
    runtimeInputs = [pkgs.aider-chat];
    text = ''
      OPENAI_API_KEY="$(cat ${config.age.secrets.openai-api-key.path})";
      CLAUDE_API_KEY="$(cat ${config.age.secrets.anthropic-api-key.path})";
      OLLAMA_API_BASE="http://eris:11434";
      export OPENAI_API_KEY CLAUDE_API_KEY OLLAMA_API_BASE
      exec aider "$@"
    '';
  };
in {
  age.secrets.openai-api-key.rekeyFile = ../../secrets/openai-api-key.age;
  age.secrets.anthropic-api-key.rekeyFile = ../../secrets/anthropic-api-key.age;

  home.packages = [aider];
}
