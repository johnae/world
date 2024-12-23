{
  config,
  pkgs,
  ...
}: let
  configDir =
    if pkgs.stdenv.isDarwin && !config.xdg.enable
    then "Library/Application Support/aichat"
    else "${config.xdg.configHome}/aichat";
  aichat = pkgs.writeShellApplication {
    name = "aichat";
    runtimeInputs = [pkgs.aichat];
    text = ''
      OPENAI_API_KEY="$(cat /run/agenix/openai-api-key)";
      CLAUDE_API_KEY="$(cat /run/agenix/anthropic-api-key)";
      ANTHROPIC_API_KEY="$(cat /run/agenix/anthropic-api-key)";
      export OPENAI_API_KEY CLAUDE_API_KEY ANTHROPIC_API_KEY
      exec aichat "$@"
    '';
  };
in {
  home.packages = [aichat];
  home.file."${configDir}/config.yaml".source = (pkgs.formats.yaml {}).generate "aichat-config.yaml" {
    model = "openai:gpt-4o";
    stream = true;
    keybindings = "vi";
    function_calling = true;
    clients = [
      {
        type = "openai-compatible";
        name = "ollama";
        api_base = "http://ollama.9000.dev:11434/v1";
        api_key = null;
      }
      {
        type = "openai";
        api_base = "https://api.openai.com/v1";
      }
      {
        type = "claude";
        api_base = "https://api.anthropic.com/v1";
      }
    ];
  };
}
