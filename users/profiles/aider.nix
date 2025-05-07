{pkgs, ...}: let
  aider = pkgs.writeShellApplication {
    name = "aider";
    runtimeInputs = [pkgs.aider-chat];
    text = ''
      OPENAI_API_KEY="$(cat /run/agenix/openai-api-key)";
      CLAUDE_API_KEY="$(cat /run/agenix/anthropic-api-key)";
      OLLAMA_API_BASE= "http://eris:11434";
      export OPENAI_API_KEY CLAUDE_API_KEY OLLAMA_API_BASE
      exec aider "$@"
    '';
  };
in {
  home.packages = [aider];
}
