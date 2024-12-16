{
  config,
  pkgs,
  ...
}: let
  configDir =
    if pkgs.stdenv.isDarwin && !config.xdg.enable
    then "Library/Application Support/aichat"
    else "${config.xdg.configHome}/aichat";
in {
  home.packages = [pkgs.aichat];
  home.file."${configDir}/config.yaml".source = (pkgs.formats.yaml {}).generate "aichat-config.yaml" {
    model = "ollama";
    stream = true;
    clients = [
      {
        type = "openai-compatible";
        name = "ollama";
        api_base = "http://ollama.9000.dev:11434/v1";
        api_key = null;
        models = [
          {
            name = "qwen2.5-coder:32b";
            max_input_tokens = 64000;
          }
        ];
      }
    ];
  };
}
