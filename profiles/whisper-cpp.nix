{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.whisper-cpp;
  inherit (lib) mkIf mkOption mkEnableOption types concatStringsSep escapeShellArg optional;
  modelPath = "/var/lib/whisper-cpp/${cfg.modelFile}";
  ## A file rather than an argument: artist names carry apostrophes, and
  ## ExecStart has its own quoting rules and % specifiers.
  promptFile = pkgs.writeText "whisper-cpp-prompt" cfg.initialPrompt;
in {
  options.services.whisper-cpp = {
    enable = mkEnableOption "whisper.cpp speech-to-text on the GPU for Home Assistant";

    port = mkOption {
      type = types.port;
      default = 10403;
      description = "Wyoming port Home Assistant connects to.";
    };

    serverPort = mkOption {
      type = types.port;
      default = 8910;
      description = "Loopback port whisper-server listens on.";
    };

    model = mkOption {
      type = types.str;
      default = "KBLab/kb-whisper-large";
      description = ''
        Hugging Face repo carrying ggml weights. Its tokenizer.json is also what
        the bridge counts the prompt budget with, so the two stay matched.
      '';
    };

    modelFile = mkOption {
      type = types.str;
      default = "ggml-model.bin";
      description = ''
        Full precision by default: q5_0 is half the latency but lost the
        spelling of foreign names in testing, which is what the prompt is for.
      '';
    };

    language = mkOption {
      type = types.str;
      default = "sv";
    };

    initialPrompt = mkOption {
      type = types.str;
      default = "";
      description = ''
        Kept ahead of Home Assistant's names within the token budget. Written
        as the kind of sentence that gets spoken, since whisper conditions on
        it as preceding speech rather than as a list of words.
      '';
    };

    promptMaxTokens = mkOption {
      type = types.int;
      default = 200;
      description = ''
        Whisper truncates the prompt at 223 tokens and starts echoing words that
        were never spoken well before that; 200 is wyoming-faster-whisper's
        default for the same reason.
      '';
    };

    hassApi = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "http://icarus:8123/api";
      description = "Home Assistant API to read names from. Null disables biasing.";
    };

    hassTokenFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "Long-lived Home Assistant token, only used to list names.";
    };

    openFirewall = mkOption {
      type = types.bool;
      default = false;
      description = "Open the wyoming port. Not needed when Home Assistant reaches it over tailscale.";
    };
  };

  config = mkIf cfg.enable {
    ## Vulkan for the same reason as Qwen3-ASR's llama.cpp: it is in the binary
    ## cache, where a rocm torch would mean compiling torch, and faster-whisper
    ## has no AMD GPU backend at all. kb-whisper-large answers in ~0.2s here
    ## against ~8s on the CPU.
    systemd.services.whisper-cpp-server = {
      description = "whisper.cpp server";
      wantedBy = ["multi-user.target"];
      after = ["network-online.target"];
      wants = ["network-online.target"];
      path = [pkgs.curl];

      ## whisper-server takes a file, not a repo, so fetch it on first start.
      ## Downloaded beside the target and renamed, so an interrupted transfer
      ## never passes for a model.
      preStart = ''
        if [ ! -s ${modelPath} ]; then
          curl -fL --retry 3 -o ${modelPath}.part \
            ${escapeShellArg "https://huggingface.co/${cfg.model}/resolve/main/${cfg.modelFile}"}
          mv ${modelPath}.part ${modelPath}
        fi
      '';

      serviceConfig = {
        ExecStart = concatStringsSep " " [
          (lib.getExe' (pkgs.whisper-cpp.override {vulkanSupport = true;}) "whisper-server")
          "-m ${modelPath}"
          "--host 127.0.0.1"
          "--port ${toString cfg.serverPort}"
          "-l ${cfg.language}"
          "-bs 5"
        ];
        Restart = "on-failure";
        RestartSec = 10;
        DynamicUser = true;
        StateDirectory = "whisper-cpp";
        ## Without HOME the vulkan backend puts its shader cache in //.cache,
        ## fails on the read-only root and recompiles shaders on every start.
        Environment = ["HOME=/var/lib/whisper-cpp"];
        ## StateDirectory does not order against the impermanence bind mount.
        RequiresMountsFor = "/var/lib/private/whisper-cpp";
        SupplementaryGroups = ["render" "video"];
        ## The first start downloads 3 GB.
        TimeoutStartSec = "30min";
      };
    };

    systemd.services.whisper-cpp = {
      description = "Wyoming bridge for whisper.cpp";
      wantedBy = ["multi-user.target"];
      after = ["whisper-cpp-server.service"];
      wants = ["whisper-cpp-server.service"];

      ## The token reaches the bridge through the file the variable names, so
      ## it stays out of both argv and the environment.
      environment = {
        HF_HOME = "/var/lib/whisper-cpp-bridge";
        WYO_WHISPER_HASS_TOKEN_FILE = mkIf (cfg.hassTokenFile != null) "%d/hass-token";
      };

      serviceConfig = {
        ExecStart = concatStringsSep " " (
          [
            (lib.getExe pkgs.wyoming-whisper-cpp)
            "--uri tcp://0.0.0.0:${toString cfg.port}"
            "--api http://127.0.0.1:${toString cfg.serverPort}"
            "--language ${cfg.language}"
            "--tokenizer ${cfg.model}"
            "--prompt-max-tokens ${toString cfg.promptMaxTokens}"
          ]
          ++ optional (cfg.initialPrompt != "") "--initial-prompt-file ${promptFile}"
          ++ optional (cfg.hassApi != null) "--hass-api ${cfg.hassApi}"
        );
        LoadCredential = mkIf (cfg.hassTokenFile != null) ["hass-token:${cfg.hassTokenFile}"];
        Restart = "on-failure";
        RestartSec = 5;
        DynamicUser = true;
        ## Only the tokenizer, but the bridge cannot start without it.
        StateDirectory = "whisper-cpp-bridge";
        RequiresMountsFor = "/var/lib/private/whisper-cpp-bridge";
      };
    };

    networking.firewall.allowedTCPPorts = mkIf cfg.openFirewall [cfg.port];

    environment.persistence."/keep".directories = mkIf config.ephemeralRoot [
      "/var/lib/private/whisper-cpp"
      "/var/lib/private/whisper-cpp-bridge"
    ];
  };
}
