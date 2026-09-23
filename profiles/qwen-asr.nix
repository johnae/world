{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.qwen-asr;
  inherit (lib) mkIf mkOption mkEnableOption types concatStringsSep;
  vocabularyFile = pkgs.writeText "qwen-asr-vocabulary" cfg.vocabulary;
in {
  options.services.qwen-asr = {
    enable = mkEnableOption "Qwen3-ASR speech-to-text for Home Assistant";

    port = mkOption {
      type = types.port;
      default = 10402;
      description = "Wyoming port Home Assistant connects to.";
    };

    llamaPort = mkOption {
      type = types.port;
      default = 8081;
      description = "Loopback port llama-server listens on.";
    };

    model = mkOption {
      type = types.str;
      default = "ggml-org/Qwen3-ASR-1.7B-GGUF";
      description = ''
        Hugging Face repo llama-server pulls. It has to carry an `mmproj`
        alongside the weights - that is the audio projector, without which the
        model is text-only.
      '';
    };

    vocabulary = mkOption {
      type = types.str;
      default = "";
      description = ''
        Biasing text placed in the system turn. Unlike whisper's 223-token
        `initial_prompt` this has no practical budget, which is the whole
        reason for running Qwen3-ASR: names it is told about come back spelled
        correctly, and everything else is unaffected.
      '';
    };

    openFirewall = mkOption {
      type = types.bool;
      default = false;
      description = "Open the wyoming port. Not needed when Home Assistant reaches it over tailscale.";
    };
  };

  config = mkIf cfg.enable {
    ## Vulkan rather than rocm: llama.cpp's vulkan backend is cached, while a
    ## rocm torch is not built anywhere and would mean compiling torch from
    ## source. RADV drives the 7900 XTX fine, and the GPU takes transcription
    ## from ~1.5s to under 150ms.
    systemd.services.qwen-asr-llama = {
      description = "llama.cpp server hosting Qwen3-ASR";
      wantedBy = ["multi-user.target"];
      after = ["network-online.target"];
      wants = ["network-online.target"];

      serviceConfig = {
        ExecStart = concatStringsSep " " [
          (lib.getExe' (pkgs.llama-cpp.override {vulkanSupport = true;}) "llama-server")
          "-hf ${cfg.model}"
          "--host 127.0.0.1"
          "--port ${toString cfg.llamaPort}"
          "-ngl 99"
          "--no-webui"
        ];
        Restart = "on-failure";
        RestartSec = 10;
        DynamicUser = true;
        StateDirectory = "qwen-asr";
        ## llama-server caches the weights under HF's usual layout.
        Environment = ["HF_HOME=/var/lib/qwen-asr"];
        SupplementaryGroups = ["render" "video"];
      };
    };

    systemd.services.qwen-asr = {
      description = "Wyoming bridge for Qwen3-ASR";
      wantedBy = ["multi-user.target"];
      after = ["qwen-asr-llama.service"];
      wants = ["qwen-asr-llama.service"];

      serviceConfig = {
        ExecStart = concatStringsSep " " (
          [
            (lib.getExe pkgs.wyoming-qwen-asr)
            "--uri tcp://0.0.0.0:${toString cfg.port}"
            "--api http://127.0.0.1:${toString cfg.llamaPort}/v1"
          ]
          ++ lib.optional (cfg.vocabulary != "") "--vocabulary-file ${vocabularyFile}"
        );
        Restart = "on-failure";
        RestartSec = 5;
        DynamicUser = true;
      };
    };

    networking.firewall.allowedTCPPorts = mkIf cfg.openFirewall [cfg.port];

    ## Several GB of weights; downloading them again on every boot would make
    ## the service unusable for minutes at a time.
    environment.persistence."/keep".directories = lib.mkIf config.ephemeralRoot [
      "/var/lib/private/qwen-asr"
    ];
  };
}
