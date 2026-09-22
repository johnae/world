{
  config,
  hostName,
  ...
}: {
  ## Kept apart from home-assistant.nix because the voice stack stands on its
  ## own: these are two plain network services Home Assistant talks to over the
  ## wyoming protocol, and removing this import leaves the rest untouched.
  ##
  ## Bound to loopback on purpose. Home Assistant runs on this host, and there
  ## is no reason to offer speech-to-text to the LAN.
  services.wyoming.faster-whisper.servers.sv = {
    enable = true;
    uri = "tcp://127.0.0.1:10300";
    language = "sv";
    ## Swedish-tuned, from the National Library of Sweden, and small enough to
    ## be quick. Size is what matters here: whisper pads every utterance to a
    ## 30s window, so the encoder does identical work for two words or twenty
    ## and short commands are entirely encoder-bound. large-v3-turbo only
    ## shrinks the *decoder*, which is the part a voice command barely uses, so
    ## it cost full large-encoder time for nothing. Measured on this host, 3s
    ## of speech: turbo 8.44s, kb-whisper-medium 11.46s, kb-whisper-small
    ## 2.1s, this 0.28s.
    ##
    ## tiny is a deliberate trade, and the entity-name biasing below is what
    ## pays for it: proper nouns like Värmdögatan are exactly what a small
    ## model guesses wrong. Move to kb-whisper-small if recognition still
    ## misses.
    ##
    ## CPU because the 5750G only has an iGPU, and ctranslate2 is CUDA-or-CPU
    ## with no rocm - a discrete AMD card elsewhere would not help either.
    model = "KBLab/kb-whisper-tiny";
    device = "cpu";
    beamSize = 1;
    extraArgs = [
      ## The published weights are float16, which CPUs can't do efficiently,
      ## so ctranslate2 silently widens them to float32 unless told otherwise.
      "--compute-type"
      "int8"
      ## Not 16, despite the 16 threads available. Measured on kb-whisper-small
      ## here: 4 threads 1.73s, 8 threads 1.33s, 12 threads 1.39s, 16 threads
      ## 1.55s. Past ~8 the thread-sync overhead outweighs the parallelism.
      "--cpu-threads"
      "8"
      ## Biasing context: the server pulls the names of conversation-exposed
      ## entities, areas and floors from Home Assistant and feeds them to
      ## whisper as the prompt for the first window. Proper nouns are what a
      ## tiny model gets wrong, and they are exactly what gets said here.
      ##
      ## Naming the API is also what installs the hass extra: the module adds
      ## it when any extraArg starts with --hass.
      "--hass-api"
      "http://127.0.0.1:8123/api"
      ## The default refreshes on every utterance, putting an HTTP round trip
      ## in front of each transcription. Names here change a few times a year.
      "--hass-refresh-seconds"
      "300"
    ];
  };
  ## A second speech-to-text server, running alongside the whisper one so the
  ## two can be compared by switching engines in the Assist pipeline.
  ##
  ## Whisper transcribes into one language's orthography, and kb-whisper is
  ## fine-tuned on Swedish alone, so a German or English name inside a Swedish
  ## sentence comes out spelled Swedish ("Schrotthagen" -> "Skotthagen").
  ## Qwen3-ASR is a speech LLM that handles Swedish, German and English in one
  ## model, and takes its biasing context as a real system turn rather than
  ## squeezing it into whisper's 223-token prompt.
  ##
  ## It also decodes in time proportional to the audio, where whisper pads
  ## every utterance to 30s, so short commands should be cheaper.
  services.wyoming.faster-whisper.servers.qwen = {
    enable = true;
    uri = "tcp://127.0.0.1:10301";
    language = "sv";
    sttLibrary = "qwen3-asr";
    ## Resolves to rhasspy/qwen3-asr-0.6b-onnx-int4-merged. The merged decoder
    ## caches the biasing prompt's KV across utterances; the split export
    ## re-prefills it every time.
    model = "auto";
    extraArgs = [
      "--cpu-threads"
      "8"
      ## Trimming silence is a real win here and a wash for whisper, which pads
      ## to a fixed window either way.
      "--vad-clip"
      "qwen3-asr"
      "--hass-api"
      "http://127.0.0.1:8123/api"
      "--hass-refresh-seconds"
      "300"
    ];
  };
  ## A third engine for the same comparison. Parakeet TDT 0.6b v3 covers 25
  ## European languages including Swedish and German, detects the language
  ## itself rather than being told, and is a FastConformer-TDT rather than an
  ## encoder-decoder - so it too decodes proportionally to the audio.
  ##
  ## The trade is biasing: --initial-prompt only reaches faster-whisper and
  ## qwen3-asr, so this one never sees the Home Assistant names. It has to get
  ## them right on its own.
  services.wyoming.faster-whisper.servers.parakeet = {
    enable = true;
    uri = "tcp://127.0.0.1:10302";
    language = "sv";
    sttLibrary = "sherpa";
    ## Resolves to sherpa-onnx-nemo-parakeet-tdt-0.6b-v3-int8.
    model = "auto";
    extraArgs = [
      "--cpu-threads"
      "8"
      "--vad-clip"
      "sherpa"
    ];
  };

  ## A long-lived Home Assistant token, read-only in practice: the server only
  ## lists names and never calls a service.
  age.secrets.whisper-hass-token.rekeyFile = ../secrets/${hostName}/whisper-hass-token.age;

  ## The token reaches the server through the file the variable names, so it
  ## stays out of both argv and the environment. LoadCredential because the
  ## unit runs under DynamicUser - there is no fixed uid to chown to.
  systemd.services = let
    hassToken = {
      serviceConfig.LoadCredential = ["hass-token:${config.age.secrets.whisper-hass-token.path}"];
      environment.WYO_WHISPER_HASS_TOKEN_FILE = "%d/hass-token";
    };
  in {
    wyoming-faster-whisper-sv = hassToken;
    wyoming-faster-whisper-qwen = hassToken;
  };

  services.wyoming.piper.servers.sv = {
    enable = true;
    uri = "tcp://127.0.0.1:10200";
    ## The only Swedish piper voice worth using. Downloaded on first start.
    voice = "sv_SE-nst-medium";
  };

  ## Both servers run DynamicUser with StateDirectory under here. The models
  ## are several GB and would otherwise be fetched again on every boot.
  environment.persistence."/keep".directories = ["/var/lib/private/wyoming"];
}
