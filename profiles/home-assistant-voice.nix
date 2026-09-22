{
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
    ## of speech: turbo 8.44s, kb-whisper-medium 11.46s, this 1.88s.
    ##
    ## CPU because the 5750G only has an iGPU, and ctranslate2 is CUDA-or-CPU
    ## with no rocm - a discrete AMD card elsewhere would not help either.
    model = "KBLab/kb-whisper-small";
    device = "cpu";
    beamSize = 1;
    ## The published weights are float16, which CPUs can't do efficiently, so
    ## ctranslate2 silently widens them to float32 unless told otherwise. The
    ## wrapper also defaults to 4 threads on a 16-thread part.
    extraArgs = [
      "--compute-type"
      "int8"
      "--cpu-threads"
      "16"
    ];
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
