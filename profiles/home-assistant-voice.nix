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
    ## large-v3-turbo. Short commands are what this transcribes all day, and
    ## on 16 threads plain large-v3 costs seconds for little gain on them.
    ## CPU because the 5750G only has an iGPU and the module offers cuda, not
    ## rocm.
    model = "turbo";
    device = "cpu";
    beamSize = 1;
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
