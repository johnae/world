{
  ## Music Assistant sits between the streaming providers and the speakers, so
  ## "spela X i köket" resolves to a real queue on a real device. Home
  ## Assistant's own spotify integration can only hand a URI to a player;
  ## everything that makes that usable by voice lives here.
  services.music-assistant = {
    enable = true;
    ## Only pulls in each provider's dependencies - the accounts themselves are
    ## linked in the Music Assistant UI, since they need interactive logins.
    providers = [
      "spotify"
      "sonos"
    ];
    ## 8097 carries the audio itself: the Sonos speakers fetch the stream from
    ## this host, so it has to be reachable on the LAN rather than loopback.
    openFirewall = true;
  };

  ## DynamicUser with StateDirectory=music-assistant, so systemd owns the
  ## directory and chowns it on start - no explicit user/group needed here,
  ## unlike the mosquitto case.
  environment.persistence."/keep".directories = ["/var/lib/private/music-assistant"];
}
