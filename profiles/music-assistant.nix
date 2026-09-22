{pkgs, ...}: {
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

    ## nixpkgs deletes the vendored librespot binaries and carries a patch that
    ## looks the binary up on PATH instead, where the module puts librespot-ma.
    ## Only the import hunk of that patch applies to 2.10.3 - the hunk changing
    ## the lookup itself silently does not - so the setup flow still searches
    ## the deleted directory and dies with "Unable to locate Librespot for
    ## linux/x86_64". Redo the intended substitution here.
    ##
    ## replace-fail, so this breaks loudly once nixpkgs repairs the patch.
    package = pkgs.music-assistant.overrideAttrs (old: {
      postPatch =
        (old.postPatch or "")
        + ''
          substituteInPlace music_assistant/providers/spotify/helpers.py \
            --replace-fail 'os.path.join(base_path, f"librespot-{system}-{architecture}")' 'which("librespot")'
        '';
    });
  };

  ## Music Assistant normally reads its shared Spotify credential from an
  ## app_secrets.json injected at build time from a private repo, so the
  ## nixpkgs build has no value for it and the setup flow hands Spotify an
  ## authorize URL with no client_id. That first step is not skippable - the
  ## "use your own developer key" option only appears *after* it succeeds -
  ## so the way in is the documented single-value override, pointed at our
  ## own registered app rather than the project's shared one.
  ##
  ## Not a secret: PKCE is precisely the flow for clients that cannot keep
  ## one, and Spotify sees this id in the browser URL anyway.
  ##
  ## The app must have https://music-assistant.io/callback registered as its
  ## redirect URI. That is a fixed page upstream which bounces back to this
  ## host, with the real callback carried in the OAuth `state`, which is how
  ## a tailnet-only instance can complete the flow at all.
  systemd.services.music-assistant.environment.MASS_APP_VAR_SPOTIFY_CLIENT_ID = "055d895137e240a1afdcaacb390eef7c";

  ## DynamicUser with StateDirectory=music-assistant, so systemd owns the
  ## directory and chowns it on start - no explicit user/group needed here,
  ## unlike the mosquitto case.
  environment.persistence."/keep".directories = ["/var/lib/private/music-assistant"];
}
