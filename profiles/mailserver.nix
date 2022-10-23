{
  lib,
  config,
  ...
}: let
  cfg = config.mailserver;
in {
  config = with cfg;
    lib.mkIf enable {
      ## we want to use dns for certs, not http
      services.nginx.enable = lib.mkForce false;
      mailserver.openFirewall = false; ## disable this explicitly so we can remove port 80 from default open ports below
      networking.firewall = with cfg;
        lib.mkIf enable {
          allowedTCPPorts =
            [25]
            ++ lib.optional enableSubmission 587
            ++ lib.optional enableSubmissionSsl 465
            ++ lib.optional enableImap 143
            ++ lib.optional enableImapSsl 993
            ++ lib.optional enablePop3 110
            ++ lib.optional enablePop3Ssl 995
            ++ lib.optional enableManageSieve 4190;
        };
      environment.state."/keep".directories = [
        "/var/vmail"
        "/var/dkim"
        "/var/sieve"
        "/var/certs"
        "/var/lib/postfix"
        "/var/lib/dhparams"
        "/var/lib/acme"
        "/var/lib/dovecot"
      ];
    };
}
