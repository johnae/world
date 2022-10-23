{lib, ...}: {
  security.acme.acceptTerms = true;
  security.acme.defaults = {
    email = "certs@9000.dev";
    dnsResolver = "1.1.1.1:53";
    dnsProvider = "cloudflare";
    credentialsFile = "/run/agenix/cloudflare-env";
  };
}
