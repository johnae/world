{
  security.acme.acceptTerms = true;
  security.acme.defaults = {
    email = "john+certs@9000.dev";
    # Scaleway's own nameserver answers for the zone, so lego sees its
    # challenge record without waiting on a resolver's cache.
    dnsResolver = "ns0.dom.scw.cloud:53";
    dnsProvider = "scaleway";
    environmentFile = "/run/agenix/scaleway-dns-env";
  };
}
