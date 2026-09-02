{...}: {
  perSystem = {
    pkgs,
    lib,
    system,
    ...
  }:
    lib.optionalAttrs (system == "x86_64-linux") {
      # A small image for cluster jobs that talk to an HTTP API and need to
      # parse the answer: curl + jq + GNU coreutils (for a `date` that reads
      # ISO 8601). Built here rather than pulled from a third party because
      # the jobs using it hold credentials (the tailscale keygen mints
      # auth keys with an OAuth client), and none of the official images
      # ship both tools. Published by hand, tagged by store hash:
      #   nix build .#curl-jq-image && ./result | skopeo copy docker-archive:/dev/stdin ...
      packages.curl-jq-image = pkgs.dockerTools.streamLayeredImage {
        name = "curl-jq";
        tag = "latest";
        contents = [
          pkgs.bash
          pkgs.coreutils
          pkgs.curl
          pkgs.jq
          pkgs.cacert
        ];
        config = {
          Env = [
            "PATH=/bin"
            "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
          ];
          Cmd = ["/bin/bash"];
        };
      };
    };
}
