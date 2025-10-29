{config, ...}: {
  ## nix run nixpkgs#openssl -- genrsa -traditional 4096 | base64 -w0
  age.secrets.atticd-env.file = ../secrets/atticd-env.age;
  services.atticd = {
    enable = true;
    environmentFile = config.age.secrets.atticd-env.path;

    settings = {
      listen = "[::]:8080";
      allowed-hosts = ["cache.9000.dev"];
      api-endpoint = "https://cache.9000.dev/";
      jwt = {};
      storage = {
        type = "s3";
        endpoint = "https://storage.9000.dev/cache";
      };
      chunking = {
        nar-size-threshold = 64 * 1024; # 64 KiB
        min-size = 16 * 1024; # 16 KiB
        avg-size = 64 * 1024; # 64 KiB
        max-size = 256 * 1024; # 256 KiB
      };
    };
  };
}
