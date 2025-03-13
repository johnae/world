{config, ...}: let
  rootCredentialsFile = config.age.secrets.minio-root-credentials-env.path;
in {
  services.minio = {
    inherit rootCredentialsFile;
    enable = true;
    region = "eu-north-1";
    # listenAddress = ":9000";
    # dataDir = "/var/lib/minio/data;"
    # consoleAddress = ":9001";
    # configDir = "/var/lib/minio/config";
    # certificatesDir = "/var/lib/minio/certs";
  };

  environment.persistence."/keep".directories = [
    "/var/lib/minio/data"
    "/var/lib/minio/config"
    "/var/lib/minio/certs"
  ];
}
