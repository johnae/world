{
  ## use a container for now
  virtualisation.oci-containers = {
    backend = "podman";
    containers.homeassistant = {
      volumes = ["home-assistant:/config"];
      environment.TZ = "Europe/Stockholm";
      image = "ghcr.io/home-assistant/home-assistant:2025.2.3";
      extraOptions = [
        "--network=host"
      ];
    };
  };
}
