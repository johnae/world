{hostName, ...}: let
  relabel_configs = [
    {
      action = "replace";
      replacement = hostName;
      target_label = "instance";
    }
  ];
in {
  services.prometheus.exporters.node.enable = true;
  services.vmagent = {
    enable = true;
    remoteWrite.url = "https://victoriametrics.9000.dev/api/v1/write";
    prometheusConfig = {
      global = {
        external_labels = {
          "host" = hostName;
        };
      };
      scrape_configs = [
        {
          job_name = "node";
          scrape_interval = "10s";
          static_configs = [
            {targets = ["127.0.0.1:9100"];}
          ];
          inherit relabel_configs;
        }
        {
          job_name = "vmagent";
          scrape_interval = "10s";
          static_configs = [
            {targets = ["127.0.0.1:8429"];}
          ];
          inherit relabel_configs;
        }
      ];
    };
  };
}
