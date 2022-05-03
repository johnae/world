{
  lib,
  pkgs,
  config,
  hostName,
  ...
}: let
  inherit (lib) optionals mkIf;
  cfg = config.services.k3s;
in {
  services.k3s.enable = true;
  services.k3s.settings.node-label.hostname = hostName;
  services.k3s.disable = ["traefik"];
  services.k3s.autoDeploy.kured = ../files/kubernetes/kured.yaml;
  services.k3s.autoDeploy.flux = "${pkgs.fluxcd-yaml}/flux.yaml";
  services.k3s.autoDeploy.flux-system-repo = {
    apiVersion = lib.mkDefault "source.toolkit.fluxcd.io/v1beta1";
    kind = lib.mkDefault "GitRepository";
    metadata.name = lib.mkDefault "flux-system";
    metadata.namespace = lib.mkDefault "flux-system";
    spec = {
      gitImplementation = lib.mkDefault "go-git";
      interval = lib.mkDefault "1m0s";
      ref.branch = lib.mkDefault "main";
      secretRef.name = lib.mkDefault "flux-system";
      timeout = lib.mkDefault "20s";
      url = lib.mkDefault "ssh://git@github.com/johnae/flux-system.git";
    };
  };
  services.k3s.autoDeploy.flux-system-kustomization = {
    apiVersion = lib.mkDefault "kustomize.toolkit.fluxcd.io/v1beta1";
    kind = lib.mkDefault "Kustomization";
    metadata.name = lib.mkDefault "flux";
    metadata.namespace = lib.mkDefault "flux-system";
    spec = {
      interval = lib.mkDefault "5m0s";
      sourceRef.kind = lib.mkDefault "GitRepository";
      sourceRef.name = lib.mkDefault "flux-system";
      path = lib.mkDefault "./components/flux";
      prune = lib.mkDefault true;
      validation = lib.mkDefault "client";
    };
  };
  services.k3s.autoDeploy.cluster-vars = {
    apiVersion = lib.mkDefault "v1";
    kind = lib.mkDefault "ConfigMap";
    metadata.name = lib.mkDefault "cluster-vars";
    metadata.namespace = lib.mkDefault "flux-system";
    data = rec {
      cluster = lib.mkDefault "home";
      context = lib.mkDefault "default";
      dns_zone = lib.mkDefault "insane.se";
      environment = lib.mkDefault "production";
      prometheus_remote_write_url = lib.mkDefault "https://metrics.insane.se/api/v1/push";
      loki_remote_write_url = lib.mkDefault "https://logs.insane.se/loki/api/v1/push";
      grafana_dns = lib.mkDefault "grafana.insane.se";
      loki_bucket = lib.mkDefault "minio.minio.svc.cluster.local/loki-d78f";
      cortex_bucket = lib.mkDefault "cortex-d78f";
      cortex_bucket_host = lib.mkDefault "minio.miniosvc.cluster.local";
      pod_subnet = lib.mkDefault cfg.k3s.settings.cluster-cidr;
    };
  };

  networking.firewall.trustedInterfaces = ["cni+" "flannel.1" "calico+" "cilium+" "lxc+"];
  environment.state."/keep" = {
    directories = [
      "/etc/rancher"
      "/var/lib/dockershim"
      "/var/lib/rancher"
      "/var/lib/kubelet"
      "/var/lib/cni"
      "/var/lib/containerd"
    ];
  };
}
