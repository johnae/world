{
  lib,
  pkgs,
  config,
  ...
}: let
  l = lib // builtins;
  inherit
    (l)
    mkOption
    mkIf
    mkMerge
    mkForce
    types
    optionals
    mapAttrsToList
    flatten
    concatStringsSep
    isList
    isString
    isPath
    isAttrs
    isBool
    mapAttrs
    sort
    lessThan
    ;
  inherit (config.systemd) enableUnifiedCgroupHierarchy;
  cfg = config.services.k3s;
  k3sManifestsDir = "/var/lib/rancher/k3s/server/manifests";
  containerdConfigDir = "/var/lib/rancher/k3s/agent/etc/containerd";
  containerdConfig = pkgs.writeText "config.toml.tmpl" ''
    [plugins.opt]
      path = "{{ .NodeConfig.Containerd.Opt }}"

    [plugins.cri]
      stream_server_address = "127.0.0.1"
      stream_server_port = "10010"
      enable_selinux = {{ .NodeConfig.SELinux }}
      enable_unprivileged_ports = true
      enable_unprivileged_icmp = true

    {{- if .DisableCgroup}}
      disable_cgroup = true
    {{end}}
    {{- if .IsRunningInUserNS }}
      disable_apparmor = true
      restrict_oom_score_adj = true
    {{end}}

    {{- if .NodeConfig.AgentConfig.PauseImage }}
      sandbox_image = "{{ .NodeConfig.AgentConfig.PauseImage }}"
    {{end}}

    {{- if .NodeConfig.AgentConfig.Snapshotter }}
    [plugins.cri.containerd]
      snapshotter = "{{ .NodeConfig.AgentConfig.Snapshotter }}"
      disable_snapshot_annotations = {{ if eq .NodeConfig.AgentConfig.Snapshotter "stargz" }}false{{else}}true{{end}}
    {{ if eq .NodeConfig.AgentConfig.Snapshotter "stargz" }}
    {{ if .NodeConfig.AgentConfig.ImageServiceSocket }}
    [plugins.stargz]
    cri_keychain_image_service_path = "{{ .NodeConfig.AgentConfig.ImageServiceSocket }}"
    [plugins.stargz.cri_keychain]
    enable_keychain = true
    {{end}}
    {{ if .PrivateRegistryConfig }}
    {{ if .PrivateRegistryConfig.Mirrors }}
    [plugins.stargz.registry.mirrors]{{end}}
    {{range $k, $v := .PrivateRegistryConfig.Mirrors }}
    [plugins.stargz.registry.mirrors."{{$k}}"]
      endpoint = [{{range $i, $j := $v.Endpoints}}{{if $i}}, {{end}}{{printf "%q" .}}{{end}}]
    {{if $v.Rewrites}}
      [plugins.stargz.registry.mirrors."{{$k}}".rewrite]
    {{range $pattern, $replace := $v.Rewrites}}
        "{{$pattern}}" = "{{$replace}}"
    {{end}}
    {{end}}
    {{end}}
    {{range $k, $v := .PrivateRegistryConfig.Configs }}
    {{ if $v.Auth }}
    [plugins.stargz.registry.configs."{{$k}}".auth]
      {{ if $v.Auth.Username }}username = {{ printf "%q" $v.Auth.Username }}{{end}}
      {{ if $v.Auth.Password }}password = {{ printf "%q" $v.Auth.Password }}{{end}}
      {{ if $v.Auth.Auth }}auth = {{ printf "%q" $v.Auth.Auth }}{{end}}
      {{ if $v.Auth.IdentityToken }}identitytoken = {{ printf "%q" $v.Auth.IdentityToken }}{{end}}
    {{end}}
    {{ if $v.TLS }}
    [plugins.stargz.registry.configs."{{$k}}".tls]
      {{ if $v.TLS.CAFile }}ca_file = "{{ $v.TLS.CAFile }}"{{end}}
      {{ if $v.TLS.CertFile }}cert_file = "{{ $v.TLS.CertFile }}"{{end}}
      {{ if $v.TLS.KeyFile }}key_file = "{{ $v.TLS.KeyFile }}"{{end}}
      {{ if $v.TLS.InsecureSkipVerify }}insecure_skip_verify = true{{end}}
    {{end}}
    {{end}}
    {{end}}
    {{end}}
    {{end}}

    {{- if not .NodeConfig.NoFlannel }}
    [plugins.cri.cni]
      bin_dir = "{{ .NodeConfig.AgentConfig.CNIBinDir }}"
      conf_dir = "{{ .NodeConfig.AgentConfig.CNIConfDir }}"
    {{end}}

    [plugins.cri.containerd.runtimes.runc]
      runtime_type = "io.containerd.runc.v2"

    [plugins.cri.containerd.runtimes.runc.options]
      SystemdCgroup = ${
      if enableUnifiedCgroupHierarchy
      then "true"
      else "false"
    }

    {{ if .PrivateRegistryConfig }}
    {{ if .PrivateRegistryConfig.Mirrors }}
    [plugins.cri.registry.mirrors]{{end}}
    {{range $k, $v := .PrivateRegistryConfig.Mirrors }}
    [plugins.cri.registry.mirrors."{{$k}}"]
      endpoint = [{{range $i, $j := $v.Endpoints}}{{if $i}}, {{end}}{{printf "%q" .}}{{end}}]
    {{if $v.Rewrites}}
      [plugins.cri.registry.mirrors."{{$k}}".rewrite]
    {{range $pattern, $replace := $v.Rewrites}}
        "{{$pattern}}" = "{{$replace}}"
    {{end}}
    {{end}}
    {{end}}

    {{range $k, $v := .PrivateRegistryConfig.Configs }}
    {{ if $v.Auth }}
    [plugins.cri.registry.configs."{{$k}}".auth]
      {{ if $v.Auth.Username }}username = {{ printf "%q" $v.Auth.Username }}{{end}}
      {{ if $v.Auth.Password }}password = {{ printf "%q" $v.Auth.Password }}{{end}}
      {{ if $v.Auth.Auth }}auth = {{ printf "%q" $v.Auth.Auth }}{{end}}
      {{ if $v.Auth.IdentityToken }}identitytoken = {{ printf "%q" $v.Auth.IdentityToken }}{{end}}
    {{end}}
    {{ if $v.TLS }}
    [plugins.cri.registry.configs."{{$k}}".tls]
      {{ if $v.TLS.CAFile }}ca_file = "{{ $v.TLS.CAFile }}"{{end}}
      {{ if $v.TLS.CertFile }}cert_file = "{{ $v.TLS.CertFile }}"{{end}}
      {{ if $v.TLS.KeyFile }}key_file = "{{ $v.TLS.KeyFile }}"{{end}}
      {{ if $v.TLS.InsecureSkipVerify }}insecure_skip_verify = true{{end}}
    {{end}}
    {{end}}
    {{end}}

    {{range $k, $v := .ExtraRuntimes}}
    [plugins.cri.containerd.runtimes."{{$k}}"]
      runtime_type = "{{$v.RuntimeType}}"
    [plugins.cri.containerd.runtimes."{{$k}}".options]
      BinaryName = "{{$v.BinaryName}}"
    {{end}}
  '';
  settingsToCli = s: let
    boolToCli = path: value:
      if value
      then "--${path}"
      else "";
    listToCli = path: value:
      concatStringsSep " "
      (map (item: "--${path} \"${toString item}\"") value);
    attrsToCli = path:
      mapAttrsToList (
        k: v:
          if isBool v
          then boolToCli path v
          else "--${path} \"${k}=${toString v}\""
      );
    fieldToCli = path: value:
      if isAttrs value
      then attrsToCli path value
      else if isBool value
      then boolToCli path value
      else if isList value
      then listToCli path value
      else "--${path} \"${toString value}\"";
  in
    flatten (mapAttrsToList fieldToCli s);
in {
  options.services.k3s.autoDeploy = mkOption {
    type = types.attrsOf (
      types.either
      types.path
      (types.attrsOf types.anything)
    );
    default = {};
    apply = mapAttrs (name: value:
      if (isPath value || isString value)
      then value
      else
        pkgs.runCommand "${name}.yaml" {} ''
          cat<<EOF>$out
          ${builtins.toJSON value}
          EOF
        '');
  };

  options.services.k3s.after = mkOption {
    type = types.listOf types.str;
    default = [];
  };

  options.services.k3s.disable = mkOption {
    type = types.listOf (types.enum ["coredns" "servicelb" "traefik" "local-storage" "metrics-server"]);
    default = [];
  };

  options.services.k3s.settings = mkOption {
    type = types.attrsOf types.anything;
    default = {};
  };

  config = mkIf cfg.enable {
    assertions = mkForce [];
    services.k3s.extraFlags = concatStringsSep " " (sort lessThan (settingsToCli cfg.settings));
    systemd.services.k3s.preStart = ''
      mkdir -p ${containerdConfigDir}
      cp ${containerdConfig} ${containerdConfigDir}/config.toml.tmpl
      ${
        if cfg.role == "server"
        then ''
          mkdir -p ${k3sManifestsDir}
          ${
            concatStringsSep "\n" (mapAttrsToList (
                name: path: "cp ${path} ${k3sManifestsDir}/${name}.yaml"
              )
              cfg.autoDeploy)
          }
          ${
            concatStringsSep "\n" (map (
                manifestName: "touch ${k3sManifestsDir}/${manifestName}.yaml.skip"
              )
              cfg.disable)
          }
        ''
        else ""
      }
    '';
    ## Random fixes and hacks for k3s networking
    ## see: https://github.com/NixOS/nixpkgs/issues/98766
    boot.kernelModules = ["br_netfilter" "ip_conntrack" "ip_vs" "ip_vs_rr" "ip_vs_wrr" "ip_vs_sh" "overlay"];
    systemd.services.k3s.after = ["network-online.service" "firewall.service"] ++ cfg.after;
  };
}
