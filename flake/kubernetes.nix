{
  self,
  inputs,
  ...
}: {
  perSystem = {
    pkgs,
    lib,
    ...
  }: {
    packages = {
      hetzner-csi-driver-yaml = pkgs.runCommand "hetzner-csi-driver.yaml" {} ''
        mkdir -p $out
        cp ${inputs.hetzner-csi-driver} $out/hetzner-csi-driver.yaml
      '';
      kured-yaml = pkgs.runCommand "kured.yaml" {} ''
        cp ${inputs.kured}/kured-ds.yaml .
        cp ${inputs.kured}/kured-rbac.yaml .
        cat<<PATCH>kured-ds-cmd.yaml
        apiVersion: apps/v1
        kind: DaemonSet
        metadata:
          name: kured
          namespace: kube-system
        spec:
          template:
            spec:
              containers:
                - name: kured
                  env:
                    - name: PATH
                      value: /usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/run/wrappers/bin:/nix/var/nix/profiles/default/bin:/run/current-system/sw/bin
                  command:
                    - /usr/bin/kured
                    - --reboot-command=/run/current-system/sw/bin/systemctl reboot
                    - --period=10m
        PATCH
        cat<<KUSTOMIZATION>kustomization.yaml
        apiVersion: kustomize.config.k8s.io/v1beta1
        kind: Kustomization
        resources:
        - kured-ds.yaml
        - kured-rbac.yaml
        patchesStrategicMerge:
        - kured-ds-cmd.yaml
        KUSTOMIZATION
        mkdir -p $out
        ${pkgs.kustomize}/bin/kustomize build . > $out/kured.yaml
      '';
      fluxcd-yaml = pkgs.runCommand "flux.yaml" {} ''
        cp ${inputs.fluxcd-install} ./flux.yaml
        cat <<PATCH>patch.yaml
        ---
        apiVersion: apps/v1
        kind: Deployment
        metadata:
          name: helm-controller
          namespace: flux-system
        spec:
          template:
            spec:
              containers:
                - name: manager
                  resources:
                    limits:
                      memory: 256Mi
                    requests:
                      cpu: 20m
                      memory: 64Mi
        ---
        apiVersion: apps/v1
        kind: Deployment
        metadata:
          name: kustomize-controller
          namespace: flux-system
        spec:
          template:
            spec:
              containers:
                - name: manager
                  resources:
                    limits:
                      memory: 256Mi
                    requests:
                      cpu: 20m
                      memory: 64Mi
        ---
        apiVersion: apps/v1
        kind: Deployment
        metadata:
          name: notification-controller
          namespace: flux-system
        spec:
          template:
            spec:
              containers:
                - name: manager
                  resources:
                    limits:
                      memory: 256Mi
                    requests:
                      cpu: 20m
                      memory: 64Mi
        ---
        apiVersion: apps/v1
        kind: Deployment
        metadata:
          name: source-controller
          namespace: flux-system
        spec:
          template:
            spec:
              containers:
                - name: manager
                  resources:
                    limits:
                      memory: 256Mi
                    requests:
                      cpu: 20m
                      memory: 64Mi
        ---
        apiVersion: apps/v1
        kind: Deployment
        metadata:
          name: image-automation-controller
          namespace: flux-system
        spec:
          template:
            spec:
              containers:
                - name: manager
                  resources:
                    limits:
                      memory: 256Mi
                    requests:
                      cpu: 20m
                      memory: 64Mi
        ---
        apiVersion: apps/v1
        kind: Deployment
        metadata:
          name: image-reflector-controller
          namespace: flux-system
        spec:
          template:
            spec:
              containers:
                - name: manager
                  resources:
                    limits:
                      memory: 256Mi
                    requests:
                      cpu: 20m
                      memory: 64Mi
        PATCH

        cat <<KUSTOMIZATION>kustomization.yaml
        apiVersion: kustomize.config.k8s.io/v1beta1
        kind: Kustomization
        resources:
        - flux.yaml
        patchesStrategicMerge:
        - patch.yaml
        KUSTOMIZATION

        mkdir -p $out
        ${pkgs.kustomize}/bin/kustomize build . > $out/flux.yaml
      '';
    };
  };
}
