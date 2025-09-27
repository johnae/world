{inputs, ...}: {
  perSystem = {pkgs, ...}: {
    packages = {
      kubevirt-operator = pkgs.runCommand "kubevirt-operator.yaml" {} ''
        mkdir -p $out
        cp ${inputs.kubevirt-operator} $out/kubevirt-operator.yaml
      '';
      kubevirt-cr = pkgs.runCommand "kubevirt-cr.yaml" {} ''
        mkdir -p $out
        cp ${inputs.kubevirt-cr} $out/kubevirt-cr.yaml
      '';
      kubevirt-cdi-cr = pkgs.runCommand "kubevirt-cdi-cr.yaml" {} ''
        mkdir -p $out
        cp ${inputs.kubevirt-cdi-cr} $out/kubevirt-cdi-cr.yaml
      '';
      kubevirt-cdi-operator = pkgs.runCommand "kubevirt-cdi-operator.yaml" {} ''
        mkdir -p $out
        cp ${inputs.kubevirt-cdi-operator} $out/kubevirt-cdi-operator.yaml
      '';
      hetzner-csi-driver-yaml = pkgs.runCommand "hetzner-csi-driver.yaml" {} ''
        mkdir -p $out
        cp ${inputs.hetzner-csi-driver} $out/hetzner-csi-driver.yaml
      '';
      juicefs-csi-driver-yaml = pkgs.runCommand "juicefs-csi-driver.yaml" {} ''
        mkdir -p $out
        cp ${inputs.juicefs-csi-driver} juicefs-csi-driver.yaml
        cat<<PATCH>juicefs-csi-driver-patch.yaml
        apiVersion: apps/v1
        kind: DaemonSet
        metadata:
          name: juicefs-csi-node
          namespace: kube-system
        spec:
          template:
            spec:
              containers:
              - name: juicefs-plugin
                args:
                - --endpoint=\$(CSI_ENDPOINT)
                - --logtostderr
                - --nodeid=\$(NODE_NAME)
                - --v=5
                - --enable-manager=true
                - --format-in-pod=true
        PATCH
        cat<<KUSTOMIZATION>kustomization.yaml
        apiVersion: kustomize.config.k8s.io/v1beta1
        kind: Kustomization
        resources:
        - juicefs-csi-driver.yaml
        patches:
        - path: juicefs-csi-driver-patch.yaml
        KUSTOMIZATION
        ${pkgs.kustomize}/bin/kustomize build . > $out/juicefs-csi-driver.yaml
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
              tolerations:
              - effect: NoExecute
                key: CriticalAddonsOnly
                operator: Exists
        PATCH
        cat<<KUSTOMIZATION>kustomization.yaml
        apiVersion: kustomize.config.k8s.io/v1beta1
        kind: Kustomization
        resources:
        - kured-ds.yaml
        - kured-rbac.yaml
        patches:
        - path: kured-ds-cmd.yaml
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
        patches:
        - path: patch.yaml
        KUSTOMIZATION

        mkdir -p $out
        ${pkgs.kustomize}/bin/kustomize build . > $out/flux.yaml
      '';
    };
  };
}
