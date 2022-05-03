final: prev: {
  fluxcd-yaml = prev.runCommand "flux.yaml" {} ''
    ${prev.fluxcd}/bin/flux install --export > flux.yaml
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
    ${prev.kustomize}/bin/kustomize build . > $out/flux.yaml
  '';
}
