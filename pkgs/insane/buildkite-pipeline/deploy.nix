{step, block, trigger, wait}:
with builtins;
let
  deploy-to-kubernetes = {
                       application,
                       shortsha,
                       manifests-path,
                       image,
                       image-tag,
                       agents ? { queue = "linux"; },
                       triggered-pipeline ? "gitops",
                       wait-for-completion ? true,
                       approval ? true,
                       only ? true,
                       env ? {}}:
    let

      stepenv = env // {
        APPLICATION = application;
        APP_SHORTSHA = shortsha;
        IMAGE = image;
        IMAGE_TAG = image-tag;
      };

      maybe-block = if approval then
        (block ":rocket: Deploy to kubernetes?" { inherit only; })
      else
        wait; ##functions as a noop (bit hacky though) since any adjacent dupes are filtered out of the pipeline
    in
      [
        maybe-block

        (trigger ":github: DEPLOY: commit ${stepenv.APPLICATION} cluster state" {
          trigger = triggered-pipeline;
          build = {
            env = stepenv;
            meta_data = {
              manifest = ''$(nix-shell -I nixpkgs=$INSANEPKGS -p kubectl --run 'kubectl kustomize ${manifests-path} | base64 -w0')'';
            };
          };
          dynamic = true;
          inherit agents only;
        })

        wait

        (step ":k8s: DEPLOY ${stepenv.APPLICATION}: sync cluster state" {
          inherit agents only;
          env = stepenv;
          command = ''
            nix-shell -I nixpkgs="$INSANEPKGS" \
            -p insane-lib.strict-bash \
            -p curl \
            --run strict-bash <<'NIXSH'
              curl -sSL -o ./argocd https://argocd.insane.se/download/argocd-linux-amd64
              chmod +x argocd

              echo "--- Syncing cluster state of $APPLICATION"
              set +e
              ./argocd --plaintext app sync "$APPLICATION" || true
              set -e

              ${if wait-for-completion then
                ''
                  echo "--- Awaiting cluster convergence"
                  ./argocd --plaintext app wait "$APPLICATION"
                ''
                else
                ''
                  echo "--- Skipping waiting for cluster convergence"
                ''
              }
            NIXSH
          '';
        })

      ];
in
  { inherit deploy-to-kubernetes; }