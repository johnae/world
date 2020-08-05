{ config, lib, pkgs, containers, ... }:
let
  cfg = config.steps;
  util = import ../util { inherit lib config; };
  inherit (util) withBuildEnv;
  inherit (lib) mkOption mkIf mkMerge mapAttrsToList bk;
  inherit (lib.types) nullOr attrsOf submodule str bool;
  inherit (builtins) toJSON getEnv substring;
in
{
  options.steps.deploys = mkOption {
    type = nullOr (attrsOf (
      submodule ({ config, name, ... }: {
        options = {
          key = mkOption {
            type = str;
            default = name;
          };
          image = mkOption {
            type = str;
            default = name;
          };
          application = mkOption {
            type = str;
            default = name;
          };
          runDeploy = mkOption {
            type = bool;
            default = (getEnv "BUILDKITE_BRANCH" == "master");
          };
          build = mkOption {
            type = attrsOf (attrsOf str);
            default = { };
          };
          agents = mkOption {
            type = nullOr (attrsOf str);
            default = null;
          };
          shortsha = mkOption {
            type = str;
            default = substring 0 7 (getEnv "BUILDKITE_COMMIT");
          };
          trigger = mkOption {
            type = str;
            default = "gitops";
          };
          waitForCompletion = mkOption {
            type = bool;
            default = true;
          };
          dependsOn = mkOption {
            type = bk.types.uniqueKeys cfg;
            default = [ ];
          };
          deployDependsOn = mkOption {
            type = bk.types.uniqueKeys cfg;
            default = [ ];
          };
        };
      })
    ));
    default = null;
  };
  config.steps.commands = mkIf (cfg.deploys != null) (
    mkMerge (
      mapAttrsToList
        (name: value:
          let
            ## so other steps have something "stable" to depend on name wise - building
            ## an image and deploying it are different steps but need to be treated as
            ## one from the outside.
            imgBuildKey = if value.runDeploy then "${name}-image" else "${name}";
            containerNixHash = builtins.head (
              builtins.split "-" (
                builtins.baseNameOf containers.${name}.archive.outPath
              )
            );
            build = value.build // {
              env = {
                APPLICATION = value.application;
                APP_SHORTSHA = value.shortsha;
                IMAGE = value.image;
                IMAGE_TAG = containerNixHash;
              };
            };
          in
          ({
            "${imgBuildKey}" = {
              inherit (value) dependsOn agents;
              label = ":nix: :docker: Build docker image for ${name}";
              command = withBuildEnv ''
                echo +++ Nix build and push image
                nix build .#containers.${name}.push
                ./result
              '';
            };
          } // (
            if value.runDeploy then
              {

                "${name}-trigger" = {
                  inherit (value) agents;
                  dependsOn = with value; deployDependsOn ++ dependsOn ++ [ cfg.commands."${imgBuildKey}" ];
                  label = ":nix: :git: Trigger gitops pipeline for ${name}";
                  command = ''
                    cat<<json | buildkite-agent pipeline upload --no-interpolation
                    {
                      "steps": [
                         {
                           "trigger": "${value.trigger}",
                           "label": "${name} - gitops",
                           "build": ${toJSON build}
                         }
                      ]
                    }
                    json
                  '';
                };

                "${name}" = {
                  label = ":nix: :k8s: Deploy ${name}";
                  dependsOn = with value; deployDependsOn ++ dependsOn ++ [
                    cfg.commands."${name}-trigger"
                  ];
                  command = with value; ''
                    nix shell .#nixpkgs.strict-bash \
                              .#nixpkgs.curl \
                    -c strict-bash <<'NIXSH'
                      annotate() {
                        style=''${1:-}
                        msg=''${2:-}
                        msg="$msg, see: https://argocd.insane.se/applications/${application}"
                        buildkite-agent annotate "$msg" \
                          --style "$style" --context 'ctx-deploy-${application}'
                      }
                      on_exit() {
                        err=$?
                        if [ "$err" -gt 0 ]; then
                          annotate error \
                            "Failed to deploy ${application}"
                        fi
                      }
                      trap on_exit EXIT

                      annotate info \
                        "Deploying ${application}"

                      curl -sSL -o ./argocd https://argocd.insane.se/download/argocd-linux-amd64
                      chmod +x argocd

                      max_wait_time_secs=240
                      current_time_secs=1

                      output_prefix="${application}.${getEnv "BUILDKITE_BUILD_NUMBER"}"
                      log="$(mktemp "$output_prefix"-app-list-log.XXXXXXX)"
                      trap 'rm -f /tmp/$output_prefix*' EXIT

                      while ! ./argocd --plaintext app list | tee -a "$log" | \
                              grep -q "${application}"
                      do
                        sleep 1
                        current_time_secs=$((current_time_secs + 1))
                        if [ $current_time_secs -ge $max_wait_time_secs ]; then
                           cat "$log"
                           echo "****************************************************************************************************"
                           echo "Waited for $max_wait_time_secs seconds but the app ${application} never showed up :-("
                           echo "you could try a rebuild of this step if this is the first time this app has been deployed as it may"
                           echo "sometimes take longer than $max_wait_time_secs seconds for ArgoCD to pick it up"
                           echo "****************************************************************************************************"
                           exit 1
                        fi
                      done

                      appdiff="$(mktemp "$output_prefix"-diff.XXXXXXX)"

                      if ./argocd --plaintext app diff --hard-refresh "${application}" > "$appdiff"; then
                        annotate default \
                          "${application} was already up-to-date, no sync necessary"
                        exit 0
                      fi

                      annotate info \
                      "Syncing cluster state of ${application}:

                      \`\`\`
                      $(cat "$appdiff")
                      \`\`\`

                      "

                      echo "--- Syncing cluster state of ${application}"
                      ./argocd --plaintext app sync "${application}" --async || true

                      ${
                      if waitForCompletion
                      then
                      ''
                        echo "--- Awaiting cluster convergence"
                        ./argocd --plaintext app wait "${application}" --timeout 600
                      ''
                      else
                      ''
                        echo "--- Skipping waiting for cluster convergence"
                      ''
                    }
                      annotate success \
                      "${application} deployed:

                      \`\`\`
                      $(cat "$appdiff")
                      \`\`\`

                      "
                    NIXSH
                  '';
                };
              }
            else { }
          ))
        )
        cfg.deploys
    )
  );
}
