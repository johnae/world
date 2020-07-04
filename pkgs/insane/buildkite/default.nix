{ lib, insane-lib }:
with builtins;
with lib;
with insane-lib;
let
  DOCKER_REGISTRY = "johnae";
  PROJECT_NAME = toLower (getEnv "BUILDKITE_PIPELINE_SLUG");
  LONGSHA = getEnv "BUILDKITE_COMMIT";
  SHORTSHA = substring 0 7 (LONGSHA);
  BRANCH = getEnv "BUILDKITE_BRANCH";
  COMMIT_MSG = getEnv "BUILDKITE_MESSAGE";
  BUILDKITE_BUILD_NUMBER = getEnv "BUILDKITE_BUILD_NUMBER";
  hostname =
    let
      hn = getEnv "BUILDKITE_AGENT_META_DATA_HOSTNAME";
    in if hn != "" then hn else "bk-hostname-missing";


  commandsListToString = commands:
    if isList commands
    then
      (
        concatStringsSep "\n"
          (filter (v: v != null) (flatten commands))
      )
    else commands;

  usingBuildEnv = nixfile: commands:
    let
      cmds = commandsListToString commands;
    in
      if nixfile != null then
        ''
          nix-shell ${nixfile} --run strict-bash <<'NIXSH'
          ${commandsListToString commands}
          NIXSH
        ''
      else cmds;

  waitDefaults = defaults@{ ... }: args:
    let
      args' = mapAttrs' (name: value: nameValuePair (toSnakeCase name) value) args;
      defaults' = mapAttrs' (name: value: nameValuePair (toSnakeCase name) value) defaults;
    in
      recursiveUpdate defaults' args';

  wait = waitDefaults {
    wait = null;
  };

  runDefaults = defaults@{ ... }: label: args:
    let
      buildRestoreArtifactsCmds = artifacts:
        (
          concatStringsSep "\n" (
            (
              map (
                artifact:
                ''
                  echo --- Restoring artifact "${artifact}"
                  buildkite-agent artifact download '${artifact}.tgz' .
                  tar zxf '${artifact}.tgz'
                ''
              ) artifacts
            )
          )
        );

      buildSaveArtifactsCmds = artifacts:
        (
          concatStringsSep "\n" (
            mapAttrsToList (
              name: value: ''
                echo --- Saving artifact "${name}"
                tar czf '${name}.tgz' ${concatStringsSep " " (map (v: "'${v}'") value)}
                buildkite-agent artifact upload '${name}.tgz'
              ''
            ) artifacts
          )
        );
      args' = mapAttrs' (name: value: nameValuePair (toSnakeCase name) value) args;
      defaults' = mapAttrs' (name: value: nameValuePair (toSnakeCase name) value) defaults;
      saveArtifacts = if hasAttr "save_artifacts" args' then args'.save_artifacts else null;
      restoreArtifacts = if hasAttr "restore_artifacts" args' then args'.restore_artifacts else null;
      exactCommand = if hasAttr "exact_command" args' then args'.exact_command else null;
      beforeCommand = [ (if hasAttr "before_command" args' then args'.before_command else null) ]
      ++ [ (if restoreArtifacts != null then (buildRestoreArtifactsCmds restoreArtifacts) else null) ];
      afterCommand = [ (if hasAttr "after_command" args' then args'.after_command else null) ]
      ++ [ (if saveArtifacts != null then (buildSaveArtifactsCmds saveArtifacts) else null) ];
      numRetries = if hasAttr "num_retries" args' then args'.num_retries else null;

      buildNixPath = if hasAttr "build_nix_path" args' then args'.build_nix_path else ".buildkite/build.nix";

      newargs = removeAttrs (recursiveUpdate defaults' args') [
        "exact_command"
        "before_command"
        "after_command"
        "num_retries"
        "save_artifacts"
        "restore_artifacts"
        "build_nix_path"
      ];
    in
      newargs
      // { inherit label; }
      // (
        if hasAttr "command" args
        then {
          command = usingBuildEnv buildNixPath
            [ beforeCommand args.command afterCommand ];
        } else { }
      )
      // (
        if exactCommand != null
        then {
          command = commandsListToString
            [ beforeCommand exactCommand afterCommand ];
        } else { }
      )
      // (
        if numRetries != null
        then
          if numRetries > 0
          then
            { retry = { automatic = { exit_status = "*"; limit = numRetries; }; }; }
          else { }
        else { }
      );

  run = runDefaults {
    agents = { queue = "linux"; };
    numRetries = 1;
    buildNixPath = ".buildkite/build.nix";
    env = { inherit DOCKER_REGISTRY PROJECT_NAME SHORTSHA; };
    softFail = false;
    timeout = 600;
  };

  blockDefaults = defaults@{ ... }: block: args:
    let
      args' = mapAttrs' (name: value: nameValuePair (toSnakeCase name) value) args;
      defaults' = mapAttrs' (name: value: nameValuePair (toSnakeCase name) value) defaults;
    in
      (recursiveUpdate defaults' args')
      // { inherit block; };

  block = blockDefaults { };

  when = cond: steps: if cond then steps else [ ];

  blockWhen = predicate: steps:
    let
      blockKey = (head (filter (s: hasAttr "block" s) steps)).key;
      isBlock = step: hasAttr "block" step;
      isDependent = step: hasAttr "depends_on" step;
    in
      (
        foldr (
          step: filtered:
            if predicate
            then [ step ] ++ filtered
            else
              if isBlock step
              then filtered
              else
                if isDependent step
                then
                  [ (step // { depends_on = (filter (k: k != blockKey) step.depends_on); }) ] ++ filtered
                else [ step ] ++ filtered
        ) [ ] steps
      );

  dynamicTrigger = label: { trigger
                          , build ? { }
                          , key ? null
                          , dependsOn ? null
                          , ...
                          }:
    (
      run "Modify pipeline, add: '${label}'" (
        {
          exactCommand = ''
            cat<<JSON | buildkite-agent pipeline upload --no-interpolation
            {
              "steps": [
                 {
                   "trigger": "${trigger}",
                   "label": "${label}",
                   "build": ${toJSON build}
                 }
              ]
            }
            JSON
          '';
        }
        // (if key != null then { inherit key; } else { })
        // (if dependsOn != null then { inherit dependsOn; } else { })
      )
    );

  dockerBuild = args@{ npmAuth ? false, additionalBuildArgs ? [ ], ... }:
    let
      runArgs = removeAttrs args [ "npmAuth" "additionalBuildArgs" ];
      check =
        if npmAuth
        then
          ''
            if [ -z "$NPM_TOKEN" ]; then
              echo Missing NPM_TOKEN environment variable
              echo this really should not happen since it should be
              echo defined as part of the buildkite agent environment
              exit 1
            fi
          ''
        else "";
      dockerArgs =
        if npmAuth
        then
          additionalBuildArgs ++ [ "--build-arg" "NPM_TOKEN=\"$NPM_TOKEN\"" ] ## double quote variable or have shellcheck yell
        else additionalBuildArgs;
    in
      (
        run ":docker: Docker build" (
          {
            agents = { inherit hostname; };
            command = ''
              ${check}
              docker build ${concatStringsSep " " dockerArgs} -t "$PROJECT_NAME" .
              docker tag "$PROJECT_NAME" "$DOCKER_REGISTRY/$PROJECT_NAME:$SHORTSHA"
              docker tag "$PROJECT_NAME" "$DOCKER_REGISTRY/$PROJECT_NAME:latest"
            '';
          }
          // runArgs
        )
      );

  dockerPush = args@{ tagLatest ? false, ... }:
    let
      args' = removeAttrs (mapAttrs' (name: value: nameValuePair (toSnakeCase name) value) args) [ "tag_latest" ];
    in
      (
        run ":docker: Docker push" (
          {
            agents = { inherit hostname; };
            command = ''
              docker push "$DOCKER_REGISTRY/$PROJECT_NAME:$SHORTSHA"
              ${
                if tagLatest
                then
                    "docker push \"$DOCKER_REGISTRY/$PROJECT_NAME:latest\""
                else ""}
            '';
          }
          // args'
        )
      );

  deploy =
    args@{ application ? PROJECT_NAME
    , shortsha ? SHORTSHA
    , image ? "${DOCKER_REGISTRY}/${PROJECT_NAME}"
    , imageTag ? SHORTSHA
    , trigger ? "gitops"
    , waitForCompletion ? true
    , dependsOn ? [ ]
    , ...
    }:
    let
      runArgs = removeAttrs args [
        "application"
        "shortsha"
        "image"
        "imageTag"
        "trigger"
        "waitForCompletion"
        "dependsOn"
      ];
      runArgsKey = if hasAttr "key" runArgs then "-${toString runArgs.key}" else "";
    in
      [
        (
          dynamicTrigger ":github: Deploy ${application}: commit cluster state" {
            inherit trigger dependsOn;
            key = "trigger-deploy-${application}${runArgsKey}";
            build = {
              env = {
                APPLICATION = application;
                APP_SHORTSHA = shortsha;
                IMAGE = image;
                IMAGE_TAG = imageTag;
              };
            };
          }
        )
        (
          run ":k8s: Deploying ${application}: waiting for cluster state convergence"
            (
              {
                dependsOn = dependsOn ++ [ "trigger-deploy-${application}${runArgsKey}" ];
                exactCommand = ''
                  nix-shell -I nixpkgs="$INSANEPKGS" \
                  -p insane-lib.strict-bash \
                  -p curl \
                  --run strict-bash <<'NIXSH'
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

                    output_prefix="${application}.${BUILDKITE_BUILD_NUMBER}"
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
              }
              // runArgs
            )
        )
      ];

  pipeline = steplist:
    let
      augment-label = label: queue:
        if queue == "linux"
        then ":nix: :linux: ${label}"
        else
          if queue == "macos"
          then ":nix: :mac: ${label}"
          else label;
      steps =
        let
          s = (
            map (
              step:
              (
                if hasAttr "command" step
                then
                  step // { label = augment-label step.label step.agents.queue; }
                else step
              )
            ) (flatten steplist)
          );
        in
          if length s > 0 then s
          else [
            (
              run "No steps to run" {
                command = ''
                  echo The pipeline was actually evaluated to an empty list
                  echo probably because of how the pipeline.nix determines what steps to run
                  echo based on git branch or other such contexts
                  echo
                  echo This is here as a placeholder
                '';
              }
            )
          ];
    in
      { inherit steps; };
in
{
  inherit run runDefaults deploy dockerBuild dockerPush block usingBuildEnv
    hostname pipeline dynamicTrigger blockWhen wait waitDefaults when
    DOCKER_REGISTRY PROJECT_NAME SHORTSHA LONGSHA BUILDKITE_BUILD_NUMBER;
}
