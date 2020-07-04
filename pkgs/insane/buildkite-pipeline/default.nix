{ lib }:
with lib;
with builtins;

let

  isWait = step: step == "wait";
  isCommand = step: !(isWait step) && hasAttr "command" step;
  isTrigger = step: !(isWait step) && hasAttr "trigger" step;
  isBlock = step: !(isWait step) && hasAttr "block" step;

  step = label: {
    command,
    env ? [],
    agents ? [],
    artifact_paths ? null,
    timeout ? 600,
    only ? true,
    soft_fail ? false,
    num_retries ? 0,
    concurrency ? null,
    concurrency_group ? null,
    ...
  }: { inherit label timeout command env agents only soft_fail
               num_retries concurrency concurrency_group; };

  cmd = step;

  trigger = label: {
      trigger,
      build ? {},
      agents ? [],
      async ? false,
      only ? true,
      branches ? "master",
      dynamic ? false,
      ...
  }: if dynamic then
    (step "Modify pipeline, add: '${label}'" {
      inherit only agents;
      command = ''
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
    })
  else
    {
      inherit trigger build async only branches label;
    };

  block-text = {
    text,
    key,
    hint ? null,
    required ? true,
    default ? null,
    ...
  }: { inherit text key hint required default; };

  block-select = {
    select,
    key,
    options,
    ...
  }: { inherit select key options; };

  block = block: {
    fields ? [],
    prompt ? null,
    branches ? null,
    only ? true
  }: {
      inherit block prompt branches only;
      fields = remove null (map (field:
                   if hasAttr "text" field then
                      (block-text field)
                   else if hasAttr "select" field then
                      (block-select field)
                   else
                      null
               ) fields);
    };

  remove-adjacent-dups = l:
    let dup-filter = h: t:
      if length t == 0 then [ h ]
      else if h == head t then dup-filter (head t) (tail t)
      else [ h ] ++ dup-filter (head t) (tail t);
    in dup-filter (head l) (tail l);

  rtrim = f: l: if (f (last l)) then init l else l;

  pipeline = steps:
    let
      trim-last-wait = rtrim (x: (isWait x));
      augment-label = label: queue:
        if queue == "linux" then ":nix: :linux: ${label}"
        else if queue == "macos" then ":nix: :mac: ${label}"
        else label;
      when = cond: value: if cond then value else {};
      ## turns one step into many based on given list of agents
      explode-build-step = step:
          map (agents:
                    { inherit agents;
                      inherit (step) command env timeout soft_fail;
                      label = augment-label step.label agents.queue;
                    }
                    // (when (step.num_retries > 0) (
                       { retry = { automatic = { exit_status = "*"; limit = step.num_retries; }; }; }
                       ))
                    // (when (hasAttr "concurrency" step && hasAttr "concurrency_group" step) (
                             { inherit (step) concurrency concurrency_group; }
                       ))
              )
              (if isAttrs step.agents then [ step.agents ]
               else (unique step.agents));
    in
      trim-last-wait
        (remove-adjacent-dups
          (flatten
            (map (step: if (isCommand step) then
                          explode-build-step
                            (filterAttrsRecursive (n: v: v!= null) step)
                        else if (isBlock step) then
                          filterAttrsRecursive (n: v: v!= null)
                            { inherit (step) block fields prompt branches; }
                        else if (isWait step) then step
                        else removeAttrs step [ "only" ] )
                 ## this always let's through wait steps and steps where "only" is true
                 (filter (step: (isWait step) || step.only) (flatten steps)))));

  wait = "wait";

  deploy = import ./deploy.nix { inherit step block trigger wait; };

in

  {
    inherit step cmd block trigger wait pipeline;
    inherit (deploy) deploy-to-kubernetes;
  }