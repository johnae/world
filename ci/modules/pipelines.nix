## create the pipeline using tektonix
## from https://github.com/johnae/tektonix
{ config, pkgs, lib, name, ... }:

let
  task = command: {
    taskRef.name = "default";
    params.giturl = "$(params.giturl)";
    params.gitrev = "$(params.gitrev)";
    params.command = command;
  };
in
{
  imports = [ ./tasks.nix ];
  resources.pipelines.default.spec = {
    params = {
      giturl = { type = "string"; };
      gitrev = { type = "string"; };
    };
    tasks.build-spook = task "world build spook -L";
    tasks.build-firefox-pipewire = task "world build firefox-pipewire -L";
    tasks.build-sway = task "world build sway -L";
    tasks.build-swaybg = task "world build swaybg -L";
    tasks.build-swayidle = task "world build swayidle -L";
  };

}
