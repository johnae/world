{ stdenv, fetchFromGitHub, lib, blur, bash, jq, grim, sway, swaylock, mkStrictShellScript }:
mkStrictShellScript {
  name = "swaylock-dope";
  src = ./swaylock-dope;
  substitutions = { inherit stdenv blur bash jq grim sway swaylock; };
}
