{ pkgs, config, lib, options, ... }:

{
  services.emacs = {
    enable = true;
    package = pkgs.my-emacs;
  };
}
