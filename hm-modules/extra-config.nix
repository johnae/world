{ config, lib, pkgs, options, ... }:
{
  options.home.extraConfig = {
    hostname = lib.mkOption
      {
        type = lib.types.str;
        example = "somename";
      };
  };
}
