{ lib, ... }:
{
  options.home.extraConfig = {
    hostName = lib.mkOption
      {
        type = lib.types.str;
        example = "somename";
      };
    userFullName = lib.mkOption
      {
        type = lib.types.str;
        example = "Someone Someonesson";
      };
    userEmail = lib.mkOption
      {
        type = lib.types.str;
        example = "some@email.com";
      };
    githubUser = lib.mkOption
      {
        type = lib.types.str;
        example = "someuser";
      };
    gitlabUser = lib.mkOption
      {
        type = lib.types.str;
        example = "someuser";
      };
  };
}
