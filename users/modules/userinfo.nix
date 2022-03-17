{lib, ...}: {
  options = with lib; {
    userinfo = {
      fullName =
        mkOption
        {
          type = types.str;
          example = "Someone Someonesson";
        };
      email =
        mkOption
        {
          type = types.str;
          example = "some@email.com";
        };
      githubUser =
        mkOption
        {
          type = types.str;
          example = "someuser";
        };
      gitlabUser =
        mkOption
        {
          type = types.str;
          example = "someuser";
        };
      gitIdMap =
        mkOption
        {
          type = types.attrsOf types.str;
          example = ''
            {
              "~/path/to/dev/folder" = "some@email.com";
            }
          '';
          default = {};
        };
    };
  };
}
