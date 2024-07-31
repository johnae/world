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
      altEmail =
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
      devRemote =
        mkOption
        {
          type = types.either types.str types.null;
          example = "somehost";
          default = null;
        };
    };
  };
}
