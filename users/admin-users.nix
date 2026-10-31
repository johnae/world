let
  darwin = {
    name = "johnaxele";
    uid = 501;
    gid = 20;
    userinfo = {
      email = "john@insane.se";
      altEmail = "johnxele@spotify.com";
      fullName = "John Axel Eriksson";
      githubUser = "johnae";
      gitlabUser = "johnae";
      devRemote = "icarus";
    };
  };
in {
  nixos = {
    name = "john";
    uid = 1337;
    gid = 1337;
    userinfo = {
      email = "john@insane.se";
      altEmail = "john@9000.dev";
      fullName = "John Axel Eriksson";
      githubUser = "johnae";
      gitlabUser = "johnae";
      devRemote = "icarus";
    };
  };
  inherit darwin;
  # Darwin hosts whose first account is named differently; same uid.
  darwinHosts = {
    mini = darwin // {name = "john";};
  };
}
