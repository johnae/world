{username, ...}: {
  imports = [../../../users/profiles/headless.nix];
  home.homeDirectory = "/home/${username}";
  userinfo = {
    email = "jeriksson@evroc.com";
    altEmail = "john@insane.se";
    fullName = "John Axel Eriksson";
    githubUser = "johnae";
    gitlabUser = "jeriksson";
    devRemote = "icarus";
  };
}
