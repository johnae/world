{username, ...}: {
  imports = [
    ../../../users/profiles/headless.nix
    ../../../users/profiles/9k.nix
  ];
  age.rekey.hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBd+krQekv46NdJr+h4sN6vBQ8vgdEeETwti5qZITrGO";
  home.homeDirectory = "/home/${username}";
  userinfo = {
    email = "john@9000.dev";
    altEmail = "john@insane.se";
    fullName = "John Axel Eriksson";
    githubUser = "johnae";
    gitlabUser = "johnae";
    devRemote = "";
  };
}
