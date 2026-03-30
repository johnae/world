{username, ...}: {
  imports = [
    ../../../users/profiles/headless.nix
    ../../../users/profiles/bizniz.nix
  ];
  age.rekey.hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKv+X8xssC74ykGACmXPWWsFZcz/Wrp/R1mPSE6ivQDa";
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
