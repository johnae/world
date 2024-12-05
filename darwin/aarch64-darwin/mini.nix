{
  adminUser,
  ...
}:
{
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL54GmqIwTv5EZ2t944ZQus3x3jXyVPu6//a89Kd/nIE";
  age.secrets = {
    ts-google-9k = {
      file = ../../secrets/ts-google-9k.age;
      owner = "${toString adminUser.uid}"
    };
  };
}
