let
  inherit (builtins) mapAttrs fromTOML readFile;
  hosts = mapAttrs (_: value: value.public_key)
    (fromTOML (readFile ../hosts.toml));
  johnae = "age1yubikey1qt7cjux5unxcsrw9dnkq8qsh0jgnwwvxzxm2jn2pxetjchtclmlk6xvpckq";
in
{
  "spotnix.age".publicKeys = [
    hosts.eris
    hosts.io
    hosts.carbon
    johnae
  ];
}
