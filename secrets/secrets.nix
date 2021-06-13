let
  inherit (builtins) mapAttrs fromTOML readFile;
  hosts = mapAttrs (_: value: value.public_key)
    (fromTOML (readFile ../hosts.toml));
  johnae = "age1yubikey1qt7cjux5unxcsrw9dnkq8qsh0jgnwwvxzxm2jn2pxetjchtclmlk6xvpckq";
in
{
  "eris.age".publicKeys = [
    hosts.eris
    johnae
  ];
  "io.age".publicKeys = [
    hosts.io
    johnae
  ];
}
