let
  inherit (builtins) any baseNameOf filter
    attrValues mapAttrs fromTOML readFile hasAttr;

  hosts = mapAttrs (_: value: {
    inherit (value) public_key;
    secrets = if hasAttr "age" value then
      attrValues (mapAttrs (_: s:
        baseNameOf s.file
      ) value.age.secrets)
    else [];
  })
  (fromTOML (readFile ../hosts.toml));

  hostKeys = secretFile: filter (v: v!=null)
    (attrValues (mapAttrs (_: h:
      if any (v: v == secretFile) h.secrets then
        h.public_key
      else null
    ) hosts));

  johnae = [ "age1yubikey1qt7cjux5unxcsrw9dnkq8qsh0jgnwwvxzxm2jn2pxetjchtclmlk6xvpckq" ];

in
{
  "spotifyd.age".publicKeys = johnae ++ (hostKeys "spotifyd.age");
  "spotnix.age".publicKeys = johnae ++ (hostKeys "spotnix.age");
}
