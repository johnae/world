let
  inherit (builtins) any replaceStrings filter
    attrValues mapAttrs fromTOML readFile hasAttr;

  hosts = mapAttrs (_: value: {
    inherit (value) publicKey;
    secrets = if hasAttr "age" value then
      attrValues (mapAttrs (_: s:
        replaceStrings ["secrets/"] [""] s.file
      ) value.age.secrets)
    else [];
  })
  (fromTOML (readFile ../hosts.toml));

  hostKeys = secretFile: filter (v: v!=null)
    (attrValues (mapAttrs (_: h:
      if any (v: v == secretFile) h.secrets then
        h.publicKey
      else null
    ) hosts));

  johnae = [
    "age1yubikey1qt7cjux5unxcsrw9dnkq8qsh0jgnwwvxzxm2jn2pxetjchtclmlk6xvpckq"
    "age1yubikey1qvkk2zuwvypyfkwanf08wzq369a07ukstj5czuwavdn2peczyec764ywpxw"
  ];

in
{
  "spotifyd.age".publicKeys = johnae ++ (hostKeys "spotifyd.age");
  "spotnix.age".publicKeys = johnae ++ (hostKeys "spotnix.age");
  "wifi-networks.age".publicKeys = johnae ++ (hostKeys "wifi-networks.age");
  "carbon/wg-home.age".publicKeys = johnae ++ (hostKeys "carbon/wg-home.age");
}
