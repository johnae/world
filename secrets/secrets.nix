let
  inherit (builtins) any replaceStrings filter foldl' elem listToAttrs
    attrValues concatMap isList mapAttrs fromTOML readFile hasAttr;

  flatten = x: if isList x
               then concatMap (y: flatten y) x
               else [x];

  unique = foldl' (acc: e: if elem e acc then acc else acc ++ [ e ]) [];

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

  secretFiles = unique (flatten (attrValues (mapAttrs (_: h:
    h.secrets
  ) hosts)));

  johnae = [
    "age1yubikey1qt7cjux5unxcsrw9dnkq8qsh0jgnwwvxzxm2jn2pxetjchtclmlk6xvpckq"
    "age1yubikey1qvkk2zuwvypyfkwanf08wzq369a07ukstj5czuwavdn2peczyec764ywpxw"
  ];

in

listToAttrs (map (name: {
  inherit name;
  value.publicKeys = johnae ++ (hostKeys name);
}) secretFiles)
