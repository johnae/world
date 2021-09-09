let
  inherit (builtins) any replaceStrings filter foldl' elem listToAttrs
    attrValues concatMap isList mapAttrs fromTOML readFile hasAttr;

  flatten = x: if isList x
               then concatMap flatten x
               else [x];

  unique = foldl' (acc: e: if elem e acc then acc else acc ++ [ e ]) [];

  hostsWithSecrets =
    filter (host: hasAttr "publicKey" host && hasAttr "age" host)
      (attrValues (builtins.getFlake (toString ../.)).hostConfigs);

  secretsToPublicKey = map (host:
    {
      inherit (host) publicKey;
      secrets = map (s: replaceStrings ["secrets/"] [""] s.file)
            (attrValues host.age.secrets);
    }
  ) hostsWithSecrets;

  secretToKeyMapping = secretName: unique (map (keyMap: keyMap.publicKey)
    (filter (keyMap: any (secret: secret == secretName) keyMap.secrets) secretsToPublicKey));

  secretFiles = unique (flatten (map (h: h.secrets) secretsToPublicKey));

  johnae = [
    "age1yubikey1qt7cjux5unxcsrw9dnkq8qsh0jgnwwvxzxm2jn2pxetjchtclmlk6xvpckq"
    "age1yubikey1qvkk2zuwvypyfkwanf08wzq369a07ukstj5czuwavdn2peczyec764ywpxw"
  ];

in

listToAttrs (map (name: {
  inherit name;
  value.publicKeys = johnae ++ (secretToKeyMapping name);
}) secretFiles)
