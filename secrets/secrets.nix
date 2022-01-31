let
  inherit (builtins) any all replaceStrings filter foldl' elem listToAttrs
    attrValues concatMap isList mapAttrs fromTOML readFile hasAttr getFlake;

  flatten = x: if isList x
               then concatMap flatten x
               else [x];

  unique = foldl' (acc: e: if elem e acc then acc else acc ++ [ e ]) [];

  hasAttrsFilter = attrsList: filter (attr: all (key: hasAttr key attr) attrsList);

  hostConfigsList = map (host: host.config) (attrValues (getFlake (toString ../.)).hostConfigurations);

  hostsWithSecrets = hasAttrsFilter [ "publicKey" "age" ] hostConfigsList;

  toLocalSecretPath = replaceStrings [ "secrets/" ] [ "" ];

  secretsList = unique (flatten (map (host: map (s: toLocalSecretPath s.file) (attrValues host.age.secrets)) hostsWithSecrets));

  mapSecretToPublicKeys = secret:
    map (host: host.publicKey)
      (filter (host: any (s: secret == toLocalSecretPath s.file) (attrValues host.age.secrets)) hostsWithSecrets);

  johnae = [
    "age1yubikey1qt7cjux5unxcsrw9dnkq8qsh0jgnwwvxzxm2jn2pxetjchtclmlk6xvpckq"
    "age1yubikey1qvkk2zuwvypyfkwanf08wzq369a07ukstj5czuwavdn2peczyec764ywpxw"
  ];

in

listToAttrs (map (name: {
  inherit name;
  value.publicKeys = johnae ++ (mapSecretToPublicKeys name);
}) secretsList)
