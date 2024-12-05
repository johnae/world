let
  inherit
    (builtins)
    all
    any
    attrValues
    concatMap
    elem
    elemAt
    filter
    foldl'
    getFlake
    hasAttr
    isList
    length
    listToAttrs
    split
    ;

  last = list: elemAt list (length list - 1);

  flatten = x:
    if isList x
    then concatMap flatten x
    else [x];

  unique = foldl' (acc: e:
    if elem e acc
    then acc
    else acc ++ [e]) [];

  hasAttrsFilter = attrsList: filter (attr: all (key: hasAttr key attr) attrsList);

  hostConfigsList = (map (host: host.config) (attrValues (getFlake (toString ../.)).nixosConfigurations)) ++ (map (host: host.config) (attrValues (getFlake (toString ../.)).darwinConfigurations));

  hostsWithSecrets = hasAttrsFilter ["publicKey" "age"] hostConfigsList;

  toLocalSecretPath = path: last (split "/secrets/" path);

  secretsList = unique (flatten (map (host: map (s: toLocalSecretPath (toString s.file)) (attrValues host.age.secrets)) hostsWithSecrets));

  mapSecretToPublicKeys = secret:
    unique (
      map (host: host.publicKey)
      (filter (host: any (s: secret == toLocalSecretPath (toString s.file)) (attrValues host.age.secrets)) hostsWithSecrets)
    );

  johnae = [
    "age1yubikey1qt7cjux5unxcsrw9dnkq8qsh0jgnwwvxzxm2jn2pxetjchtclmlk6xvpckq"
    "age1yubikey1qvkk2zuwvypyfkwanf08wzq369a07ukstj5czuwavdn2peczyec764ywpxw"
  ];
in
  listToAttrs (map (name: {
      inherit name;
      value.publicKeys = johnae ++ (mapSecretToPublicKeys name);
    })
    secretsList)
