{ stdenv
, sops
, git
, btrfs-progs
, update-wireguard-keys
, update-wifi-networks
, writeStrictShellScriptBin
, ...
}:

writeStrictShellScriptBin "initialize-user" ''
  PATH=${sops}/bin:${git}/bin:${update-wireguard-keys}/bin:${update-wifi-networks}/bin''${PATH:+:}$PATH
  export PATH
  cd ~

  chmod 0700 .gnupg

  if [ ! -e Development/world ]; then
    git clone --recursive git@github.com:johnae/world Development/world
  else
    echo world already exists at Development/world
  fi

  if [ ! -e Development/secret-world ]; then
    git clone --recursive git@github.com:johnae/secret-world Development/secret-world
  else
    echo world already exists at Development/secret-world
  fi

  if [ ! -e "$PASSWORD_STORE_DIR/.git" ]; then
    echo Cloning password store to "$PASSWORD_STORE_DIR"
    if [ -e "$PASSWORD_STORE_DIR" ]; then
      cd "$PASSWORD_STORE_DIR"
      git clone git@github.com:johnae/passwords .
    else
      git clone git@github.com:johnae/passwords "$PASSWORD_STORE_DIR"
    fi
  else
    echo Password store "$PASSWORD_STORE_DIR" already present
  fi

  mu init --maildir ~/.mail

  sudo mkdir -p /root/.ssh
  sudo chmod 0700 /root/.ssh
  sops -d Development/nixos-metadata/backup_id_ed25519 | \
          sudo tee /root/.ssh/backup_id_ed25519 >/dev/null
  sudo chmod 0600 /root/.ssh/backup_id_ed25519

  sops -d Development/nixos-metadata/builder_id_ed25519 | \
          sudo tee /root/.ssh/id_ed25519 >/dev/null
  sudo chmod 0600 /root/.ssh/id_ed25519

  update-wifi-networks
  update-wireguard-keys
''
