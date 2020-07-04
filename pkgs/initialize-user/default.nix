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

  if [ ! -e Development/nixos-configuration ]; then
    git clone --recursive git@github.com:johnae/nixos-configuration Development/nixos-configuration
  else
    echo nixos-configuration already exists at Development/nixos-configuration
  fi

  if [ ! -e "$PASSWORD_STORE_DIR" ]; then
    echo Cloning password store to "$PASSWORD_STORE_DIR"
    git clone git@github.com:johnae/passwords "$PASSWORD_STORE_DIR"
  else
    echo Password store "$PASSWORD_STORE_DIR" already present
  fi

  mu init --maildir ~/.mail

  sudo mkdir -p /root/.ssh
  sudo chmod 0700 /root/.ssh
  sops -d Development/nixos-configuration/metadata/backup_id_rsa | \
          sudo tee /root/.ssh/backup_id_rsa >/dev/null
  sudo chmod 0600 /root/.ssh/backup_id_rsa

  update-wifi-networks
  update-wireguard-keys
''
