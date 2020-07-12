{ hostName, modulesPath, systemClosure, lib, pkgs, inputs, ... }:
let
  isoConf =
    let
      conf = "${inputs.secrets}/${hostName}/isoconf.nix";
    in
    if builtins.pathExists conf then
      (builtins.exec [
        "${pkgs.sops}/bin/sops"
        "-d"
        "${conf}"
      ])
    else
      { };
in
{
  imports = [
    "${modulesPath}/installer/cd-dvd/installation-cd-minimal-new-kernel.nix"
  ];

  environment.etc = {
    "install.sh" = {
      source = ./install.sh;
      mode = "0700";
    };
    "system-closure-path" = {
      text = toString systemClosure;
    };
  };

  environment.etc."profile.local".text = ''
    echo "nameserver 1.1.1.1" | sudo tee -a /etc/resolv.conf
    sleep 15
    if ! curl -sf --connect-timeout 5 --max-time 5 http://www.google.com > /dev/null; then
        cat<<EOF
    No network - please set it up, then exit the shell to continue.
    For example, on a laptop, you might want to run something like:

    wpa_supplicant -B -i INTERFACE -c <(wpa_passphrase 'NETWORK' 'PASSWORD')

    EOF
        sudo bash
    fi

    ${lib.concatMapStringsSep "\n"
      (s: "export ${s}")
      (lib.mapAttrsToList (name: value: "${name}=\"${value}\"") isoConf)}

    sudo --preserve-env=DISK_PASSWORD,ADDITIONAL_VOLUMES,ADDITIONAL_DISK \
          /etc/install.sh | tee /tmp/install.log

    echo Shutting down in 15 seconds
    sleep 15
    sudo shutdown -h now
  '';

  isoImage.storeContents = [ systemClosure ];
}
