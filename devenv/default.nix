{
  pkgs,
  ansiEscape,
  ...
}: let
  startTestVM = pkgs.writeShellApplication {
    name = "start-test-vm";
    runtimeInputs = [pkgs.qemu];
    text = ''
      set -euxo pipefail

      if ! [ -L cached-iso.iso ]; then
        ISO="$(nix run github:nix-community/nixos-generators -- -f iso -o result --flake .#installer)"
        ln -s "$ISO" cached-iso.iso
      fi

      mkdir -p ovmf
      cp -r ${pkgs.OVMF.fd}/FV/* ovmf/
      chmod -R u+w ovmf

      qemu-img create -f qcow2 nvm0.img 20G
      qemu-img create -f qcow2 nvm1.img 20G

      qemu-system-x86_64 \
        -m 8G \
        -enable-kvm \
        -drive file=./cached-iso.iso,media=cdrom,readonly=on \
        -drive if=pflash,format=raw,unit=0,readonly=on,file=./ovmf/OVMF_CODE.fd \
        -drive if=pflash,format=raw,unit=1,file=./ovmf/OVMF_VARS.fd \
        -drive file=nvm0.img,if=none,id=nvm0,format=qcow2 \
        -drive file=nvm1.img,if=none,id=nvm1,format=qcow2 \
        -device nvme,serial=3576de6e,drive=nvm0 \
        -device nvme,serial=642b56ae,drive=nvm1 \
        -device e1000,netdev=net0 \
        -netdev user,id=net0,hostfwd=tcp:127.0.0.1:5555-:22,hostfwd=tcp:127.0.0.1:6666-:2222
    '';
  };

  runTestVM = pkgs.writeShellApplication {
    name = "run-test-vm";
    runtimeInputs = [pkgs.qemu];
    text = ''
      set -euxo pipefail

      mkdir -p ovmf
      cp -r ${pkgs.OVMF.fd}/FV/* ovmf/
      chmod -R u+w ovmf

      qemu-system-x86_64 \
        -m 8G \
        -enable-kvm \
        -drive if=pflash,format=raw,unit=0,readonly=on,file=./ovmf/OVMF_CODE.fd \
        -drive if=pflash,format=raw,unit=1,file=./ovmf/OVMF_VARS.fd \
        -drive file=nvm0.img,if=none,id=nvm0,format=qcow2 \
        -drive file=nvm1.img,if=none,id=nvm1,format=qcow2 \
        -device nvme,serial=3576de6e,drive=nvm0 \
        -device nvme,serial=642b56ae,drive=nvm1 \
        -device e1000,netdev=net0 \
        -netdev user,id=net0,hostfwd=tcp:127.0.0.1:5555-:22,hostfwd=tcp:127.0.0.1:6666-:2222
    '';
  };

  installTestVM = pkgs.writeShellApplication {
    name = "install-test-vm";
    text = ''
      set -euxo pipefail
      while true; do
        echo Probing...
        if timeout 5 bash -c "</dev/tcp/127.0.0.1/5555"; then
          echo Host is up - continuing
          scp -P 5555 -oStrictHostKeyChecking=no -oUserKnownHostsFile=/dev/null -i ~/.ssh/id_ed25519_alt hosts/x86_64-linux/test.nix root@127.0.0.1:
          cat<<'SSH' | ssh -p 5555 root@127.0.0.1 -oStrictHostKeyChecking=no -oUserKnownHostsFile=/dev/null -i ~/.ssh/id_ed25519_alt bash
            echo install the vm here
            if ! [ -e world ]; then
              git clone https://github.com/johnae/world
            else
              (
                cd world
                git reset HEAD --hard
                git pull
              )
            fi
            mv test.nix world/hosts/x86_64-linux/
            cd world
            git add hosts/x86_64-linux/test.nix
            nix build --accept-flake-config .#test-diskformat
            DISK_PASSWORD="123456" ./result/bin/diskformat
            mkdir -p /keep/secrets
            mkdir -p /mnt/keep/secrets
            ssh-keygen -t ed25519 -C test-vm-key -f /mnt/keep/secrets/initrd_ed25519_key -N ""
            cp /mnt/keep/secrets/initrd_ed25519_key /keep/secrets/
            chmod 0600 /mnt/keep/secrets/initrd_ed25519_key
            chmod 0600 /keep/secrets/initrd_ed25519_key
            lsblk -f
            echo nixos-install --flake .#test --no-root-passwd --impure
      SSH
          break
        fi
        sleep 5
      done
    '';
  };
in {
  name = "world";

  packages = with pkgs; [
    agenix
    age-plugin-yubikey
    alejandra
    hcloud
    just
    nil
    rage
    statix
    tofuWithPlugins
    world
    yj
    startTestVM
    runTestVM
    installTestVM
  ];

  enterShell = ansiEscape ''
     echo -e "
      {bold}{106}Declarative Today. {127}Utopia Tomorrow.{reset}

      This repo contains all my machine definitions and extra packages I like to keep up-to-date with upstream or that
      have been tweaked somehow.
    "
  '';
}
