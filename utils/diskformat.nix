{ hostName, config, lib, writeStrictShellScriptBin, ... }:

let
  inherit (lib) mapAttrsToList listToAttrs splitString concatStringsSep last flatten;
  inherit (builtins) filter match head foldl' replaceStrings;
  diskLabels = {
    boot = "boot";
    encCryptkey = "cryptkey";
    swap = "swap";
    encSwap = "encrypted_swap";
    root = "root";
    encRoot = "encrypted_root";
    extra = "extra";
    encExtra = "encrypted_extra";
  };
  efiSpace = "500M";
  luksKeySpace = "20M";
  ramGb = "$(free --giga | tail -n+2 | head -1 | awk '{print $2}')";
  uuidCryptKey = config.config.boot.initrd.luks.devices.cryptkey.keyFile != null;
  subvolumes = lib.unique (filter (v: v != null)
        (flatten
            (map (match "^subvol=(.*)")
              (foldl' (a: b: a ++ b.options) []
                (filter (v: v.fsType == "btrfs") (mapAttrsToList (_: v: v) config.config.fileSystems))
              )
            )
        )
  );
in
  writeStrictShellScriptBin "diskformat" ''
    DIR=$(CDPATH=''' cd -- "$(dirname -- "$0")" && pwd -P)

    if [ -d /sys/firmware/efi/efivars ]; then
      BOOTMODE="''${BOOTMODE:-UEFI}"
    else
      BOOTMODE="''${BOOTMODE:-Legacy}"
    fi
    DEVRANDOM=/dev/urandom

    if [ "$(systemd-detect-virt)" = "none" ]; then
      CRYPTKEYFILE="''${CRYPTKEYFILE:-/sys/class/dmi/id/product_uuid}"
    else
      CRYPTKEYFILE="''${CRYPTKEYFILE:-/sys/class/dmi/id/product_version}"
    fi

    USER_DISK_PASSWORD=${if uuidCryptKey then "no" else "yes"}

    DISK_PASSWORD=""
    if [ "$USER_DISK_PASSWORD" = "yes" ]; then
      while true; do
        echo -n Disk password:
        read -r -s DISK_PASSWORD
        echo
        echo -n Enter disk password again:
        read -r -s DISK_PASSWORD2
        if [ "$DISK_PASSWORD" = "$DISK_PASSWORD2" ]; then
          if [ -z "$DISK_PASSWORD" ]; then
            unset DISK_PASSWORD
            unset DISK_PASSWORD2
            echo "Passwords are empty, please enter them again"
          else
            unset DISK_PASSWORD2
            break
          fi
        else
          unset DISK_PASSWORD
          unset DISK_PASSWORD2
          echo "Passwords don't match, please enter them again"
        fi
      done
    fi

    if [ ! -d "/secrets" ]; then
      mkdir -p /secrets
      mount -t tmpfs -o size=64m tmpfs /secrets
    fi

    if [ -n "$DISK_PASSWORD" ]; then
      CRYPTKEYFILE=/secrets/disk_password
      echo -n "$DISK_PASSWORD" > "$CRYPTKEYFILE"
    fi

    if [ "$(stat -c %s "$CRYPTKEYFILE")" -lt 2 ]; then
        echo "$CRYPTKEYFILE too small, less than 2 bytes"
        exit 1
    fi

    DISK=/dev/nvme0n1
    PARTITION_PREFIX="p"

    if [ ! -b "$DISK" ]; then
      echo "$DISK" is not a block device
      PARTITION_PREFIX=""
      DISK=/dev/sda
    fi

    if [ ! -b "$DISK" ]; then
      echo "$DISK" is not a block device
      echo Giving up
      exit 1
    fi

    echo Formatting disk "$DISK"

    set -x

    wipefs -fa "$DISK"
    sgdisk -z "$DISK"
    partprobe "$DISK"

    efi_space="${efiSpace}"
    luks_key_space="${luksKeySpace}"
    ramgb="${ramGb}"
    swap_space="$((ramgb / 2))"
    if [ "$swap_space" = "0" ]; then
      swap_space="1"
    else
      swap_space="$((swap_space + ramgb))"
    fi
    swap_space="$swap_space"G
    echo Will use a "$swap_space" swap space partition

    sgdisk -og "$DISK"
    partprobe "$DISK"

    partnum=0

    if [ "$BOOTMODE" = "Legacy" ]; then
      partnum=$((partnum + 1))
      sgdisk -n 0:0:+20M -t 0:ef02 -c 0:"biosboot" -u 0:"21686148-6449-6E6F-744E-656564454649" "$DISK" # 1
    fi
    sgdisk -n 0:0:+$efi_space -t 0:ef00 -c 0:"efi" "$DISK" # 1
    sgdisk -n 0:0:+$luks_key_space -t 0:8300 -c 0:"cryptkey" "$DISK" # 2
    sgdisk -n 0:0:+$swap_space -t 0:8300 -c 0:"swap" "$DISK" # 3
    sgdisk -n 0:0:0 -t 0:8300 -c 0:"root" "$DISK" # 4
    partprobe "$DISK"

    echo "PREFIX: $PARTITION_PREFIX"

    DISK_EFI_LABEL=${diskLabels.boot}
    partnum=$((partnum + 1))
    DISK_EFI="$DISK$PARTITION_PREFIX$partnum"
    ENC_DISK_CRYPTKEY_LABEL=${diskLabels.encCryptkey}
    partnum=$((partnum + 1))
    DISK_CRYPTKEY="$DISK$PARTITION_PREFIX$partnum"
    DISK_SWAP_LABEL=${diskLabels.swap}
    ENC_DISK_SWAP_LABEL=${diskLabels.encSwap}
    partnum=$((partnum + 1))
    DISK_SWAP="$DISK$PARTITION_PREFIX$partnum"
    DISK_ROOT_LABEL=${diskLabels.root}
    ENC_DISK_ROOT_LABEL=${diskLabels.encRoot}
    partnum=$((partnum + 1))
    DISK_ROOT="$DISK$PARTITION_PREFIX$partnum"

    sgdisk -p "$DISK"

    partprobe "$DISK"
    fdisk -l "$DISK"

    echo Formatting cryptkey disk "$DISK_CRYPTKEY", using keyfile "$CRYPTKEYFILE"
    cryptsetup luksFormat --label="$ENC_DISK_CRYPTKEY_LABEL" -q --key-file="$CRYPTKEYFILE" "$DISK_CRYPTKEY"
    DISK_CRYPTKEY=/dev/disk/by-label/"$ENC_DISK_CRYPTKEY_LABEL"

    echo Opening cryptkey disk "$DISK_CRYPTKEY", using keyfile "$CRYPTKEYFILE"
    cryptsetup luksOpen --key-file="$CRYPTKEYFILE" "$DISK_CRYPTKEY" "$ENC_DISK_CRYPTKEY_LABEL"

    echo Writing random data to /dev/mapper/"$ENC_DISK_CRYPTKEY_LABEL"
    dd if=$DEVRANDOM of=/dev/mapper/"$ENC_DISK_CRYPTKEY_LABEL" bs=1024 count=14000 || true

    echo Creating encrypted swap
    cryptsetup luksFormat --label="$ENC_DISK_SWAP_LABEL" -q --key-file=/dev/mapper/"$ENC_DISK_CRYPTKEY_LABEL" "$DISK_SWAP"

    echo Creating encrypted root
    cryptsetup luksFormat --label="$ENC_DISK_ROOT_LABEL" -q --key-file=/dev/mapper/"$ENC_DISK_CRYPTKEY_LABEL" "$DISK_ROOT"

    echo Opening encrypted swap using keyfile
    cryptsetup luksOpen --key-file=/dev/mapper/"$ENC_DISK_CRYPTKEY_LABEL" "$DISK_SWAP" "$ENC_DISK_SWAP_LABEL"
    mkswap -L "$DISK_SWAP_LABEL" /dev/mapper/"$ENC_DISK_SWAP_LABEL"

    echo Opening encrypted root using keyfile
    cryptsetup luksOpen --key-file=/dev/mapper/"$ENC_DISK_CRYPTKEY_LABEL" "$DISK_ROOT" "$ENC_DISK_ROOT_LABEL"

    echo Creating btrfs filesystem on /dev/mapper/"$ENC_DISK_ROOT_LABEL"
    mkfs.btrfs -f -L "$DISK_ROOT_LABEL" /dev/mapper/"$ENC_DISK_ROOT_LABEL"

    echo Creating vfat disk at "$DISK_EFI"
    mkfs.vfat -n "$DISK_EFI_LABEL" "$DISK_EFI"

    partprobe /dev/mapper/"$ENC_DISK_SWAP_LABEL"
    partprobe /dev/mapper/"$ENC_DISK_CRYPTKEY_LABEL"
    partprobe /dev/mapper/"$ENC_DISK_ROOT_LABEL"

    mount -t tmpfs none /mnt
    mkdir -p "/mnt/tmproot" ${concatStringsSep " " (map (v: "/mnt/${replaceStrings ["@"] [""] v}") subvolumes)} "/mnt/boot"

    echo Temporarily mounting root btrfs volume from "/dev/disk/by-label/$DISK_ROOT_LABEL" to /mnt/tmproot
    mount -o rw,noatime,compress=zstd,ssd,space_cache /dev/disk/by-label/"$DISK_ROOT_LABEL" /mnt/tmproot

    # now create the btrfs subvolumes we're interested in having
    echo Creating btrfs subvolumes at /mnt/tmproot
    cd /mnt/tmproot
    ${concatStringsSep "\n" (map (v: "btrfs sub create ${v}") subvolumes)}

    cd "$DIR"

    echo Unmounting /mnt/tmproot
    umount /mnt/tmproot
    rmdir /mnt/tmproot

    echo Devices with uuids
    ls -lah /dev/disk/by-uuid/

    echo Devices with labels
    ls -lah /dev/disk/by-label/

    ${concatStringsSep "\n" (map (v: ''mount -o rw,noatime,compress=zstd,ssd,space_cache,subvol=${v} /dev/disk/by-label/"$DISK_ROOT_LABEL" /mnt/${replaceStrings ["@"] [""] v}'') subvolumes)}

    # and mount the boot partition
    echo Mounting boot partition
    mount /dev/disk/by-label/"$DISK_EFI_LABEL" /mnt/boot
 ''
