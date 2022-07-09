{
  config,
  lib,
  writeStrictShellScriptBin,
  ...
}: let
  inherit (lib) mapAttrsToList listToAttrs splitString concatStringsSep last flatten;
  inherit (builtins) filter match head foldl' replaceStrings;
  inherit (config.config) boot cryptsetup btrfs machinePurpose;
  bootMode =
    if boot.loader.systemd-boot.enable
    then "UEFI"
    else "Legacy";
  luksFormatExtraParams = cryptsetup.luksFormat.extraParams;
  btrfsFormatExtraParams = btrfs.format.extraParams;
  btrfsDisks = btrfs.disks;
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
  subvolumes = lib.unique (
    filter (v: v != null)
    (
      flatten
      (
        map (match "^subvol=(.*)")
        (
          foldl' (a: b: a ++ b.options) []
          (filter (v: v.fsType == "btrfs") (mapAttrsToList (_: v: v) config.config.fileSystems))
        )
      )
    )
  );
in
  writeStrictShellScriptBin "diskformat" ''
    DIR=$(CDPATH=''' cd -- "$(dirname -- "$0")" && pwd -P)

    retry() {
      n=''${1:-1}
      sleepwait=5
      shift
      if [ "$n" -le 0 ]; then
         echo "\"$*\"" failed - giving up
         sleep 10
         exit 1
      fi
      n=$((n - 1))
      # shellcheck disable=SC2294
      if ! eval "$@"; then
        echo "\"$*\" failed, will retry in 5 seconds"
        sleep "$sleepwait"
        echo retrying "\"$*\""
        retry "$n" "$@"
      else
          echo "\"$*\"" succeeded
      fi
    }
    retryDefault() {
        retry 3 "$@"
    }
    waitForPath() {
      systemctl restart systemd-udev-trigger.service || true
      retryDefault test -e "$1"
    }

    BOOTMODE="${bootMode}"
    DEVRANDOM=/dev/urandom

    if [ "$(systemd-detect-virt)" = "none" ]; then
      CRYPTKEYFILE="''${CRYPTKEYFILE:-/sys/class/dmi/id/product_uuid}"
      if [ ! -e "$CRYPTKEYFILE" ]; then
        CRYPTKEYFILE="/sys/firmware/devicetree/base/serial-number"
      fi
    else
      CRYPTKEYFILE="''${CRYPTKEYFILE:-/sys/class/dmi/id/product_version}"
    fi

    USER_DISK_PASSWORD=${
      if uuidCryptKey
      then "no"
      else "yes"
    }

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

    DISK=${builtins.head btrfsDisks}
    PARTITION_PREFIX="p"

    if [ ! -b "$DISK" ]; then
      echo "$DISK" is not a block device
      PARTITION_PREFIX=""
      DISK=/dev/sda
    fi

    if [ ! -b "$DISK" ]; then
      echo "$DISK" is not a block device
      PARTITION_PREFIX=""
      DISK=/dev/vda
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
    fi

    ${
      if machinePurpose == "server"
      then ''
        ## no support for hibernation
        swap_space="$ramgb"G
      ''
      else ''
        ## support hibernation
        swap_space="$((swap_space + ramgb))"G
      ''
    }

    echo Will use a "$swap_space" swap space partition

    sgdisk -og "$DISK"
    partprobe "$DISK"

    partnum=0

    if [ "$BOOTMODE" = "Legacy" ]; then
      partnum=$((partnum + 1))
      sgdisk -n 0:0:+20M -t 0:ef02 -c 0:"biosboot" -u 0:"21686148-6449-6E6F-744E-656564454649" "$DISK" # 1
    fi
    sgdisk -n 0:0:+"$efi_space" -t 0:ef00 -c 0:"p_efi" "$DISK" # 1
    sgdisk -n 0:0:+"$luks_key_space" -t 0:8300 -c 0:"p_cryptkey" "$DISK" # 2
    sgdisk -n 0:0:+"$swap_space" -t 0:8300 -c 0:"p_swap" "$DISK" # 3
    sgdisk -n 0:0:0 -t 0:8300 -c 0:"p_root" "$DISK" # 4

    echo "PREFIX: $PARTITION_PREFIX"

    partnum=$((partnum + 1))
    DISK_EFI="$DISK$PARTITION_PREFIX$partnum"
    partnum=$((partnum + 1))
    DISK_CRYPTKEY="$DISK$PARTITION_PREFIX$partnum"
    partnum=$((partnum + 1))
    DISK_SWAP="$DISK$PARTITION_PREFIX$partnum"
    partnum=$((partnum + 1))
    DISK_ROOT="$DISK$PARTITION_PREFIX$partnum"

    partprobe "$DISK"
    sgdisk -p "$DISK"
    fdisk -l "$DISK"

    echo Formatting cryptkey disk "$DISK_CRYPTKEY", using keyfile "$CRYPTKEYFILE"
    cryptsetup ${luksFormatExtraParams} luksFormat --label=${diskLabels.encCryptkey} -q --key-file="$CRYPTKEYFILE" "$DISK_CRYPTKEY"
    DISK_CRYPTKEY=/dev/disk/by-label/${diskLabels.encCryptkey}

    waitForPath "$DISK_CRYPTKEY"

    echo Opening cryptkey disk "$DISK_CRYPTKEY", using keyfile "$CRYPTKEYFILE"
    cryptsetup luksOpen --key-file="$CRYPTKEYFILE" "$DISK_CRYPTKEY" ${diskLabels.encCryptkey}

    waitForPath "/dev/mapper/${diskLabels.encCryptkey}"

    echo Writing random data to /dev/mapper/${diskLabels.encCryptkey}
    dd if=$DEVRANDOM of=/dev/mapper/${diskLabels.encCryptkey} bs=1024 count=14000 || true

    echo Creating encrypted swap
    # shellcheck disable=SC2086
    cryptsetup ${luksFormatExtraParams} luksFormat --label=${diskLabels.encSwap} -q --key-file=/dev/mapper/${diskLabels.encCryptkey} "$DISK_SWAP"

    echo Creating encrypted root
    # shellcheck disable=SC2086
    cryptsetup ${luksFormatExtraParams} luksFormat --label=${diskLabels.encRoot} -q --key-file=/dev/mapper/${diskLabels.encCryptkey} "$DISK_ROOT"

    ${
      lib.concatStringsSep "\n" (lib.imap1 (
        idx: disk: ''

          wipefs -fa "${disk}"
          sgdisk -z "${disk}"
          partprobe "${disk}"
          sgdisk -og "${disk}"
          partprobe "${disk}"

          sgdisk -n 0:0:0 -t 0:8300 -c 0:"p_root${toString idx}" "${disk}" # 1
          partprobe "${disk}"
          sgdisk -p "${disk}"
          fdisk -l "${disk}"

          echo Creating encrypted root - disk ${disk}
          # shellcheck disable=SC2086
          cryptsetup ${luksFormatExtraParams} luksFormat --label=${diskLabels.encRoot}${toString idx} -q --key-file=/dev/mapper/${diskLabels.encCryptkey} "${disk}$PARTITION_PREFIX"1
        ''
      ) (builtins.tail btrfsDisks))
    }

    echo Opening encrypted swap using keyfile
    cryptsetup luksOpen --key-file=/dev/mapper/${diskLabels.encCryptkey} "$DISK_SWAP" ${diskLabels.encSwap}

    waitForPath "/dev/mapper/${diskLabels.encSwap}"
    mkswap -L ${diskLabels.swap} /dev/mapper/${diskLabels.encSwap}

    waitForPath "/dev/disk/by-label/${diskLabels.swap}"

    echo Opening encrypted root using keyfile
    cryptsetup luksOpen --key-file=/dev/mapper/${diskLabels.encCryptkey} "$DISK_ROOT" ${diskLabels.encRoot}

    waitForPath "/dev/mapper/${diskLabels.encRoot}"

    ${
      lib.concatStringsSep "\n" (lib.imap1 (
        idx: disk: ''
          echo Opening encrypted root - disk ${disk}
          cryptsetup luksOpen --key-file=/dev/mapper/${diskLabels.encCryptkey} "${disk}$PARTITION_PREFIX"1 ${diskLabels.encRoot}${toString idx}

          sgdisk -p "${disk}"
          partprobe "${disk}"
          fdisk -l "${disk}"

          waitForPath "/dev/mapper/${diskLabels.encRoot}${toString idx}"
        ''
      ) (builtins.tail btrfsDisks))
    }

    echo Creating btrfs filesystem on /dev/mapper/${diskLabels.encRoot}
    mkfs.btrfs ${btrfsFormatExtraParams} -f -L ${diskLabels.root} /dev/mapper/${diskLabels.encRoot}
    waitForPath "/dev/disk/by-label/${diskLabels.root}"

    echo Creating vfat disk at "$DISK_EFI"
    mkfs.fat -F 32 -n ${diskLabels.boot} "$DISK_EFI"
    waitForPath "/dev/disk/by-label/${diskLabels.boot}"

    mount -t tmpfs none /mnt
    mkdir -p "/mnt/tmproot" ${concatStringsSep " " (map (v: "/mnt/${replaceStrings ["@"] [""] v}") subvolumes)} "/mnt/boot"
    mkdir -p /mnt/tmp
    chmod 777 /mnt/tmp

    echo Listing /dev/mapper
    ls -lah /dev/mapper/

    echo Listing /dev/disk/by-label
    ls -lah /dev/disk/by-label/

    echo Temporarily mounting root btrfs volume from "/dev/disk/by-label/${diskLabels.root}" to /mnt/tmproot
    retryDefault mount -o rw,noatime,compress=zstd /dev/disk/by-label/${diskLabels.root} /mnt/tmproot

    ${
      lib.concatStringsSep "\n" (lib.imap1 (
        idx: disk: let
          device = "/dev/mapper/${diskLabels.encRoot}${toString idx}";
        in ''
          echo Adding device ${device}
          btrfs device add -f ${device} /mnt/tmproot
        ''
      ) (builtins.tail btrfsDisks))
    }

    ${
      if builtins.length btrfsDisks > 1
      then ''
        echo Balancing btrfs filesystem at /mnt/tmproot
        btrfs balance start -dconvert=raid1 -mconvert=raid1 /mnt/tmproot
      ''
      else ""
    }

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

    ${concatStringsSep "\n" (map (v: ''mount -o rw,noatime,compress=zstd,subvol=${v} /dev/disk/by-label/${diskLabels.root} /mnt/${replaceStrings ["@"] [""] v}'') subvolumes)}

    # and mount the boot partition
    echo Mounting boot partition
    mount /dev/disk/by-label/${diskLabels.boot} /mnt/boot
  ''
