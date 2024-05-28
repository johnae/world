{
  config,
  lib,
  writeShellApplication,
  keyutils,
  ...
}: let
  inherit (lib) attrByPath mapAttrsToList concatStringsSep flatten;
  inherit (builtins) filter match foldl' replaceStrings elemAt length head substring split;
  inherit (config.config) boot cryptsetup bcachefs machinePurpose disk;
  inherit (bcachefs) subvolumes;
  bootMode =
    if boot.loader.systemd-boot.enable
    then "UEFI"
    else "Legacy";
  mbr =
    if disk.dosLabel
    then "true"
    else "false";
  luksFormatExtraParams = cryptsetup.luksFormat.extraParams;
  bcacheFsDisks = bcachefs.disks;
  ## TODO: fix this to be more generic as support for bcachefs gets better
  bcacheUuid =
    if lib.hasInfix "/by-uuid/" (head bcachefs.devices)
    then lib.last (split "by-uuid/" (head bcachefs.devices))
    else null;
  diskLabels = {
    boot = "boot";
    swap = "swap";
    encSwap = "encrypted_swap";
    root = "root";
  };
  efiSpace = "500M";
  ramGb = "$(free --giga | tail -n+2 | head -1 | awk '{print $2}')";
in
  writeShellApplication {
    name = "diskformat";
    runtimeInputs = [keyutils];
    text = ''
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

      DISK=${builtins.head bcacheFsDisks}
      PARTITION_PREFIX=""

      if echo "$DISK" | grep -q nvme; then
        echo "$DISK" is an NVMe device
        PARTITION_PREFIX="p"
      fi

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
      partprobe "$DISK"

      efi_space="${efiSpace}"
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

      sgdisk -Z "$DISK"
      partprobe "$DISK"

      partnum=0

      echo Formatting disk using GPT
      sgdisk -og "$DISK"
      if [ "$BOOTMODE" = "Legacy" ]; then
        echo Legacy disk formatting
        partnum=$((partnum + 1))
        sgdisk -n 0:0:+20M -t 0:ef02 -c 0:"biosboot" -u 0:"21686148-6449-6E6F-744E-656564454649" "$DISK" # 1
      fi
      sgdisk -n 0:0:+"$efi_space" -t 0:ef00 -c 0:"p_efi" "$DISK" # 1
      sgdisk -n 0:0:+"$swap_space" -t 0:8300 -c 0:"p_swap" "$DISK" # 2
      sgdisk -n 0:0:0 -t 0:8300 -c 0:"p_root" "$DISK" # 3

      echo "PREFIX: $PARTITION_PREFIX"

      partnum=$((partnum + 1))
      DISK_EFI="$DISK$PARTITION_PREFIX$partnum"
      partnum=$((partnum + 1))
      DISK_SWAP="$DISK$PARTITION_PREFIX$partnum"
      partnum=$((partnum + 1))
      DISK_ROOT="$DISK$PARTITION_PREFIX$partnum"

      partprobe "$DISK"
      sgdisk -p "$DISK"
      fdisk -l "$DISK"

      BCACHEFS_DISKS="$DISK_ROOT"

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

            BCACHEFS_DISKS="$BCACHEFS_DISKS":"${disk}$PARTITION_PREFIX"1
          ''
        ) (builtins.tail bcacheFsDisks))
      }

      echo Creating encrypted root bcachefs
      # shellcheck disable=SC2086
      bcachefs format ${
        if bcacheUuid != null
        then "--uuid=${bcacheUuid} "
        else " "
      }--discard --encrypted --compression=zstd --replicas=${toString (builtins.length bcacheFsDisks)} -L root --label=ssd.ssd1 "$DISK_ROOT" ${lib.concatStringsSep " " (lib.imap1 (idx: disk: ''--label=ssd.ssd${toString (idx + 1)} ${disk}"$PARTITION_PREFIX"1'') (builtins.tail bcacheFsDisks))}

      keyctl link @u @s

      echo Unlocking bcachefs volume
      bcachefs unlock "$DISK_ROOT"

      mount -t tmpfs none /mnt
      mkdir -p "/mnt/keep"
      mkdir -p /mnt/tmp
      chmod 777 /mnt/tmp

      echo Mounting bcachefs volume at /mnt/keep
      bcachefs mount "$BCACHEFS_DISKS" /mnt/keep

      echo Generating encrypted swap key
      dd if=$DEVRANDOM of=/mnt/keep/encrypted_swap.key bs=512 count=4

      echo Creating encrypted swap
      # shellcheck disable=SC2086
      cryptsetup ${luksFormatExtraParams} luksFormat --label=${diskLabels.encSwap} -q --key-file=/mnt/keep/encrypted_swap.key "$DISK_SWAP"

      echo Opening encrypted swap using keyfile
      cryptsetup luksOpen --key-file=/mnt/keep/encrypted_swap.key "$DISK_SWAP" ${diskLabels.encSwap}

      waitForPath "/dev/mapper/${diskLabels.encSwap}"
      mkswap -L ${diskLabels.swap} /dev/mapper/${diskLabels.encSwap}

      waitForPath "/dev/disk/by-label/${diskLabels.swap}"

      echo Creating vfat disk at "$DISK_EFI"
      mkfs.fat -F 32 -n ${diskLabels.boot} "$DISK_EFI"
      waitForPath "/dev/disk/by-label/${diskLabels.boot}"

      # now create the bcachefs subvolumes we're interested in having
      echo Creating bcachefs subvolumes at /mnt/keep
      ${concatStringsSep "\n" (map (v: "bcachefs subvolume create /mnt/keep/${v}") subvolumes)}

      cd "$DIR"

      echo Bind mounting subvolumes
      mkdir -p ${concatStringsSep " " (map (sub: "/mnt/${sub}") subvolumes)} /mnt/boot
      ${concatStringsSep "\n" (map (sub: "mount --bind /mnt/keep/${sub} /mnt/${sub}") subvolumes)}

      # and mount the boot partition
      echo Mounting boot partition
      mount /dev/disk/by-label/${diskLabels.boot} /mnt/boot
    '';
  }
