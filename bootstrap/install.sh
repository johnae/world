#!/usr/bin/env nix-shell
#!nix-shell -i bash -p btrfs-progs -p mount -p utillinux -p parted -p gptfdisk -p cryptsetup -p procps -p e2fsprogs -p coreutils
# shellcheck shell=bash

set -euo pipefail

debug() {
    echo "$@"
    ########
    echo Waiting 5 seconds before continuing
    sleep 5
}

## we do this because there may be some usb devices and afaiu
## we sometimes have to wait for them, this doesn't perfectly
## solve the problem but makes it generally fine
retry() {
    n=${1:-1}
    sleepwait=5
    shift
    if [ "$n" -le 0 ]; then
       echo "\"$*\"" failed - giving up
       sleep 10
       exit 1
    fi
    n=$((n - 1))
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
    retry 2 "$@"
}

SKIP_INSTALL=${SKIP_INSTALL:-}
DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd -P)

## This script bootstraps a nixos install. The assumptions are:
# 1. You want an EFI System Partition (500MB) - so no BIOS support
# 2. You want encrypted root and swap
# 3. You want swap space size to be half of RAM as per modern standards
# 4. You want to use btrfs for everything else and you want to use subvolumes
#    for /, /var and /home
# 5. You want to not care about atime and you want
#    to compress your fs using zstd

## on servers we might want additional btrfs subvolumes
## for use with kubernetes local volume provisioner
ADDITIONAL_VOLUMES="${ADDITIONAL_VOLUMES:-}"
## if we want to format an additional disk where we
## probably want those additional volumes above to go
ADDITIONAL_DISK="${ADDITIONAL_DISK:-}"
## allow giving the disk password as an env var. Not safe yadda yadda.
DISK_PASSWORD="${DISK_PASSWORD:-}"

if [ -d /sys/firmware/efi/efivars ]; then
    BOOTMODE="UEFI"
else
    BOOTMODE="Legacy"
fi

echo BOOTMODE: "$BOOTMODE"

DEVRANDOM=/dev/urandom

if [ "$(systemd-detect-virt)" = "none" ]; then
  CRYPTKEYFILE="${CRYPTKEYFILE:-/sys/class/dmi/id/product_uuid}"
else
  CRYPTKEYFILE="${CRYPTKEYFILE:-/sys/class/dmi/id/product_version}" ## we're testing here
fi

if [ -n "$DISK_PASSWORD" ]; then
    if [ ! -d "/secrets" ]; then
      mkdir -p /secrets
      mount -t tmpfs -o size=64m tmpfs /secrets
    fi
    CRYPTKEYFILE=/secrets/disk_password
    echo -n "$DISK_PASSWORD" > "$CRYPTKEYFILE"
fi

if [ ! -e "$CRYPTKEYFILE" ]; then
    debug "$CRYPTKEYFILE not found, aborting"
    exit 1
fi

if [ "$(stat -c %s "$CRYPTKEYFILE")" -lt 2 ]; then
    debug "$CRYPTKEYFILE too small, less than 2 bytes"
    exit 1
fi

DISK=/dev/nvme0n1
DISK2=/dev/nvme1n1 ## possibly another disk on servers - use it to setup raid1

if [ ! -b "$DISK" ]; then
    echo "$DISK" is not a block device
    DISK=/dev/sda
fi

if [ ! -b "$DISK" ]; then
    echo "$DISK" is not a block device
    echo Giving up
    exit 1
fi

PARTITION_PREFIX=""
if echo "$DISK" | grep -q "nvme"; then
    PARTITION_PREFIX="p"
fi

echo "Formatting disk '$DISK'"

# clear out the disk completely
wipefs -fa "$DISK"
sgdisk -Z "$DISK"
partprobe "$DISK"

if [ -b "$DISK2" ]; then
    wipefs -fa "$DISK2"
    sgdisk -Z "$DISK2"
    partprobe "$DISK2"
fi

efi_space=500M # EF00 EFI Partition or EF02 BIOS Boot Partition - if we're Legacy booting
luks_key_space=20M # 8300
ramgb="$(free --giga | tail -n+2 | head -1 | awk '{print $2}')"
# set to half amount of RAM (but see further down for hibernation etc)
swap_space="$((ramgb / 2))"
# special case when there's very little ram
if [ "$swap_space" = "0" ]; then
    swap_space="1"
elif [ "$swap_space" -ge "33" ]; then
    swap_space="8" ## this is likely a server so we don't want so much swap
else
    swap_space="$((swap_space + ramgb))" ## to ensure hibernation works properly
fi
swap_space="$swap_space"G
echo Will use "$swap_space" swap space partition
# rest (eg. root) will use the remaining space (btrfs) 8300

# now ensure there's a fresh GPT on there
sgdisk -og "$DISK"
partprobe "$DISK"

if [ -b "$DISK2" ]; then
  sgdisk -og "$DISK2"
  partprobe "$DISK2"
fi

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

if [ -b "$DISK2" ]; then
  if [ "$BOOTMODE" = "Legacy" ]; then
    sgdisk -n 0:0:+20M -t 0:ef02 -c 0:"biosboot" -u 0:"21686148-6449-6E6F-744E-656564454649" "$DISK2" # 1
  fi
  sgdisk -n 0:0:+$efi_space -t 0:ef00 -c 0:"efi" "$DISK2" # 1
  sgdisk -n 0:0:+$luks_key_space -t 0:8300 -c 0:"cryptkey" "$DISK2" # 2
  sgdisk -n 0:0:+$swap_space -t 0:8300 -c 0:"swap" "$DISK2" # 3
  sgdisk -n 0:0:0 -t 0:8300 -c 0:"root" "$DISK2" # 4
  partprobe "$DISK2"
fi

DISK_EFI_LABEL=boot
partnum=$((partnum + 1))
DISK_EFI="$DISK$PARTITION_PREFIX$partnum"
ENC_DISK_CRYPTKEY_LABEL=cryptkey
partnum=$((partnum + 1))
DISK_CRYPTKEY="$DISK$PARTITION_PREFIX$partnum"
DISK_SWAP_LABEL=swap
ENC_DISK_SWAP_LABEL=encrypted_swap
partnum=$((partnum + 1))
DISK_SWAP="$DISK$PARTITION_PREFIX$partnum"
DISK_ROOT_LABEL=root
ENC_DISK_ROOT_LABEL=encrypted_root
partnum=$((partnum + 1))
DISK_ROOT="$DISK$PARTITION_PREFIX$partnum"
if [ -b "$DISK2" ]; then
  ENC_DISK_ROOT2_LABEL=encrypted_root2
  DISK_ROOT2="$DISK2$PARTITION_PREFIX$partnum"
fi
DISK_EXTRA_LABEL=extra
ENC_DISK_EXTRA_LABEL=encrypted_extra
DISK_EXTRA=

sgdisk -p "$DISK"

# make sure everything knows about the new partition table
partprobe "$DISK"
fdisk -l "$DISK"

if [ -b "$DISK2" ]; then
  sgdisk -p "$DISK2"
  partprobe "$DISK2"
  fdisk -l "$DISK2"
fi

debug "$DISK wiped and formatted"

if [ -n "$ADDITIONAL_DISK" ]; then
    echo "Formatting disk '$ADDITIONAL_DISK'"
    wipefs -fa "$ADDITIONAL_DISK"
    sgdisk -Z "$ADDITIONAL_DISK"
    sgdisk -og "$ADDITIONAL_DISK"
    sgdisk -n 0:0:0 -t 0:8300 -c 0:"extra" "$ADDITIONAL_DISK" # 1

    DISK_EXTRA="$ADDITIONAL_DISK"1

    sgdisk -p "$ADDITIONAL_DISK"
    partprobe "$ADDITIONAL_DISK"
    fdisk -l "$ADDITIONAL_DISK"

    debug "$ADDITIONAL_DISK wiped and formatted"
fi

# create a disk for the key used to decrypt the other volumes
# either using password or locked to the uuid of the product hardware (less secure ofc)
echo Formatting cryptkey disk "$DISK_CRYPTKEY", using keyfile "$CRYPTKEYFILE"
retryDefault cryptsetup luksFormat --label="$ENC_DISK_CRYPTKEY_LABEL" -q --key-file="$CRYPTKEYFILE" "$DISK_CRYPTKEY"
DISK_CRYPTKEY=/dev/disk/by-label/"$ENC_DISK_CRYPTKEY_LABEL"

echo Opening cryptkey disk "$DISK_CRYPTKEY", using keyfile "$CRYPTKEYFILE"
retryDefault cryptsetup luksOpen --key-file="$CRYPTKEYFILE" "$DISK_CRYPTKEY" "$ENC_DISK_CRYPTKEY_LABEL"

# dump random data into what will be our key
echo Writing random data to /dev/mapper/"$ENC_DISK_CRYPTKEY_LABEL"
dd if=$DEVRANDOM of=/dev/mapper/"$ENC_DISK_CRYPTKEY_LABEL" bs=1024 count=14000 || true

debug "$DISK_CRYPTKEY wiped, formatted and written to"

# create encrypted swap partition
echo Creating encrypted swap
retryDefault cryptsetup luksFormat --label="$ENC_DISK_SWAP_LABEL" -q --key-file=/dev/mapper/"$ENC_DISK_CRYPTKEY_LABEL" "$DISK_SWAP"

# create the encrypted root partition
echo Creating encrypted root
retryDefault cryptsetup luksFormat --label="$ENC_DISK_ROOT_LABEL" -q --key-file=/dev/mapper/"$ENC_DISK_CRYPTKEY_LABEL" "$DISK_ROOT"

if [ -b "$DISK2" ]; then
  # create the encrypted root 2 partition
  echo Creating encrypted root 2
  retryDefault cryptsetup luksFormat --label="$ENC_DISK_ROOT2_LABEL" -q --key-file=/dev/mapper/"$ENC_DISK_CRYPTKEY_LABEL" "$DISK_ROOT2"
fi

# open those crypt volumes now
echo Opening encrypted swap using keyfile
retryDefault cryptsetup luksOpen --key-file=/dev/mapper/"$ENC_DISK_CRYPTKEY_LABEL" "$DISK_SWAP" "$ENC_DISK_SWAP_LABEL"
retryDefault mkswap -L "$DISK_SWAP_LABEL" /dev/mapper/"$ENC_DISK_SWAP_LABEL"

echo Opening encrypted root using keyfile
retryDefault cryptsetup luksOpen --key-file=/dev/mapper/"$ENC_DISK_CRYPTKEY_LABEL" "$DISK_ROOT" "$ENC_DISK_ROOT_LABEL"

if [ -b "$DISK2" ]; then
  echo Opening encrypted root2 using keyfile
  retryDefault cryptsetup luksOpen --key-file=/dev/mapper/"$ENC_DISK_CRYPTKEY_LABEL" "$DISK_ROOT2" "$ENC_DISK_ROOT2_LABEL"
fi

debug "$DISK_SWAP and $DISK_ROOT wiped and formatted"

if [ -b "$DISK2" ]; then
  echo Creating btrfs RAID1 filesystem on /dev/mapper/"$ENC_DISK_ROOT_LABEL" and /dev/mapper/"$ENC_DISK_ROOT2_LABEL"
  retryDefault mkfs.btrfs -L "$DISK_ROOT_LABEL" -d raid1 -m raid1 /dev/mapper/"$ENC_DISK_ROOT_LABEL" /dev/mapper/"$ENC_DISK_ROOT2_LABEL"
else
  echo Creating btrfs filesystem on /dev/mapper/"$ENC_DISK_ROOT_LABEL"
  retryDefault mkfs.btrfs -L "$DISK_ROOT_LABEL" /dev/mapper/"$ENC_DISK_ROOT_LABEL"
fi

# and create the efi boot partition
echo Creating vfat disk at "$DISK_EFI"
retryDefault mkfs.vfat -n "$DISK_EFI_LABEL" "$DISK_EFI"

partprobe /dev/mapper/"$ENC_DISK_SWAP_LABEL" ## in case partprobe failed (it might sometimes, but will likely succeed for the given device here)

# enable swap on the decrypted swap device
echo Enabling swap on "/dev/disk/by-label/$DISK_SWAP_LABEL"
swapon /dev/disk/by-label/"$DISK_SWAP_LABEL"

mount -t tmpfs none /mnt
mkdir -p "/mnt/tmproot" "/mnt/nix" "/mnt/boot" "/mnt/keep"

# mount the decrypted cryptroot to /mnt (btrfs)
echo Temporarily mounting root btrfs volume from "/dev/disk/by-label/$DISK_ROOT_LABEL" to /mnt/tmproot
retryDefault mount -o rw,noatime,compress=zstd,ssd,space_cache /dev/disk/by-label/"$DISK_ROOT_LABEL" /mnt/tmproot

# now create btrfs subvolumes we're interested in having
echo Creating btrfs subvolumes at /mnt/tmproot
cd /mnt/tmproot
btrfs sub create @nix
btrfs sub create @keep

debug "Btrfs subvolumes etc created on $DISK_ROOT, swap turned on, efi mounted at /mnt/boot etc"

if [ -n "$ADDITIONAL_VOLUMES" ]; then
  echo Creating additional btrfs subvolumes
  cd /mnt/tmproot
  mkdir -p "mnt/disks/cow"
  mkdir -p "mnt/disks/nocow"

  if [ -n "$DISK_EXTRA" ] && [ -e "$DISK_EXTRA" ]; then

      echo Creating encrypted fs on additional disk "$DISK_EXTRA"
      retryDefault cryptsetup luksFormat --label="$ENC_DISK_EXTRA_LABEL" -q --key-file=/dev/mapper/"$ENC_DISK_CRYPTKEY_LABEL" "$DISK_EXTRA"

      echo Opening "$DISK_EXTRA" encrypted fs at /dev/mapper/cryptxtra
      retryDefault cryptsetup luksOpen --key-file=/dev/mapper/"$ENC_DISK_CRYPTKEY_LABEL" "$DISK_EXTRA" "$ENC_DISK_EXTRA_LABEL"

      partprobe /dev/mapper/"$ENC_DISK_EXTRA_LABEL"

      echo Creating btrfs filesystem on /dev/mapper/"$ENC_DISK_EXTRA_LABEL"
      retryDefault mkfs.btrfs -L "$DISK_EXTRA_LABEL" /dev/mapper/"$ENC_DISK_EXTRA_LABEL"

      partprobe /dev/mapper/"$ENC_DISK_EXTRA_LABEL"

      echo Mounting extra fs from "/dev/disk/by-label/$DISK_EXTRA_LABEL" to @/mnt/disks
      retryDefault mount -o rw,noatime,compress=zstd,ssd,space_cache /dev/disk/by-label/"$DISK_EXTRA_LABEL" mnt/disks
      cd mnt/disks
      for i in $(seq 1 20); do btrfs sub create "@local-disk-$i"; done
      for i in $(seq 1 20); do
          btrfs sub create "@local-disk-nocow-$i"
          chattr +C "@local-disk-nocow-$i"
      done
      cd /mnt

      umount mnt/disks

      debug "$DISK_EXTRA encrypted, formatted with btrfs fs and mounted"
  else
      for i in $(seq 1 20); do btrfs sub create "@local-disk-$i"; done
      for i in $(seq 1 20); do
          btrfs sub create "@local-disk-nocow-$i"
          chattr +C "@local-disk-nocow-$i"
      done
      debug "No extra disk, created btrfs subvolumes"
  fi

  for i in $(seq 1 20); do mkdir -p "mnt/disks/cow/local-disk-$i"; done
  for i in $(seq 1 20); do mkdir -p "mnt/disks/nocow/local-disk-$i"; done

  debug "Created all mount points for additional volumes"
fi

cd "$DIR"
# umount the "real" root and mount those subvolumes in place instead
echo Unmounting /mnt/tmproot
umount /mnt/tmproot
rmdir /mnt/tmproot

echo Devices with uuids
ls -lah /dev/disk/by-uuid/

echo Devices with labels
ls -lah /dev/disk/by-label/

echo Mounting nix subvolume at /mnt/nix
mount -o rw,noatime,compress=zstd,ssd,space_cache,subvol=@nix \
      /dev/disk/by-label/"$DISK_ROOT_LABEL" /mnt/nix

debug "Mounted the nix volume"

echo Mounting keep subvolume at /mnt/keep
mount -o rw,noatime,compress=zstd,ssd,space_cache,subvol=@keep \
      /dev/disk/by-label/"$DISK_ROOT_LABEL" /mnt/keep

debug "Mounted the keep volume"

# and mount the boot partition
echo Mounting boot partition
mount /dev/disk/by-label/"$DISK_EFI_LABEL" /mnt/boot

debug "Mounted the boot volumes from $DISK_EFI_LABEL"

if [ -z "$SKIP_INSTALL" ]; then
  nixos-install --no-root-passwd --option binary-caches "" --system "$(cat /etc/system-closure-path)"
fi

debug "Install completed, exiting..."