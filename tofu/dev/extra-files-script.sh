#!/usr/bin/env bash

mkdir -p etc/ssh/secrets var/lib/nixos keep/var/lib/nixos

if [ -n "$HCLOUD_DEV_SSH_HOSTKEY" ]; then
  echo "$HCLOUD_DEV_SSH_HOSTKEY" | base64 -d > etc/ssh/ssh_host_ed25519_key
else
  rbw get hetzner -- hcloud_dev_ssh_host_key | base64 -d > etc/ssh/ssh_host_ed25519_key
fi

if [ -n "$HCLOUD_DEV_SSH_INITRD_KEY" ]; then
  echo "$HCLOUD_DEV_SSH_INITRD_KEY" | base64 -d > etc/ssh/secrets/initrd_ed25519_key
else
  rbw get hetzner -- hcloud_dev_ssh_initrd_key | base64 -d > etc/ssh/secrets/initrd_ed25519_key
fi

if [ -n "$TS_AUTH_KEY" ]; then
  echo "$TS_AUTH_KEY" > etc/ts-auth-key
  chmod 0600 etc/ts-auth-key
fi

chmod 0600 etc/ssh/ssh_host_ed25519_key
chmod 0600 etc/ssh/secrets/initrd_ed25519_key
chmod 0755 var/lib/nixos keep/var/lib/nixos
