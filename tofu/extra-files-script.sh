#!/usr/bin/env bash

mkdir -p etc/ssh

if [ -n "$CLOUD_SSH_HOSTKEY" ]; then
  echo "$CLOUD_SSH_HOSTKEY" | base64 -d > etc/ssh/ssh_host_ed25519_key
else
  rbw get hetzner -- cloud_ssh_host_key | base64 -d > etc/ssh/ssh_host_ed25519_key
fi

if [ -n "$CLOUD_SSH_INITRD_KEY" ]; then
  echo "$CLOUD_SSH_INITRD_KEY" | base64 -d > etc/ssh/initrd_ed25519_key
else
  rbw get hetzner -- cloud_ssh_initrd_key | base64 -d > etc/ssh/initrd_ed25519_key
fi

chmod 0600 etc/ssh/ssh_host_ed25519_key
chmod 0600 etc/ssh/initrd_ed25519_key
