#!/usr/bin/env bash

if [ -n "$CLOUD_DISK_PASSWORD" ]; then
  echo -n "$CLOUD_DISK_PASSWORD"
  exit 0
fi

rbw get hetzner -- cloud_disk_password
