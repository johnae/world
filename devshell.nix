{ writeText, writeStrictShellScriptBin, mkDevShell, agenix, age-plugin-yubikey, pixiecore }:

let

  pixieboot = writeStrictShellScriptBin "pixieboot" ''
    echo Hey, you may need to turn off the firewall for this to work
    nix build .#nixosConfigurations.pxebooter -o /tmp/netboot
    n="$(realpath /tmp/netboot)"
    init="$(grep -ohP 'init=\S+' "$n/netboot.ipxe")"
    sudo ${pixiecore}/bin/pixiecore boot "$n/bzImage" "$n/initrd" \
      --cmdline "$init loglevel=4" \
      --debug --dhcp-no-bind --port 64172 --status-port 64172
  '';
in

mkDevShell {
  name = "world";
  packages = [ agenix age-plugin-yubikey pixiecore pixieboot ];
  intro = ''

    Hello, world!

  '';
}
