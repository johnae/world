{
  services.pipewire = {
    media-session.config.bluez-monitor.rules = [
      {
        # Matches all cards
        matches = [ { "device.name" = "~bluez_card.*"; } ];
        actions = {
          "update-props" = {
            "bluez5.autoswitch-profile" = true;
          };
        };
      }
    ];
  };
}
