{pkgs, ...}: {
  programs.zen-browser = {
    enable = true;
    nativeMessagingHosts = [pkgs.firefoxpwa];
    policies = let
      mkExtensionSettings = builtins.mapAttrs (_: pluginId: {
        install_url = "https://addons.mozilla.org/firefox/downloads/latest/${pluginId}/latest.xpi";
        installation_mode = "force_installed";
      });
    in {
      AutofillAddressEnabled = true;
      AutofillCreditCardEnabled = false;
      DisableAppUpdate = true;
      DisableFeedbackCommands = true;
      DisableFirefoxStudies = true;
      DisablePocket = true;
      DisableTelemetry = true;
      DontCheckDefaultBrowser = true;
      NoDefaultBookmarks = true;
      OfferToSaveLogins = false;
      ExtensionSettings = mkExtensionSettings {
        "zen-tab-search@extension.example" = "zen-tab-search";
        "{446900e4-71c2-419f-a6a7-df9c091e268b}" = "bitwarden-password-manager";
        "@react-devtools" = "react-devtools";
        "uBlock0@raymondhill.net" = "ublock-origin";
        "{91aa3897-2634-4a8a-9092-279db23a7689}" = "zen-internet";
        "jid1-ZAdIEUB7XOzOJw@jetpack" = "duckduckgo-for-firefox";
        "jid1-MnnxcxisBPnSXQ@jetpack" = "privacy-badger17";
      };
      EnableTrackingProtection = {
        Value = true;
        Locked = true;
        Cryptomining = true;
        Fingerprinting = true;
      };
    };
  };
}
