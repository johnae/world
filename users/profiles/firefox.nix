{pkgs, ...}: let
  firefox = pkgs.firefox.override {
    nativeMessagingHosts = [pkgs.tridactyl-native];
  };
in {
  programs.firefox = {
    enable = true;
    package = firefox;
    profiles = {
      default = {
        extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
          tridactyl
          adsum-notabs
          ublock-origin
          react-devtools
          duckduckgo-privacy-essentials
          privacy-badger
        ];
        settings = {
          "browser.compactmode.show" = true;
          "browser.startup.homepage" = "";
          "browser.search.region" = "SE";
          "browser.search.isUS" = false;
          "distribution.searchplugins.defaultLocale" = "sv-SE";
          "general.useragent.locale" = "sv-SE";
          "browser.bookmarks.showMobileBookmarks" = true;
          "browser.tabs.opentabfor.middleclick" = false;
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "media.peerconnection.enabled" = true;
          "gfx.webrender.all" = true;
          "media.ffmpeg.vaapi.enabled" = true; ## keep this with ff 96
          "media.rdd-ffmpeg.enabled" = true; ## keep this with ff 96
          "media.ffvpx.enabled" = false; ## remove on ff 96
          "media.navigator.mediadatadecoder_vpx_enabled" = true; ## remove on ff 96
          "media.rdd-vpx.enabled" = false; ## remove on ff 96
        };

        #userChrome = ''
        #  #TabsToolbar {
        #  visibility: collapse;
        #  }
        #'';
      };
    };
  };
}
