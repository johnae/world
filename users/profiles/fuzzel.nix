{
  programs.fuzzel = {
    enable = true;
    settings = {
      main = {
        # output=<not set>
        font = "Roboto Medium:size=10";
        # dpi-aware=auto
        prompt = ''">    "'';
        icon-theme = "Nordic-darker";
        # icons-enabled=yes
        # fields=filename,name,generic
        # password-character=*
        # fuzzy=yes
        # show-actions=no
        # terminal=$TERMINAL -e  # Note: you cannot actually use environment variables here
        # launch-prefix=<not set>

        lines = 20;
        width = 60;
        horizontal-pad = 40;
        vertical-pad = 16;
        inner-pad = 6;
        # image-size-ratio=0.5;
        # line-height=<use font metrics>;
        #letter-spacing=0;
        layer = "top";
        # exit-on-keyboard-focus-loss = yes;
      };

      #[border]
      # width=1
      # radius=10

      #[dmenu]
      # mode=text  # text|index
      # exit-immediately-if-empty=no

      #[key-bindings]
      # cancel=Escape Control+g
      # execute=Return KP_Enter Control+y
      # execute-or-next=Tab
      # cursor-left=Left Control+b
      # cursor-left-word=Control+Left Mod1+b
      # cursor-right=Right Control+f
      # cursor-right-word=Control+Right Mod1+f
      # cursor-home=Home Control+a
      # cursor-end=End Control+e
      # delete-prev=BackSpace
      # delete-prev-word=Mod1+BackSpace Control+BackSpace
      # delete-next=Delete
      # delete-next-word=Mod1+d Control+Delete
      # delete-line=Control+k
      # prev=Up Control+p
      # prev-with-wrap=ISO_Left_Tab
      # prev-page=PageUp KP_PageUp
      # next=Down Control+n
      # next-with-wrap=none
      # next-page=Page_Down KP_Page_Down

      # custom-N: *dmenu mode only*. Like execute, but with a non-zero
      # exit-code; custom-1 exits with code 10, custom-2 with 11, custom-3
      # with 12, and so on.

      # custom-1=Mod1+1
      # custom-2=Mod1+2
      # custom-3=Mod1+3
      # custom-4=Mod1+4
      # custom-5=Mod1+5
      # custom-6=Mod1+6
      # custom-7=Mod1+7
      # custom-8=Mod1+8
      # custom-9=Mod1+9
      # custom-10=Mod1+0
      # custom-11=Mod1+exclam
      # custom-12=Mod1+at
      # custom-13=Mod1+numbersign
      # custom-14=Mod1+dollar
      # custom-15=Mod1+percent
      # custom-16=Mod1+dead_circumflex
      # custom-17=Mod1+ampersand
      # custom-18=Mod1+asterix
      # custom-19=Mod1+parentleft

      # colors = {
      #   background = "282a36dd";
      #   text = "f8f8f2ff";
      #   match = "8be9fdff";
      #   selection-match = "8be9fdff";
      #   selection = "44475add";
      #   selection-text = "f8f8f2ff";
      #   border = "bd93f9ff";
      # };
    };
  };
}
