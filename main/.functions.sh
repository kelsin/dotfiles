# Set Title
function set-title {
  case $TERM in
    (tmux|screen)*)
      if [ -n "$TMUX" ]; then
        tmux rename-window -t${TMUX_PANE} $*
      else
        printf "\ek$*\e\\"
      fi
      ;;
    (alacritty|xterm|cygwin)*)
      printf "\033]0;$*\007"
      ;;
  esac
}

function rt {
    mkdir -p "$HOME/share/terms"
    terminalizer record -k "$HOME/share/terms/$1"
}

function rtp {
    terminalizer play "$HOME/share/terms/$1"
}

function rtr {
    terminalizer render "$HOME/share/terms/$1" -o "$HOME/share/terms/$1.gif" && open "$HOME/share/terms"
}
