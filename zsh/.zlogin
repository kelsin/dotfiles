#
# Executes commands at login post-zshrc.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

if [[ -z "$TMUX" && -z "$EMACS" && -z "$VIM" ]]; then
  while true; do
    tmux new-session -A -s base || break
  done
else
  # Execute code that does not affect the current session in the background.
  {
    # Compile the completion dump to increase startup speed.
    zcompdump="${ZDOTDIR:-$HOME}/.zcompdump"
    if [[ -s "$zcompdump" && (! -s "${zcompdump}.zwc" || "$zcompdump" -nt "${zcompdump}.zwc") ]]; then
      zcompile "$zcompdump"
    fi
  } &!

  if (( $+commands[figlet] && $+commands[lolcat] )); then
    toilet -f future $USER | lolcat
  fi
fi
