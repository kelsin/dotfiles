if [[ $+commands[figlet] && $+commands[lolcat] && -z ${TMUX} ]]; then
  figlet $USER | lolcat -t -p 2.0 -F 0.2
fi
