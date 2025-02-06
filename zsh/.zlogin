if [[ $+commands[figlet] && $+commands[lolcat] && -z ${TMUX} ]]; then
  figlet $USER | lolcat
fi
