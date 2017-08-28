if (( $+commands[figlet] && $+commands[lolcat] )); then
  figlet $USER | lolcat
fi
