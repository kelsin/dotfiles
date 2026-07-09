if (($+commands[zoxide])) && [[ -z "$CLAUDECODE" ]]; then
  eval "$(zoxide init zsh)"
fi
