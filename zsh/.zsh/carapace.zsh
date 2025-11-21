if (($+commands[carapace])); then
  export CARAPACE_BRIDGES='zsh,bash'
  zstyle ':completion:*' format $'\e[2;37mCompleting %d\e[m'
  source <(carapace _carapace)
fi
