#!/usr/bin/env zsh
PATH="$HOME/.nodenv/bin:$PATH"

if (( $+commands[nodenv] )) ; then
  eval "$(nodenv init -)"
fi
