#!/usr/bin/env zsh
PATH="$HOME/.rbenv/bin:$PATH"

if (( $+commands[rbenv] )) ; then
  eval "$(rbenv init -)"
fi
