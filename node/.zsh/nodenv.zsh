#!/usr/bin/env zsh

if (( $+commands[nodenv] )) ; then
  eval "$(nodenv init -)"
fi
