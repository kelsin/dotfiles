#!/usr/bin/env zsh

if (( $+commands[rbenv] )) ; then
  eval "$(rbenv init -)"
fi
