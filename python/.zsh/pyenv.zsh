#!/usr/bin/env zsh

if (( $+commands[pyenv] )) ; then
  eval "$(pyenv init --path)"
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi
