#!/usr/bin/env zsh
export PYENV_VIRTUALENV_DISABLE_PROMPT=1

PATH="$HOME/.pyenv/bin:$PATH"

if (( $+commands[pyenv] )) ; then
  eval "$(pyenv init --path)"
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi
