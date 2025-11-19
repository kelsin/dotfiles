#!/usr/bin/env zsh

if [ -d "$HOME/.jenv/bin" ]; then
  export PATH="$HOME/.jenv/bin:$PATH"
fi

if (( $+commands[jenv] )) ; then
  eval "$(jenv init -)"
fi
