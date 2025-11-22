#!/usr/bin/env zsh

if [ ! -d $HOME/.rustup ]; then
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- --no-modify-path -y --profile default
fi

PATH="$HOME/.cargo/bin:$PATH"
. "$HOME/.cargo/env"
