#!/usr/bin/env zsh

# Install nvm if needed
[ -d ~/.nvm ] || (\
  git clone https://github.com/creationix/nvm.git ~/.nvm && \
    cd ~/.nvm && \
    git checkout `git describe --abbrev=0 --tags` )

export NVM_DIR="$HOME/.nvm"
. $NVM_DIR/nvm.sh

# Customized Auto Script
unset NODE_AUTO_VERSION

function nvm_auto() {
  local dir="$PWD/" version

  until [[ -z "$dir" ]]; do
    dir="${dir%/*}"

    if { read -r version <"$dir/.node-version"; } 2>/dev/null || [[ -n "$version" ]]; then
      if [[ "$version" == "$NODE_AUTO_VERSION" ]]; then return
      else
        NODE_AUTO_VERSION="$version"
        nvm use "$version" > /dev/null
        return $?
      fi
    fi
  done

  if [[ -n "$NODE_AUTO_VERSION" ]]; then
    unset NODE_AUTO_VERSION
  fi
}

if [[ ! "$preexec_functions" == *nvm_auto* ]]; then
  chpwd_functions+=("nvm_auto")
fi

# Run it once for this shell
[ -f ~/.node-version ] && nvm use `cat ~/.node-version` > /dev/null
