#!/usr/bin/env zsh
PATH="$HOME/.nodenv/bin:$PATH"

if [ ! -d "$HOME/.nodenv" ]; then
    # Install nodenv
    git clone https://github.com/nodenv/nodenv.git $HOME/.nodenv
    pushd $HOME/.nodenv
    src/configure
    make -C src
    popd

    # Install node-build
    mkdir -p "$(nodenv root)"/plugins
    git clone https://github.com/nodenv/node-build.git "$(nodenv root)"/plugins/node-build

    # Install default node
    nodenv install `cat $HOME/.node-version`
fi

if (( $+commands[nodenv] )) ; then
  eval "$(nodenv init -)"
fi
