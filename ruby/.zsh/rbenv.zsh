#!/usr/bin/env zsh
PATH="$HOME/.rbenv/bin:$PATH"

if [ ! -d "$HOME/.rbenv" ]; then
    # Install rbenv
    git clone https://github.com/rbenv/rbenv.git $HOME/.rbenv
    pushd $HOME/.rbenv
    src/configure
    make -C src
    popd

    # Install ruby-build
    mkdir -p "$(rbenv root)"/plugins
    git clone https://github.com/rbenv/ruby-build.git "$(rbenv root)"/plugins/ruby-build

    # Install default ruby
    rbenv install `cat $HOME/.ruby-version`
fi

if (( $+commands[rbenv] )) ; then
  eval "$(rbenv init -)"
fi
