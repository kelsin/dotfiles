#!/usr/bin/env zsh
export VIRTUAL_ENV_DISABLE_PROMPT=1
export PYENV_VIRTUALENV_DISABLE_PROMPT=1
export PYENV_ROOT="$HOME/.pyenv"
PATH="$HOME/.pyenv/bin:$PATH"

if [ ! -d "$HOME/.pyenv" ]; then
    # Install pyenv
    git clone https://github.com/pyenv/pyenv.git $HOME/.pyenv
    pushd $HOME/.pyenv
    src/configure
    make -C src
    popd

    # Install pyenv-virtualenv
    mkdir -p "$(pyenv root)"/plugins
    git clone https://github.com/pyenv/pyenv-virtualenv.git $(pyenv root)/plugins/pyenv-virtualenv

    # Install default python
    pyenv install `cat $HOME/.python-version`
fi

if (( $+commands[pyenv] )) ; then
  eval "$(pyenv init --path)"
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi
