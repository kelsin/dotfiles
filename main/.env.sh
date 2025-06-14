# Stow and Dotfiles directories
export STOW_DIR=$HOME/.dotfiles
export DOTFILES=$STOW_DIR

# Setup DISPLAY variable if one isn't already set
export DISPLAY=${DISPLAY-:0.0}

# Setup Emacs Shell
export ESHELL=/bin/bash

# Language
[[ -z "$LANG" ]] && export LANG='en_US.UTF-8'
[[ -z "$LC_ALL" ]] && export LC_ALL='en_US.UTF-8'

# Emacs Info Files
[ -d "$HOME/.info" ] && export INFOPATH="~/.info"

# Tex Inputs
[ -d "$HOME/.tex" ] && export TEXINPUTS="~/.tex//:"

# Browser
[[ "$OSTYPE" == darwin* ]] && export BROWSER='open'

# Less
export LESS="-R -J -f -i -M -Q -S"

# Directory Colors
export CLICOLOR=true
export LSCOLORS=ExfxcxdxBxegedabagacad

# Grep Colors
export GREP_COLOR='37;45'           # BSD.
export GREP_COLORS="mt=$GREP_COLOR" # GNU.

# Editors
export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'

# Debian packaging
export DEBFULLNAME="Christopher Giroir"
export DEBEMAIL="kelsin@valefor.com"

# Go
export GOPATH="$HOME/src/go"

# Java and Maven setup
export CATALINA_TMP=/tmp
export CATALINA_TMPDIR=/tmp

# k8s
export KUBECONFIG="$HOME/.kube/config"

# Dev Helpers
export PORT=3000

# Silent direnv
export DIRENV_LOG_FORMAT=

# Python
export PYTEST_ADDOPTS="-W ignore::Warning"

# WORK
export WORK=tobiko
