# Stow and Dotfiles directories
export STOW_DIR=$HOME/.dotfiles
export DOTFILES=$STOW_DIR

# Setup DISPLAY variable if one isn't already set
export DISPLAY=${DISPLAY-:0.0}

export XDG_CONFIG_HOME="${HOME}/.config"

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
export LESS="-R -J -f -i -M -Q -S -X -F"

# Directory Colors
export CLICOLOR=true
export LSCOLORS=ExfxcxdxBxegedabagacad

# Grep Colors
export GREP_COLOR='37;45'           # BSD.
export GREP_COLORS="mt=$GREP_COLOR" # GNU.

# Editors
export EDITOR='nvim'
export VISUAL='nvim'
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

# Bat
export BAT_THEME="Catppuccin Mocha"

# k9s
export K9S_CONFIG_DIR="${XDG_CONFIG_HOME}/k9s"

# Fzf
export FZF_DEFAULT_OPTS=" \
--color=bg+:#313244,bg:#1E1E2E,spinner:#F5E0DC,hl:#F38BA8 \
--color=fg:#CDD6F4,header:#F38BA8,info:#CBA6F7,pointer:#F5E0DC \
--color=marker:#B4BEFE,fg+:#CDD6F4,prompt:#CBA6F7,hl+:#F38BA8 \
--color=selected-bg:#45475A \
--color=border:#6C7086,label:#CDD6F4"

# Silent direnv
export DIRENV_LOG_FORMAT=

# Python
export PYTEST_ADDOPTS="-W ignore::Warning"

# Keyboards
export KBD_CORNE="crkbd/rev4_1/standard"
export KBD_PREONIC="preonic/rev3_drop"

# WORK
export WORK=tobiko
