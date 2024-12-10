# .bashrc

# Source main environment
. ~/.env.sh

# If not running interactively, return
[ -z "$PS1" ] && return

# need the host name set sometimes
[ -z "$HOSTNAME" ] && export HOSTNAME=$(hostname)

# Path setup for bash
export PATH="$HOME/bin:/usr/local/sbin:/usr/local/bin:$PATH"

# no duplicates in history
export HISTCONTROL=ignoredups
export HISTCONTROL=ignoreboth

# Check window size after commands
shopt -s checkwinsize

# Let Ctrl-O work ing terminals
if [ -f /Applications ]; then
  stty discard undef
fi

# Source global definitions
if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi

if [ -f /etc/bash.bashrc ]; then
  . /etc/bash.bashrc
fi

# Functions / Aliases
[ -f ~/.functions.sh ] && . ~/.functions.sh
[ -f ~/.aliases.sh ] && . ~/.aliases.sh

# Prompt
function __kelsin_prompt {
  local EXIT="$?"
  local RED="\[\033[0;31m\]"
  local GREEN="\[\033[0;32m\]"
  local YELLOW="\[\033[0;33m\]"
  local BLUE="\[\033[0;34m\]"
  local PURPLE="\[\033[0;35m\]"
  local CYAN="\[\033[0;36m\]"

  local RESET="\[\033[0m\]"

  local USERNAME=""

  [[ "$SSH_CONNECTION" != '' ]] && USERNAME="${GREEN}\u${RESET}@${YELLOW}\h${RESET} "
  [[ $UID -eq 0 ]] && USERNAME="${RED}\u${RESET}@${YELLOW}\h${RESET} "

  local COLOR=$PURPLE
  if [ $EXIT != 0 ]; then
    COLOR=$RED
  fi

  export PS1="${BLUE}\w${RESET}\n${USERNAME}${COLOR}❯${RESET} "
  export PS2="${CYAN}❯${RESET} "
}

export PROMPT_COMMAND=__kelsin_prompt

# chruby
if which brew > /dev/null; then
  . `brew --prefix`/share/chruby/chruby.sh
  . `brew --prefix`/share/chruby/auto.sh
fi

# NVM
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
