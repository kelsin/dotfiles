export FPATH=/usr/local/Cellar/zsh/5.7.1/share/zsh/functions:$FPATH

# ZPlug Setup
# https://github.com/b4b4r07/zplug
[ -d ~/.zplug ] || git clone https://github.com/b4b4r07/zplug ~/.zplug
. ~/.zplug/init.zsh

# Emacs Keybindings
bindkey -e

# Bash style word jumping
autoload -U select-word-style
select-word-style bash

# Environment
zplug "sorin-ionescu/prezto", use:"modules/environment/init.zsh"

# History
zplug "sorin-ionescu/prezto", use:"modules/history/init.zsh"

# Directory
zplug "sorin-ionescu/prezto", use:"modules/directory/init.zsh"

# Completion
if (( $+commands[brew] )); then
	fpath=("`brew --prefix`/share/zsh/site-functions" $fpath)
fi

# zplug "plugins/kubectl", from:oh-my-zsh
zplug "sorin-ionescu/prezto", use:"modules/completion/init.zsh"
zplug "tmuxinator/tmuxinator", use:"completion/tmuxinator.zsh"

# Dircolors
(( $+commands[gdircolors] )) && eval $(gdircolors ~/.dircolors)

# Source highlighting in less
LESSPIPE=`which src-hilite-lesspipe.sh`
export LESSOPEN="| ${LESSPIPE} %s"
export LESS="-R -J -f -i -M -Q -S -X -F"

# Highlighting
zplug "zsh-users/zsh-syntax-highlighting", defer:2

# Install plugins if there are plugins that have not been installed
zplug check || zplug install

# Load Plugins
zplug load

# Functions and Aliases
[ -f ~/.functions ] && . ~/.functions
[ -f ~/.aliases ] && . ~/.aliases

# Load my local plugins
for script in ~/.zsh/**/*.zsh; do
  . $script
done

# gcloud
if [ -d /usr/local/Caskroom/google-cloud-sdk ]; then
  source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc'
  source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc'
fi

# direnv
if (($+commands[direnv])); then
  eval "$(direnv hook zsh)"
fi

# airbnb
[ -f $HOME/.zshrc.airbnb ] && source $HOME/.zshrc.airbnb

# Don't close shell on Ctrl-D
set -o ignoreeof

# PATH
export PATH="$HOME/src/cc65/bin:$HOME/.emacs.d/bin:$HOME/src/go/bin:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/homebrew/Caskroom/miniconda/base/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh" ]; then
        . "/opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh"
    else
        export PATH="/opt/homebrew/Caskroom/miniconda/base/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<


# >>> mamba initialize >>>
# !! Contents within this block are managed by 'mamba init' !!
export MAMBA_EXE="/opt/homebrew/bin/micromamba";
export MAMBA_ROOT_PREFIX="/Users/chris_giroir/micromamba";
__mamba_setup="$("$MAMBA_EXE" shell hook --shell zsh --prefix "$MAMBA_ROOT_PREFIX" 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__mamba_setup"
else
    if [ -f "/Users/chris_giroir/micromamba/etc/profile.d/micromamba.sh" ]; then
        . "/Users/chris_giroir/micromamba/etc/profile.d/micromamba.sh"
    else
        export  PATH="/Users/chris_giroir/micromamba/bin:$PATH"  # extra space after export prevents interference from conda init
    fi
fi
unset __mamba_setup
# <<< mamba initialize <<<
