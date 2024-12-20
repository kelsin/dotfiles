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
[ -f ~/.functions.sh ] && . ~/.functions.sh
[ -f ~/.aliases.sh ] && . ~/.aliases.sh

# Load my local plugins
for script in ~/.zsh/**/*.zsh; do
  . $script
done

# gcloud
if [ -d /usr/local/Caskroom/google-cloud-sdk ]; then
  source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc'
  source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc'
fi

# Work
[ -f $HOME/.zshrc.$WORK.zsh ] && source $HOME/.zshrc.$WORK.zsh

# direnv
if (($+commands[direnv])); then
  eval "$(direnv hook zsh)"
fi
