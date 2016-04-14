# ZPlug Setup
# https://github.com/b4b4r07/zplug
[ -d ~/.zplug ] || git clone https://github.com/b4b4r07/zplug ~/.zplug
. ~/.zplug/zplug

# Functions and Aliases
[ -f ~/.functions ] && . ~/.functions
[ -f ~/.aliases ] && . ~/.aliases

# Emacs Keybindings
bindkey -e

# Bash style word jumping
autoload -U select-word-style
select-word-style bash

# Environment
zplug "sorin-ionescu/prezto", of:"modules/environment/init.zsh"

# Dircolors
[ $+commands[gdircolors] ] && eval $(gdircolors ~/.dircolors)

# Lesspipe
[ $+commands[lesspipe.sh] ] && eval $(lesspipe.sh)

# History
zplug "sorin-ionescu/prezto", of:"modules/history/init.zsh"

# Directory
zplug "sorin-ionescu/prezto", of:"modules/directory/init.zsh"

# Completion
fpath=("`brew --prefix`/share/zsh/site-functions" $fpath)
zplug "sorin-ionescu/prezto", of:"modules/completion/init.zsh"
zplug "tmuxinator/tmuxinator", of:"completion/tmuxinator.zsh"

# Load my local plugins
for script in ~/.zsh/**/*.zsh; do
  . $script
done

# Load Blizzard plugins
if [ -d ~/blizzard/src/configs ]; then
  for script in ~/blizzard/src/configs/**/*.zsh; do
    . $script
  done
fi

# Highlighting
zplug "zsh-users/zsh-syntax-highlighting", nice:10

# Install plugins if there are plugins that have not been installed
zplug check || zplug install

# Load Plugins
zplug load
