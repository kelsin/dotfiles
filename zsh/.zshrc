ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
[ ! -d $ZINIT_HOME ] && mkdir -p "$(dirname $ZINIT_HOME)"
[ ! -d $ZINIT_HOME/.git ] && git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
source "${ZINIT_HOME}/zinit.zsh"

# Keybindings
bindkey -e

# Plugins
zinit light zdharma-continuum/fast-syntax-highlighting

# Bash style word jumping
autoload -U select-word-style
select-word-style bash
WORDCHARS=${WORDCHARS//[-_]}

# Functions and Aliases
[ -f ~/.functions.sh ] && . ~/.functions.sh
[ -f ~/.aliases.sh ] && . ~/.aliases.sh

# Load my local plugins
for script in ~/.zsh/**/*.zsh; do
  . $script
done

# Work
[ -f ~/.zshrc.$WORK.zsh ] && . ~/.zshrc.$WORK.zsh

# Starship
if (($+commands[starship])); then
  eval "$(starship init zsh)"
fi
