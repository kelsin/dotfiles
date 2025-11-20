# Emacs Keybindings
bindkey -e

# Bash style word jumping
autoload -U select-word-style
select-word-style bash

# Completion
if (( $+commands[brew] )); then
	fpath=("`brew --prefix`/share/zsh/site-functions" $fpath)
fi

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
