# Source main environment file
. ~/.env.sh

# Paths
# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

# Brew
eval "$(/opt/homebrew/bin/brew shellenv)"

# Set the list of directories that Zsh searches for programs.
path=($HOME/.local/bin $HOME/bin /usr/local/{bin,sbin} $path)
