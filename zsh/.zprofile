# Source main environment file
. ~/.env

# Paths
# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

# Set the the list of directories that cd searches.
cdpath=($HOME/src/improbable $HOME/src $HOME $cdpath)

# Set the list of directories that Zsh searches for programs.
path=($HOME/bin /usr/local/{bin,sbin} $path)
