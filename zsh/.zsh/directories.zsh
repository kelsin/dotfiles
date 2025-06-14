#!/usr/bin/env zsh

# Directory Variables
typeset -A dir_names
dir_names+=($HOME home)
dir_names+=($HOME/src src)
dir_names+=($HOME/src/18xx 18xx.games)
dir_names+=($HOME/src/$WORK $WORK)
dir_names+=($HOME/src/18xx-maker 18xx-maker)
dir_names+=($HOME/src/18xx-maker/18xx-maker 18xx-maker/site)

# CD Path
cdpath=($HOME/src/$WORK $HOME/src $HOME $cdpath)

# Ability to name tabs as we CD around
project_folder() {
  local git_folder=$(git rev-parse --show-toplevel 2> /dev/null)
  local dir_name=${${dir_names[$PWD]}:-${dir_names[$git_folder]}}
  local name=${dir_name:-${${git_folder:t}:-Terminal}}

  set-title $name
}
# chpwd_functions+=("project_folder")
# project_folder
