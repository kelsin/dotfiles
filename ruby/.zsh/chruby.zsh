#!/usr/bin/env zsh

# Load chruby from homebrew
. `brew --prefix`/share/chruby/chruby.sh

# Customized Auto Script
unset RUBY_AUTO_VERSION

function chruby_auto() {
  local dir="$PWD/" version

  until [[ -z "$dir" ]]; do
    dir="${dir%/*}"

    if { read -r version <"$dir/.ruby-version"; } 2>/dev/null || [[ -n "$version" ]]; then
      if [[ "$version" == "$RUBY_AUTO_VERSION" ]]; then return
      else
        RUBY_AUTO_VERSION="$version"
        chruby "$version"
        return $?
      fi
    fi
  done

  if [[ -n "$RUBY_AUTO_VERSION" ]]; then
    unset RUBY_AUTO_VERSION
  fi
}

if [[ ! "$chpwd_functions" == *chruby_auto* ]]; then
  chpwd_functions+=("chruby_auto")
fi

# Run it once for this shell
[ -f ~/.ruby-version ] && chruby `cat ~/.ruby-version`
