#!/usr/bin/env zsh

git_symbol=`echo -e '\ue0a0'`
node_symbol=`echo -e '\u2b22'`
ruby_symbol=`echo -e '\u2756'`
lambda_symbol=`echo -e '\u03bb'`

git_arrows() {
  prompt_git_arrows=
  command git rev-parse --abbrev-ref @'{u}' &>/dev/null || return

  local arrow_status
  # check git left and right arrow_status
  arrow_status="$(command git rev-list --left-right --count HEAD...@'{u}' 2>/dev/null)"
  # exit if the command failed
  (( !$? )) || return

  # left and right are tab-separated, split on tab and store as array
  arrow_status=(${(ps:\t:)arrow_status})
  local arrows left=${arrow_status[1]} right=${arrow_status[2]}

  (( ${right:-0} > 0 )) && arrows+="%F{green}⇣%f"
  (( ${left:-0} > 0 )) && arrows+="%F{yellow}⇡%f"

  [[ -n $arrows ]] && prompt_git_arrows="${arrows}"
}
precmd_functions+=(git_arrows)

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn
zstyle ':vcs_info:git*' formats "%b"
zstyle ':vcs_info:git*' actionformats "%b|%a"
precmd_functions+=(vcs_info)

git_branch() {
  prompt_git_branch=

  if git rev-parse --is-inside-work-tree &>/dev/null; then
    if git diff-index --cached --quiet HEAD; then
      if git diff --no-ext-diff --quiet --exit-code; then
        if [ -n "$(git ls-files --others --exclude-standard)" ]; then
          branch_symbol="%F{yellow}`echo -e '\u2605'`%f"
        else
          branch_symbol="%F{green}`echo -e '\u2713'`%f"
        fi
      else
        branch_symbol="%F{blue}`echo -e '\u271a'`%f"
      fi
    else
      branch_symbol="%F{red}`echo -e '\u2731'`%f"
    fi

    prompt_git_branch=" $vcs_info_msg_0_%f $branch_symbol $prompt_git_arrows "
  fi
}
precmd_functions+=(git_branch)

[[ "$SSH_CONNECTION" != '' ]] && prompt_username='%F{green}%n%f@%F{yellow}%m%f '
[[ $UID -eq 0 ]] && prompt_username='%F{red}%n%f '

prompt_status() {
  print -P "%F{blue}%~%f\$prompt_git_branch"
}
precmd_functions+=(prompt_status)

setopt prompt_subst
export PROMPT="$prompt_username%(?.%F{magenta}.%F{red})${lambda_symbol}%f "
export PROMPT2="%F{cyan}%_❯%f "
export PROMPT3="%F{cyan}?❯%f "
export PROMPT4="%F{red}+%N:%i❯%f "
export RPROMPT="%F{red}$ruby_symbol%f \${RUBY_VERSION:-system} %F{green}$node_symbol%f \${\${NVM_BIN:-/a/system/a}:h:t:s/v/} %F{blue}$git_symbol%f \${vcs_info_msg_0_:-none}"
export PROMPT_EOL_MARK="%F{red}↵%f"
