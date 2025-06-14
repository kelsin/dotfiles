#!/usr/bin/env zsh
dir_symbol=`echo -e '\uf413'`
git_symbol=`echo -e '\ue702'`
node_symbol=`echo -e '\ued0d'`
ruby_symbol=`echo -e '\ue791'`
python_symbol=`echo -e '\ue73c'`
lambda_symbol=`echo -e '\u03bb'`
k8s_symbol=`echo -e '\ue81d'`

# Run simple prompt in VSCODE
if [[ "$TERM_PROGRAM" == "vscode" ]]; then
  SIMPLE_PROMPT=1
fi

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
[[ -z $SIMPLE_PROMPT ]] && precmd_functions+=(git_arrows)

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn
zstyle ':vcs_info:git*' formats "%b"
zstyle ':vcs_info:git*' actionformats "%b|%a"
[[ -z $SIMPLE_PROMPT ]] && precmd_functions+=(vcs_info)

git_branch() {
  prompt_git_branch=

  if git rev-parse --is-inside-work-tree &>/dev/null; then
    if git diff-index --cached --quiet HEAD &>/dev/null; then
      if git diff --no-ext-diff --quiet --exit-code &>/dev/null; then
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
[[ -z $SIMPLE_PROMPT ]] && precmd_functions+=(git_branch)

[[ "$SSH_CONNECTION" != '' ]] && prompt_username='%F{green}%n%f@%F{yellow}%m%f '
[[ $UID -eq 0 ]] && prompt_username='%F{red}%n%f '

prompt_status() {
  print -P "%F{blue}%~%f\$prompt_git_branch"
}
[[ -z $SIMPLE_PROMPT ]] && precmd_functions+=(prompt_status)

typeset -A k8s_context_names
k8s_context() {
     local context=$(kubectl config current-context 2>/dev/null)
     echo ${k8s_context_names[${context}]-${context}}
}

k8s_namespace() {
     kubectl config view --minify --output 'jsonpath={..namespace}' 2>/dev/null
}

k8s_prompt() {
  local context="${$(k8s_context):-none}"
  [[ "$context" != 'none' ]] && echo " %F{#326ce5}$k8s_symbol%f ${context}:${$(k8s_namespace):-default}"
}

python_prompt() {
  local conda_name="${CONDA_DEFAULT_ENV:-base}"
  if [[ "$conda_name" != 'base' ]]; then
    echo " %F{#4584b6}$python_symbol%f conda:${conda_name}"
    return
  fi

  if [[ $+commands["pyenv"] ]] && [[ -n "${PYENV_VIRTUAL_ENV}" ]]; then
    local pyenv_name="${$(pyenv version-name):-system}"
    if [[ "$pyenv_name" != 'system' ]]; then
        echo " %F{#4584b6}$python_symbol%f ${pyenv_name}"
        return
    fi
  fi

  local venv_name="${$(basename "${VIRTUAL_ENV}"):-system}"
  if [[ "$venv_name" != 'system' ]]; then
    echo " %F{#4584b6}$python_symbol%f venv:${venv_name}"
    return
  fi

  if [[ $+commands["pyenv"] ]]; then
    local pyenv_name="${$(pyenv version-name):-system}"
    [[ "$pyenv_name" != 'system' ]] && echo " %F{#4584b6}$python_symbol%f ${pyenv_name}"
  fi
}

ruby_prompt() {
  local rbenv_name="${$(rbenv version-name):-system}"
  [[ "$rbenv_name" != 'system' ]] && echo " %F{#c52e28}$ruby_symbol%f ${rbenv_name}"
}

node_prompt() {
  local nodenv_name="${$(nodenv version-name):-system}"
  [[ "$nodenv_name" != 'system' ]] && echo " %F{#339933}$node_symbol%f ${nodenv_name}"
}

direnv_prompt() {
  [[ -n ${DIRENV_FILE} ]] && echo " %F{red}$dir_symbol%f"
}

setopt prompt_subst
export PROMPT="$prompt_username%(?.%F{magenta}.%F{red})${lambda_symbol}%f "
export PROMPT2="%F{cyan}%_❯%f "
export PROMPT3="%F{cyan}?❯%f "
export PROMPT4="%F{red}+%N:%i❯%f "
export PROMPT_EOL_MARK="%F{red}↵%f"
if [[ -z $SIMPLE_PROMPT ]]; then
    # export RPROMPT="\${\$(direnv_prompt)}\${\$(python_prompt)}\${\$(ruby_prompt)}\${\$(node_prompt)}\${\$(k8s_prompt)}"
    export RPROMPT="\${\$(direnv_prompt)}\${\$(python_prompt)}\${\$(ruby_prompt)}\${\$(node_prompt)}"
else
    unset RPROMPT
fi
