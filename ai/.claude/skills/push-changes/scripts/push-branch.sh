#!/usr/bin/env bash
# push-branch.sh — runs the mechanical part of the push-changes skill
# (default-branch guard, push, error classification) in one call instead of
# 2-3 separate git commands with hand-parsed stderr. Judgment calls that
# require conversation context — whether this push follows an intentional
# history rewrite, whether the user explicitly asked to push to the default
# branch — are NOT made by this script. They're passed in as flags by the
# agent, which is the only one that knows them.
#
# Usage:
#   push-branch.sh [--force] [--allow-default]
#
#   --force          Force-push (git push --force-with-lease). Only pass this
#                     after the skill's Step 1.2 judgment (confirm with the
#                     user when run standalone; ship-it skips the prompt).
#   --allow-default  Allow pushing when the current branch IS the repo's
#                     default branch. Only pass this if the user explicitly
#                     asked for that; otherwise the script stops rather than
#                     pushing to default by default.
#
# Exit codes / STATUS values (printed on stdout, last line/block):
#   STATUS=OK             push succeeded
#   STATUS=ON_DEFAULT      current branch is the default branch and
#                          --allow-default wasn't passed — ask the user
#   STATUS=REJECTED        push was rejected (non-force): likely someone
#                          else pushed to this branch — investigate, don't
#                          escalate to force automatically
#   STATUS=ERROR           unexpected git failure — message printed
#
# Nothing here decides *when* to force-push or push to default — those
# judgment calls stay with the agent/user, per the skill's rules.

set -uo pipefail

# Non-interactive guards: never wait on a prompt, editor, or pager.
export GIT_TERMINAL_PROMPT=0 GIT_EDITOR=true GIT_SEQUENCE_EDITOR=true GIT_PAGER=cat PAGER=cat
export GH_PROMPT_DISABLED=1 GH_PAGER=cat GH_NO_UPDATE_NOTIFIER=1 GH_SPINNER_DISABLED=1
export GIT_SSH_COMMAND="ssh -o BatchMode=yes -o ConnectTimeout=15"

# to <seconds> <cmd...> — bounded run; exit status 124 means it timed out.
to() {
  local secs="$1"; shift
  if command -v timeout >/dev/null 2>&1; then timeout "$secs" "$@"
  elif command -v gtimeout >/dev/null 2>&1; then gtimeout "$secs" "$@"
  else "$@"; fi
}

# Local lookup first; `git remote show` hits the network, so it's only a fallback.
default_branch() {
  local b
  b="$(git symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null | sed 's#^origin/##')"
  [[ -z "$b" ]] && b="$(to 20 git remote show origin 2>/dev/null </dev/null | sed -n 's/.*HEAD branch: //p')"
  [[ -z "$b" ]] && b="$(to 20 gh repo view --json defaultBranchRef -q .defaultBranchRef.name 2>/dev/null </dev/null || true)"
  printf '%s' "$b"
}

FORCE=0
ALLOW_DEFAULT=0
for arg in "$@"; do
  case "$arg" in
    --force) FORCE=1 ;;
    --allow-default) ALLOW_DEFAULT=1 ;;
    *) echo "STATUS=ERROR"; echo "MESSAGE=unknown argument: $arg"; exit 1 ;;
  esac
done

ERRFILE="$(mktemp -t push-branch-err)"
trap 'rm -f "$ERRFILE"' EXIT

die() {
  echo "STATUS=ERROR"
  echo "MESSAGE=$1"
  exit 1
}

command -v git >/dev/null || die "git not found"
git rev-parse --is-inside-work-tree >/dev/null 2>&1 || die "not inside a git repository"

DEFAULT_BRANCH="$(default_branch)"
[[ -z "$DEFAULT_BRANCH" ]] && die "could not determine default branch"

CURRENT_BRANCH="$(git rev-parse --abbrev-ref HEAD)"

if [[ "$CURRENT_BRANCH" == "$DEFAULT_BRANCH" && $ALLOW_DEFAULT -eq 0 ]]; then
  echo "STATUS=ON_DEFAULT"
  echo "DEFAULT_BRANCH=$DEFAULT_BRANCH"
  exit 3
fi

PUSH_ARGS=(-u origin "$CURRENT_BRANCH")
[[ $FORCE -eq 1 ]] && PUSH_ARGS=(--force-with-lease "${PUSH_ARGS[@]}")

to 180 git push "${PUSH_ARGS[@]}" </dev/null >"$ERRFILE" 2>&1
PUSH_RC=$?
if [[ $PUSH_RC -ne 0 ]]; then
  OUTPUT="$(cat "$ERRFILE")"
  if [[ $PUSH_RC -eq 124 ]]; then
    echo "STATUS=ERROR"
    echo "MESSAGE=push timed out after 180s — pre-push hook, SSH approval, or network"
    echo "--- git output ---"
    echo "$OUTPUT"
    exit 1
  fi
  if [[ $FORCE -eq 0 ]] && grep -qi "rejected\|non-fast-forward\|fetch first" <<<"$OUTPUT"; then
    echo "STATUS=REJECTED"
    echo "CURRENT_BRANCH=$CURRENT_BRANCH"
    echo "--- git output ---"
    echo "$OUTPUT"
    exit 4
  fi
  echo "STATUS=ERROR"
  echo "MESSAGE=push failed"
  echo "--- git output ---"
  echo "$OUTPUT"
  exit 1
fi

echo "STATUS=OK"
echo "DEFAULT_BRANCH=$DEFAULT_BRANCH"
echo "CURRENT_BRANCH=$CURRENT_BRANCH"
echo "FORCED=$FORCE"
