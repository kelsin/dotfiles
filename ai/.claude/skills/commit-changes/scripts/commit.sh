#!/usr/bin/env bash
# commit.sh — mutating half of the commit-changes skill's Step 2 (Branching)
# and Step 3 (Stage and commit). Takes the branch name and commit message as
# inputs, since only the agent can produce those from reading the diff —
# this script does not write prose or infer anything from content, it just
# performs the mechanical branch-create/stage/commit sequence in one call.
#
# Usage:
#   commit.sh [--branch <name>] [--exclude <path>]...   (message on stdin)
#
#   --branch <name>   Create and switch to this branch before committing.
#                      Only pass this when preflight.sh reported ON_DEFAULT=1
#                      — the skill's rule is: commit directly on a non-default
#                      branch, only branch off when currently on default.
#   --exclude <path>  A path to leave out of staging (repeatable). Use this
#                      for anything preflight.sh's secret scan flagged that
#                      the user wants excluded rather than committed.
#
# The commit message is read from stdin so multi-line bodies and special
# characters survive intact (same reasoning as the skill's HEREDOC usage).
#
# Exit codes / STATUS values:
#   STATUS=OK           committed successfully — branch and commit SHA printed
#   STATUS=HOOK_FAILED   commit rejected by a pre-commit/commit-msg hook
#   STATUS=SIGN_TIMEOUT  commit hung for 120s (signing approval or slow hook)
#   STATUS=ERROR         unexpected failure — message printed

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

die() {
  echo "STATUS=ERROR"
  echo "MESSAGE=$1"
  exit 1
}

BRANCH=""
EXCLUDES=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --branch) BRANCH="$2"; shift 2 ;;
    --exclude) EXCLUDES+=("$2"); shift 2 ;;
    *) die "unknown argument: $1" ;;
  esac
done

command -v git >/dev/null || die "git not found"
git rev-parse --is-inside-work-tree >/dev/null 2>&1 || die "not inside a git repository"

[[ -t 0 ]] && die "commit message must be piped on stdin"
MESSAGE="$(cat)"
[[ -z "$MESSAGE" ]] && die "no commit message provided on stdin"

if [[ -n "$BRANCH" ]]; then
  git checkout -b "$BRANCH" || die "failed to create/switch to branch $BRANCH"
fi

git add -A || die "failed to stage changes"

for path in "${EXCLUDES[@]:-}"; do
  [[ -n "$path" ]] && git restore --staged -- "$path" 2>/dev/null
done

ERRFILE="$(mktemp -t commit-branch-err)"
trap 'rm -f "$ERRFILE"' EXIT

to 120 git commit -m "$MESSAGE" </dev/null >"$ERRFILE" 2>&1
COMMIT_RC=$?
if [[ $COMMIT_RC -ne 0 ]]; then
  OUTPUT="$(cat "$ERRFILE")"
  if [[ $COMMIT_RC -eq 124 ]]; then
    echo "STATUS=SIGN_TIMEOUT"
    echo "MESSAGE=git commit timed out after 120s — likely waiting on a 1Password signing approval (unlock/approve it), or a slow pre-commit hook"
    echo "--- git output ---"
    echo "$OUTPUT"
    exit 5
  fi
  echo "STATUS=HOOK_FAILED"
  echo "--- git output ---"
  echo "$OUTPUT"
  exit 3
fi

echo "STATUS=OK"
echo "BRANCH=$(git rev-parse --abbrev-ref HEAD)"
echo "COMMIT=$(git rev-parse --short HEAD)"
