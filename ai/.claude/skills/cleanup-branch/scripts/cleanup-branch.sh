#!/usr/bin/env bash
# cleanup-branch.sh — mutating half of the cleanup-branch skill. Takes the
# branch to delete and the default branch to land on (both already
# verified safe by branch-state.sh — this script does not re-check PR
# state) and performs the mechanical sequence: checkout default, delete
# the local branch, pull latest. Checkout must happen before delete since
# git refuses to delete the currently-checked-out branch.
#
# Usage:
#   cleanup-branch.sh --branch <name> --default <name> [--force]
#
#   --branch <name>    Local branch to delete. Must not be the current
#                       HEAD's default branch (branch-state.sh's ON_DEFAULT
#                       case already rules that out).
#   --default <name>   Default branch to check out and pull.
#   --force            Use `git branch -D` instead of `-d`. Only pass this
#                       after confirming with the user that discarding any
#                       commits not reachable from the default branch is
#                       fine (e.g. the PR was CLOSED without merging, or a
#                       squash merge means local commits are never
#                       fast-forward-ancestors of the default branch).
#
# Exit codes / STATUS values (printed on stdout, last line/block):
#   STATUS=OK          checked out default, deleted the branch, pulled
#   STATUS=UNMERGED    `git branch -d` refused (branch has commits not
#                       reachable from the default branch) and --force
#                       wasn't passed — ask the user before retrying with
#                       --force
#   STATUS=ERROR       unexpected failure — message printed
#
# Nothing here decides whether force-deleting is safe — that judgment call
# stays with the agent/user, per the skill's rules.

set -uo pipefail

die() {
  echo "STATUS=ERROR"
  echo "MESSAGE=$1"
  exit 1
}

BRANCH=""
DEFAULT_BRANCH=""
FORCE=0
while [[ $# -gt 0 ]]; do
  case "$1" in
    --branch) BRANCH="$2"; shift 2 ;;
    --default) DEFAULT_BRANCH="$2"; shift 2 ;;
    --force) FORCE=1; shift ;;
    *) die "unknown argument: $1" ;;
  esac
done

command -v git >/dev/null || die "git not found"
git rev-parse --is-inside-work-tree >/dev/null 2>&1 || die "not inside a git repository"
[[ -z "$BRANCH" ]] && die "--branch is required"
[[ -z "$DEFAULT_BRANCH" ]] && die "--default is required"
[[ "$BRANCH" == "$DEFAULT_BRANCH" ]] && die "refusing to delete the default branch"

ERRFILE="$(mktemp -t cleanup-branch-err)"
trap 'rm -f "$ERRFILE"' EXIT

git checkout "$DEFAULT_BRANCH" >"$ERRFILE" 2>&1 || die "checkout failed: $(cat "$ERRFILE")"

DELETE_FLAG="-d"
[[ $FORCE -eq 1 ]] && DELETE_FLAG="-D"

if ! git branch "$DELETE_FLAG" "$BRANCH" >"$ERRFILE" 2>&1; then
  OUTPUT="$(cat "$ERRFILE")"
  if [[ $FORCE -eq 0 ]] && grep -qi "not fully merged" <<<"$OUTPUT"; then
    echo "STATUS=UNMERGED"
    echo "BRANCH=$BRANCH"
    echo "--- git output ---"
    echo "$OUTPUT"
    exit 4
  fi
  die "branch delete failed: $OUTPUT"
fi

if ! git pull >"$ERRFILE" 2>&1; then
  echo "STATUS=ERROR"
  echo "MESSAGE=pull failed after branch delete"
  echo "--- git output ---"
  cat "$ERRFILE"
  exit 1
fi

echo "STATUS=OK"
echo "DELETED_BRANCH=$BRANCH"
echo "DEFAULT_BRANCH=$DEFAULT_BRANCH"
echo "FORCED=$FORCE"
