#!/usr/bin/env bash
# rebase-branch.sh — runs the mechanical, judgment-free steps of the
# rebase-branch skill (pre-flight, pull, rebase, stash-reapply) in one shot,
# and stops with a clear STATUS line the moment something needs agent
# judgment (a real conflict, a diverged branch, etc). This collapses the
# ~8-10 separate git tool calls the agent would otherwise make one-by-one
# into a single call for the common conflict-free case, and leaves the repo
# in a well-defined state for the agent to take over from when it can't.
#
# IMPORTANT ordering note: rebase happens BEFORE the stash is popped, not
# after. `git rebase` refuses to even start if the working tree has any
# uncommitted changes — staged or not, conflicting or not. Popping the
# stash first would make every stashed run fail on that hard refusal
# instead of on an actual conflict.
#
# Exit codes / STATUS values (printed on stdout, last line/block):
#   STATUS=OK                  clean rebase (+ stash reapply, if any) completed
#   STATUS=NOOP_DEFAULT        current branch IS the default branch
#   STATUS=DIVERGED_CURRENT    current branch has diverged from its remote
#   STATUS=REBASE_CONFLICT     rebase stopped on a conflict — resolve manually
#   STATUS=STASH_CONFLICT      post-rebase stash pop conflicted — resolve manually
#   STATUS=ERROR               unexpected git failure — message printed
#
# Nothing here guesses at conflict resolution. Textual/semantic conflicts
# are always left for the agent (or user) to resolve by hand.

set -uo pipefail

ERRFILE="$(mktemp -t rebase-branch-err)"
trap 'rm -f "$ERRFILE"' EXIT

die() {
  echo "STATUS=ERROR"
  echo "MESSAGE=$1"
  exit 1
}

require_clean_exit() {
  if ! "$@" >/dev/null 2>"$ERRFILE"; then
    local err
    err=$(cat "$ERRFILE")
    die "command failed: $* :: $err"
  fi
}

conflicting_files() {
  git status --porcelain | awk '$1 ~ /U/ || $1 == "AA" || $1 == "DD" {print $2}'
}

command -v git >/dev/null || die "git not found"
git rev-parse --is-inside-work-tree >/dev/null 2>&1 || die "not inside a git repository"

STASHED=0

# --- Step 1: Pre-flight ---

if [[ -n "$(git status --porcelain)" ]]; then
  git stash push -u -m "rebase-branch: autostash" >/dev/null 2>&1 \
    || die "failed to stash local changes"
  STASHED=1
fi

DEFAULT_BRANCH="$(git remote show origin 2>/dev/null | sed -n 's/.*HEAD branch: //p')"
if [[ -z "$DEFAULT_BRANCH" ]]; then
  DEFAULT_BRANCH="$(gh repo view --json defaultBranchRef -q .defaultBranchRef.name 2>/dev/null || true)"
fi
if [[ -z "$DEFAULT_BRANCH" ]]; then
  [[ $STASHED -eq 1 ]] && git stash pop >/dev/null 2>&1
  die "could not determine default branch"
fi

CURRENT_BRANCH="$(git rev-parse --abbrev-ref HEAD)"
if [[ "$CURRENT_BRANCH" == "$DEFAULT_BRANCH" ]]; then
  [[ $STASHED -eq 1 ]] && git stash pop >/dev/null 2>&1
  echo "STATUS=NOOP_DEFAULT"
  echo "DEFAULT_BRANCH=$DEFAULT_BRANCH"
  exit 0
fi

# --- Step 2: Pull latest into both branches ---

git fetch origin "$DEFAULT_BRANCH" "$CURRENT_BRANCH" >/dev/null 2>&1

if ! git fetch origin "$DEFAULT_BRANCH:$DEFAULT_BRANCH" >/dev/null 2>&1; then
  # Local default diverged from remote — it should always mirror remote, so force it.
  require_clean_exit git fetch origin "+$DEFAULT_BRANCH:$DEFAULT_BRANCH"
fi

if git show-ref --verify --quiet "refs/remotes/origin/$CURRENT_BRANCH"; then
  if ! git merge --ff-only "origin/$CURRENT_BRANCH" >/dev/null 2>&1; then
    # ff-only failed for one of two reasons:
    #   (a) genuine divergence — someone else pushed new commits to the branch, or
    #   (b) the branch was already rebased locally and not yet re-pushed, so the
    #       remote just holds stale copies of our own pre-rebase commits.
    # Any remote commit NOT already patch-present on HEAD means (a); a clean
    # result here means (b), which is safe to proceed past (the later
    # force-push overwrites the stale remote copies).
    NOVEL="$(git log --cherry-pick --right-only --no-merges --oneline \
      "HEAD...origin/$CURRENT_BRANCH" 2>/dev/null)"
    if [[ -n "$NOVEL" ]]; then
      [[ $STASHED -eq 1 ]] && git stash pop >/dev/null 2>&1
      echo "STATUS=DIVERGED_CURRENT"
      echo "DEFAULT_BRANCH=$DEFAULT_BRANCH"
      echo "CURRENT_BRANCH=$CURRENT_BRANCH"
      echo "STASHED=$STASHED"
      echo "--- remote-only commits not present locally ---"
      echo "$NOVEL"
      exit 4
    fi
  fi
fi

# --- Step 3: Rebase (must happen before the stash is reapplied) ---

if ! git rebase "$DEFAULT_BRANCH" >/dev/null 2>"$ERRFILE"; then
  echo "STATUS=REBASE_CONFLICT"
  echo "DEFAULT_BRANCH=$DEFAULT_BRANCH"
  echo "CURRENT_BRANCH=$CURRENT_BRANCH"
  echo "STASHED=$STASHED"
  echo "--- conflicting files ---"
  conflicting_files
  echo "--- git status ---"
  git status | sed -n '/rebase in progress/,/^$/p'
  exit 6
fi

# --- Step 4: Reapply stashed changes ---

if [[ $STASHED -eq 1 ]]; then
  if ! git stash pop >/dev/null 2>"$ERRFILE"; then
    echo "STATUS=STASH_CONFLICT"
    echo "DEFAULT_BRANCH=$DEFAULT_BRANCH"
    echo "CURRENT_BRANCH=$CURRENT_BRANCH"
    echo "--- conflicting files ---"
    conflicting_files
    exit 5
  fi
fi

# --- Step 5: Report ---

echo "STATUS=OK"
echo "DEFAULT_BRANCH=$DEFAULT_BRANCH"
echo "CURRENT_BRANCH=$CURRENT_BRANCH"
echo "--- rebased commits ---"
git log "$DEFAULT_BRANCH..HEAD" --oneline
