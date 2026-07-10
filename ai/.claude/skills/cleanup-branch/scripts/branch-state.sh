#!/usr/bin/env bash
# branch-state.sh — read-only fact-gathering for the cleanup-branch skill's
# Step 1 (Check PR state). Determines the current branch, its associated
# PR (if any), and whether the working tree is clean, in one call instead
# of 3-4 separate git/gh commands. Makes no changes to the repo.
#
# Exit codes / STATUS values (printed on stdout, last line/block):
#   STATUS=ON_DEFAULT   current branch IS the repo's default branch —
#                        nothing to clean up
#   STATUS=DIRTY         working tree has uncommitted/untracked changes —
#                        resolve those first, don't silently stash work
#                        that's about to be orphaned by a branch deletion
#   STATUS=NO_PR         no PR found for the current branch — can't verify
#                        the precondition, ask the user how to proceed
#   STATUS=OPEN          a PR exists and is still OPEN — do not clean up
#   STATUS=DONE          a PR exists and is CLOSED or MERGED — safe to
#                        proceed to cleanup-branch.sh
#   STATUS=ERROR         unexpected failure — message printed
#
# Always printed on OPEN/DONE:
#   PR_NUMBER=<n>
#   PR_URL=<url>
#   PR_STATE=<OPEN|CLOSED|MERGED>
#
# Always printed on DONE (needed by cleanup-branch.sh):
#   CURRENT_BRANCH=<branch>
#   DEFAULT_BRANCH=<branch>

set -uo pipefail

die() {
  echo "STATUS=ERROR"
  echo "MESSAGE=$1"
  exit 1
}

command -v git >/dev/null || die "git not found"
command -v gh >/dev/null || die "gh not found"
git rev-parse --is-inside-work-tree >/dev/null 2>&1 || die "not inside a git repository"

DEFAULT_BRANCH="$(git remote show origin 2>/dev/null | sed -n 's/.*HEAD branch: //p')"
if [[ -z "$DEFAULT_BRANCH" ]]; then
  DEFAULT_BRANCH="$(gh repo view --json defaultBranchRef -q .defaultBranchRef.name 2>/dev/null || true)"
fi
[[ -z "$DEFAULT_BRANCH" ]] && die "could not determine default branch"

CURRENT_BRANCH="$(git rev-parse --abbrev-ref HEAD)"

if [[ "$CURRENT_BRANCH" == "$DEFAULT_BRANCH" ]]; then
  echo "STATUS=ON_DEFAULT"
  echo "DEFAULT_BRANCH=$DEFAULT_BRANCH"
  exit 0
fi

if [[ -n "$(git status --porcelain)" ]]; then
  echo "STATUS=DIRTY"
  echo "CURRENT_BRANCH=$CURRENT_BRANCH"
  exit 2
fi

PR_JSON="$(gh pr view --json state,number,url 2>&1)"
if [[ $? -ne 0 ]]; then
  if grep -qi "no pull requests found" <<<"$PR_JSON"; then
    echo "STATUS=NO_PR"
    echo "CURRENT_BRANCH=$CURRENT_BRANCH"
    exit 3
  fi
  die "gh pr view failed: $PR_JSON"
fi

PR_STATE="$(python3 -c "import json,sys; print(json.load(sys.stdin)['state'])" <<<"$PR_JSON")"
PR_NUMBER="$(python3 -c "import json,sys; print(json.load(sys.stdin)['number'])" <<<"$PR_JSON")"
PR_URL="$(python3 -c "import json,sys; print(json.load(sys.stdin)['url'])" <<<"$PR_JSON")"

if [[ "$PR_STATE" == "OPEN" ]]; then
  echo "STATUS=OPEN"
  echo "PR_NUMBER=$PR_NUMBER"
  echo "PR_URL=$PR_URL"
  echo "PR_STATE=$PR_STATE"
  exit 4
fi

echo "STATUS=DONE"
echo "PR_NUMBER=$PR_NUMBER"
echo "PR_URL=$PR_URL"
echo "PR_STATE=$PR_STATE"
echo "CURRENT_BRANCH=$CURRENT_BRANCH"
echo "DEFAULT_BRANCH=$DEFAULT_BRANCH"
