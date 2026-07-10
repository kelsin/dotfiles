#!/usr/bin/env bash
# pr-state.sh — read-only fact-gathering for the create-pr skill's Step 1
# (Determine PR state), plus context-gathering for Steps 2/3 (log + diff
# against the default branch). Collapses ~4-5 separate `gh`/`git` calls
# into one. Writing the PR title/body is still the agent's job — this
# script only gathers the facts and raw context needed to write them.
#
# Note: GitHub syncs a PR's *commits* automatically on push — this script
# does not check whether the PR's remote head matches local HEAD, because
# that's always true right after a push and says nothing about whether the
# PR *description* is stale. Whenever an open PR exists, the description
# should be (re)generated from the current diff.
#
# Exit codes / STATUS values (printed on stdout, last line/block):
#   STATUS=NO_PR          no open/mergeable PR exists — go to Step 3 (create)
#   STATUS=OPEN           an OPEN PR exists — refresh its description (Step 2)
#   STATUS=NO_COMMITS     branch has no commits ahead of the default branch —
#                          nothing to describe; report PR_URL if present, stop
#   STATUS=ERROR          unexpected failure — message printed
#
# Always printed on OPEN/NO_COMMITS (when a PR exists):
#   PR_NUMBER=<n>
#   PR_URL=<url>
#
# Always printed on NO_PR/OPEN (the two cases that need fresh context to
# write/rewrite a description):
#   DEFAULT_BRANCH=<branch>
#   --- commits (default-branch..HEAD) ---
#   --- diff (default-branch...HEAD) ---

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

COMMITS="$(git log "$DEFAULT_BRANCH..HEAD" --oneline)"

print_context() {
  echo "DEFAULT_BRANCH=$DEFAULT_BRANCH"
  echo "--- commits ($DEFAULT_BRANCH..HEAD) ---"
  echo "$COMMITS"
  echo "--- diff ($DEFAULT_BRANCH...HEAD) ---"
  git diff "$DEFAULT_BRANCH...HEAD"
}

PR_JSON="$(gh pr view --json url,state,number 2>&1)"
if [[ $? -ne 0 ]]; then
  if grep -qi "no pull requests found" <<<"$PR_JSON"; then
    if [[ -z "$COMMITS" ]]; then
      echo "STATUS=NO_COMMITS"
      exit 0
    fi
    echo "STATUS=NO_PR"
    print_context
    exit 0
  fi
  die "gh pr view failed: $PR_JSON"
fi

PR_STATE="$(python3 -c "import json,sys; print(json.load(sys.stdin)['state'])" <<<"$PR_JSON")"
PR_NUMBER="$(python3 -c "import json,sys; print(json.load(sys.stdin)['number'])" <<<"$PR_JSON")"
PR_URL="$(python3 -c "import json,sys; print(json.load(sys.stdin)['url'])" <<<"$PR_JSON")"

if [[ "$PR_STATE" != "OPEN" ]]; then
  if [[ -z "$COMMITS" ]]; then
    echo "STATUS=NO_COMMITS"
    exit 0
  fi
  echo "STATUS=NO_PR"
  print_context
  exit 0
fi

if [[ -z "$COMMITS" ]]; then
  echo "STATUS=NO_COMMITS"
  echo "PR_NUMBER=$PR_NUMBER"
  echo "PR_URL=$PR_URL"
  exit 0
fi

echo "STATUS=OPEN"
echo "PR_NUMBER=$PR_NUMBER"
echo "PR_URL=$PR_URL"
print_context
