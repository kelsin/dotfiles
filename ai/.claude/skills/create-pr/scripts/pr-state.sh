#!/usr/bin/env bash
# pr-state.sh — read-only fact-gathering for the create-pr skill's Step 1
# (Determine PR state) and Step 2.1 (behind check), plus context-gathering
# for Steps 2/3 (log + diff against the default branch). Collapses ~4-5
# separate `gh`/`git` calls into one. Writing the PR title/body is still
# the agent's job — this script only gathers the facts and raw context
# needed to write them.
#
# Exit codes / STATUS values (printed on stdout, last line/block):
#   STATUS=NO_PR         no open/mergeable PR exists — go to Step 3 (create)
#   STATUS=OPEN_CURRENT   an OPEN PR exists and already reflects local HEAD —
#                          report its URL, do not edit it
#   STATUS=OPEN_BEHIND     an OPEN PR exists but local HEAD has new commits
#                          not yet reflected — refresh it (Step 2)
#   STATUS=ERROR           unexpected failure — message printed
#
# Always printed on OPEN_CURRENT/OPEN_BEHIND:
#   PR_NUMBER=<n>
#   PR_URL=<url>
#
# Always printed on NO_PR/OPEN_BEHIND (the two cases that need fresh
# context to write/rewrite a description):
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

print_context() {
  echo "DEFAULT_BRANCH=$DEFAULT_BRANCH"
  echo "--- commits ($DEFAULT_BRANCH..HEAD) ---"
  git log "$DEFAULT_BRANCH..HEAD" --oneline
  echo "--- diff ($DEFAULT_BRANCH...HEAD) ---"
  git diff "$DEFAULT_BRANCH...HEAD"
}

PR_JSON="$(gh pr view --json url,state,number,headRefOid 2>&1)"
if [[ $? -ne 0 ]]; then
  if grep -qi "no pull requests found" <<<"$PR_JSON"; then
    echo "STATUS=NO_PR"
    print_context
    exit 0
  fi
  die "gh pr view failed: $PR_JSON"
fi

PR_STATE="$(python3 -c "import json,sys; print(json.load(sys.stdin)['state'])" <<<"$PR_JSON")"
PR_NUMBER="$(python3 -c "import json,sys; print(json.load(sys.stdin)['number'])" <<<"$PR_JSON")"
PR_URL="$(python3 -c "import json,sys; print(json.load(sys.stdin)['url'])" <<<"$PR_JSON")"
PR_HEAD_OID="$(python3 -c "import json,sys; print(json.load(sys.stdin)['headRefOid'])" <<<"$PR_JSON")"

if [[ "$PR_STATE" != "OPEN" ]]; then
  echo "STATUS=NO_PR"
  print_context
  exit 0
fi

LOCAL_HEAD="$(git rev-parse HEAD)"

if [[ "$PR_HEAD_OID" == "$LOCAL_HEAD" ]]; then
  echo "STATUS=OPEN_CURRENT"
  echo "PR_NUMBER=$PR_NUMBER"
  echo "PR_URL=$PR_URL"
  exit 0
fi

echo "STATUS=OPEN_BEHIND"
echo "PR_NUMBER=$PR_NUMBER"
echo "PR_URL=$PR_URL"
print_context
