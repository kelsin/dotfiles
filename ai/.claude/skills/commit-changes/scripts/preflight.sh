#!/usr/bin/env bash
# preflight.sh — read-only fact-gathering for the commit-changes skill's
# Step 1 (Pre-flight) and Step 2.1 (default-branch lookup). Runs the
# nothing-to-commit check, gh-account lookup, test/lint detection+run, a
# secret-pattern scan, and default-branch detection in one call instead of
# 5-8 separate tool calls with hand-parsed output. Makes no changes to the
# repo — no staging, no commits, no branch switches.
#
# The commit message and branch slug still require reading the diff and
# writing prose, which this script deliberately leaves to the agent.
#
# Exit codes / STATUS values (printed on stdout, last line/block):
#   STATUS=OK            pre-flight passed; facts below, safe to proceed
#   STATUS=NOTHING        no staged/unstaged/untracked changes — stop
#   STATUS=TEST_FAILED     project test/lint command failed — do not commit
#   STATUS=TEST_TIMEOUT    test command exceeded TEST_TIMEOUT_SECS (default 300)
#   STATUS=ERROR           unexpected failure — message printed
#
# Always printed on STATUS=OK (even if empty):
#   GH_USERNAME=<account>            gh account matching the repo's host
#   DEFAULT_BRANCH=<branch>
#   CURRENT_BRANCH=<branch>
#   ON_DEFAULT=0|1
#   TEST_COMMAND=<command or empty>  the test/lint command that was run
#   SECRET_HITS=<n>                  count of files flagged by the secret scan
#   --- secret scan hits --- / <paths, if any>
#   --- diff summary --- / git diff --stat output (staged + unstaged)

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

die() {
  echo "STATUS=ERROR"
  echo "MESSAGE=$1"
  exit 1
}

command -v git >/dev/null || die "git not found"
git rev-parse --is-inside-work-tree >/dev/null 2>&1 || die "not inside a git repository"

# --- Nothing-to-commit check ---

if [[ -z "$(git status --porcelain)" ]]; then
  echo "STATUS=NOTHING"
  exit 2
fi

# --- gh account for this host ---

REMOTE_URL="$(git remote get-url origin 2>/dev/null || true)"
HOST=""
if [[ -n "$REMOTE_URL" ]]; then
  HOST="$(sed -E 's#^[a-zA-Z]+://##; s#^[^@]+@##; s#[:/].*##' <<<"$REMOTE_URL")"
fi

GH_USERNAME=""
if command -v gh >/dev/null 2>&1 && [[ -n "$HOST" ]]; then
  GH_USERNAME="$(to 15 gh auth status --hostname "$HOST" 2>&1 </dev/null | sed -n 's/.*Logged in to [^ ]* account \([^ ]*\).*/\1/p' | head -1)"
fi

# --- Default branch / current branch ---

DEFAULT_BRANCH="$(default_branch)"
[[ -z "$DEFAULT_BRANCH" ]] && die "could not determine default branch"

CURRENT_BRANCH="$(git rev-parse --abbrev-ref HEAD)"
ON_DEFAULT=0
[[ "$CURRENT_BRANCH" == "$DEFAULT_BRANCH" ]] && ON_DEFAULT=1

# --- Test/lint command detection + run ---

TEST_COMMAND=""
REPO_ROOT="$(git rev-parse --show-toplevel)"

if [[ -f "$REPO_ROOT/package.json" ]] && command -v python3 >/dev/null 2>&1; then
  # Package manager from the lockfile; script preference avoids watch-mode
  # commands (e.g. "test": "vitest") by favoring test:run / test:ci.
  PM="npm"
  [[ -f "$REPO_ROOT/pnpm-lock.yaml" ]] && PM="pnpm"
  [[ -f "$REPO_ROOT/yarn.lock" ]] && PM="yarn"
  [[ -f "$REPO_ROOT/bun.lockb" || -f "$REPO_ROOT/bun.lock" ]] && PM="bun"
  SCRIPT="$(python3 - "$REPO_ROOT/package.json" <<'PY' 2>/dev/null
import json, sys
s = json.load(open(sys.argv[1])).get("scripts", {})
for name in ("test:run", "test:ci", "test", "lint"):
    if name in s:
        print(name)
        break
PY
)"
  if [[ -n "$SCRIPT" ]]; then
    if [[ "$SCRIPT" == "test" ]]; then TEST_COMMAND="$PM test"; else TEST_COMMAND="$PM run $SCRIPT"; fi
  fi
elif [[ -f "$REPO_ROOT/Rakefile" ]] && grep -qE '^\s*(task\s+:test|task\s+:spec)' "$REPO_ROOT/Rakefile" 2>/dev/null; then
  TEST_COMMAND="rake test"
elif [[ -f "$REPO_ROOT/Makefile" ]] && grep -qE '^(test|lint):' "$REPO_ROOT/Makefile" 2>/dev/null; then
  TEST_COMMAND="make $(grep -oE '^(test|lint):' "$REPO_ROOT/Makefile" | head -1 | tr -d ':')"
fi

if [[ -n "$TEST_COMMAND" ]]; then
  # CI=1 keeps watchers (vitest/jest) non-interactive; the timeout turns a
  # hang into a reported failure. TEST_TIMEOUT_SECS overrides the 300s default.
  TIMEOUT_BIN="$(command -v timeout || command -v gtimeout || true)"
  TIMEOUT_PREFIX=""
  [[ -n "$TIMEOUT_BIN" ]] && TIMEOUT_PREFIX="$TIMEOUT_BIN ${TEST_TIMEOUT_SECS:-300}"
  TEST_OUTPUT="$(cd "$REPO_ROOT" && CI=1 eval "$TIMEOUT_PREFIX $TEST_COMMAND" </dev/null 2>&1)"
  TEST_EXIT=$?
  if [[ $TEST_EXIT -eq 124 ]]; then
    echo "STATUS=TEST_TIMEOUT"
    echo "TEST_COMMAND=$TEST_COMMAND"
    echo "--- test output (partial) ---"
    echo "$TEST_OUTPUT" | tail -40
    exit 4
  fi
  if [[ $TEST_EXIT -ne 0 ]]; then
    echo "STATUS=TEST_FAILED"
    echo "TEST_COMMAND=$TEST_COMMAND"
    echo "--- test output ---"
    echo "$TEST_OUTPUT"
    exit 3
  fi
fi

# --- Secret scan ---

SECRET_PATTERN='\.env($|\.[^.]+$)|\.pem$|id_rsa$|id_ed25519$|credentials\.json$|\.p12$|\.pfx$'
SECRET_HITS_FILES="$( (git status --porcelain | awk '{print $2}'; ) | grep -E "$SECRET_PATTERN" || true)"

echo "STATUS=OK"
echo "GH_USERNAME=$GH_USERNAME"
echo "DEFAULT_BRANCH=$DEFAULT_BRANCH"
echo "CURRENT_BRANCH=$CURRENT_BRANCH"
echo "ON_DEFAULT=$ON_DEFAULT"
echo "TEST_COMMAND=$TEST_COMMAND"
echo "SECRET_HITS=$(grep -c . <<<"$SECRET_HITS_FILES" 2>/dev/null || echo 0)"
echo "--- secret scan hits (filename-based; review file contents too) ---"
[[ -n "$SECRET_HITS_FILES" ]] && echo "$SECRET_HITS_FILES"
echo "--- diff summary ---"
git diff --stat HEAD 2>/dev/null || true
git status --porcelain | awk '$1 == "??" {print "?? " $2}'
