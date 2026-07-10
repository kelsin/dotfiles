#!/usr/bin/env bash
# pr-write.sh — mutating half of the create-pr skill. Takes an
# already-written title and body and either creates a new PR or edits an
# existing one. It does not generate title/body content — that's the
# agent's job (Summary/Test Plan bullets derived from the diff), same
# reasoning as commit-changes/scripts/commit.sh not writing commit messages.
#
# Usage:
#   pr-write.sh create --title <title>   (body on stdin)
#   pr-write.sh update <pr-number> [--title <title>]   (body on stdin)
#
#   create            Opens a new PR against the default branch, ready for
#                      review (not draft).
#   update <number>    Edits an existing PR's body (and title, if --title
#                      is passed) in place.
#
# Exit codes / STATUS values:
#   STATUS=OK       created/updated successfully — URL printed
#   STATUS=ERROR    unexpected failure — message printed

set -uo pipefail

die() {
  echo "STATUS=ERROR"
  echo "MESSAGE=$1"
  exit 1
}

command -v gh >/dev/null || die "gh not found"

MODE="${1:-}"
shift || true

case "$MODE" in
  create)
    TITLE=""
    while [[ $# -gt 0 ]]; do
      case "$1" in
        --title) TITLE="$2"; shift 2 ;;
        *) die "unknown argument: $1" ;;
      esac
    done
    [[ -z "$TITLE" ]] && die "--title is required for create"
    BODY="$(cat)"
    [[ -z "$BODY" ]] && die "no PR body provided on stdin"

    OUTPUT="$(gh pr create --title "$TITLE" --body "$BODY" 2>&1)"
    if [[ $? -ne 0 ]]; then
      echo "STATUS=ERROR"
      echo "MESSAGE=gh pr create failed"
      echo "--- gh output ---"
      echo "$OUTPUT"
      exit 1
    fi
    echo "STATUS=OK"
    echo "PR_URL=$OUTPUT"
    ;;

  update)
    NUMBER="${1:-}"
    shift || true
    [[ -z "$NUMBER" ]] && die "PR number is required for update"
    TITLE=""
    while [[ $# -gt 0 ]]; do
      case "$1" in
        --title) TITLE="$2"; shift 2 ;;
        *) die "unknown argument: $1" ;;
      esac
    done
    BODY="$(cat)"
    [[ -z "$BODY" ]] && die "no PR body provided on stdin"

    EDIT_ARGS=("$NUMBER" --body "$BODY")
    [[ -n "$TITLE" ]] && EDIT_ARGS+=(--title "$TITLE")

    OUTPUT="$(gh pr edit "${EDIT_ARGS[@]}" 2>&1)"
    if [[ $? -ne 0 ]]; then
      echo "STATUS=ERROR"
      echo "MESSAGE=gh pr edit failed"
      echo "--- gh output ---"
      echo "$OUTPUT"
      exit 1
    fi
    echo "STATUS=OK"
    echo "$OUTPUT"
    ;;

  *)
    die "unknown mode: $MODE (expected 'create' or 'update')"
    ;;
esac
