---
name: ship-it
description: This skill should be used when the user asks to "commit and push", "commit push and PR", "create a PR for this", "open a pull request for this", "ship this change", "ship it", ":rocket:", or otherwise wants edited code committed, pushed to the remote, and turned into a pull request in one flow.
version: 0.2.0
---

# Ship It

Run the full flow autonomously by invoking three sub-skills in sequence: commit, push, then open a PR. Do not pause for confirmation between steps unless genuinely blocked (e.g. a sub-skill stops with an error, no clean way to proceed).

## Step 1: Commit

Invoke the `commit-changes` skill. If it stops (e.g. nothing to commit, failing tests, secrets detected), stop here and surface its report — do not proceed to Step 2.

## Step 2: Push

Invoke the `push-changes` skill. If it stops (e.g. push rejected), stop here and surface its report — do not proceed to Step 3.

## Step 3: Open or reuse the PR

Invoke the `create-pr` skill and report the resulting PR URL back to the user.

## Notes

- Never force-push, skip hooks (`--no-verify`), or bypass signing.
- Never use destructive git operations (`reset --hard`, `clean -f`) as part of this flow.
- If any sub-skill fails, stop and surface the actual error rather than working around it silently.
