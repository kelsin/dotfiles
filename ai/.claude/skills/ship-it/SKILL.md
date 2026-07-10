---
name: ship-it
description: This skill should be used when the user asks to "commit and push", "commit push and PR", "create a PR for this", "open a pull request for this", "ship this change", "ship it", ":rocket:", or otherwise wants edited code committed, pushed to the remote, and turned into a pull request in one flow.
version: 0.2.0
---

# Ship It

Run the full flow autonomously by invoking four sub-skills in sequence: commit, rebase, push, then open a PR. Do not pause for confirmation between steps unless genuinely blocked (e.g. a sub-skill stops with an error, no clean way to proceed).

## Step 1: Commit

Invoke the `commit-changes` skill. If it stops (e.g. nothing to commit, failing tests, secrets detected), stop here and surface its report — do not proceed to Step 2.

## Step 2: Rebase

Invoke the `rebase-branch` skill to sync the branch onto the latest default branch before pushing. If it stops (e.g. unresolvable conflict, diverged branch), stop here, surface its report, and ask the user how to proceed — do not proceed to Step 3.

## Step 3: Push

Invoke the `push-changes` skill. Because Step 2 just rebased the branch, this push will force-push (`--force-with-lease`) — that's expected and doesn't need separate confirmation. If it stops (e.g. push rejected), stop here and surface its report — do not proceed to Step 4.

## Step 4: Open or reuse the PR

Invoke the `create-pr` skill and report the resulting PR URL back to the user.

## Notes

- Never plain force-push (`--force`), skip hooks (`--no-verify`), or bypass signing. `--force-with-lease` after the Step 2 rebase is expected, per the `push-changes` skill.
- Never use destructive git operations (`reset --hard`, `clean -f`) as part of this flow.
- If any sub-skill fails, stop and surface the actual error rather than working around it silently.
