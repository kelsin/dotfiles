---
name: push-changes
description: This skill should be used when the user asks to "push this", "push my branch", or otherwise wants local commits pushed to the remote (without opening a PR). Also used internally by the ship-it skill as its second step, and after the rebase-branch skill to land a rebased branch.
version: 0.2.0
---

# Push Changes

Push the current branch to the remote with upstream tracking. Force-push only when the local history was intentionally rewritten (e.g. by the `rebase-branch` skill) — never as a reflexive fix for a rejected push.

## Step 1: Push

1. Confirm the current branch is not the repo's default branch (check `git remote show origin | grep 'HEAD branch'` or `gh repo view --json defaultBranchRef`). Pushing directly to the default branch should only happen if the user explicitly asked for that.
2. Determine whether this push follows an intentional history rewrite: the caller (e.g. `rebase-branch`) rebased, amended, or otherwise rewrote commits on this branch during the current session, or the user has explicitly said so.
   - If yes, push with `git push --force-with-lease -u origin <branch-name>`. `--force-with-lease` (not plain `--force`) refuses the push if the remote branch moved since your last fetch, so it won't clobber someone else's concurrent work.
   - Otherwise, push normally: `git push -u origin <branch-name>`.

## Notes

- Never use plain `--force` — always `--force-with-lease` when a force push is warranted, so a concurrent remote update blocks it instead of being silently overwritten.
- Do not force-push just because a plain push was rejected — a rejection on a branch that wasn't intentionally rewritten usually means someone else pushed to it; investigate (e.g. `git log origin/<branch> --oneline`) rather than overwriting.
- Never skip hooks (`--no-verify`).
- If the push is rejected (e.g. diverged history with no known rewrite, or `--force-with-lease` rejected because the remote moved), stop and surface the actual error rather than escalating to a stronger force.
