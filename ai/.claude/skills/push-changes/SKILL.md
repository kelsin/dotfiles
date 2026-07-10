---
name: push-changes
description: This skill should be used when the user asks to "push this", "push my branch", or otherwise wants local commits pushed to the remote (without opening a PR). Also used internally by the ship-it skill as its third step (after rebase-branch), and after the rebase-branch skill to land a rebased branch.
version: 0.2.0
---

# Push Changes

Push the current branch to the remote with upstream tracking. Force-push only when the local history was intentionally rewritten (e.g. by the `rebase-branch` skill) — never as a reflexive fix for a rejected push.

## Step 1: Push

1. Confirm the current branch is not the repo's default branch (check `git remote show origin | grep 'HEAD branch'` or `gh repo view --json defaultBranchRef`). Pushing directly to the default branch should only happen if the user explicitly asked for that.
2. Determine whether this push follows an intentional history rewrite: the `rebase-branch` skill rebased, amended, or otherwise rewrote commits on this branch during the current session (whether invoked by the caller or run earlier in the same session), or the user has explicitly said so.
   - If yes, confirm with the user before force-pushing, then push with `git push --force -u origin <branch-name>`. Plain `--force` is used instead of `--force-with-lease` per user preference — some repos change too often for the lease's staleness check to be practical without constantly getting rejected. Because plain `--force` has no built-in check against a concurrent remote update, confirming with the user first is the only safeguard against clobbering someone else's work — do not skip it even when a rebase happened earlier in the same session.
   - Otherwise, push normally: `git push -u origin <branch-name>`.

## Notes

- Do not force-push just because a plain push was rejected — a rejection on a branch that wasn't intentionally rewritten usually means someone else pushed to it; investigate (e.g. `git log origin/<branch> --oneline`) rather than overwriting.
- Never skip hooks (`--no-verify`).
- If a plain push is rejected with no known history rewrite, stop and surface the actual error rather than escalating to force.
