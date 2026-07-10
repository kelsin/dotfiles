---
name: push-changes
description: This skill should be used when the user asks to "push this", "push my branch", or otherwise wants local commits pushed to the remote (without opening a PR). Also used internally by the ship-it skill as its third step (after rebase-branch), and after the rebase-branch skill to land a rebased branch.
version: 0.2.0
---

# Push Changes

Push the current branch to the remote with upstream tracking. Force-push only when the local history was intentionally rewritten (e.g. by the `rebase-branch` skill) — never as a reflexive fix for a rejected push.

## Fast path: run the script first

Run `scripts/push-branch.sh` to do the actual push instead of running `git push` by hand. It determines the default branch, guards against pushing to it unintentionally, runs the push, and classifies the result — collapsing what would otherwise be 2-3 separate commands plus manual stderr-reading into one call. It does **not** decide whether to force-push or push to the default branch; those are judgment calls from Step 1 below that only the agent/user can make, so pass them in as flags:

- Plain push (the default case): `scripts/push-branch.sh`
- Force-push, only after confirming with the user per Step 1.2: `scripts/push-branch.sh --force`
- Push to the default branch, only if the user explicitly asked for it: `scripts/push-branch.sh --allow-default` (combine with `--force` if both apply)

Read the last output block for a `STATUS=` line:

- `STATUS=OK` — pushed successfully. Report the branch and whether it was forced.
- `STATUS=ON_DEFAULT` — current branch is the default branch and `--allow-default` wasn't passed. Confirm with the user before retrying with that flag.
- `STATUS=REJECTED` — a non-force push was rejected (remote has commits you don't). Per the Notes below, investigate (e.g. `git log origin/<branch> --oneline`) rather than retrying with `--force`.
- `STATUS=ERROR` — an unexpected git failure; the `MESSAGE=` and git output show why.

Only fall back to running `git push` by hand if the script is missing or `STATUS=ERROR` needs manual diagnosis. Step 1 below is the authoritative description of the judgment calls the script defers to you.

## Step 1: Push

1. Confirm the current branch is not the repo's default branch (check `git remote show origin | grep 'HEAD branch'` or `gh repo view --json defaultBranchRef`). Pushing directly to the default branch should only happen if the user explicitly asked for that.
2. Determine whether this push follows an intentional history rewrite: the `rebase-branch` skill rebased, amended, or otherwise rewrote commits on this branch during the current session (whether invoked by the caller or run earlier in the same session), or the user has explicitly said so.
   - If yes, confirm with the user before force-pushing, then push with `git push --force -u origin <branch-name>`. Plain `--force` is used instead of `--force-with-lease` per user preference — some repos change too often for the lease's staleness check to be practical without constantly getting rejected. Because plain `--force` has no built-in check against a concurrent remote update, confirming with the user first is the only safeguard against clobbering someone else's work — do not skip it even when a rebase happened earlier in the same session.
   - Otherwise, push normally: `git push -u origin <branch-name>`.

## Notes

- Do not force-push just because a plain push was rejected — a rejection on a branch that wasn't intentionally rewritten usually means someone else pushed to it; investigate (e.g. `git log origin/<branch> --oneline`) rather than overwriting.
- Never skip hooks (`--no-verify`).
- If a plain push is rejected with no known history rewrite, stop and surface the actual error rather than escalating to force.
