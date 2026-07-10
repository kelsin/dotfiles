---
name: cleanup-branch
description: This skill should be used when the user asks to "clean up this branch", "clean up my branch", "delete this branch", "I'm done with this PR", or otherwise wants the local branch for a PR removed once that PR is closed/merged, followed by checking out the default branch and pulling the latest changes. Only deletes the LOCAL branch — never touches the remote branch or the PR itself.
version: 0.1.0
---

# Clean Up Branch

Verify the current branch's PR is closed (merged or closed without merging) before deleting the local branch, checking out the default branch, and pulling latest. Never delete a branch whose PR is still open. Force-delete is expected and automatic when the PR is `MERGED` (squash/rebase merges routinely leave local commits unreachable from the default branch — that's not data loss, the changes already landed). Only ask for confirmation before force-deleting when the PR is `CLOSED` without merging, since those commits may not exist anywhere else.

## Fast path: run the scripts first

Two scripts cover the mechanical parts of this skill; deciding whether it's actually safe to delete is still the agent's job, so neither script makes that call on its own.

**`scripts/branch-state.sh`** — read-only. Determines the current branch, its default branch, working-tree cleanliness, and the associated PR's state in one call instead of 3-4 separate git/gh commands. Run it first, always. Read the last output block for a `STATUS=` line:

- `STATUS=ON_DEFAULT` — already on the default branch, nothing to clean up. Report this and stop.
- `STATUS=DIRTY` — the working tree has uncommitted or untracked changes. Stop and surface this to the user rather than silently stashing or discarding work that's about to be orphaned by a branch switch/delete.
- `STATUS=NO_PR` — no PR found for this branch. Stop and ask the user how they want to proceed (e.g. confirm it's safe to delete anyway) — don't assume it's safe just because no PR exists.
- `STATUS=OPEN` — a PR exists and is still open. Stop; do not delete the branch. Report the `PR_URL`.
- `STATUS=DONE` — a PR exists and is `CLOSED` or `MERGED`. Safe to proceed to cleanup. Note the `PR_STATE`:
  - `MERGED` — go straight to `cleanup-branch.sh` with `--force` (see below). Squash/rebase merges routinely leave local commits unreachable from the default branch even though the PR merged cleanly, so a plain `-d` delete will predictably fail; skip straight to force rather than confirming first.
  - `CLOSED` (not merged) — the branch's commits may not exist anywhere else. Confirm with the user before force-deleting.
- `STATUS=ERROR` — unexpected failure; `MESSAGE=` has why. Fall back to the manual steps below.

**`scripts/cleanup-branch.sh`** — mutating. Takes the branch name and default branch (from `branch-state.sh`'s `DONE` output) and checks out default, deletes the local branch, and pulls — it does not re-verify PR state. Usage:
```bash
scripts/cleanup-branch.sh --branch <current-branch> --default <default-branch>
```
If `PR_STATE=MERGED`, pass `--force` immediately on this first call — don't wait for `STATUS=UNMERGED` to retry, since a squash/rebase merge failing the plain delete is the expected case, not a surprise:
```bash
scripts/cleanup-branch.sh --branch <current-branch> --default <default-branch> --force
```
Read its `STATUS=` line:
- `STATUS=OK` — done. Report the deleted branch and that the default branch is now checked out and up to date.
- `STATUS=UNMERGED` — `git branch -d` refused because the branch has commits not reachable from the default branch. If `PR_STATE=CLOSED` (not merged), confirm with the user that discarding those commits is fine before retrying with `--force`. If `PR_STATE=MERGED` and you're seeing this because `--force` wasn't passed on the first call, just retry with `--force` — no confirmation needed, the changes are already in the default branch.
- `STATUS=ERROR` — unexpected failure; `MESSAGE=` plus raw git output.

Only fall back to the manual steps below if a script is missing or its `STATUS=ERROR` needs manual diagnosis. The steps below remain the authoritative description of the judgment calls (PR-state interpretation, force-delete confirmation) the scripts defer to you.

## Step 1: Check PR state

1. Confirm the current branch is not the default branch — nothing to clean up if it is.
2. Confirm the working tree is clean (`git status`). If not, stop — don't discard or stash uncommitted work as a side effect of cleanup.
3. Look up the PR associated with the current branch: `gh pr view --json state,number,url`.
   - If no PR is found, stop and ask the user — don't assume it's safe to delete.
   - If the PR is `OPEN`, stop — do not delete the branch.
   - If the PR is `CLOSED` or `MERGED`, proceed to Step 2.

## Step 2: Delete the local branch

1. Check out the default branch: `git checkout <default-branch>`. (Git refuses to delete the currently-checked-out branch, so this must happen first.)
2. Delete the local branch:
   - If the PR was `MERGED`, go straight to `git branch -D <branch-name>`. Squash/rebase merges routinely leave local commits unreachable from the default branch even though the change already landed, so failing `-d` first and then force-deleting is pure overhead — force immediately, no confirmation needed.
   - If the PR was `CLOSED` without merging, use `git branch -d <branch-name>` first. If it fails because the branch isn't fully merged, confirm with the user that discarding those commits is fine (they may not exist anywhere else), then use `git branch -D <branch-name>`.

## Step 3: Pull latest

1. `git pull` on the now-checked-out default branch to bring in the latest remote changes (including whatever landed from the just-cleaned-up PR).

## Notes

- This only deletes the LOCAL branch. It never deletes the remote branch or touches the PR itself — GitHub/the repo host handles remote branch cleanup on merge (or the user can do it separately).
- Force-delete (`-D`) is automatic for `MERGED` PRs (per Step 2) — no confirmation needed, since the changes are already in the default branch. Only confirm with the user before force-deleting when the PR was `CLOSED` without merging.
- If any step fails unexpectedly, stop and surface the actual error rather than working around it silently.
