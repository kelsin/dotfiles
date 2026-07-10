---
name: rebase-branch
description: This skill should be used when the user asks to "rebase this", "rebase my branch", "rebase onto main", "pull latest and rebase", "sync my branch with main/default", or otherwise wants their branch updated with the latest remote state and replayed on top of the latest default branch. Resolves conflicts automatically when safe, otherwise asks the user. After it completes, the push-changes skill can safely force-push the result. Also used internally by the ship-it skill as its second step, between commit and push.
version: 0.1.0
---

# Rebase Branch

Pull the latest remote state into both the current branch and the local default branch, then rebase the current branch's commits on top of the latest default branch. Any uncommitted local changes are always stashed before pulling and reapplied afterward, so the working tree carries forward across the rebase. Resolve conflicts automatically when the resolution is unambiguous; ask the user when it isn't.

## Fast path: run the script first

Run `scripts/rebase-branch.sh` before doing any of Steps 1-4 by hand. It performs the entire mechanical sequence (stash, determine default branch, fetch, fast-forward/force the local default, rebase, reapply the stash) in one call instead of ~8-10 separate git commands, and stops cleanly the moment something needs judgment. Read its last output block for a `STATUS=` line:

- `STATUS=OK` — done. Skip straight to Step 5 (Report) using the commits it already printed.
- `STATUS=NOOP_DEFAULT` — current branch is the default branch; nothing to do. Report this to the user.
- `STATUS=DIVERGED_CURRENT` — the current branch has diverged from its remote counterpart (Step 2.3's case). Stop and surface this; don't guess which side is authoritative.
- `STATUS=REBASE_CONFLICT` — the rebase (Step 3) stopped on a real conflict. The working tree is left mid-rebase with conflict markers; resolve using Step 3's judgment rules, then continue by hand (`git add`, `git rebase --continue`).
- `STATUS=STASH_CONFLICT` — the rebase succeeded but reapplying the autostash (Step 4) conflicted. Resolve using Step 4's judgment rules.
- `STATUS=ERROR` — an unexpected git failure; the `MESSAGE=` line has the raw error. Fall back to the manual steps below to diagnose.

Only fall back to running Steps 1-4 by hand if the script is missing, or `STATUS=ERROR` needs manual diagnosis. The step-by-step instructions below are the authoritative description of what the script does and how to resolve each stop point — read them regardless of whether you run the script.

## Step 1: Pre-flight

1. Run `git status` to check for uncommitted changes (staged, unstaged, or untracked).
   - If there are any, always stash them rather than asking: `git stash push -u -m "rebase-branch: autostash"`. Note that a stash was created — it must be popped in Step 4.
   - If there are none, skip the stash; there's nothing to reapply later.
2. Determine the default branch name (e.g. `main`): `git remote show origin | grep 'HEAD branch'` or `gh repo view --json defaultBranchRef`.
3. Confirm the current branch is not the default branch itself — rebasing it onto itself is a no-op. If they're the same, stop and report that (but pop the stash from Step 1.1 first, if one was created, so the user's working tree isn't left stashed).

## Step 2: Pull latest into both branches

1. Fetch both remote-tracking refs in one go: `git fetch origin <default-branch> <current-branch>`.
2. Update the local default branch to match its remote without checking it out: `git fetch origin <default-branch>:<default-branch>`.
   - This only succeeds as a fast-forward. If it's rejected, the local default branch has commits the remote doesn't (unusual — likely stray local work on that branch). The local default branch should always mirror the remote, so force it to match: `git fetch origin +<default-branch>:<default-branch>`. This only rewrites the local default branch ref (never the current branch), so it's safe to do without asking — any stray local commits on the default branch are recoverable via reflog if the user actually wanted them.
3. Update the current branch to match its remote counterpart, if one exists: `git merge --ff-only origin/<current-branch>`.
   - If the current branch has no upstream (never pushed), there's nothing to fast-forward — skip this.
   - If the ff-only merge fails, the local and remote copies of the current branch have diverged (e.g. someone else pushed to it). Stop and surface this rather than guessing which side is authoritative (pop the Step 1.1 stash first, if any).

## Step 3: Rebase

1. Rebase the current branch onto the freshly-updated default branch: `git rebase <default-branch>`.
   - This must happen while the working tree is still clean from Step 1's stash — `git rebase` refuses outright to even start if there are any uncommitted changes (staged or not), tracked-file conflict or not. Reapplying the stash before rebasing (as opposed to after) would make every stashed run fail for this reason, not because of an actual conflict.
2. If it completes cleanly, proceed to Step 4.
3. If it stops on a conflict:
   - Inspect the conflicting hunks (`git diff` shows conflict markers).
   - Resolve automatically only when the resolution is unambiguous — e.g. identical changes on both sides, a clean addition on one side with no overlapping change on the other, or whitespace-only differences. Base the resolution on the actual surrounding code, not a guess.
   - When the correct resolution isn't obvious — both sides changed the same logic differently, a semantic conflict that doesn't show up as textual markers, or a deleted-vs-modified file — stop and ask the user, showing the specific conflicting hunks, rather than picking a side.
   - After resolving a hunk, `git add` the file(s) and continue with `git rebase --continue`.
4. Only use `git rebase --skip` if the user confirms a commit's changes are now fully redundant. Never use `git rebase --abort` unless the user asks for it — that would discard conflict-resolution work already done.

## Step 4: Reapply stashed changes

1. If Step 1.1 created a stash, reapply it now, after the rebase has completed: `git stash pop`.
2. If it applies cleanly, proceed to Step 5.
3. If it conflicts, resolve using the same judgment as rebase conflicts in Step 3: auto-resolve only unambiguous cases (e.g. the stash and the rebased commits touch unrelated lines), otherwise stop and ask the user, showing the specific conflicting hunks. After resolving, `git add` the affected files — the stash entry is dropped automatically once `stash pop` finishes applying, but conflicted pops leave the stash entry in place, so drop it explicitly with `git stash drop` once resolution is confirmed correct.

## Step 5: Report

1. Run `git log <default-branch>..HEAD --oneline` to show the user the rebased commits now sitting on top of the default branch.
2. Note that this rewrote the branch's history, so pushing it now requires `--force-with-lease` — the `push-changes` skill treats a rebase performed earlier in the session as license to do that automatically; no separate confirmation is needed for the force-push itself.

## Notes

- Never use plain `--force` anywhere in this flow.
- Never skip hooks (`--no-verify`) or bypass signing.
- If uncommitted changes, a diverged branch, or an unresolvable conflict blocks progress, stop and surface the actual state rather than working around it silently.
