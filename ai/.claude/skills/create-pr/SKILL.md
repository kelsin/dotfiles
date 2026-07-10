---
name: create-pr
description: This skill should be used when the user asks to "open a PR", "create a PR for this", "open a pull request for this", "update the PR", "sync the PR description", or otherwise wants a pull request opened, reused, or refreshed with new pushed commits for the current branch, which must already be pushed to the remote. Also used internally by the ship-it skill as its third step.
version: 0.2.0
---

# Open, Reuse, or Update a PR

Open a pull request for the current branch against the repo's default branch. If an open PR already exists, refresh its description to reflect any commits pushed since it was last updated instead of leaving it stale.

## Step 1: Determine PR state

1. Check whether a PR already exists for the current branch: `gh pr view --json url,state,number,headRefOid`.
   - If a PR exists but is `MERGED` or `CLOSED`, treat it as not existing — proceed to Step 3 (create).
   - If no PR exists (command errors with "no pull requests found"), proceed to Step 3 (create).
   - If a PR exists and is `OPEN`, proceed to Step 2 (update check).

## Step 2: Update an existing open PR if it's behind

1. Compare the PR's `headRefOid` from Step 1 to the current local `git rev-parse HEAD`.
   - If they match, the PR already reflects the current branch state. Report its URL back to the user and stop — do not edit it.
   - If they differ, new commits were pushed since the PR was last synced — proceed to refresh it.
2. Gather updated context: `git log <default-branch>..HEAD --oneline` and `git diff <default-branch>...HEAD`.
3. Regenerate the Summary and Test Plan from the full accumulated diff/commits on the branch (not just the newly pushed ones, so the description stays a coherent whole) and update the PR in place:
   ```bash
   gh pr edit <number> --body "$(cat <<'EOF'
   ## Summary
   - <bullet 1>
   - <bullet 2>

   ## Test Plan
   - [ ] <verification step derived from the change>
   EOF
   )"
   ```
   - Leave the existing title as-is unless it no longer describes the change, in which case update it too with `--title`.
   - Same content rules as PR creation below: no AI/agent attribution, no "Generated with"/"🤖" footer.
4. Report the resulting PR URL back to the user and note that the description was refreshed.

## Step 3: Create a new PR

1. Gather context for the PR body: `git log <default-branch>..HEAD --oneline` and `git diff <default-branch>...HEAD`.
2. Create the PR against the repo's default branch, ready for review (not draft):
   ```bash
   gh pr create --title "<short title, under 70 chars>" --body "$(cat <<'EOF'
   ## Summary
   - <bullet 1>
   - <bullet 2>

   ## Test Plan
   - [ ] <verification step derived from the change>
   EOF
   )"
   ```
   - Title: imperative, under 70 characters, summarizing the change.
   - Summary: 1-3 bullets describing what changed and why, derived from the actual diff/commits — not a generic restatement.
   - Test Plan: checklist of concrete verification steps relevant to this change (e.g. "run test suite", "manually exercise X flow"). Skip steps that don't apply.
   - Do NOT include a "Generated with"/"🤖" footer, Claude/AI attribution, or any marker indicating the PR was created by an agent. The PR must read as authored solely by the user.
3. Report the resulting PR URL back to the user.

## Notes

- If PR creation or update fails, stop and surface the actual error rather than working around it silently.
