---
name: create-pr
description: This skill should be used when the user asks to "open a PR", "create a PR for this", "open a pull request for this", "update the PR", "sync the PR description", or otherwise wants a pull request opened, reused, or refreshed with new pushed commits for the current branch, which must already be pushed to the remote. Also used internally by the ship-it skill as its fourth step.
version: 0.2.0
---

# Open, Reuse, or Update a PR

Open a pull request for the current branch against the repo's default branch. If an open PR already exists, refresh its description to reflect any commits pushed since it was last updated instead of leaving it stale.

## Fast path: run the scripts first

Two scripts cover the mechanical parts of this skill; writing the title, Summary bullets, and Test Plan from the diff is still the agent's job, so neither script does that.

**`scripts/pr-state.sh`** — read-only. Determines the default branch, checks for an existing PR, and (when a description needs to be written or rewritten) gathers the commit log and diff against the default branch — collapsing Step 1, Step 2.1, and the context-gathering in Steps 2/3 into one call. Run it first, always. Read the last output block for a `STATUS=` line:

- `STATUS=NO_PR` — no open PR exists (none found, or the existing one is `MERGED`/`CLOSED`). Go to Step 3; the commit log and diff are already printed below the status.
- `STATUS=OPEN_CURRENT` — an open PR exists and already matches local `HEAD`. Report `PR_URL` and stop — do not edit it.
- `STATUS=OPEN_BEHIND` — an open PR exists but is behind local `HEAD`. Go to Step 2 using the printed `PR_NUMBER`, commit log, and diff.
- `STATUS=ERROR` — unexpected failure; `MESSAGE=` has why. Fall back to the manual steps below.

**`scripts/pr-write.sh`** — mutating. Takes an already-written title and body and creates or updates the PR — it does not generate content. Usage:
```bash
# create
printf '## Summary\n- ...\n\n## Test Plan\n- [ ] ...\n' | scripts/pr-write.sh create --title "<title>"

# update (title optional — omit to leave it as-is)
printf '## Summary\n- ...\n' | scripts/pr-write.sh update <pr-number> --title "<title>"
```
Read its `STATUS=` line: `OK` (done — `PR_URL` printed, report it) or `ERROR` (`MESSAGE=` plus raw `gh` output).

Only fall back to the manual steps below if a script is missing or its `STATUS=ERROR` needs manual diagnosis. The steps below remain the authoritative description of the judgment calls (title wording, Summary/Test Plan content) the scripts defer to you.

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
