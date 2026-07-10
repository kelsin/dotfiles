---
name: commit-changes
description: This skill should be used when the user asks to "commit this", "commit my changes", or otherwise wants edited code checked for safety and committed locally (without pushing or opening a PR). Also used internally by the ship-it skill as its first step.
version: 0.1.0
---

# Commit Changes

Run pre-flight checks, ensure a suitable branch, then stage and commit local changes. Do not pause for confirmation between steps unless genuinely blocked (e.g. pre-flight checks fail, no clean way to proceed).

## Fast path: run the scripts first

Two scripts cover the mechanical parts of this skill; writing the commit message and branch slug from the diff is still the agent's job, so neither script does that.

**`scripts/preflight.sh`** — read-only. Runs the nothing-to-commit check, gh-account lookup, test/lint detection+run, a filename-based secret scan, and default-branch detection in one call instead of 5-8 separate commands. Run it first, always. Read the last output block for a `STATUS=` line:

- `STATUS=OK` — proceed. Facts are in the `KEY=value` lines (`GH_USERNAME`, `DEFAULT_BRANCH`, `CURRENT_BRANCH`, `ON_DEFAULT`, `TEST_COMMAND`, `SECRET_HITS`) plus a diff summary at the bottom — use these instead of running `git diff`/`git status` separately. If `SECRET_HITS` > 0, review the listed files' actual contents (the scan is filename-based only) and decide with the user whether to exclude them.
- `STATUS=NOTHING` — no changes to commit. Stop and report that.
- `STATUS=TEST_FAILED` — the detected test/lint command failed; its output is included. Stop and surface it — do not commit broken code.
- `STATUS=ERROR` — unexpected failure; the `MESSAGE=` line has why. Fall back to the manual steps below.

**`scripts/commit.sh`** — mutating. Takes the branch name (only when `ON_DEFAULT=1`, per Step 2) and any paths to exclude (from the secret scan or user request) as flags, and the finished commit message on stdin. It stages everything except excluded paths and commits — it does not generate the message or slug. Example:
```bash
echo "feat(auth): add token refresh" | scripts/commit.sh --branch "$GH_USERNAME/add-token-refresh"
```
Read its `STATUS=` line: `OK` (done — report the branch/commit SHA), `HOOK_FAILED` (a commit hook rejected it — surface the output, don't retry with `--no-verify`), or `ERROR`.

Only fall back to the manual steps below if a script is missing or its `STATUS=ERROR`/`HOOK_FAILED` needs manual diagnosis. The steps below remain the authoritative description of the judgment calls (message wording, slug, secret handling) the scripts defer to you.

## Step 1: Pre-flight

1. Run `git status` to confirm there are staged/unstaged/untracked changes. If there is nothing to commit, stop and report that.
2. Run `git remote get-url origin` to determine the repo host (e.g. `github.com` vs an internal GHE host like `git.musta.ch`). Use `gh auth status` to find the matching authenticated account for that host — this account's username becomes the branch prefix in Step 2.
3. Scan for a project test/lint command:
   - Node: `package.json` → look for `scripts.test` or `scripts.lint`.
   - Ruby: `Rakefile` with a `test`/`spec` task.
   - Make-based: `Makefile` with a `test` or `lint` target.
   - Other common conventions (`tox.ini`, `justfile`, etc.) if present.
   If found, run it. If it fails, stop and surface the failure output — do not commit broken code.
4. Scan the diff (`git diff` and untracked files) for likely secrets before staging anything: `.env` files, private keys (`*.pem`, `id_rsa`), credential files, tokens embedded in code. If found, exclude them from staging and warn the user rather than committing them silently.

## Step 2: Branching

1. Determine the repo's default branch: `git remote show origin | grep 'HEAD branch'` or `gh repo view --json defaultBranchRef`.
2. If the current branch IS the default branch:
   - Generate a branch name as `<gh-username>/<slug>`, where `<gh-username>` is the account identified in Step 1.2 and `<slug>` is a 1-3 word kebab-case summary of the change (derived from the diff content, not a generic word like "update").
   - Create and switch to it: `git checkout -b <branch-name>`.
3. If the current branch is NOT the default branch, commit directly on it — do not create a new branch.

## Step 3: Stage and commit

1. Stage everything: `git add -A` (minus anything excluded for secrets in Step 1.4).
2. Review `git diff --staged` to understand the actual change.
3. Write the commit message using [Conventional Commits](https://www.conventionalcommits.org) format: `<type>[optional scope]: <description>`.
   - `type` is one of: `feat`, `fix`, `refactor`, `perf`, `test`, `docs`, `build`, `ci`, `chore`, `revert`. Pick the one that matches the actual change — `feat` for new capability, `fix` for a bug fix, `refactor` for restructuring with no behavior change, etc.
   - `scope` is optional — a short noun for the affected area (e.g. `auth`, `api`, package/module name), derived from the diff. Omit it if nothing specific stands out.
   - `description` is imperative, lowercase, no trailing period, focused on *why* the change was made, not a restatement of the diff.
   - If the change is breaking, append `!` after the type/scope (e.g. `feat(api)!: ...`) and add a `BREAKING CHANGE:` footer explaining the impact.
   - Add a body (blank line, then free text) only when the summary line needs more explanation than the title allows.
4. Do NOT add a `Co-Authored-By` trailer, "Generated with"/"🤖" line, or any other marker attributing the commit to an AI/agent — the commit should read as authored solely by the user. Use a HEREDOC for correct formatting:
   ```bash
   git commit -m "$(cat <<'EOF'
   <type>[scope]: <description>
   EOF
   )"
   ```

## Notes

- Never use destructive git operations (`reset --hard`, `clean -f`) as part of this flow.
- Never skip hooks (`--no-verify`) or bypass signing.
- If any step fails (pre-flight checks, commit rejected by hook), stop and surface the actual error rather than working around it silently.
