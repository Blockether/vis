# Drafts

A **draft** is an isolated, throwaway copy of your repository that a session
works inside instead of your real files. You make changes, look at the result,
and then decide in one clean step: **land them** into your repo (`/draft apply`)
or **discard them** (`/draft abandon`). Nothing a draft does touches your working
directory until you apply.

Drafts are **opt-in**. By default a session edits your real cwd directly — Vis
calls this **trunk**. You only ever enter a draft on purpose.

## Why drafts exist

The point of a draft is a **safe, reversible sandbox** for work you are not sure
about yet:

- **Isolation** — the agent can refactor, delete, or rewrite files freely; your
  real repo is untouched until you apply. A bad turn costs you nothing.
- **One clean decision** — instead of hand-reverting a pile of edits, you make a
  single call at the end: apply (keep everything) or abandon (keep nothing).
- **A named unit of work** — a draft is labelled (`feature-x`), so the header and
  the draft list always tell you *which* piece of work you are looking at.

Think of it as "try this in a scratch copy" without the ceremony of a git branch,
a stash, or a second checkout.

## The lifecycle

A session is always in exactly **one** place: **trunk** (your real cwd) or a
**single draft**. It is a linear trunk ↔ draft overlay, not a set of live
branches you juggle. The header shows `<label> (DRAFT)` while you are in one.

```
                 /draft new <label>        /draft apply   → changes land in cwd
   ┌────────┐   ───────────────────────►  ┌─────────┐  ─────────────────────────►  ┌────────┐
   │ trunk  │   /draft-blank <label>       │  draft  │     /draft abandon  → discarded  │ trunk  │
   │ (cwd)  │   ◄───────────────────────   │ <label> │  ◄─────────────────────────  │ (cwd)  │
   └────────┘        /draft stash          └─────────┘        /draft resume <label>  └────────┘
                (park it, keep it alive)              (re-enter a parked draft)
```

You **enter** a draft two ways, **leave** it three ways.

## Entering a draft

- **`/draft new <label>`** — clone your current cwd into an isolated draft named
  `<label>` and enter it. The draft starts as a full copy of HEAD, so the agent
  sees your real files and edits the copy.
- **`/draft-blank <label>`** — like `/draft new`, but the draft starts **empty**:
  no files from your repo are carried in. Useful for generating something from
  scratch (a fresh module, a spike) without your existing tree in the way.

A draft **must be named** — an unnamed draft is anonymous and indistinguishable
in the draft list. Rich channels (the TUI) pop a prompt for the label; other
channels return a "Name the draft" nudge.

You can only be in one draft at a time. Trying to start a second one while you're
already in a draft is refused with *"Already in draft '…' — /draft apply or
/draft abandon it first"*.

## Leaving a draft

There are three exits, two permanent and one reversible:

- **`/draft apply`** — **land** the draft's changes into your real cwd, then
  leave the draft. This is the "keep it" exit: after apply, the changes are real
  files you can commit, branch, or keep editing on trunk. The draft is consumed.
- **`/draft abandon [reason]`** — **discard** the draft and leave it. This is
  destructive: the draft's working copy is physically reclaimed on a background
  thread. There is no undo, and an abandoned draft cannot be resumed. The
  optional `reason` is recorded for history.
- **`/draft stash`** — **park** the draft without discarding it, and return to
  trunk. The draft stays alive on disk; `/draft resume` re-enters it later. This
  is the non-destructive twin of abandon — use it when you want to switch back to
  trunk (or to a different draft) but keep this work for later.

> **apply keeps, abandon destroys, stash parks.** If you're unsure, stash — it's
> the only exit you can walk back.

## Parking & coming back: stash, list, resume

Because a session holds only one draft at a time, "switch to another draft" is
really **stash the current one, then resume the other**:

- **`/draft stash`** — leave the current draft on trunk, keeping it alive.
- **`/draft list`** — show every active/stashed draft in this repo, newest first,
  with the current one marked. This is how you find a parked draft to go back to.
- **`/draft resume <label>`** — re-enter a stashed draft by its label. With no
  label it lists the stashed drafts to choose from. Resume is **refused while you
  are already in a draft** — stash, apply, or abandon the current one first.

The gateway keeps stashed drafts alive until they are applied or abandoned, so a
stashed draft survives across turns (and session resumes) until you decide its
fate. A typical "switch drafts" flow:

```
/draft stash                 # park draft A, back on trunk
/draft list                  # see A (and any others) parked
/draft resume feature-b      # hop into a different parked draft
```

## Checking where you are

- **`/draft`** (bare) — reports whether you're on **trunk** or in a **draft**, and
  when in a draft, how many files have changed plus your exit options.

## Drafts are local — no model, no provider

Every `/draft …` command is handled **entirely inside Vis**. It never calls the
LLM and never sends anything to a provider — it just manipulates local workspace
state. Consequently a draft command:

- **does not consume tokens** and costs nothing;
- **does not show a model/provider footer** under its reply (the little
  `provider/model · time` line that assistant answers carry). A draft reply is a
  command result, not a model turn, so attributing it to a model would be
  misleading — Vis deliberately suppresses that footer for slash commands.

If you ever see a draft reply tagged with a provider/model line, that's a display
bug (the command still never reached a provider).

## Requirements

Drafts need a workspace backend that supports **isolation, rollback, merge-back,
and retained revisions**. When the current root can't provide an isolated copy,
`/draft new` / `/draft-blank` return *"No workspace backend can create an
isolated draft here"* with the capability matrix, and you keep working on trunk.

## Command reference

| Command | What it does |
| --- | --- |
| `/draft` | Show whether you're on trunk or in a draft. |
| `/draft new <label>` | Clone cwd into a draft named `<label>`, enter it. |
| `/draft-blank <label>` | Start an **empty** draft (nothing carried from HEAD), enter it. |
| `/draft apply` | Land the draft's changes into cwd, leave the draft. |
| `/draft stash` | Park the draft (keep it alive), return to trunk. |
| `/draft list` | List every active/stashed draft in this repo. |
| `/draft resume <label>` | Re-enter a stashed draft by label. |
| `/draft abandon [reason]` | Discard the draft and leave it (destructive). |

## A canonical gateway feature

Drafts are **owned by the gateway daemon**, not by any one channel. The daemon
holds the session's database and the on-disk clones, so every draft operation
runs *server-side* and every channel (TUI, web) sees the exact same state. This
is the same design as the session's filesystem roots and model preference: one
source of truth in the daemon, read back identically everywhere.

That means a draft you start in the TUI is visible and resumable from the web
surface, survives a client disconnect/reconnect, and never depends on
client-local state. Two channels attached to the same session share one draft
list.

### Gateway HTTP API

Alongside typing the `/draft …` slash commands, channels drive drafts through
the session's workspace endpoints — the canonical, channel-agnostic surface:

| Method + path | Purpose | Returns |
| --- | --- | --- |
| `GET  /v1/sessions/:sid/workspace/drafts` | List active/stashed drafts for the session's repo. | `{"drafts": [{"workspace_id", "label", "root", "repo_root", "fork_ms", "is_current"}]}` |
| `POST /v1/sessions/:sid/workspace/stash` | Park the current draft (non-destructive), return to trunk. | refreshed `{"workspace": …}` |
| `POST /v1/sessions/:sid/workspace/resume` | Switch into `{"workspace_id"}` (stashing any current draft first). | refreshed `{"workspace": …}` |

All three are served in **the canonical string-keyed wire shape** (`wire/canonical`)
on both transports, so a remote client passes the payload through verbatim — one
representation, no re-hydration. `resume` returns HTTP `409` with a typed error
when the target is not a resumable active draft (e.g. it was applied/abandoned,
or is pinned by another session).

The engine facades backing these routes live in `gateway/state.clj`
(`list-drafts`, `stash-draft!`, `resume-draft!`) with client stubs in
`gateway/client.clj`; the `/draft …` slash commands and any GUI drafts picker are
both just front-ends over the same gateway operations.
