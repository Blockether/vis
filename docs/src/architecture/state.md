# State Ownership

## State lifetime table

| State | Location | Lifetime |
|-------|----------|----------|
| LLM Router (singleton) | `internal/loop.clj :: router-atom` | Process |
| LLM Router (snapshot) | `environment :router` | Conversation |
| Conversation cache | `internal/loop.clj :: cache` | Process |
| SCI sandbox | `environment :sci-ctx` | Conversation |
| Extensions | `environment :extensions` | Conversation |
| Var-index cache | `environment :var-index-atom` | Conversation |
| Recursion depth | `environment :depth-atom` | Conversation |
| Current iteration counter | `environment :current-iteration-atom` (query-scoped, int counter starting at 0) | Query |
| Current iteration row id | `environment :current-iteration-id-atom` (query-scoped, UUID of the latest `db-store-iteration!` row, or nil) | Query |
| Token usage | `usage-atom` (local in iteration loop) | Query |
| Seen-expression hashes | `seen-expression-hashes-atom` (local in iteration loop) | Query |
| Dedup cache | `dedup-cache-atom` (local in iteration loop) | Query |

## Environment map

The environment is the runtime map representing one live conversation.
See [Environment Map](../extensions/environment.md) for every key, its
type, and what you can/cannot do with it from extension code.

## Conversation cache

`internal/loop.clj` maintains a process-level `(defonce cache (atom {}))`
mapping conversation ID strings to `{:environment env :lock Object}`.

- `ensure-env!` — find-or-create in cache
- `cache-env!` — insert into cache
- `close!` — dispose environment + remove from cache
- `close-all!` — dispose all + reset cache (process shutdown)

The per-conversation `:lock` object serializes `send!` calls — only one
turn runs at a time per conversation.

## Router lifecycle

The router lives in **two places** on purpose, and the relationship
between them is non-obvious enough to have caused a real bug — read
this section before changing provider/model-switching code.

1. **Global singleton** — `internal/loop.clj :: router-atom`. Refreshed
   by `lp/rebuild-router! config`, which
   `(reset! router-atom (llm/make-router (:providers config)))`. Read
   by anything that needs to know the *currently configured* model:
   TUI status bar (`screen.clj :: chosen-model-info`), new env
   construction in `open-env!`, etc.
2. **Per-environment snapshot** — `(:router env)`, captured in
   `lp/create-environment` at env-creation time. Read by the iteration
   loop's actual LLM call site
   (`internal/loop.clj :: (svar/ask-code! (:router environment) …)`).
   The env is the unit of routing for in-flight queries — sub-RLM
   forks, extensions, and the iteration engine all use the env's
   router, never the global.

**Implication:** `rebuild-router!` alone does NOT change the model
used by an already-open conversation. The cached env keeps talking
to whatever provider was primary when it was created. Symptom: the
status bar shows the new model, but `query_state.llm_root_model` and
the assistant bubble's footer keep showing the old one.

**Resolution:** `lp/refresh-cached-routers! router` walks the
conversation cache and reseats `:router` on every cached env while
preserving env identity (locks, `:state-atom`, `:db-info`, `:sci-ctx`,
etc. all stay the same object). Every code path that rebuilds the
global router MUST also call this:

- `lp/set-provider!` — SDK / programmatic flow (re-exported from
  `com.blockether.vis.core`).
- `ext/channel_tui/state.clj :: :set-config` event — TUI provider dialog.
- Any future channel that exposes "switch model" UX must do the same.

If you add a new caller of `rebuild-router!` and skip
`refresh-cached-routers!`, you are reintroducing the bug. Treat the
pair as one operation.

## TUI threading model

The TUI runs **two** threads against the Lanterna screen:

1. **Input thread** (`ext/channel_tui/screen.clj :: run-chat!`'s main
   loop) — polls keys via `pollInput`, dispatches events into
   `state/app-db`, and opens modal dialogs through
   `with-dialog-lock`. **It never touches the screen buffer
   directly.** Its only Lanterna call on the hot path is
   `pollInput`, which uses an internal input queue and is
   safe to call concurrently with drawing.

2. **Render thread** (`ext/channel_tui/screen.clj :: render-loop!`,
   spawned daemon thread named `vis-channel-tui-render`) — sleeps on
   `state/render-monitor.wait` and only repaints when one of:
   - `:render-version` advanced (every dispatch except the events in
     `no-render-bump-events` increments it, then calls
     `notifyAll` on `render-monitor`),
   - the terminal got resized.

   On every paint it computes the messages-area layout and pushes
   it back into app-db via `[:set-layout {…}]` (one of the
   `no-render-bump-events` so it doesn't livelock). The input
   thread reads `:layout` for scroll math.

### Draw lock

`ReentrantLock` in `ext/channel_tui/screen.clj`. Sole lock guarding
screen-mutation methods:
`doResizeIfNecessary`, `setCharacter`/`putString`, `refresh`,
`setCursorPosition`. Held by:

- the render thread for the duration of one paint;
- `with-dialog-lock` for the entire dialog session.

While a dialog is open, the render thread's `tryLock` fails and it
backs off (50 ms tryLock timeout + 100 ms monitor wait). The dialog
closes → `:set-dialog-open false` dispatch → `notifyAll` → render
thread acquires the lock and paints over the dialog area.

### Render caches

`fmt-cache` in `ext/channel_tui/render.clj`. Every hot formatter —
`format-answer-with-thinking`,
`format-answer-markdown`, `bubble-height`, `wrap-text` — is
identity-keyed against the source string/vec stored on the
immutable message map. Finalized assistant bubbles never mutate, so
subsequent paints on the same conversation are pure cache hits.
Cache is bounded (cap 512 entries, fully cleared on overflow);
`render/invalidate-cache!` is called from `:update-settings` so
toggling thinking/iterations doesn't leak entries.

### Why this matters

Before this split, every key event — even idle 16 ms ticks with no
input — ran the full render pipeline on the input thread:
`apply-settings` over every message, `format-answer-with-thinking`
over every assistant bubble's trace, `wrap-text` over every line,
twice per frame. With a long conversation that pegged a CPU core
and made keystrokes feel laggy. The dirty-version + dedicated
render thread + memoization combo is what makes typing snappy
again. Keep `render-frame!` confined to the render thread; the
input thread signals via `:render-version` and lets the render
thread paint.
