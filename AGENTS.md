# AGENTS.md

Guidance for humans and coding agents working on Vis. This file focuses on the
**memory / CPU investigation** tooling — how the runtime observes itself and how
to read what it emits.

## Vis Companion UI (`apps/vis-companion`)

Vis Companion is one universal product for **web, iOS, and Android**. Every UI
change must be polished and fully usable at both desktop and phone widths; never
treat mobile as a reduced or deferred version of the web interface.

- Use **Tailwind CSS v4 utilities exclusively** for layout, spacing, typography,
  color, borders, states, and responsive behavior. Do not add component-specific
  CSS classes, CSS modules, CSS-in-JS, inline style objects, or another styling
  framework.
- Keep `src/index.css` limited to Tailwind imports, shared gateway/TUI theme token
  declarations, bundled font setup, and unavoidable document-level base rules.
  Express component styling in JSX with Tailwind utilities.
- Design mobile-first, then add deliberate `sm:`, `md:`, and wider adaptations.
  Check narrow phones, desktop widths, wrapping/overflow, touch targets, safe-area
  insets, virtual-keyboard/composer behavior, and both light and dark gateway
  themes before considering UI work complete.
- Preserve the Vis TUI's information hierarchy, palette, role colors, compactness,
  and transcript semantics while adapting interaction and density appropriately
  for touch and pointer input. Visual parity does not justify a broken mobile
  layout.
- The companion has NO eslint. React Compiler is the linter: `npm run lint`
  runs `scripts/react-compiler-lint.mjs` (compiler analysis over every `src`
  file, fails on any non-Todo diagnostic) and `vite build` runs the compiler
  with `panicThreshold: 'critical_errors'`. Never reintroduce eslint.
- For frontend changes, verify at least `npm run lint` and `npm run build` in
  `apps/vis-companion`; when browser tooling is available, inspect one phone-size
  and one desktop-size viewport.

## Memory & CPU monitoring

Vis has three layers of self-observation. The first two are **on by default**;
the third is opt-in deep profiling.

| Layer | What it tells you | Default | Where it lands |
| --- | --- | --- | --- |
| Per-block sampler | heap growth + CPU per Python block | **on** | `~/.vis/vis-pyblock.log` |
| Env-reaper sweep | gateway cache heap/CPU each sweep | **on** | `~/.vis/logs/vis.log` |
| JFR profiler | method-level hotspots, alloc call trees | opt-in (`--jfr`) | `~/.vis/logs/vis-*.jfr` |

You do **not** need `--jfr` for everyday memory/CPU monitoring — the first two
layers already give heap and CPU numbers. Reach for `--jfr` only when the
numbers point to a burst and you need to know *which code* caused it.

### 1. Per-block heap + CPU sampler (`~/.vis/vis-pyblock.log`)

Every Nth Python block (`VIS_PY_BLOCK_LOG_EVERY`, default **25**) the sandbox
appends one line, bypassing the async log handler so it is visible even when the
JVM is pegged:

```
2025-07-21T…Z  python-block-eval blocks=100 heap=812MB/2048MB old=610MB gc=179 cpu=proc0%/sys16% load=3.91 Δ=+42MB (~+430KB/block over last 25)
```

- `heap=used/max`, `old=` old-gen used, `gc=` total GC count.
- `cpu=proc%` this JVM's CPU, `sys%` whole-machine CPU, `load=` 1-min load avg.
- `Δ=` heap change since the previous sample and the per-block slope — a steady
  positive slope that never falls after GC is the leak signal.

### 2. Env-reaper sweep summary (`~/.vis/logs/vis.log`)

The gateway's environment reaper logs **once per sweep** (even when it evicts
nothing), so gateway CPU/heap bursts show up with no eviction:

```
env-reaper evicted=0 (ttl=0 lru=0) heap=1% cpu=0% pressure=false cache=0
```

- `evicted=` total this sweep, split into `ttl=`/`lru=`.
- `heap=%` heap used, `cpu=%` process CPU, `pressure=` under-memory-pressure
  flag, `cache=` live cached environments.

### The master switch: `VIS_MEM_LOG`

Both default layers share one flag. They are **enabled unless** `VIS_MEM_LOG` is
a falsey token (`0`, `false`, `off`, `no`). Anything else (or unset) = on.

```bash
VIS_MEM_LOG=0 vis …    # silence both memory logs
VIS_PY_BLOCK_LOG_EVERY=5 vis …   # sample every 5th block instead of 25 (0 disables)
```

### 3. JFR deep profiler (`--jfr`)

`vis --jfr …` sets `VIS_JFR`, which starts one JDK Flight Recording per process.
The var is inherited by the spawned gateway daemon, so client and gateway each
get their own role-tagged dump under `~/.vis/logs/`:

```
vis-client-<pid>-<ts>.jfr     ← the TUI / web / one-shot process
vis-gateway-<pid>-<ts>.jfr    ← the long-lived gateway daemon
```

Recordings dump on exit; the newest few are kept and older ones pruned on each
start. Read them with the JDK `jfr` tool or open in JMC:

```bash
jfr print --events jdk.ExecutionSample ~/.vis/logs/vis-gateway-<pid>-<ts>.jfr   # CPU hotspots
jfr print --events jdk.ObjectAllocationSample ~/.vis/logs/vis-*.jfr             # allocations
```

On the compiled native binary JFR works only when built with
`--enable-monitoring=jfr`; on the JVM it always works. It never throws and never
blocks startup — if unavailable it silently no-ops.

### Investigation recipes

- **Suspected Python-side leak:** tail `~/.vis/vis-pyblock.log`; watch the `Δ`
  slope across many blocks. Rising `old=` that GC (`gc=` climbing) never claws
  back = a real leak. Lower `VIS_PY_BLOCK_LOG_EVERY` for finer resolution.
- **Gateway CPU burst with no eviction:** watch the `cpu=%`/`heap=%` in the
  per-sweep `env-reaper` lines in `~/.vis/logs/vis.log`.
- **"Which code is hot?":** re-run the same workload with `--jfr`, reproduce the
  burst, then `jfr print --events jdk.ExecutionSample` the matching role's dump.
  Compare the client vs gateway dumps when a client seems to wait on the gateway.

## Feature toggles

Every feature toggle is part of the public product surface, so treat these as
hard requirements when you add or change one:

- **Snake_case string ids only.** Toggle ids are plain snake_case strings
  (`reasoning_level`, `shell`) — never keywords, never namespaced, no kebab or
  slashes. The registry/spec rejects anything else.
- **Document it.** Give the `register-toggle!` call a clear description and
  keep any related docstrings/`sandbox.md`/`configuration.md` wording in sync;
  a toggle with no user-facing explanation is incomplete.
- **Keep it in the config spec.** `toggles:` is a validated top-level block in
  `config_spec.clj` (name→scalar map). Anything that changes how toggles are
  read/written must keep that schema — and its round-trip through
  `->yaml-safe`/`keywordize-yaml` — correct.
- **Configurable via vis.yml.** A toggle must hydrate from the merged config
  (`toggles/hydrate-from-config!`) so `toggles: {id: value}` works and is
  `/reload`-live; project `vis.yml` overrides the machine `state.yml`.
- **Test all three surfaces.** Cover the registry/spec, the vis.yml hydrate +
  coercion, and the settings wire (TUI dialog / gateway `/v1/settings`) so a
  new toggle appears and round-trips everywhere, not just in code.

## Sandbox Python shims

GraalPy in the `python_execution` sandbox cannot pip-install CPython packages
that need native wheels. A shim makes `import X` work anyway by publishing a
pure-Java-backed or pure-Python module. Shims live in
`src/com/blockether/vis/internal/foundation/shim_*.clj` (`shim_pil.clj`,
`shim_fonttools.clj`, `shim_sqlite3.clj`, …). Treat these as hard requirements
when you add or change one:

- **One shim per file, registered once.** A shim ns requires
  `com.blockether.vis.core :as vis`, defines a private `…-shim-src` Python
  preamble string, then `(def vis-extension (vis/extension {:ext/name … :ext/sandbox-shims [{:shim/name "x" :shim/preamble …-src}]}))`
  and ends with `(vis/register-extension! vis-extension)`. Add the ns to the
  `builtin-extension-nses` vector in `extension.clj` (~L3518-3532) or it never
  loads.
- **Prefer lazy over eager — it is not optional here.** `capture-shim-triggers`
  (`env_python.clj` ~L2347-2423) runs each preamble ONCE in a throwaway probe
  context and diffs `sys.modules` + `builtins`: `autoload` = new `builtins`
  names, `provides` = new top-level modules whose name is in
  `#{shim-id} ∪ autoload`. A shim that publishes NEITHER stays **eager** and
  re-runs its whole preamble on every context init — a real per-block cost for
  a heavy shim. To be lazy, the preamble must staple its import name(s) onto
  `builtins` when the module name differs from the shim id. Example: the
  `fonttools` shim (id `"fonttools"`) staples `fontTools` and `brotli`, so
  `capture-shim-triggers` reports
  `{:provides ["brotli" "fontTools"] :autoload ["brotli" "fontTools"]}` and it
  loads only on first `import`. Verify in the REPL: create a context, assert
  the module is absent from `sys.modules` at init, then present after `import`.
- **The trigger cache invalidates on preamble change.** Lazy triggers are
  memoized to `~/.vis/cache/shim-triggers.json`, keyed by a hash of ALL
  preambles, so adding or editing a shim recomputes the set — do not hand-edit
  the cache.
- **Docstring the boundary.** Say what `import X` now works and, plainly, what
  is NOT supported (e.g. fonttools is WOFF2->TTF decompress-only: no
  `brotli.compress`, no WOFF1, not the full `ttLib.TTFont` API). No pip, no
  native wheel, no host binary unless the shim is bindings-backed.
- **Add a compat test.** `test/com/blockether/vis/internal/<name>_compat_shim_test.clj`
  drives a real `ep/create-python-context`, evals Python that exercises the
  module, and expects observable results — never just "import didn't throw".


## Gateway wire contract

The HTTP/SSE gateway wire has ONE dumb, deterministic boundary
(`gateway/wire.clj`). Treat it as a hard requirement:

- **Wire keys are snake_case STRINGS, never keywords.** `wire/->wire`
  encodes every engine value; keyword/symbol map keys become snake_case
  strings. Never hand-emit keyword keys onto the wire, and never JSON-encode
  around `wire/json-str`.
- **Boolean flags are `is_<foo>` on the wire and `:is-<foo>` in engine EDN — a
  plain mechanical `_`↔`-` mirror.** `is_authenticated` ↔ `:is-authenticated`,
  `is_unlimited` ↔ `:is-unlimited`. So the ONE shared connection verdict is
  `is_authenticated`/`:is-authenticated` end-to-end (dot, status dialog,
  routing, every provider). NEVER invent a `:foo?` alias for a wire flag and
  NEVER translate `is_foo`→`:foo?` at the gateway boundary — that asymmetry is
  exactly what drifts and makes a live-connected provider read red.
- **No bespoke per-endpoint `<-wire` restore that renames individual fields.**
  Inbound, keywordize the canonical string-keyed map with the uniform
  mechanical `_`→`-` (the exact inverse of `wire-key`), or read the string keys
  directly. A hand-written restore that must mirror `wire-key` by hand will
  drift from it.
- **Grep guard.** `:authenticated?` (or any `:foo?` that shadows a wire flag)
  must not reappear for a wire field; the gateway/TUI/routing all read the
  `:is-<foo>` mirror. (The Python-extension `py-key->kw` adapter is a SEPARATE
  boundary — Python identifiers can't carry `?` — and keeps `is_authenticated`
  verbatim as `:is-authenticated`.)

## Rendering TUI dialogs to ASCII (headless virtual terminal)

To SEE what a dialog actually looks like—real geometry, margins, scrollbar—never
eyeball it. Render the real paint code into a headless Lanterna
`DefaultVirtualTerminal` in the `vis-channel-tui` REPL and dump the back-buffer.
This is the source of truth for every layout question.

Key facts:

- Every dialog routes through `dialogs/draw-dialog-chrome!` (border, accent
  title text, separators) and returns
  `{:left :top :right :bottom :inner-w :inner-h}`.
- Dialogs paint directly on `t/terminal-bg`. They have no panel tint, title-bar
  fill, or drop shadow. `t/dialog-bg`, `t/dialog-title-bg`, and
  `t/dialog-shadow` are compatibility aliases of `t/terminal-bg`.
- Full-bleed dialogs (magit, sessions, theme, copy) size via
  `render/golden-dialog-size`, clamped to `(- cols 8) × (- rows 9)`.
- `box-left = (quot (- cols box-w) 2)` and
  `box-top = (quot (- rows box-h) 2)`.
- Scrollbar via `scrollbar/draw!`; body text via `primitives/put-str!`.

Recipe (run in the `vis-channel-tui` REPL):

```clojure
(require '[clojure.string :as str]
         '[com.blockether.vis.ext.channel-tui.dialogs :as dlg]
         '[com.blockether.vis.ext.channel-tui.primitives :as p]
         '[com.blockether.vis.ext.channel-tui.scrollbar :as sb]
         '[com.blockether.vis.ext.channel-tui.theme :as t])
(import '[com.googlecode.lanterna TerminalSize]
        '[com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]
        '[com.googlecode.lanterna.screen TerminalScreen])

(let [cols 120 rows 40
      scr  (doto (TerminalScreen.
                   (DefaultVirtualTerminal. (TerminalSize. cols rows)))
             (.startScreen))
      g    (.newTextGraphics scr)]
  (p/set-bg! g t/terminal-bg)
  (p/fill-rect! g 0 0 cols rows)
  (let [{:keys [left top right bottom]}
        (dlg/draw-dialog-chrome! g cols (dec rows) "Git — main"
                                 (- cols 4) (max 1 (- rows 5)))]
    (sb/draw! g {:col (dec right) :top (+ top 3) :track-h (- bottom top 5)
                 :total-h 60 :inner-h (- bottom top 5) :scroll 6})
    (.refresh scr)
    (assert
      (every? true?
              (for [y (range rows) x (range cols)]
                (= t/terminal-bg
                   (.getBackgroundColor (.getBackCharacter scr (int x) (int y)))))))
    (println
      (str/join "\n"
        (for [y (range rows)]
          (str/replace
            (apply str
              (for [x (range cols)]
                (or (some-> (.getBackCharacter scr (int x) (int y))
                            .getCharacterString)
                    " ")))
            #"\s+$" "")))))
  (.stopScreen scr))
```

At 120×40 this yields a 112×30 box at left 4 / top 4, with equal horizontal
margins and no background-colored shadow. The assertion guards the flat-surface
contract.

### Full frame—outer chrome around the dialog

The dialog is only the middle band. To render everything the user sees, paint
`header/draw-header!`, the dialog, `render/draw-input-box!`, and
`footer/draw-footer!` into the same buffer before refreshing and dumping it.
Use a minimal app-db:

```clojure
{:session (java.util.UUID/randomUUID)
 :messages []
 :settings {}
 :input {:lines ["git status is clean"] :crow 0 :ccol 0}}
```

At 120×42 the magit box is 112×30 (left 4 / top 5 / bottom 34), the header tab
bar occupies rows 0–2, and the composer plus footer remain below the frame.
