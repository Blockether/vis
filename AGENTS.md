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
- For frontend changes, verify at least `npm run build` in
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
