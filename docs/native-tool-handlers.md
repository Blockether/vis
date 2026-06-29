# Native tool handlers + non-bound symbols

A `vis/symbol` can play up to three **independent** roles. Until now they were
fused: a native tool *had* to be a Python verb, because its only executor was
"synthesize Python → run in GraalPy → call the bound fn." This spec decouples
them so a symbol can be a **native-tool-only control verb** — a clean `tool_use`
with an op-card that never round-trips through the interpreter.

## The three roles

| Role | Declared by | Effect |
|---|---|---|
| **Native tool** | `:native-tool {:name :description :schema :render :color-role}` | advertised in `:tools`; the model calls it as a `tool_use`; renders an op-card |
| **Python verb** | bound into the GraalPy env (default) | the model can call it in code: `await cat("x")` |
| **Executor** | `:native-tool :handler` (new) OR synthesized Python (default) | how a `tool_use` actually runs |

## New spec on `vis/symbol`

```clojure
(vis/symbol #'skill
  {:symbol    'skill
   :bind?     false                  ; NEW — do NOT bind into the GraalPy env (no Python verb)
   :active-fn (fn [env] (toggles/enabled? :vis/harness-skills))  ; NEW — dynamic per-symbol gate
   :tag       :observation
   :native-tool
   {:name        "skill"
    :description "Load a harness SKILL on demand …"
    :schema      {:type "object"
                  :properties {"name" {:type "string"}}
                  :required ["name"]}
    :handler     (fn [env input] …)  ; NEW — direct Clojure executor (fn [env input] -> result-value)
    :render      (fn [result] {:summary … :body …})
    :color-role  :tool-color/meta}})
```

### `:bind?` (default `true`)
When `false`, the symbol is **not** `putMember`'d into the Python env by
`sync-active-extension-symbols!`. It therefore never appears as a Python name,
in `apropos`, in the engine prompt block, or in protected-names. It exists
**only** as a native tool. (For a non-bound symbol a `:native-tool :handler` is
required — there is no env fn to fall back to.)

### `:active-fn` (optional, `(fn [env] -> bool)`, default `true`)
A **per-symbol activation predicate** evaluated against the current `env`. When
it returns false the symbol is INACTIVE this iteration: its native tool is **not
advertised** in `:tools` and its handler is **not dispatchable**
(`native-tool-schemas`/`-handlers` skip it). `env` may be nil — toggle-style
predicates ignore it.

This is the lever for **sub-extension gating**: one extension with several
toggles (the harness has `:vis/harness-skills` + `:vis/harness-agents`, active if
*either*). Extension-level activation already gates whole extensions; `:active-fn`
gates a single symbol within one — e.g. `skill` advertises only when
`:vis/harness-skills` is on, even while the extension is alive for `agents`. It
replaces the old "advertise it but soft-fail when called" pattern.

`:active-fn` is the **one gate** for both native tools and bound Python verbs: a
bound verb whose `:active-fn` is false is *removed* from the env by
`sync-active-extension-symbols!` (not bound, not in `apropos`).

### `:inject-env?` (optional boolean, default `false`)
When true, the live `env` is prepended as the call's **first arg** — the impl is
`(fn [env & model-args])`. Orthogonal to gating: env-injection is mechanical.

This exists because gating and env-injection used to be **conflated** in a
per-verb `:before-fn` (it both refused-when-off and injected env). Splitting them
gives one knob each — `:active-fn` to gate, `:inject-env?` to hand over `env` —
and leaves `:before-fn`/`:after-fn`/`:on-error-fn` as *pure* lifecycle hooks for
genuine call interception (rare), never gating. `agent` is the reference:
`:active-fn` (toggle) + `:inject-env? true`, **no `:before-fn`**.

### `:native-tool :handler` (optional)
`(fn [env input] -> result-value-or-envelope)`.
- **Present** → the loop dispatches the `tool_use` **straight to this fn** with
  the parsed JSON `input` map. No `tool-call->python-source`, no GraalPy.
- **Absent** → today's behavior: synthesize `name(args)` and run it through
  GraalPy via the bound fn (cat / rg / patch / move / find are unchanged).

The handler returns either a bare result value (wrapped as `{:result v}`) or a
full envelope `{:result … :error …}`. The loop normalizes it to the **same**
shape `execute-code` returns (`{:result :error :lru :duration-ms :timeout?
:execution-*-at-ms}`), so everything downstream — `tool_result` pairing by
`:svar/tool-call-id`, the `:result-render` op-card, DB persistence, the
append-only `<results>` trailer, resume — is byte-identical to a Python tool.

## How dispatch forks (loop)

1. **Block build** (per `tool_use`): a tool whose symbol declares a
   `:native-tool :handler` produces a block carrying `:vis/native-handler` (the
   fn) + `:vis/native-input` (the parsed input) and a **display-only**
   `:source` — the synthetic call repr `skill("code-review")` — so the trace,
   DB row, and replay still show *what was called*. The op-card paints over it
   (code chrome already hides on a successful tool form).
2. **Execute**: at the eval branch, `(:vis/native-handler entry)` present →
   call `(handler env input)` and normalize to the result envelope; otherwise →
   `execute-code` (GraalPy) as today.

## Confinement

A bound tool inherits GraalPy path-confinement. A `:handler` runs in raw
Clojure, so **it owns its own safety** (e.g. `skill` only reads from discovered
skill dirs). The spec requires handlers to confine themselves.

## Why

- Control / meta verbs (`skill`, later `agent`) are discrete actions, not data
  the model composes in `gather(...)` loops. A native tool with a card is the
  natural fit, and skipping GraalPy is cleaner + faster for them.
- Fully backward-compatible: no `:handler` → the existing synthesized-Python
  path; `:bind?` defaults `true`.
- It directly retires the "native calls round-trip through synthesized Python"
  cost for the tools that never needed the interpreter.

## Rollout

`skill` is the reference implementation. `agent` is the obvious next candidate
(same shape; heavier because the handler runs a sub-loop) — done after `skill`
proves the path.
