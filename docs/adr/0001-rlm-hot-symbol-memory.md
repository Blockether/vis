# ADR 0001: RLM hot symbol memory

Status: Accepted

Date: 2026-05-01

## Context

Vis runs a recursive language model (RLM) inside a per-conversation SCI
sandbox. The model observes recent execution through `<journal>` and a
rendered symbol directory through `<var_index>`.

Unbounded live user symbols weaken the loop:

- `<var_index>` can become a second transcript instead of a hot working set.
- old scratch bindings compete with current state for prompt attention.
- helper functions and raw data can accumulate across turns.
- removing bindings must not destroy provenance or restart/resume data.

The journal remains important: it is the recent code/result/error trace and
must keep executable evidence addressable by `iN.K`. This decision bounds the
hot symbol set; it does not compact the journal into summaries.

## Decision

Vis uses an archive-based hot symbol memory policy per conversation branch.

### Hot-set caps

- `<var_index>` renders at most **100** live user-defined symbols.
- The live SCI sandbox targets at most **100** live user-defined symbols.
- After a final successful accepted `(answer ...)`, automatic archive compacts
  the live user-symbol set down to **80** symbols.
- During a turn, live symbols may temporarily exceed 100. `<var_index>` renders
  the top 100 plus a compact overflow summary.
- The 100/80 policy is per conversation branch/state lineage, not global.

### Archive semantics

Archive means removing a symbol from the current live SCI sandbox and prompt
hot set. Archive never deletes persisted history.

- Manual archive commands are silent housekeeping and return `:vis/silent`.
- Manual archive may archive docstring-protected symbols because it is explicit.
- Automatic archive runs only after a final successful accepted answer.
- Automatic archive does not run during a turn, after cancellation, or after
  error/max-error termination.
- Automatic archive happens after the final iteration and variable snapshot are
  persisted.
- Archive compaction does not create a fake post-archive iteration or variable
  snapshot.

Archive state is runtime-only for now:

- no durable archive tombstones/events are persisted;
- on process restart, persisted restorable symbols may become live again;
- `<var_index>` still enforces its render cap and overflow summaries;
- durable archive tombstones are future work if restart-stable archive state is
  needed.

### Protected symbols

Automatic archive never removes:

- SYSTEM symbols;
- initial sandbox symbols;
- user-defined symbols with non-blank docstrings.

Docstrings are the only user-facing pin/protection mechanism. There are no
separate pin/unpin helpers.

Protected user-defined symbols still count toward the 100/80 live-symbol caps.
If protected symbols alone exceed the cap/target, automatic archive removes all
eligible symbols it safely can and emits memory-pressure diagnostics/nudges
rather than violating protection.

### Archive ranking

Eligible user-defined symbols are ranked by hybrid usefulness. The runtime keeps
more useful symbols and archives least useful symbols until the hot set reaches
the target.

Ranking should consider:

1. recent references;
2. recent definition/redefinition;
3. value kind and size, preferring archive of large raw data before compact
   state or helpers;
4. branch-local provenance.

There is one shared cap for function and data symbols now. The runtime should
collect enough kind/size/reference metadata to support adaptive split budgets
later if real usage justifies it.

### Restore semantics

Archived symbols are automatically restored when referenced.

Target behavior:

1. Before evaluation, scan emitted forms for referenced archived symbols and
   restore them and their dependencies.
2. If evaluation still fails with an unresolved symbol that exists in archived
   memory, restore it and retry once.
3. Restore dependencies in topological order.

Explicit restore commands may also exist. Restore commands are silent in normal
user-facing UI, but restore transitions are visible to the model in `<journal>`.

Restore journal system events:

- are model-facing only, not normal user UI;
- do not consume fake `iN.K` expression ids;
- may appear even when no visible model form evaluated;
- list requested/referenced symbol names only;
- include a compact dependency count when dependency symbols were restored.

Example:

```text
i8 system: restored symbols summarize-hits, route-plan (+3 dependencies)
```

`<var_index>` does not distinguish restored symbols from other live symbols.
Restore is a transition, not a durable symbol status.

### Safe restore only

All restore paths use the same safe-restore rule:

- data vars restore from persisted Nippy values/result payloads;
- data vars are never restored by re-evaluating their original `(def name expr)`;
- functions restore from safe `defn` source when available;
- dependencies restore before dependents;
- arbitrary effectful expressions are never replayed.

Example:

```clojure
(def hits (v/rg ["foo"] "src"))
```

Restoring `hits` binds the persisted value of `hits`; it does not rerun `v/rg`.

```clojure
(defn summarize-hits [hits] ...)
```

Restoring `summarize-hits` may re-evaluate the safe `defn` source.

If a symbol cannot be safely restored, the runtime marks that symbol version
unavailable. It does not replay unsafe source and does not create a placeholder.
The model must recreate/redefine a new version intentionally if it needs that
resource.

Unavailable restore status is cached by `(symbol, version)` so repeated
references do not spam restore attempts. A newer persisted version may be tried
again.

### Prompt surfaces

`<journal>` remains the recent execution trace: code blocks, results, errors,
stdout/stderr, timings, and restore system events. It is not replaced by
summaries.

`<var_index>` is the hot symbol directory:

- render top live symbols, max 100;
- if live symbols exceed 100, include names-only overflow summary;
- include compact archive summary with count and top archived names;
- include unavailable count only, not unavailable names or values;
- archived/overflow summaries render names only, using unqualified symbol names
  unless collisions require disambiguation.

No archived values are rendered in `<var_index>`.

### Symbol history surface

Do not add a new symbol-memory section to `v/inspect`.

The symbol-memory browser is `var-history`:

- `(var-history 'sym)` returns history for a known symbol;
- `(var-history)` returns a compact symbol index;
- the index includes archived/live/unavailable status, version, kind, and
  provenance;
- the index does not render large values.

Provenance must remain first-class. Symbol-history entries should retain enough
information to answer where a symbol came from and how it can be restored:

- conversation id / soul id;
- conversation state / branch lineage;
- conversation turn id;
- iteration id and visible `iN.K` block reference when available;
- symbol name and version;
- source expression for safe source-backed symbols;
- result/value storage mode such as Nippy data or safe `defn` source;
- dependency symbols;
- timestamps for definition, archive, restore, and last reference when known;
- unavailable reason and recreate guidance when restore is unsafe/unavailable.

## Consequences

- Prompt memory stays bounded without deleting history.
- The model can rely on recent execution evidence in `<journal>` and hot symbols
  in `<var_index>`.
- Docstrings become the intentional durable-memory marker.
- Archive/restore bookkeeping is mostly invisible to users but observable to the
  model when restore changes the live symbol set.
- Restart currently restores all safe persisted symbols; archive state does not
  survive process restart until durable memory events are introduced.

## Future work

- Persist archive/restore tombstones if restart-stable archive state becomes
  necessary.
- Add reference tracking for better hybrid ranking.
- Add adaptive function/data symbol budgets if one shared cap proves too coarse.
- Implement pre-eval restore scanning; unresolved-symbol retry can be an interim
  fallback.
