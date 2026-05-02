# RLM Memory

Vis gives the model several memory surfaces. They are intentionally different.
Keeping those boundaries clear is what makes the recursive loop usable.

## Memory tiers

| Surface | Purpose | Lifetime |
|---------|---------|----------|
| lexical values | scratch computation inside one emitted form | current form only |
| `<journal>` | recent execution evidence: code, results, errors, stdout/stderr | rolling prompt window |
| `<var_index>` | hot live user-defined symbols | current conversation sandbox |
| `var-history` | cold symbol history and provenance | persisted conversation history |

## Journal

`<journal>` is the recent execution trace. It keeps the blocks the model emitted
and the results the host observed, addressable as `iN.K`.

It should contain:

- executed Clojure source;
- result/error per evaluated top-level form;
- stdout/stderr;
- timings;
- model-facing restore system events.

It is not a summary cache. The model may write summaries into vars, but the
journal remains the recent evidence stream.

## Var index

`<var_index>` is the hot symbol directory for the live SCI sandbox. It is not a
second transcript.

Rules:

- render at most 100 live user-defined symbols;
- render type-aware previews, not unlimited values;
- show compact overflow/archive summaries when needed;
- do not render archived values;
- do not render unavailable symbol names, only an unavailable count.

Example shape:

```clojure
<var_index>
;; v=3 scope=live n=2
(def turn-state {:keys [:phase :findings]})
;; v=1 scope=live
(defn summarize-hits [hits] "Summarize search hits." …)

;; overflow-live-symbols: 12
;; hidden live symbols: old-hits, tmp-plan, parse-result
;; archived-symbols: 37, unavailable: 2
;; recent archived: summarize-auth, route-plan, old-findings
;; use (var-history) to browse symbol history/provenance
</var_index>
```

Overflow and archived summaries are names-only. Names are unqualified unless a
collision requires qualification.

## Hot symbol caps

Hot symbol memory is per conversation branch.

- hard render/live cap: 100 user-defined symbols;
- successful-answer compaction target: 80 user-defined symbols;
- one shared budget for function and data symbols;
- functions and data may get adaptive sub-budgets in the future.

During a turn, Vis may temporarily exceed 100 live user-defined symbols. This is
allowed so automatic restore does not disrupt evaluation. After a final
successful accepted `(answer ...)`, Vis archives eligible symbols until the live
hot set is back near 80.

Automatic archive does not run during the turn, after cancellation, or after
error/max-error termination.

## Archive, not forget

Archive removes a symbol from the current live SCI sandbox and from prompt hot
memory. It does not delete persisted history.

Automatic archive:

- runs only after a final successful accepted answer;
- runs after the final iteration and variable snapshot are persisted;
- does not create fake iteration rows or housekeeping snapshots;
- is not shown in normal user UI;
- is not written as a `<journal>` system event;
- is visible through logs and symbol history behavior.

Manual archive commands should be silent housekeeping:

```clojure
(v/archive-symbol! 'old-hits)
(v/archive-symbols! '[old-hits tmp-plan])
```

They return `:vis/silent`, do not appear in normal code/result traces, and do not
create journal events. Manual archive may archive docstring-protected symbols
because it is explicit.

## Protection

Automatic archive never removes:

- SYSTEM symbols;
- initial sandbox symbols;
- user-defined symbols with non-blank docstrings.

Docstrings are the only user-facing protection mechanism. There are no separate
pin/unpin helpers.

```clojure
(def migration-plan
  "Durable plan for the schema migration."
  {:steps [...]})
```

Protected symbols still count toward the 100/80 caps. If protected symbols alone
exceed the target, Vis archives what it can and emits memory-pressure diagnostics
rather than silently violating protection.

## Archive ranking

Eligible user-defined symbols are archived by hybrid usefulness. Ranking should
prefer keeping:

1. recently referenced symbols;
2. recently defined/redefined symbols;
3. compact state and helper functions over large raw data;
4. branch-local symbols relevant to the current conversation state.

## Restore

Archived symbols are restored automatically when referenced.

Target behavior:

1. Before evaluating emitted forms, Vis scans for archived symbol references and
   restores the referenced symbols plus dependencies.
2. If evaluation still fails with an unresolved archived symbol, Vis restores it
   and retries once.
3. Dependencies restore before dependents.

Explicit restore commands may also exist. Restore is silent in normal user UI but
visible to the model as a compact journal system event.

```text
i8 system: restored symbols summarize-hits, route-plan (+3 dependencies)
```

Restore events:

- list requested/referenced symbols only;
- include dependency count when applicable;
- do not list values or shapes;
- may appear even if no visible model form evaluated;
- do not create fake `iN.K` expression ids.

After restore, the symbol is just live again. `<var_index>` does not mark it as
restored.

## Safe restore

All restore paths use the same safety rule.

Data vars restore from persisted values, typically Nippy payloads. They are never
restored by replaying the original `def` expression.

```clojure
(def hits (v/rg ["foo"] "src"))
```

Restoring `hits` binds the stored value. It does not rerun `v/rg`.

Function vars restore from safe `defn` source when available:

```clojure
(defn summarize-hits [hits]
  ...)
```

Arbitrary effectful source is never replayed. If a symbol cannot be safely
restored, Vis reports that symbol version as unavailable. The model must recreate
or redefine a new version intentionally.

Unavailable restore status is cached by symbol and version. A newer persisted
version may be tried again.

## Restart behavior

Archive state is runtime-only for now. Vis does not persist archive tombstones.

On process restart, safe persisted symbols may be restored into the live sandbox
again, even if the count exceeds 100. `<var_index>` still renders only the top
100 plus overflow/archive summaries. The next final successful answer compacts
back toward 80.

Durable archive/restore tombstones are future work.

## Symbol history and provenance

`v/inspect` is not the symbol-memory browser. Use `var-history`.

- `(var-history 'sym)` returns history for a known symbol.
- `(var-history)` returns a compact symbol index.

The zero-arity index should include status and provenance without rendering large
values. Provenance matters because every restored or archived symbol should be
traceable to its origin.

A symbol-history entry should carry, where available:

- symbol name;
- version;
- status: live, archived, unavailable;
- kind: data, function, or other;
- conversation and branch/state identifiers;
- turn id;
- iteration id and visible `iN.K` block reference;
- definition timestamp;
- source expression for safe source-backed symbols;
- value storage mode, such as Nippy data or safe `defn` source;
- dependencies;
- last-reference / archive / restore timestamps when tracked;
- unavailable reason and recreate guidance.

This keeps cold memory programmatic and provenance-rich without bloating the
prompt.

## ADR

See [ADR 0001: RLM hot symbol memory](../../adr/0001-rlm-hot-symbol-memory.md)
for the accepted architecture decision.
