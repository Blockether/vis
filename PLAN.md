# PLAN — Collapse the model-facing tool surface to `python_execution` alone

*One jail, one verb: Python is the whole instrument, and everything else is a name inside it.*

## Context

### State before

The provider request today advertises **eighteen** JSON-schema tools. They are assembled in
`src/com/blockether/vis/internal/loop.clj:4843` (`native-tools`) from two sources:

| Tool | Declared at | Kind |
| --- | --- | --- |
| `struct_index` | `src/com/blockether/vis/internal/foundation/editing/core.clj:6180` | extension symbol, `:native-tool? true` |
| `cat` | `src/com/blockether/vis/internal/foundation/editing/core.clj:6272` | extension symbol |
| `grep` | `src/com/blockether/vis/internal/foundation/editing/core.clj:6296` | extension symbol |
| `patch` | `src/com/blockether/vis/internal/foundation/editing/core.clj:6341` | extension symbol |
| `struct_patch` | `src/com/blockether/vis/internal/foundation/editing/core.clj:6746` | extension symbol |
| `struct_nodes` | `src/com/blockether/vis/internal/foundation/editing/core.clj:6966` | extension symbol |
| `format_code` | `src/com/blockether/vis/internal/foundation/language_surface.clj:1081` | extension symbol |
| `lint_code` | `src/com/blockether/vis/internal/foundation/language_surface.clj:1111` | extension symbol |
| `run_tests` | `src/com/blockether/vis/internal/foundation/language_surface.clj:1135` | extension symbol |
| `repl_eval` | `src/com/blockether/vis/internal/foundation/language_surface.clj:1178` | extension symbol |
| `repl` | `src/com/blockether/vis/internal/foundation/language_surface.clj:1208` | extension symbol |
| `skill` | `src/com/blockether/vis/internal/foundation/harness/core.clj:180` | extension symbol |
| `mcp__call` | `src/com/blockether/vis/internal/foundation/mcp/core.clj:1114` | extension symbol |
| `search` | `extensions/common/vis-foundation-search/src/com/blockether/vis/ext/foundation_search/core.clj:1968` | extension symbol |
| `apropos` | `src/com/blockether/vis/internal/loop.clj:4799` | engine tool |
| `doc` | `src/com/blockether/vis/internal/loop.clj:4821` | engine tool |
| `session_fold` | `src/com/blockether/vis/internal/loop.clj:4765` | engine tool |
| `python_execution` | `src/com/blockether/vis/internal/loop.clj:4735` | engine tool |

Every one of those fourteen extension symbols is ALSO a bare Python name in the sandbox: no symbol in the
tree declares `:engine-bound? false` (verified by grep over `src/**` and `extensions/**`), so the entire
native surface is a duplicate advertisement of names that already exist inside `python_execution`.

The machinery that exists ONLY to support that duplicate advertisement:

- **Declaration + validation** — `src/com/blockether/vis/internal/extension.clj`:
  `:ext.symbol/native-tool?` and `:ext.symbol/schema` specs at `:421-500`; the flat-form assembly at
  `:1428-1470`; five refusal rules at `:1495-1553` (`native-tool-missing-schema`,
  `native-tool-nonportable-schema`, `native-tool-missing-description`, `native-tool-missing-result`,
  `native-tool-result-in-description`, `native-tool-result-has-label`, `native-tool-over-budget`).
- **Projection** — `extension.clj:993` `native-tools-for`, `:1027` `native-tool-replay-policies`,
  `:1036-1146` `wire-schema` / `advertise-tool` / `native-tool-schemas`, `:1148` `native-tool-handlers`,
  `:1173` `native-tool-call-shapes`, `:1187`/`:1199` finish/start call renderers, `:2157`
  `native-tool-finish-call-renderers-by-op`, `:1211` `native-tool-tags`, `:1226` `symbol-bound?`.
- **Dispatch** — `loop.clj:978-1060` (native `:handler` execution under the
  `:vis/native-tool-timeout` watchdog), `:4707` `finalize-engine-native-tool`, `:4838`
  `engine-native-tool-call-shapes`, `:4856` `advertised-native-capability-names`, `:4867` `py-literal` +
  `tool-call->python-source` (synthesizing a Python call from tool-call JSON), `:5169`
  `native-tool-call-block`, `:5370-5373` and `:5516-5527` in the request/response path,
  `:8257` replay policies, `:9413` renderer lookup.
- **Rendering** — `src/com/blockether/vis/internal/form.clj:43-63` (native-tool badge identity and the
  `python_execution` → `RESULT` label table), `:158-180` `native-tool-form?`;
  `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render.clj:2570, 3624,
  5201-5645` (native-tool cards, per-tool op-cards, error compaction, code-chrome hiding);
  `.../channel_tui/chat.clj:214, 918`.
- **Prior-result recovery (`ntr`)** — `resources/vis-python/async_runtime.py:1884-1890, 2103-2550`;
  host callbacks in `src/com/blockether/vis/internal/loop.clj:11156-11214`; delegates in
  `src/com/blockether/vis/internal/persistance.clj:616-634`; storage/queries in
  `extensions/persistance/vis-persistance-sqlite/src/com/blockether/vis/ext/persistance_sqlite/core.clj:4548-4740`;
  guard lists in `src/com/blockether/vis/internal/env_python.clj:886, 997-1002, 2117-2122`.
- **Three documentation verbs and none of them searches the documents** — `apropos`/`doc` are
  GraalPy `ProxyExecutable`s in `src/com/blockether/vis/internal/env_python.clj:1000-1130` plus
  `resources/vis-python/apropos_table.py`, and `apropos` matches a substring against the NAME and
  GROUP only — never a gist, never a `__vis_docs__` body. Product documentation is a SEPARATE verb
  `vis_docs` in `src/com/blockether/vis/internal/foundation/self_docs.clj:52-110`, and skill bodies
  (`foundation/harness/discovery.clj:531`) and MCP tool descriptions are searchable from nowhere at
  all.
- **Prompt text that exists to arbitrate between surfaces** —
  `src/com/blockether/vis/internal/prompt.clj:209-269` (`CORE_SYSTEM_PROMPT`, in particular `:214`
  "`vis_docs()` is product docs", `:217` "Native descriptions and JSON Schemas are authoritative",
  `:219-229` the batching/routing paragraphs), plus `foundation/introspection.clj:1310` and
  `foundation/language_surface.clj:110-116` (the `LANGUAGE TOOLS` block).

Measured cost of the duplicate surface: the eighteen advertised schemas plus their projected
`Raw result:` contracts are re-sent on EVERY provider request in the turn, and the per-symbol budget
check at `extension.clj:1534-1553` exists purely to keep that block from growing without limit.

### Root problem

A tool schema is a weak, non-polymorphic description of a capability: it cannot express a union, a
callable, a lazily-composed pipeline, or "do these four things and print only the difference". Every
capability we ship therefore exists TWICE — once as a rich Python name, once as a flattened JSON
schema — and the model spends its routing budget choosing between two spellings of the same thing.
Because a native call is cheap to emit and its result lands whole in context, the presence of the
native surface actively pulls the model AWAY from the surface that can filter. The duplication also
propagates: it forces `ntr` (a persistence-backed way to get a native result back after it was
folded), the op-card renderers, the badge vocabulary, six extension-registry refusal rules, and a
prompt paragraph whose only job is to arbitrate between the two surfaces.

### What we solve

- The model is offered exactly ONE tool, `python_execution`. Everything else is a Python name.
- Function discovery moves entirely to `apropos()` / `doc()` inside the sandbox, paid for only when
  the model actually asks.
- `doc()` becomes the single documentation verb: bare `doc()` is the index, `doc(name)` is a
  function contract, `doc(slug)` is a Vis documentation page, `doc(skill)` is its whole `SKILL.md`.
  `vis_docs` is deleted.
- `ntr` and the native-result persistence lookups are deleted: with one tool, every result the model
  sees is what its own program printed.
- Rendering collapses to one card shape (code + printed output), so the TUI stops carrying per-tool
  op-card identity.

### What we explicitly do NOT solve

- We do not remove any CAPABILITY. `grep`, `struct_index`, `struct_nodes`, `struct_patch`,
  `run_tests`, `repl_eval`, `skill`, `search`, `mcp__call`, `shell`, … all remain, as Python names.
- We do not touch the gateway wire contract, human-input contract, or channel protocols beyond the
  form fields that only existed to identify a native tool.
- We do not add a compatibility flag, a toggle, or a "legacy native tools" mode. There is no
  migration path: the old surface is deleted (repo rule: no backward compatibility).

### Alternatives considered

1. **Keep a small native surface (`grep`, `struct_index`, `struct_patch`) and drop the rest.** Lost:
   it keeps every line of machinery — schema specs, advertisement, dispatch, call-shape synthesis,
   op-card renderers, `ntr` — for three tools, so none of the complexity is actually removed, and the
   model still faces the "which surface?" decision that causes the behaviour we are fixing.
2. **Keep the native surface but hide it behind a config toggle.** Lost: two live code paths, both
   needing tests and prompt text forever; the token cost returns for anyone with the toggle on, and
   the repo forbids stopgaps kept "for later".
3. **Keep `ntr` after removing the native surface.** Lost: with one tool there is nothing to look up
   — a `python_execution` block stores printed text, not a structured result — so `ntr` would index
   an empty table while still costing a DB schema, five host callbacks, an AST scanner and a prompt
   sentence.
4. **Keep `vis_docs` separate from `doc`.** Lost: two verbs, two prompt sentences and a permanent
   "which one?" question, for one lookup that differs only in where the text is stored.
5. **Delete `cat`/`patch` immediately in phase 1.** Lost: they are still the safest anchored write
   path and are used by the harness's own tests; removing them at the same time as the surface change
   would make a regression impossible to attribute. It gets its own phase, last.

## Phase 1 — Advertise only `python_execution`

**Rationale.** Without this nothing else is provable: as long as eighteen schemas are on the wire, no
measurement of prompt size or model routing reflects the change. This phase is the one that must ship
first and be observable on its own.

**Data.** None. The change deletes entries from the provider `:tools` vector; no persisted, wire, or
cross-language shape changes.

**Acceptance criteria.**
- `src/com/blockether/vis/internal/loop.clj` — `native-tools` (`:4843`) returns exactly
  `[(finalize-engine-native-tool (python-execution-tool caps))]`; the `apropos`/`doc`/`session_fold`
  engine tools are no longer advertised.
- `src/com/blockether/vis/internal/loop.clj` — `advertised-native-capability-names` (`:4856`) and its
  call site (`:5371`) are deleted; `env/set-advertised-native-tools!` no longer exists.
- `src/com/blockether/vis/internal/env_python.clj` — `set-advertised-native-tools!` (`:939`) and the
  `__vis_advertised_native_tools__` global (`:945`) removed; `apropos` in
  `resources/vis-python/apropos_table.py` stops suppressing "already advertised" names, since nothing
  is advertised.
- Test: `test/com/blockether/vis/internal/loop_test.clj` gains
  `only-python-execution-is-advertised-test` asserting the provider `:tools` vector has exactly one
  entry named `python_execution`; `test/com/blockether/vis/internal/native_tool_provider_contract_test.clj:69,116`
  is rewritten to that single-tool contract (or deleted with its assertions moved, decided in phase 2).

**Unknowns.** Does any provider adapter in `svar` require a non-empty tool list with more than one
entry for a particular model family? Do any channel clients read the advertised-name set for
autocompletion?

## Phase 2 — Delete the native-tool declaration and dispatch machinery

**Rationale.** Phase 1 leaves the whole apparatus alive but unused, which is the worst state: dead
code that still has to be maintained, still validated at registry build, and still tempting. Without
this phase the `:schema` blocks stay in every symbol registration and keep drifting.

**Data.** None. `:ext.symbol/native-tool?` / `:ext.symbol/schema` are registry-internal keys that never
cross a boundary; this phase only deletes them.

**Acceptance criteria.**
- `src/com/blockether/vis/internal/extension.clj` — delete `:ext.symbol/native-tool?` and
  `:ext.symbol/schema` specs (`:421-500`), `native-tool-root-union-keys` /
  `portable-native-tool-schema?` (`:440-448`), `native-tools-for` (`:993`),
  `native-tool-replay-policies` (`:1027`), `wire-schema` / `advertise-tool` / `native-tool-schemas`
  (`:1036-1146`), `native-tool-handlers` (`:1148`), `native-tool-call-shapes` (`:1173`),
  `native-tool-finish-call-renderers` (`:1187`), `native-tool-start-call-renderers` (`:1199`),
  `native-tool-tags` (`:1211`), `native-tool-finish-call-renderers-by-op` (`:2157`), `symbol-bound?`
  (`:1226`, every symbol is bound now) and the six refusal rules at `:1495-1553`.
- `src/com/blockether/vis/internal/extension.clj:1805` — `remove :ext.symbol/native-tool?` filter in the
  sandbox-symbol projection becomes an unconditional pass; `:2766-2769` loses the `symbol-bound?` filter.
- `src/com/blockether/vis/internal/loop.clj` — delete native handler execution (`:978-1060`, including
  the `:vis/native-tool-timeout` error type), `finalize-engine-native-tool` (`:4707`),
  `engine-native-tool-call-shapes` (`:4838`), `py-literal` + `tool-call->python-source` (`:4867`+),
  `native-tool-call-block` (`:5169`), the handler/call-shape lookups at `:5516-5527`, the replay-policy
  lookup at `:8257`, the renderer lookups at `:5565-5622` and `:9413`. The block builder becomes: one
  tool call, name `python_execution`, `:lang "python"`, `:source` = its `code` argument.
- `src/com/blockether/vis/internal/runtime_settings.clj:17-51` — `NATIVE_TOOL_TIMEOUT_MS` and
  `native-tool-timeout-ms` are removed or renamed to the python-block budget they now exclusively serve.
- Symbol registrations lose `:native-tool? true`, `:schema`, `:call`, `:render-start-call-fn`,
  `:render-finish-call-fn`, `:replay` where those existed only for the native surface:
  `foundation/editing/core.clj:6180,6272,6296,6341,6746,6966`;
  `foundation/language_surface.clj:1081,1111,1135,1178,1208`; `foundation/harness/core.clj:180`;
  `foundation/mcp/core.clj:1114`;
  `extensions/common/vis-foundation-search/.../core.clj:1848-1971` (the `:native-tool? false` lines at
  `:1848,1873,1898,1924,1948` are deleted outright as the key no longer exists). `:description` and
  `:result` REMAIN — they become the `doc(name)` body.
- Tests: see the test map in part 5 for the exact per-describe disposition. Deleted here:
  `test/com/blockether/vis/internal/extension_test.clj` (`flat-native-tool` + `flat-native-tool-spec-test`,
  `:22-234`, 44 assertions; `constrained-native-tool` + `wire-schema-constraints-test`, `:519-551`),
  `test/com/blockether/vis/internal/native_tool_provider_contract_test.clj` (whole file, 166 lines),
  seven `loop_test` describes (`:449, 509, 701, 3712, 3719, 3823, 4028`),
  `foundation/language_surface_test.clj:442,591`, `foundation/shell_test.clj:1469,1604`,
  `python_extensions_test.clj:1672`. Kept and narrowed:
  `test/com/blockether/vis/internal/tool_surface_boundary_test.clj:75` (keep the
  "string-keyed payload for every no-arg observation tool" assertions, drop the native-schema half),
  `extensions/common/vis-foundation-search/test/.../core_test.clj:586-590` (delete both expectations),
  `test/com/blockether/vis/internal/foundation/surface_contract_test.clj:43` (rewrite to assert every
  symbol is Python-bound and carries `:description` + `:result`).
- Test that proves it done: `surface_contract_test` gains
  `every-function-is-a-python-name-test` — walk the live registry, assert no symbol declares a
  schema key and every symbol resolves to a bound sandbox name.

**Unknowns.** Does any channel read `:vis/native-handler` off a persisted block from an older session
file? (`:ext.symbol/replay` is RESOLVED — see cross-validation §G: no production symbol declares it,
so the whole policy path is already dead and goes with the surface.)

## Phase 3 — Collapse rendering to the single `python_execution` card

**Rationale.** Until the renderers stop branching on tool identity, the TUI keeps painting op-cards
that can never appear again, and `form.clj` keeps a badge vocabulary with one live entry. The visible
promise of the whole change — "you see the program and what it printed, nothing else" — lands here.

**Data.** None. Form keys are in-process display data between the loop and the channels; the phase
deletes keys rather than adding any.

**Acceptance criteria.**
- `src/com/blockether/vis/internal/form.clj:43-63` — delete the native-tool badge identity block and
  the wire-name → label table; `:158-180` `native-tool-form?` and the `errored?`/`python_execution`
  branch collapse to "a form is a python block". `result-card`/`result-cards` (`:88-154`) SURVIVE —
  they now describe a PRINTED result, not a native call (see §K).
- `src/com/blockether/vis/internal/loop.clj:5600-5620` — the printed-card renderer stops being
  `native-tool-finish-call-renderers-by-op` and becomes one function of the result value (`:op` +
  count → headline, pretty value → body), so §A's 21 renderer defns can go without a printed `grep`
  result degrading to raw EDN.
- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render.clj` — delete the
  native-tool headline band (`:2570`), the tool identity in the render cache key (`:3611-3632`),
  `native-tool-error?` and its compaction (`:5205-5298`), the native-only `:result-render` gate
  (`:5381-5409`), `tool-label` (`:5483, 5513`), the running-native headline (`:5599-5604`), the
  code-less op-card and its stacking (`:5625-5652`), the tool name in the progress label (`:5859,
  5929`) and every `(not= … "python_execution")` guard — the condition is now always true. KEEP
  `compact-tool-card-body-entries` and `tool-card-entries`: they paint printed results.
- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/chat.clj:214, 918` —
  drop the pre-rendered native-tool card path and the `:vis/tool-name` badge read-back.
- `apps/vis-companion/src/components/ChatContent.tsx` — delete `toolLabelOverrides` (`:118`),
  `toolLabel` (`:970`), `compactToolSummary` (`:1010`), `RUNNING_CODE_TOOLS` (`:1229`) and the eleven
  `form.tool_name` branches; `toolCards` (`:999`) becomes `form.cards ?? []`. `ToolCard`, `CardGrid`,
  `FormTrace` and `CARD_BAND` keep their geometry. Followers: `lib/types.ts:708, 728`,
  `SessionScreen.tsx:507, 516, 561, 686-688`, `lib/artifacts.ts:239`.
- `src/com/blockether/vis/internal/loop.clj:3854-4290` — the tool-result display path keeps only the
  `python_execution` branch: `:stdout` verbatim, the "printed nothing" hint (`:4283`), and the error row.
- Tests: `render_test.clj:75` and `chat_test.clj:619-650` are reduced to the single card;
  `render_test.clj:150` `native-tool-error-compact-test` is deleted and its intent re-expressed as
  `python-block-error-row-test`; the ~33 tool-IDENTITY sites in `render_test.clj` go, the
  card-GEOMETRY sites are rewritten against printed results; `chat_test.clj:548,553,637-657` follow.
  `virtual_test.clj:150,333,370,673,1140` stays UNTOUCHED on purpose — its `:result-render` fixtures are
  python-block stdout, the thing that survives, and a prefix-match deletion there would drop the
  height-estimation and huge-output regressions. Companion: `ChatContent.test.tsx:228-231`
  and `artifacts.test.ts:163-189` re-fixture, plus one ADDED `a-card-titles-itself-from-its-op`;
  `npm run lint && npm run build` must pass, and the card band is measured at 390 and 1440 on the
  running app (`npm run dev` + `spel`), height unchanged.
- Test that proves it done: `render_test.clj` gains `one-card-shape-test` — a rendered iteration with
  three tool calls paints three code+output cards and zero per-tool op-cards — and `loop_test.clj`
  gains `printed-result-card-headline-test`, which builds a headline for `{:op "grep" …}` with NO
  extension registered.

**Unknowns.** Phase 3's original Unknown is RESOLVED: the companion reads `tool_name` in five places,
all of them a label or a memo key — grouping is `scope`, which survives. Remaining: is `user-shell`
op-card identity (`loop.clj:9318`) a separate concern that must survive, and does the data headline need
a count for shapes other than a map of files (decide from the printed results a real session produces,
not from taste)?

## Phase 4 — Two discovery verbs with one job each: `apropos` SEARCHES, `doc` RETRIEVES

**Rationale.** With no schemas on the wire, discovery IS the contract surface, so it must answer two
different questions and never three. Today it answers neither well. `apropos(query)`
(`env_python.clj:1050-1057`) lowercases the query and matches a SUBSTRING against the NAME and the
GROUP only — the gist it prints is never searched, the full `__vis_docs__` body is never searched,
and Vis's own documentation pages are not in the corpus at all, because they live behind a third
verb (`foundation/self_docs.clj:52`, `vis_docs`). A model that knows the word "viewport" but not the
name `spel`, or the phrase "wire contract" but not the slug, finds nothing and guesses instead.
`doc(name)` (`:1092-1116`) is equally narrow: it resolves a sandbox GLOBAL and nothing else, so a
documentation page is unreachable through it, and `:1112` even advertises the dead end
(`<not found> — try apropos("")`).

The resolved split, and it is the whole phase:

- **`apropos(query)` is FULL-TEXT SEARCH over every document the session can reach.** One corpus,
  assembled once per env from four seeders and closed so nothing hides behind a fifth verb:
  function contracts (sandbox verbs, shims, providers, language tools), Vis documentation pages
  (`foundation/self_docs.clj` `pages`), skills — the WHOLE `SKILL.md` body, `foundation/harness/
  discovery.clj:531` `:content` — and MCP tools (server, tool name and the server-supplied
  description, the only place that text survives once `mcp__call` stops being a schema).
  `apropos("viewport")` finds `spel`, `apropos("wire contract")` finds the gateway page,
  `apropos("TestFlight")` finds the release skill; `apropos("")` is still the complete dict.
- **`doc(target)` RETRIEVES the one authoritative text** for a function name, a documentation slug or
  a skill name — the model never has to know which of the four it is holding. `doc()` with no
  argument returns the CURATED index (below).

### One record, one vocabulary: `function | skill | documentation_page | mcp_tool`

Four seeders, four words, and those are the ONLY words used for them anywhere — code, tests, prompt
sentences, this plan. They are snake_case because they are read in Python:

- **`function`** — a Python name callable in the sandbox (`grep`, `attach`, `shell`, `run_tests`).
  It replaces "capability", a word that named nothing the model can type.
- **`skill`** — a `SKILL.md` in the precedence chain, possibly owned by a nested project.
- **`documentation_page`** — Vis product markdown (`foundation/self_docs.clj` `pages`).
- **`mcp_tool`** — a tool on a paired MCP server.

The word is still NOT a column on the row. `:kind` was a taxonomy the model cannot act on: it does
not change how anything is read, and it costs a field on every hit. What a hit actually needs is ONE
thing — the Python expression that uses it — so `call` is the field, and the vocabulary is what it
says:

- a `function` answers `grep(query=...)`,
- an `mcp_tool` answers `mcp__call("server", "tool", {...})`,
- a `skill` answers `await skill("spel")`, so the activation effect stays visible without a tag
  warning about it,
- a `documentation_page` answers NOTHING; a missing `call` is exactly "this is prose, read it".

**A skill's `text` IS its `SKILL.md`, in full — no gist, no metadata stub.** Every entry, whatever
seeded it, carries the authoritative document and nothing else, so `doc("spel")` returns the same
body `apropos` searched and there is no second, shorter description of a skill anywhere to drift
from it. That leaves `skill(name)` with exactly the job `doc` cannot do: activation is a SESSION
effect — it marks the skill active, resolves its owning project root, cwd and bundled resources, and
survives past this iteration — while `doc` is a read inside one iteration with no tape effect. The
prompt says that in one line: read a skill with `doc`, work under it with `skill`.

**`group` is deleted.** `filesystem | shell | mcp | providers | languages | shims | engine` is an
editorial taxonomy maintained by hand, stale the day a function lands in the wrong bucket, printed
on every row of every listing and searched as if it were content. Once `apropos` is full text, a
group name is just a word: `apropos("filesystem")` matches the sentences that actually say what the
filesystem verbs do, which is a better answer than a bucket label. The one thing group bought — a
bare group name listing a family — is replaced by the curated index, which is written rather than
bucketed.

**`gist` stops being a stored field and becomes a RENDERING.** Two texts for one entry are two
places to drift, and the drift is invisible because nothing reads both at once. There is ONE
document per entry; its FIRST LINE is the gist, by construction. The index prints first lines, `doc`
prints the whole thing, and an entry whose first line is not a usable one-liner is a lint failure on
the contract text, not a second field.

**`doc()` is CURATED, not a dump.** Roughly 60 functions plus documentation pages plus skills is not
it is another prompt. `doc()` answers a hand-ordered short list — the verbs a session actually
starts from — each as `name — first line`, and ends with the sentence that the rest is reachable by
`apropos(text)`. The curated order lives beside the seeders as data, one vector of names; anything
not on it is discoverable but not advertised, which is the whole point of dropping the schemas.

**Data.** The `apropos` and `doc` payloads cross the Clojure -> GraalPy boundary and are what the
model reads, so they are specified before the code:

```clojure
;; ONE record for every document, whatever seeded it.
(s/def :vis.doc/name string?)              ;; the only handle: "grep", "spel", "gateway-wire"
(s/def :vis.doc/text string?)              ;; full markdown; its FIRST LINE is the gist
(s/def :vis.doc/call (s/nilable string?))  ;; python that USES it; nil = prose, nothing to call
(s/def :vis.doc/entry (s/keys :req-un [:vis.doc/name :vis.doc/text]
                              :opt-un [:vis.doc/call]))

;; `doc()` = curated index; `doc(target)` = one entry.
(s/def :vis.doc/entries (s/coll-of :vis.doc/entry :kind vector?))
(s/def :vis.doc/result (s/or :index (s/keys :req-un [:vis.doc/entries])
                             :entry :vis.doc/entry))

;; `apropos` = the same records, ranked. Rank IS the "where did it match" answer:
;; a name hit outranks a body hit, so a per-hit field set would only restate the order.
(s/def :vis.apropos/score number?)
(s/def :vis.apropos/hit (s/merge :vis.doc/entry (s/keys :req-un [:vis.apropos/score])))
(s/def :vis.apropos/result (s/coll-of :vis.apropos/hit :kind vector?))
```

**Acceptance criteria.**
- `src/com/blockether/vis/internal/env_python.clj:1026-1091` — `apropos` searches the full corpus:
  the `matched` filter at `:1053-1057` becomes a scorer over name and the whole text, ANDing
  whitespace-separated terms, ranking name above body with a stable tie-break. The returned value
  stays a real guest dict `{name -> first line}` so `list()/in/sorted/**` keep working (`:1082-1089`);
  `call` and `score` ride in the rendering and in `apropos(query, detail=True)`.
- `src/com/blockether/vis/internal/env_python.clj` env build — one corpus, one record shape, four
  seeders (function contracts, `self_docs/pages`, `discovery.clj:531` skill bodies, live MCP
  catalogue),
  assembled once and cached with the docs registry; a fifth source is a new seeder, not a new verb.
- The `group`/`__vis_groups__` machinery is DELETED — `env_python.clj:1984+` group seeding,
  `apropos_table.py`'s group column, `extension.clj:3822-3939` group assembly, and every prompt
  sentence that names a group.
- Searching a skill must NOT activate it: `apropos`/`doc` read the discovery registry only and never
  call `foundation/harness/core.clj` `skill`, so no search touches the provider tape.
- `src/com/blockether/vis/internal/env_python.clj:1092-1116` — `doc` resolves in order: function
  name, then documentation slug (case-, whitespace- and `.md`-insensitive, exactly as `self_docs`
  normalizes today), then skill name; a skill answers its whole `SKILL.md`. No argument returns the
  curated index; a miss answers with the top `apropos` hits for the same string, not `<not found>`.
- `src/com/blockether/vis/internal/foundation/self_docs.clj` — `vis-docs-tool` (`:52`) and
  `render-vis-docs` (`:94`) are deleted and the symbol leaves `foundation/core.clj:100`;
  `pages`/`listing`/slug normalization stay as a seeder into the corpus.
- `resources/vis-python/apropos_table.py:13` — the table becomes `name — first line` plus `call`,
  loses the group column, and drops the `__vis_advertised_native_tools__` hiding (after phase 1 that
  set holds only `python_execution`).
- `src/com/blockether/vis/internal/prompt.clj:214` — one sentence: `apropos` searches everything,
  `doc` retrieves one thing, `doc()` is the index (see phase 7).
- Tests: `test/com/blockether/vis/internal/env_python_test.clj` (33 doc/apropos sites) gains
  `apropos-searches-doc-body-test`, `apropos-searches-documentation-pages-test`,
  `apropos-searches-skill-bodies-test`, `apropos-does-not-activate-a-skill-test`,
  `apropos-searches-mcp-tool-descriptions-test`, `apropos-ranks-name-above-body-test`,
  `apropos-ands-multiple-terms-test`, `doc-no-arg-returns-curated-index-test`,
  `doc-resolves-page-slug-test`, `doc-resolves-skill-name-test`,
  `doc-returns-whole-skill-body-test` (the string `doc` answers for a skill is byte-identical to
  `discovery.clj` `:content`, and no shorter skill description exists in the corpus),
  `doc-resolves-function-before-page-test`, and `every-entry-has-a-usable-first-line-test` (the
  lint that replaces the gist field: first line non-empty, one sentence, <= 240 chars, for every
  seeded entry); `test/com/blockether/vis/internal/foundation/self_docs_test.clj` is rewritten
  against `doc`/`apropos`.
- Test that proves it done: `env_python_test/discovery-is-two-verbs-test` — `vis_docs` is absent
  from the sandbox globals, no entry carries a group or a kind, a word appearing ONLY inside a page
  body finds that page through `apropos`, and `doc` answers the same record shape for a function
  name, a page slug and a skill name.

**Unknowns.** When a documentation slug collides with a function name, the function wins — is an
explicit
`doc("docs:<slug>")` escape worth it, or is renaming the page the right answer? Does the scorer need
a real inverted index, or is a linear scan enough (roughly 60 functions, the embedded pages, and
every `SKILL.md` body in the precedence chain — measure the assembled corpus size and the p95
`apropos` call before building one)? Should the corpus reach further still — the workspace guidance
chain (`AGENTS.md` and nearer overrides) is already injected into the prompt verbatim, so indexing
it would be a second copy; is a repo `docs/` tree in scope, or does that belong to `grep`?

## Phase 5 — Retire `ntr` / `native_tools_results`, the native-result store, AND the fold's recovery half

**Rationale.** `ntr` exists to hand back a native call's STRUCTURED result after its wire step was
collapsed. `loop.clj:2024-2025` already states the rule that kills it: *"python_execution PRINTS
instead of returning a `:result`, so it stores nothing and contributes no accessor."* With
`python_execution` the only tool, `ntr-entries-of` returns `[]` for every iteration, by construction —
so the store is empty, `ntr.describe()` lists nothing, every `# saved:` stamp is unreachable, and the
`session_fold` receipt's whole recovery half (`recover raw result/no rerun: …`, `· more results:
ntr.describe()`) is a promise about rows that do not exist. Leaving it is worse than removing it: the
model is told a coordinate exists, spends tokens fetching it, and gets a `KeyError`. **Folding becomes
one job — a breadcrumb with a gist — and its only recovery pointer is `session_state()`.**

**Data.** Deletes read paths over already-persisted rows; no new persisted shape. The SQLite columns
that stored native results are dropped in the same phase (repo rule: no compatibility layer), and the
persistence namespace docstring records that a session file from before this change simply has rows
nothing reads.

**Acceptance criteria — the sandbox side.**
- `resources/vis-python/async_runtime.py` — delete `ntr` / `native_tools_results` and their machinery:
  the prime-on-scan hook (`:1884-1890`), the whole `__VisNativeResults__` mapping object and every
  accessor on it (`:2103-2550`) — subscript `ntr["tN/iM/fK"]`, the coordinate resolver
  `__vis_entries_at__`/`ntr.at()` (`:2171-2260`), **`ntr.describe(limit, ids)` (`:2457-2523`), which is
  the labelled discovery half and dies with the rest — there is no shorter surviving "list what is
  stored" verb, because nothing is stored** — the literal-id AST scanner `__vis_native_result_scan__`
  (`:2542`), the stored-result dict normalization and its mixed-sweep comments (`:643`, `:662`, `:678`,
  `:852`), the "call returns what it print()s" nudge (`:2220-2230`), the `# saved:` coordinate
  sentences (`:2312`, `:2333`) and the `repr` blurb advertising `ntr.describe()` (`:2527-2530`). The
  non-deferred kwargs list at `:2004-2006` STAYS — `session_fold(target, gist=…)` still takes kwargs.
- `src/com/blockether/vis/internal/loop.clj:11156-11214` — delete the five `__vis_native_result_*`
  host callbacks (`prime`, `fetch`, `ids`, `index`, `scope`). `index` (`:11170`) is `describe()`'s
  backend and has no other caller; the sqlite `db-native-result-index-*` queries below are its store
  half, so `describe` disappears end to end — sandbox verb, host callback and SQL.
- `src/com/blockether/vis/internal/env_python.clj:886, 997-1002, 2117-2122` — drop `ntr` /
  `native_tools_results` and the five `__vis_native_result_*` names from the protected-name,
  non-deferred and defer-exclusion sets; `:2696` loses "re-read stored results with `ntr[\"<tool id>\"]`"
  from the block-failure recovery nudge.

**Acceptance criteria — the store.**
- `src/com/blockether/vis/internal/persistance.clj:616-634` — delete the five `defdelegate`s.
- `extensions/persistance/vis-persistance-sqlite/src/.../core.clj:4487-4740` — delete the
  `ntr[tool_id]` branch index section header, `db-native-results-for-tool-ids`,
  `db-native-result-ids-for-session`, `db-native-result-index-for-session`,
  `db-native-result-index-for-latest-turn`, `db-native-result-index-for-scope` and the result-body
  persistence and columns that fed them.

**Acceptance criteria — the fold, which is the point of this phase.**
- `loop.clj:2017-2043` — delete `ntr-entries-of` outright (its own docstring says python_execution
  contributes nothing).
- `loop.clj:2045-2175` `stamp-iter-universe!` — delete the `:2104-2105` branch that accumulates a
  scope → ntr-id map; the fn keeps stamping the iteration universe and live skill activations, and its
  docstring drops "recoverable native result ids" from the sentence at `:2046`.
- `loop.clj:2734` — delete `(declare ntr-recover-hint)`. This is the repo's only forward declaration in
  this file's fold section and the AGENTS.md `declare` ban makes its removal non-optional; nothing is
  re-ordered to keep it.
- `loop.clj:3140, 3153-3163, 3244, 3270` — the fold receipt builder loses `recover-hint` and the
  `ntr-recover-hint` call; the receipt becomes `folded <label><note><kept-note> → <gist>`.
- `loop.clj:3290-3296` — delete `ntr-recover-hint` (the `· more results: ntr.describe()` suffix).
- `loop.clj:3384-3392` `code-ntr`, `3426-3460` `recover-bullet`, and the `3514-3517` branches inside
  `session-fold-card` — deleted; the card's `markup-bullet` collapses to `bold-lead-word`, and the
  metric vocabulary in the `:3413` comment loses `recover`.
- `loop.clj:3568-3569, 3660-3665, 3690` — delete `rec-ntr-ids` and `:summary-ntr-ids` from the fold
  summary form; `loop.clj:4091-4100` loses the `recover` metric from the rendered summary.
- `loop.clj:4187` — delete the per-result `# saved: ntr[…] — re-read without re-running` stamp
  emitter. No result line carries a coordinate any more.
- `loop.clj:4748` — the `python_execution` description loses "stay at their `# saved:` coordinate
  (`ntr[\"t5/i1/f2\"]`); engine-bound natives are bare snake_case" (phase 7 rewrites the rest).
- `loop.clj:4775-4777` — the `session_fold` description loses the recoverable/`ntr` clause entirely:
  folding changes rendering, not storage, and a folded step's content is readable only through
  `await session_state()` when introspection is on.
- `src/com/blockether/vis/internal/env_python.clj:1141` — the sandbox `session_fold` docstring loses
  both `ntr` sentences and keeps the `session_state()` filter path.
- `resources/vis-docs/token-optimization.md:111, 116` — delete the "One folded native result: its
  `# saved:` coordinate" bullet and the sentence claiming the coordinate survives with introspection
  OFF (with introspection OFF a folded step is simply not recoverable — say that).
- `apps/vis-companion/src/components/ChatContent.tsx:534` — the comment naming `ntr[…]`-carrying
  metric bullets is reworded to the surviving metric bullets; no rendering change (the column rule
  stays).

**Tests.**
- DELETE: `extensions/persistance/vis-persistance-sqlite/test/.../core_test.clj:2562-2925` (four
  describes, ~364 lines — `native-results-for-tool-ids-test`, `native-result-ids-for-session-test`,
  `native-result-index-for-session-test`, `native-result-index-for-scope-test`);
  `test/com/blockether/vis/internal/env_python_test.clj:680, 723` (both `ntr` describes);
  `test/com/blockether/vis/internal/compaction_verbs_test.clj:574-613`
  (`session-fold-native-tool-test`).
- REWRITE: any `compaction_verbs_test` describe asserting the receipt's shape keeps its fold/gist/
  supersede claims and loses the `recover …` and `more results:` segments.
- ADD: `env_python_test/ntr-is-gone-test` — `ntr`, `native_tools_results`, `ntr.describe`, `ntr.at`
  and every `__vis_native_result_*` name raise `NameError` in a fresh sandbox, and no sandbox
  docstring, prompt line or tool description mentions `describe()` or `# saved:`;
  `compaction_verbs_test/fold-receipt-has-no-recovery-coordinate-test` — a fold over an iteration that
  ran `python_execution` produces a receipt containing neither `ntr` nor `# saved:` nor `recover`, and
  the description of `session_fold` names `session_state()` as the only recovery path.

**Unknowns.** None. The companion reads no native-result row (the only hit is a comment), and
compaction decides what is safe to fold from the iteration universe and skill activations, never from
native-result rows (`stamp-iter-universe!` keeps both after the `:2104` branch is removed).


## Phase 6 — Decide the fate of `cat` and `patch`

**Rationale.** These two are the anchored `lineno:hash` protocol. Once they are no longer schema-
advertised, their cost is entirely in the prompt and in the model's head: `cat` competes with
`open().read()` and `patch` competes with `Path.write_text()` plus `struct_patch`. Keeping them
undecided leaves a permanent "which read?" question inside the one surface we just cleaned.

Recommendation: **delete `cat` and `patch` as model-facing verbs.** `grep` (with `context`) covers
targeted reading, `struct_index`/`struct_nodes` cover structured reading, `struct_patch` covers
structured writing, and plain Python covers everything else. What is lost is the atomic
anchor-verified multi-file text edit; that is re-expressed as a small Python helper in the sandbox
prelude if a reproduction shows it is missed.

**Data.** None. Both verbs are read/write helpers over the filesystem; nothing persisted changes.

**Acceptance criteria.**
- `src/com/blockether/vis/internal/foundation/editing/core.clj` — `cat-tool` (`:6272`) and
  `patch-tool` (`:6341`) registrations removed; internal functions they wrapped are kept only if a
  remaining verb calls them, otherwise deleted with them.
- `test/com/blockether/vis/internal/foundation/editing/core_test.clj` — the `cat`/`patch`
  describes are deleted; anchor-hash helpers used only by them go with them.
- `src/com/blockether/vis/internal/prompt.clj` — the `cat`/`patch` sentences in `CORE_SYSTEM_PROMPT`
  (`:215-233`) are replaced by "read with `grep`/`struct_*` or plain Python; write with `struct_patch`
  or plain Python".
- `AGENTS.md` — no rule references `cat`/`patch`; if one does, it changes in the same commit.
- Test that proves it done: `editing/core_test/anchored-verbs-are-gone-test` plus the existing
  `struct_patch` suite still green, proving structural editing carries the load.

**Unknowns.** Does any harness skill, extension, or bundled documentation page instruct the model to
call `cat`/`patch`? Is atomic multi-file text editing exercised anywhere the structural path cannot
reach (Markdown, YAML, plain prose)?

## Phase 7 — Rewrite every model-facing sentence for a one-tool world, and measure the win

**Rationale.** Nine files still teach a world with eighteen doors. The prompt arbitrates between
surfaces that no longer exist ("Direct native tools *vs* `python_execution`"), names a retrieval verb
that is gone (`vis_docs()`), and spends four lines on a recovery store that stores nothing (`ntr`).
Left alone, the model keeps choosing between one thing and one thing, keeps calling a function that
returns a `NameError`, and keeps paying for text describing deleted machinery. This is also the phase
that produces the number the whole plan is justified by.

**Data.** None. Prompt text is assembled per request from source strings; it is never persisted, never
sent as structured data, and no other language mirrors it.

### 7.1 The complete model-facing inventory (every string the model reads, with its producer)

| # | Producer | `file:line` | Block in the assembled prompt | Fate |
|---|---|---|---|---|
| 1 | `CORE_SYSTEM_PROMPT` | `prompt.clj:209-269` | `;; -- SYSTEM-PROMPT --` | REWRITTEN §1 §2 §3 §6 (7.2) |
| 2 | `python-execution-tool` `:description`/`:result` | `loop.clj:4735-4762` | the ONE tool schema | REWRITTEN (7.3) |
| 3 | `session-fold-tool` description | `loop.clj:4765-4797` | was a schema → becomes `doc("session_fold")` | REWRITTEN (7.3) |
| 4 | `apropos-tool` / `doc-tool` descriptions | `loop.clj:4799-4836` | was a schema → becomes `doc("apropos")`, `doc("doc")` | REWRITTEN by Phase 4 (7.3) |
| 5 | 33 extension symbol `:description` + `:result` | 7 files (see cross-validation B) | were schemas → become each function's `doc` text | KEPT, deduped (7.4) |
| 6 | `extensions-prompt-block` fragments | `prompt.clj:530-556` | `;; -- EXTENSIONS --` | BUDGETED + linted (7.4) |
| 7 | `capability-matrix` (`LANGUAGE TOOLS`) | `language_surface.clj:107-127` | inside EXTENSIONS | REWORDED (7.5) |
| 8 | `skills-prompt` / `agents-prompt` | `harness/core.clj:343-371` | inside EXTENSIONS | REWORDED (7.5) |
| 9 | `INTROSPECTION_PROMPT` | `introspection.clj:1303-1310` | inside EXTENSIONS (toggle) | REWORDED (7.5) |
| 10 | `sandbox-shims-prompt-block` | `prompt.clj:558-640` | `;; -- SANDBOX-SHIMS --` | ONE line changed (7.5) |
| 11 | `cli-autonomous-rules` | `prompt.clj:719` | `;; -- CLI-AUTONOMOUS --` | UNTOUCHED (no tool nouns) |
| 12 | `AGENTS.md` (workspace guidance) | repo root | `;; -- PROJECT-INSTRUCTIONS --` | REWRITTEN (7.6) |
| 13 | `resources/vis-docs/*.md` | `index.md:37-46`, `extending.md` | `doc(slug)` corpus | Phase 4 + (7.6) |

Nothing else reaches the model. `form.clj`, `render.clj` and the companion render the TRANSCRIPT, not
the prompt, and are Phase 3's problem.

### 7.2 `CORE_SYSTEM_PROMPT` — the exact rewrite

Four of seven sections change. §4, §5 and §7 are untouched, word for word — nothing in them names a
tool. Each edit below is stated as *what is deleted*, *what replaces it*, and *why the sentence exists*.

**§1 Identity + Epistemic stance** (`:213-218`)

- DELETE `:214-215` — "`vis_docs()` is product docs, open it only for product questions." The verb is
  deleted in Phase 4.
- DELETE `:217-218` — "Native descriptions and JSON Schemas are authoritative: obey hard
  preconditions and follow the documented contract." There is one schema left and it has one field.
- KEEP `:214` first clause and `:216` (trust order) verbatim.
- ADD the discovery contract, which is the whole point of Phase 4 and must be in §1 because a model
  that does not know `apropos` exists will never type it:

```
- Host project default. Code: `grep(...)` FIRST, scoped to real paths.
- Trust order: runtime > source > docs > assumption; report what the tools showed.
- `apropos(text)` full-text searches every function, skill and Vis documentation page; `doc(name)`
  returns one whole document and is the authoritative contract — obey its stated preconditions;
  bare `doc()` is the curated index. Read a skill with `doc`, work under it with `skill`.
```

**§2 Execution surfaces** (`:219-231`) — the section that shrinks most: 13 lines → 11, and every
sentence about *choosing* a surface dies.

- DELETE `:219-220` — "Direct native tools: single operations, simple edits, small fixed call sets."
  This is the routing rule for a fork that no longer exists.
- DELETE `:225-228` — the four `ntr` lines (`ntr[key]`, `# saved:`, `ntr.describe()`,
  `ntr.keys()/items()`). Phase 5 deletes the store; the shape-inspection half of the sentence is kept
  because it is about Python data, not about `ntr`.
- DELETE from `:229-230` — "Call advertised native tools directly; use `apropos`/`doc` only for
  unadvertised capabilities." Nothing is advertised; §1 now owns discovery unconditionally.
- KEEP verbatim: `:221` (Python default for data work), `:222` (shell handle), `:223-224` (the
  higher-order-helper rule), `:229` first clause (`session` map) and `:231` (REPL reuse).
- ADD one opening sentence that states the new world once, and one printing rule that replaces what
  `ntr` used to buy:

```
- ONE call exists: `python_execution`. Every action — search, read, edit, test, shell, browse — is
  Python in that sandbox; there is no second door and no tool to choose.
- Batch independent work in ONE block: plural arguments first, then `await gather(...)` for
  independent calls. State persists between blocks; only `print(...)` returns.
- `python_execution`: default for most Python/data work, YAML/JSON/TOML/CSV; prefer over shell.
- No shell TOOL: `await shell("npm test")` answers a HANDLE — `sh.logs()`/`sh.wait(s)`/`sh.type()`/`sh.stop()`, each carrying status.
- Define once and reuse a small higher-order helper (functions that accept or return callables):
  NEVER paste a near-identical loop or block twice; on the second occurrence factor it out and call it.
- Results are raw Python data, not rendered text: inspect shape before indexing, and after an error
  inspect keys/types, then adapt. Print only what the answer needs — an unprinted result costs no context,
  and a result you did not print is gone when the block ends.
- `session` is a live read-only map; inspect it directly. Reuse a live REPL; query status only when
  absent or stale.
```

  The last clause of the printing rule is load-bearing and NEW: with `ntr` gone, "I can fetch it later"
  is false, and the prompt must say so or the model will under-print and re-run work.

**§3 Inspect** (`:232-246`)

- REWRITE `:232-235`. "**Filesystem work goes through native tools**" is now a contradiction:

```
- **Filesystem work is Python**: `grep(...)` searches, `ls(dir)` maps an unknown tree FIRST
  (`depth` descends) so no path is guessed, `shell(...)` runs programs, and reading or CHANGING the
  tree is plain Python (`pathlib`, `os`, `shutil`).
```

  The five deleted-verb guards in `prompt_test.clj:94` (`copy`/`move`/`delete`/`create_directory`/
  `file_exists`) stay green and stay meaningful.
- REWRITE `:239-241` — drop `cat` and `patch` per Phase 6, keep the ordered workflow the test at
  `prompt_test.clj:78-83` pins: `grep` → `struct_index` → `struct_nodes` → `struct_patch`.
- REWRITE `:244-245` — "BATCH every tool" → "BATCH inside one block", since batching is now a
  property of the Python program, not of a tool's plural argument.
- KEEP `:236-238`, `:242-243`, `:246` verbatim.

**§6 Manage context** (`:257-263`)

- DELETE from `:261` the words "recovery IDs" — Phase 5 removes the only IDs that phrase meant.
- ADD to the fold bullet the sentence the receipt used to carry: "Folding changes rendering, not
  storage — a folded step is NOT re-readable, so the gist is the only thing that survives."
- KEEP the rest verbatim.

**Budget ratchet.** `prompt_test.clj:76` asserts `(< (count text) 4750)` with a comment recording every
time the budget moved. This phase moves it DOWN — the first time it has ever gone down — and the
comment records why in the same shape as the existing entries: *4750 → 4300, when eighteen tools
became one: §1 lost `vis_docs` and the JSON-Schema clause, §2 lost the native/Python routing fork and
the four `ntr` lines, §3 stopped naming `cat`/`patch`.* The exact new ceiling is set from the measured
count + 5 %, recorded in step 39.

### 7.3 The four engine descriptions

- `loop.clj:4744-4756` `python_execution` `:description` — DELETE the `# saved:`/`ntr` sentence at
  `:4747-4748` and the "engine-bound natives are bare snake_case, native-only ones absent" clause,
  which described a distinction that no longer exists. First sentence becomes: *"Run Python in the
  session sandbox — the only call. Batch, filter and chain work here, then print only what the answer
  needs."* Everything from "A shell is WATCHED here" to the descriptor cap is KEPT verbatim: it is the
  only place the file-handle and bounded-loop rules are stated. The `caps` capability line stays.
- `loop.clj:4757-4758` `:result` — kept as prose, but it stops being schema metadata: with
  `advertise-tool` gone it is appended to the description directly (`:4731` does this today).
- `loop.clj:4765-4797` `session_fold` — Phase 5 already deletes `:4775-4777` and `:4784`. Here the
  remaining text is re-pointed: the description becomes the `doc("session_fold")` document, its
  `:schema` argument prose (`:4788-4795`) folded into that text as the argument paragraph, since a
  Python function's arguments are documented in its docstring, not in a JSON Schema.
- `loop.clj:4799-4836` `apropos`/`doc` — Phase 4 rewrites these; Phase 7 only verifies each is a valid
  `doc` document under the "first line is the gist" lint from Phase 4.

### 7.4 The EXTENSIONS block — the largest measured win, and the rule that keeps it won

Once schemas are gone, every symbol's `:description`/`:result` becomes its `doc(name)` text: pulled on
demand, not pushed every request. The failure mode is obvious and must be blocked in this commit — an
extension author, seeing the schema gone, pastes the signature into `:ext/prompt-fn` and the payload
comes back in the prompt where it costs MORE.

- RULE, written into `extension.clj`'s prompt docstring and into `resources/vis-docs/extending.md`: an
  `:ext/prompt-fn` fragment states ROUTING and POLICY only — when this extension is the right approach,
  and what it refuses. It never restates a signature, an argument name, a return shape or an example
  call; that text lives in the symbol docstring and is reached with `doc(name)`.
- TEST `prompt_test/extension-fragments-do-not-restate-doc-text-test`: for every active extension
  fragment, assert it contains no `(` immediately following a registered function name (a call
  signature), and that no line of it is a duplicate of that symbol's `doc` first line.
- `extension.clj:1785-1830` `symbol-doc-text` becomes the single renderer for every symbol (previously
  Python-only ones); its docstring loses the "Native tools use the separate contract below" split.

### 7.5 The four small blocks

- `language_surface.clj:110` — docstring example header "LANGUAGE TOOLS (active packs; call via the
  facade, language first)" → "(active packs; language first)", matching the emitted string at `:116`
  which already says it. `:126-127`'s clj-kondo sentence is unchanged. The block itself stays: it is
  the only place the ACTIVE language set is stated, and it is ~6 lines.
- `harness/core.clj:351-353` — "call `skill(\"name\")` to load the FULL instructions on demand" →
  "`doc(\"name\")` reads a skill, `await skill(\"name\")` activates it (session effect: sets its project
  root, cwd and resources)". The `[project]` clause is kept. `:360` `clip … 180` stays: the listing is
  progressive disclosure and `apropos` now indexes the full body behind it.
- `harness/core.clj:369` agents line — unchanged; `agent(...)` was never a native tool.
- `introspection.clj:1308` — "Current transcript and folded-content recovery" →
  "Folded content is readable ONLY here: `transcript/turns/iterations/blocks` (`code`/`result`)", the
  sentence Phase 5 makes true. `:1306`, `:1307`, `:1309`, `:1310` unchanged.
- `prompt.clj:621` — "Auto-imported by `python_execution` (no `import`)" is unchanged and now simply
  accurate. The block's closing sentence about `subprocess` never spawning is unchanged.

### 7.6 Documentation in the tree

- `AGENTS.md` — three edits, in this same commit: the `run_tests`/`repl_eval` guidance keeps its
  content but stops calling them native tools; "BATCH every tool" phrasing aligned with §3; the
  gateway-client and companion-dev-server sections are untouched (they name Clojure/npm, not tools).
- `resources/vis-docs/index.md:37-46` — the "Two gears: native tools and the Python sandbox" section
  becomes "One gear: the Python sandbox", deleting the `:42` and `:45` bullets.
- `resources/vis-docs/extending.md` — Phase 2's `:schema` deletion already removes the authoring
  chapter (`:184` "Native tools from Python", `:1146`); Phase 7 adds the replacement paragraph: what a
  symbol's docstring must contain now that it IS the contract, plus the `:ext/prompt-fn` rule from 7.4.
- `resources/vis-docs/token-optimization.md:111,116` — Phase 5 removes the `ntr` advice; Phase 7
  replaces it with the print-discipline rule, since that file is the one page about this exact cost.

**Acceptance criteria.**

- `prompt.clj:209-269` rewritten as 7.2; `loop.clj:4735-4797` as 7.3; the five files in 7.5-7.6 as
  written there.
- `test/com/blockether/vis/internal/prompt_test.clj` surgery, exactly:
  - `prompt-core-test:59-63` ("keeps live native contracts authoritative") — DELETE the two `expect`s
    at `:61-62`; the describe survives on `:63` plus new assertions.
  - `:100` ordering expect (`grep` FIRST before `vis_docs()`) — REWRITE to `grep` FIRST before
    `apropos(text)`.
  - `:112-115` required-terms vector — DROP `"`vis_docs()`"`, `"Native descriptions and JSON Schemas
    are authoritative"`, `"hard preconditions"` (now in the new §1 sentence, re-added in its new
    spelling), `"Direct native tools: single operations"`.
  - `:122` — DROP `"Use `ntr[key]"`, `"# saved:"`, `"`ntr.describe()`"`; KEEP `"Inspect shape before
    indexing"`.
  - `:145-147` — DELETE the `ntr[key]` single-occurrence assertion and its two comment lines.
  - `:91` — `"**Filesystem work goes through native tools**"` → `"**Filesystem work is Python**"`.
  - `:79-81` ordered-steps vector — DROP `"`cat` `ranges`"`.
  - `:149-155` surplus vector — ADD `"native tool"`, `"JSON Schema"`, `"vis_docs"`, `"ntr"`,
    `"# saved:"`, `"Direct native tools"`; this is the regression gate that stops the old vocabulary
    creeping back.
  - `:76` budget — `4750` → the measured ceiling, with the ratchet comment from 7.2.
  - ADD `prompt-names-one-tool-test`: the assembled prompt contains `python_execution`, and contains
    none of `ntr`, `vis_docs`, `native tool`, `JSON Schema`, `advertised`.
  - ADD `extension-fragments-do-not-restate-doc-text-test` (7.4).
- `run_tests` green: `prompt-test`, `loop-test`, `env-python-test`, `harness/core-test`,
  `language-surface-test`, `introspection-test`.

**Measurement (the number this plan is justified by).** Recorded in part 5 below and in the commit
message, captured with `repl_eval` on a clean REPL against a fixed fixture env (workspace = this repo,
all default extensions active, `introspection` OFF), before on `main` and after on the branch:

| metric | how |
|---|---|
| CORE prompt chars | `(count CORE_SYSTEM_PROMPT)` |
| assembled system prompt chars | `(count (prompt/build-system-prompt {}))` |
| EXTENSIONS block chars | `(count (#'prompt/extensions-prompt-block env exts))` |
| shims block chars | `(count (#'prompt/sandbox-shims-prompt-block exts))` |
| provider `:tools` payload chars | `(count (wire/json-str (#'loop/native-tools env)))` — after: one tool |
| total first-request overhead | sum of the above |
| approximate tokens | chars ÷ 3.6, stated as approximate |

The `:tools` payload is expected to dominate: 18 advertised tools with full JSON Schemas today, one
tool with a single `code` property after. A drop under 40 % of total first-request overhead means the
savings landed somewhere unmeasured — find it before claiming the phase done.

**Unknowns.**

- How much of §3's batching discipline survives once batching is a property of writing one Python
  block? Resolved by measurement: keep the sentence if removing it changes nothing, delete it if the
  new §2 "Batch independent work in ONE block" already says it. Decide with the count, not by taste.
- Does the EXTENSIONS block need a hard character budget (a test that fails over N chars), or is the
  7.4 lint enough? Answer after the after-measurement shows what that block actually costs.


## Cross-validation — the SECOND inventory (measured against the tree; phases 1-7 missed these)

Every figure below was counted this pass — balanced-brace spans and full-tree greps, not recall.
Each item names the phase it attaches to; none of them is a new phase.

### Two corrections to the first inventory

- **`:native-tool? false` DOES appear — 12 sites**, the whole `shell` family among them
  (`foundation/shell.clj:3136, 3174, 3205, 3234, 3257`). The turn-1 claim was about
  `:ext.symbol/engine-bound?`, which is still declared `false` NOWHERE (`extension.clj:423`,
  default true) — so the capability claim stands: all **33** `:native-tool? true` symbols are also
  engine-bound Python names and lose nothing when the schema goes.
- **Those 12 `false` symbols carry `:schema` and renderer callbacks anyway** — `shell.clj:3241-3247`
  is a JSON Schema on a symbol that explicitly is not a native tool. That is dead weight ALREADY in
  the tree; the sweep removes it in the same edit instead of leaving a second inventory behind.

### A. Per-symbol renderer callbacks — ~895 lines that exist only to paint op-cards (Phase 3)

Phase 3 said "collapse rendering to one card" and named `form.clj` and the TUI. It never named the
CALLBACKS the cards are painted from. Declarations: `:render-finish-call-fn` **49**,
`:render-start-call-fn` **29**, `:on-error-fn` **26**, `:ticker-fn` **11**. Behind them sit **21 named
`defn`s totalling ~895 lines**, plus the factories `shell-start-renderer`, `shell-ticker`,
`shell-on-error`, `mcp-on-error`, `tool-failure-on-error` and the two adapters
`render-start-call-adapter` / `render-finish-call-adapter` (`python_extensions.clj`).
Owning files: `foundation/editing/core.clj`, `foundation/language_surface.clj`, `foundation/shell.clj`,
`extensions/common/vis-foundation-search/.../core.clj`, `foundation/mcp/core.clj`,
`foundation/harness/core.clj`.

- **Delete** `:render-start-call-fn`, `:render-finish-call-fn`, their spec keys, the adapters and the
  IDENTITY renderers: with one form kind there is no op-card to render into. **§L confirms this holds for
  ALL 21** — including the four language-pack renderers — so this bullet has no exception.
- **`:on-error-fn` — verify before deleting.** Read `shell-on-error` and `tool-failure-on-error`
  first: if they only shape a DISPLAY string they go with the renderers; if they build the structured
  failure map the Python caller receives, they stay and lose only their display half. A named step,
  never an assumption.
- **`:ticker-fn` — KEEP (recommendation).** It is not a card: it feeds `progress.clj:340-352`
  (`:activity :tool-call`, `:tool/op`, `:tool/label`), the live spinner for a call running INSIDE a
  `python_execution` block. A three-minute `shell` run with a frozen bubble is a regression, not a
  simplification. If it should go too, `progress.clj:340-352`, `:tool/op`, `:tool/label` and the 11
  declarations go in one step.

### L. The language packs lose their presentation too — the raw result IS the answer (Phase 3, confirms §A)

**The worry, stated plainly:** with everything printed from Python, does a Clojure REPL blow-up, a lint run
or a failing test suite still read like something a human can use, or does it become a raw dict in a stdout
wall? It becomes the raw dict. **The human ACCEPTED that trade explicitly** (turn 15: *"the REPL eval will
be going by Python. I ACCEPT THAT. We don't need it to be pretty."*), so this section exists to record the
decision and its blast radius, not to argue it back.

**Verdict: all 21 renderers die. There is no Family 2.** The four editorial functions in
`foundation/language_surface.clj` go with the identity ones:

| function | line | lines | what disappears from the screen |
|---|---|---|---|
| `render-lint-result` + `findings->table` | `:498-567` | ~70 | the boxed `file` / `row:col` / `level` / `provider` / `message` grid and the fenced stdin snippet |
| `render-test-result` + `failures->table` | `:569-700` | ~132 | expected/actual columns, the "`message` only when there is no expected/actual pair" rule, `<what ran> — pass/total (Nms)` |
| `render-test-call` | `:797-836` | ~40 | the pending "SELECTION" card (a call inside a python block has no start event anyway) |
| `render-repl-eval-result` + `sect` | `:788-950` | ~163 | FORM / RESULT / ERROR / STDOUT / STDERR / TIMEOUT sections, `(+ 1 1)  ⇒ 2`, `⧖ timed out after 30000ms` |
| `render-repl-start-result` | `:750-786` | ~37 | the REPL start/connect line (`repl up on port N`) |
| `render-format-result` | `:1093` | ~8 | `` `path` — <label> `` |

What replaces them is §K's data card: `<op>` + a count the result already carries + the pretty-printed
value. Every fact the tables drew is still IN the result map — `findings` carry `file/row/col/level/message`,
a fault carries `expected`/`actual`, a `repl_eval` failure carries `err`/`ex`/`root_ex`/`timed_out`. The
layout is gone; the data is not. **No `:vis.result/rendered` spec, no `dispatch!` merge step, no summary/body
keys** — that whole mechanism is withdrawn from the plan, and `dispatch!` (`language_surface.clj:172-280`)
returns exactly what the pack produced.

- **This deletes ~413 more lines than §A counted**, and removes the last reason for a callback to survive
  "for just these four" — the seam the entire plan exists to close.
- **The model is not worse off.** It already received the raw vectors; only the channel got the grid. A block
  that wants a table writes one — `tabulate` is a prebound shim, and `findings` is a list of dicts.
- *Alternative — move the four renderers into the result map (`:summary`/`:body`).* Rejected BY THE HUMAN
  after being proposed: it keeps 413 lines of layout code, adds a spec that crosses into GraalPy, spends
  tokens on markdown in every printed result, and buys prettiness that was explicitly declined.
- *Alternative — keep `:render-finish-call-fn` for language packs only.* Rejected: one surviving callback
  re-opens the per-symbol renderer table.

**Two things that still do NOT die, so nobody deletes them by association:**

- **`:ticker-fn` — KEEP (§A stands).** It is not a card; it feeds `progress.clj:340-352` while a call runs
  INSIDE a python block. `test-target` (`language_surface.clj:662-673`) survives ONLY as the ticker's label
  source, so a multi-minute Clojure suite still names what it is running instead of showing a mute spinner.
- **`:on-error-fn`'s error→DATA half — KEEP.** This is not about prettiness: a pack failure must reach Python
  as a value the block can print and branch on (`{:ok false :error … :trace …}`), never as a Python
  traceback wrapping a Java stack trace. Its DISPLAY half goes with the renderers. Same verdict for
  `shell-on-error` / `mcp-on-error` / `tool-failure-on-error`, which step 44 confirms by reading them.

### B. JSON Schema literals — 371 lines, in SEVEN files, not two (Phase 2)

Measured by balanced-brace span of every `:schema {`:

| file | schema lines |
|---|---|
| `foundation/editing/core.clj` | 135 |
| `vis-foundation-search/.../core.clj` | 84 |
| `foundation/language_surface.clj` | 78 |
| `foundation/shell.clj` | 37 |
| `loop.clj` | 25 |
| `foundation/mcp/core.clj` | 8 |
| `foundation/harness/core.clj` | 4 |

Phase 2 named only `extension.clj` and `loop.clj`. The other five files are where the schemas
actually live; without them the `:schema` spec key is deleted while 346 lines of orphan maps stay.

### C. `form.clj` is not a guard change — 344 lines go to roughly 115 (Phase 3, amended by §K)

`native-tool-form?` (`:158`) is only the entry point. Everything TOOL-IDENTITY below dies with it:
`label-overrides` (`:55`), `tool-label` (`:66`), `compact-path-summary-tools` (`:75`),
`compact-tool-summary` (`:77`), `hide-tool-code?` (`:164`), `coalescable-tools` (`:182`), `tool-name-s`
(`:189`), `coalesce-error-form?` (`:194`), `result-field` (`:200`), `format-summary-entries` (`:210`),
`merge-format-forms` (`:245`), `merge-run` (`:268`), `coalesce-key` (`:278`), `coalesce-forms` (`:284`)
— the entire adjacent-op-card COALESCING subsystem, whose only real customer is per-file `format_code`
acks. `display-fields` (`:18-49`) shrinks to the python-block fields; `with-display-code`, `->display`,
`<-wire` survive.

**Amended by §K:** `result-card` (`:88`) and `result-cards` (`:143`) STAY. They describe a PRINTED
result inside a python block, not a native call, and they lose only their `:vis/tool-name` badge
lookup. That is why this section now says ~115 lines rather than ~80, and why the TUI's
`tool-card-entries` is kept rather than deleted.

Consumers to follow: `src/com/blockether/vis/core.clj` (public re-exports of `coalesce-forms` /
`hide-tool-code?`), TUI `render.clj` (26 display/card sites), and the tests — `form_test.clj` (10
`hide-tool-code?` assertions) and TUI `render_test.clj` (**50** display/card sites, 33 renderer sites),
which the test map splits: identity assertions to DELETE, card geometry to REWRITE.

### D. The Python extension AUTHORING surface still teaches native tools (Phases 2 + 7)

`resources/vis-python/extension_bootstrap.py:106-158`: `vis.symbol(...)` takes `schema=`,
`is_native_tool=`, `description=`, `result=`, `render_start_call_fn=`, `render_finish_call_fn=` and
carries ~40 lines of validation for them (root must be `type: object`, no root `oneOf/anyOf/allOf`,
non-empty `description`/`result`, the reserved `Raw result:` label). All of it goes; the signature
becomes `symbol(fn, name=None, tag='observation', is_hidden=False)` and **the docstring is the doc**,
which is exactly Phase 4's "one document per entry". The `Raw result:` label rule disappears with it
(**24** sites tree-wide: `extension.clj` 6, `loop.clj` 5, bootstrap 2, `extending.md` 2, tests 9).

### E. Wire, DB and companion carry a per-tool identity that becomes a constant (Phase 3)

`:vis/tool-name` has **160** references. Once every form is `python_execution` the name is a constant
and the field is noise:

- `gateway/state.clj:1214-1236` — `tool_name` in the `:tool-preview` / `:form-start` / `:form-result`
  payloads (`tool_call_id` STAYS: `python_execution` is still a provider `tool_use`).
- `ctx_engine.clj:1035-1044` — `:vis/tool-name` and `:result-render` on the restored block.
- `persistance_sqlite/.../core.clj` and `resources/db/sqlite/migration/V1__schema.sql` — the
  `tool_name` and `result_render` columns. No compatibility layer, per repo rule: drop the columns.
- Companion: `lib/types.ts` (`result_render`, `tool_name`), `components/ChatContent.tsx` (4 sites),
  `screens/SessionScreen.tsx`, `ChatContent.test.tsx`.
- `progress.clj:198-232` — both `form/->display` merges shrink with `display-fields`.

### F. The per-tool wall clock, and the hook that exists to escape it (Phase 2)

`runtime_settings.clj:17-53` — `NATIVE_TOOL_TIMEOUT_MS`, `native-tool-timeout-grace-ms` and
`native-tool-timeout-ms` (37 lines) exist because a native HANDLER runs outside the Python eval
watchdog. With one tool there is one watchdog: `*eval-timeout-ms*`. The escape hatch goes with it —
**`:vis/outside-tool-wall`, 6 sites** (`extension.clj` 2, `loop.clj` 2, `runtime_settings.clj` 1,
`language_surface_test.clj` 1) plus the language-clojure `test_runner.clj` declarations. Phase 2 named
only the constant.

### G. Per-tool context REPLAY policy — DEAD IN PRODUCTION, delete the whole path (Phase 2)

**The finding that decides it: nothing declares `:replay`.** Grepping `:replay {` / `:elide-args`
across `src`, `extensions` and `resources` returns the spec itself (`extension.clj:479-493`), the
projection (`:1021`), the builder passthrough (`:1456-1457`), the loop's consumers — and **zero
registrations**. The only producers in the tree are two test literals
(`extension_test.clj:40` `{"x" 1024}`, `loop_test.clj:1806` `{"python_execution" {"code" 8192}}`)
and one docs paragraph (`extending.md:1156,1178`). So `native-tool-replay-policies` returns `{}` on
every real request, `compactable-native-call` returns `nil` on every iteration, and the `compacted-call`
branch of `conversation-suffix` has never been taken outside a test. This is not "a table with one
row" as the earlier draft said — it is a table with **no** rows, threaded through six function
signatures.

The exact removal, in order (each step leaves the file compiling):

| site | what goes |
|---|---|
| `extension.clj:479-493` | the `:ext.symbol/replay` spec + its two comment lines |
| `extension.clj:503` | `:ext.symbol/replay` out of `::fn-symbol-entry` `:opt` |
| `extension.clj:997, 1021` | `:replay` out of the `native-tools-for` docstring and projection map (the fn itself dies in Phase 2 anyway) |
| `extension.clj:1027-1034` | `native-tool-replay-policies`, both arities |
| `extension.clj:1456-1457` | the `(:replay opts)` passthrough in the symbol builder |
| `loop.clj:4301-4309` | `oversized-arg-receipts` |
| `loop.clj:4311-4330` | `compactable-native-call` |
| `loop.clj:4332-4376` | `plain-tool-results` and `compacted-native-replay` — reachable ONLY from the compacted branch |
| `loop.clj:4296-4299` | `successful-tool-call?` — verify it has no other caller, then delete with them |
| `loop.clj:4565` | `conversation-suffix` loses its `& [replay-policies]` tail arg |
| `loop.clj:4598-4599, 4631` | the `compacted-call` binding and its `cond` branch; every iteration takes the verbatim path |
| `loop.clj:7430, 7459, 7494, 7556, 7576` | `replay-policies` out of the emergency-fold parameter lists and both `conversation-suffix` calls |
| `loop.clj:8257, 8270, 8422-8423` | the `extension/native-tool-replay-policies` call site and the `:replay-policies` key on the request map |
| `resources/vis-docs/extending.md:1156, 1178` | the `:replay` table row and the `elide-args` paragraph (folds into step 53) |

**`replay-target` STAYS** — different concept, same prefix: it carries provider/model so
`conversation-suffix` can decide thinking-block preservation and vision-capable image replay
(`loop.clj:4561-4582`). Only `replay-policies` goes.

**Decision, with the alternative recorded.** The rejected alternative is *keep argument elision as an
unconditional constant on `python_execution`'s `code`* — plausible, because `code` is now the only big
string the provider re-uploads on every request. It loses because nobody ever turned it on: shipping a
threshold no measurement asked for re-introduces a compaction path whose behaviour (a successful call's
arguments silently replaced by a sha256 receipt) is invisible until it misleads the model about what it
ran. If Phase 7's measurement shows replayed `code` payloads dominate the trailer, it comes back as ONE
constant in `conversation-suffix` with a test — not as a per-symbol policy key.

**Tests.** DELETE `loop_test/large-arg-replay-compaction-test` (`:1799-1846`, 48 lines) — the behaviour
is gone. ADJUST `loop_test/tool-call-door-strings-only-test` (`:5875-5903`): drop the
`#'lp/oversized-arg-receipts` assertion at `:5897` and keep the rest of the describe.
`extension_test:40,55,62-63` disappear with `flat-native-tool-spec-test` (already in the DELETE table).
The `:replay? false` / `:preserved-thinking/replay?` keys in `loop_test:1607,1744-2009` are the
THINKING-preservation flag, not this policy — untouched. Add nothing: the removal is proven by the
surviving `conversation-suffix` describes still passing with a shorter signature.


### H. Two more `ntr` sites, and the advertisement SUPPRESSION list (Phases 1 + 5)

- `env_python.clj:886` — `ntr` / `native_tools_results` in `protected-baseline-names`.
- `env_python.clj:1001-1002` — `non-tool-names #{"ntr" "native_tools_results" "asyncio"}`; only
  `asyncio` remains.
- `env_python.clj:939-947` — `set-advertised-native-tools!` and the
  `__vis_advertised_native_tools__` global, whose whole job is teaching `apropos` to SUPPRESS names
  the provider already advertised. With one advertised tool that suppression is unconditional, so the
  global, the setter and the suppression branch in `install-introspection!` all go (Phase 1 named the
  setter, not the global or the branch).

### I. `:tag` — verify, then probably delete (Phase 2)

`:tag :mutation` / `:observation`, **20** sites. Trace the consumers (`extension.clj:1148-1224` tags,
`foundation_bridge/core.clj`, `transcript_test.clj`) before deciding: if it only colours a badge or
feeds the tool advertisement it goes; if `transcript.clj` uses it to decide what a REPLAY keeps, it
stays. One read, one decision, recorded here.

### J. Documentation that teaches the removed surface (Phase 7)

Phase 7 named the prompt, `introspection.clj`, `language_surface.clj` and `AGENTS.md`. It missed the
PRODUCT docs, which are exactly the pages `doc(slug)` serves after Phase 4:
`resources/vis-docs/extending.md` (the native-tool authoring chapter — `:schema`, `:native-tool?`,
`Raw result:`, renderer callbacks; **7** native-tool sites) and `resources/vis-docs/index.md:55`. A
page documenting a deleted API is WORSE after Phase 4, because `apropos` full-text searches it.

### K. The presentation layer — what the TUI and the companion actually lose (Phase 3)

Phase 3 named `form.clj`, `render.clj` and `chat.clj`; §C added `form.clj`'s coalescing and §E the wire
fields. None of them answered the question that decides how much presentation code survives: **a python
block PRINTS results, and a printed result is still rendered as a card.**

**The op-card is not only the native-tool card.** `loop.clj:5751-5777` builds `:cards` — one canonical
MINI-FORM per printed tool result inside a block — through
`extension/native-tool-finish-call-renderers-by-op` (`:5610`), keyed by the printed value's own `:op`.
`form.clj:143-154` (`result-cards`) turns them into op-cards, TUI `render.clj:4833-4946`
(`tool-card-entries`) paints them, and the companion paints the same thing with
`toolCards`/`ToolCard`/`CardGrid` (`ChatContent.tsx:999, 1072, 1326`). So deleting §A's 21 renderer
defns does not merely delete badges — **it silently turns every printed `grep` result back into raw
EDN in a stdout wall.** That is a regression this plan would have shipped.

**Decision — ONE card, driven by DATA, and every per-symbol renderer still goes.** The printed-card
renderer is replaced by a single function of the RESULT: headline `<op>` plus a count the value already
carries, body the pretty-printed value. `:vis/tool-name` still dies (§E), because a card's identity is
now the printed result's own `:op`, not a symbol registered at boot.

- *Alternative — keep the 21 renderers.* Rejected: it preserves ~895 lines and the per-tool identity the
  whole change exists to remove, for prettier headlines on a handful of ops.
- *Alternative — delete cards entirely, print stdout verbatim.* Tempting and the smallest code, but a
  block that prints five results becomes one undifferentiated wall — which is the exact problem the
  coalescing subsystem was built to paper over. Rejected unless Phase 7's measurement reopens it.

**TUI — what dies, what stays** (`extensions/channels/vis-channel-tui/src/…/channel_tui/`):

| stays | dies |
|---|---|
| `render.clj:4769-4831` `compact-tool-card-body-entries`, `:4833-4946` `tool-card-entries` — they paint PRINTED results | `:2570` native headline band; `:3611-3632` `:vis/tool-name` in the render cache key; `:5205-5208`, `:5265-5267`, `:5298` `native-tool-error?` + compaction; `:5331`; `:5381-5409` the "`:result-render` for NATIVE TOOL forms only" gate; `:5483`, `:5513` `tool-label`; `:5599-5604` running-native headline-first; `:5625-5652` the code-less op-card and its adjacent-card stacking; `:5859`, `:5929` the tool name in the progress label |
| `virtual.clj:440, 566` — `:result-render` is now stdout, `:cards` bodies still measure | `chat.clj:214, 918` pre-rendered native card + badge read-back |
| `screen.clj:995`, `theme.clj:128` — comments only, one word each | — |

`format-iteration-entry-entries` (`:4948-5759`, **812 lines**) is where most of this sits: it loses every
`(not= … "python_execution")` branch and its two card paths collapse into one.

**Companion — the surface is smaller than it looks, and mostly it SIMPLIFIES rather than disappears**
(`apps/vis-companion/src/`):

- Dies: `ChatContent.tsx:118-122` `toolLabelOverrides`, `:970-973` `toolLabel`, `:1010-1020`
  `compactToolSummary`, `:1229` `RUNNING_CODE_TOOLS` (a two-member set with one member left), and the
  eleven `form.tool_name` branches (`:1007, 1099, 1122, 1195, 1233, 1245, 1348, 1374-1376, 2039, 2195`).
- Stays, retitled: `toolCards` (`:999-1008`) becomes `form.cards ?? []`; `ToolCard` (`:1072-1192`),
  `CardGrid` (`:1326-1354`) and `FormTrace` (`:1356-1414`) keep their geometry — a card is now "a printed
  result", named by its `op`. `CARD_BAND` is untouched: one header band, as `ui.tsx` requires.
- Follows: `lib/types.ts:708, 728` (`result_render`, `tool_name` out; `cards` stays),
  `SessionScreen.tsx:507, 516, 561, 686-688` (form kind is always `code`), `lib/artifacts.ts:239`
  (`op`, not `tool_name`), `ChatContent.test.tsx:228-231`, `artifacts.test.ts:163-189`.
- **Answers Phase 3's Unknown:** the companion does read `tool_name` — five places — but only for a
  LABEL and a memo key, never for grouping. Grouping is `card.scope` / `form.scope` (`:1348, 2039`),
  which is the iteration/form coordinate and survives untouched.

The gateway is the same story: `gateway/state.clj:1214-1236` drops two payload fields; `cards`,
`tool_call_id` and the phases stay, so no channel needs a new event.

### Revised size of the change

| bucket | lines removed |
|---|---|
| JSON Schema literals (7 files) | ~371 |
| renderer / ticker / on-error defns (21) | ~895 |
| `form.clj` op-card + coalescing (card entry points KEPT) | ~230 |
| TUI tool-identity branches (`render.clj`, `chat.clj`) | ~260 |
| companion label/identity paths (`ChatContent.tsx` and followers) | ~120 |
| `runtime_settings.clj` native wall | ~37 |
| `extension_bootstrap.py` native branch | ~40 |
| previously counted production (phases 1-7) | ~1 400 |
| §L language-pack renderers (`language_surface.clj`, no exception) | ~413 |
| tests (~1 150 counted, ~415 §L render describes, plus ~33 TUI identity sites) | ~1 900 |

Roughly **5 300 lines removed** against ~600 added (the `apropos`/`doc` corpus and seeders, the one data
card renderer, the 16 new tests — no `:vis.result/rendered` spec, since §L withdrew it).


## How we verify the whole change

1. **Unit/behaviour, smallest namespaces first** — `run_tests` on
   `com.blockether.vis.internal.extension-test`, `…loop-test`, `…env-python-test`,
   `…prompt-test`, `…foundation.surface-contract-test`, `…foundation.editing.core-test`,
   `…foundation.harness.core-test`, `…compaction-verbs-test`, `…tool-surface-boundary-test`,
   `…python-extensions-test`, the `vis-channel-tui` render/chat tests, and the sqlite
   persistence tests.
2. **Registry-wide contract** — `surface_contract_test` (`test/…/foundation/surface_contract_test.clj:43`)
   walks the LIVE registry, so a symbol that still declares a removed key fails the build rather
   than the wire.
3. **Prompt regression** — `prompt_test` pins the ABSENCE of every removed term, which is what stops
   the old vocabulary creeping back one sentence at a time.
4. **End to end, by hand, once per phase** — a real session in the TUI: ask for a code change that
   needs search, structural edit and a test run; confirm the transcript shows only
   `python_execution` cards, that `apropos` finds a documentation page by a word in its body and a
   skill by a word only inside its `SKILL.md` (without activating it), that `doc(<skill>)` answers
   that whole body, that `doc()` answers the curated index, and that a fold receipt no longer
   promises `ntr`.
5. **Gate** — `lint_code` (clj-kondo + reflection) clean, `format_code` run, `vis-agent python -m ruff
   check resources/vis-python` clean for the `async_runtime.py` edit. `test/com/blockether/vis/native_reachability_test.clj`
   is about GraalVM REFLECTION metadata, not native tools — it must simply stay green; nothing in
   this plan edits it.

### Test map — every test this plan touches, and what happens to it

The surface shrinks, so the suite shrinks with it. Measured on the current tree: **36 top-level
describes are DELETED, 14 are REWRITTEN, 13 are ADJUSTED (wording/filters only), and 16 new ones are
added** — a net loss of 20 describes and roughly 1 565 test lines. A test is deleted only when the
BEHAVIOUR it pinned no longer exists; a test that pinned a behaviour we keep is rewritten in the new
vocabulary, never dropped.

**DELETE — the behaviour itself is gone.**

| Phase | File | Describe / var (line) | Why it can go |
| --- | --- | --- | --- |
| 2 | `test/…/internal/native_tool_provider_contract_test.clj` | WHOLE NAMESPACE, 166 lines: `native-tool-provider-contract-test` (`:69`), `provider-test-router` (`:106`), `native-tool-provider-callability-test` (`:117`) | The file exists to prove native tools reach a provider. There is one tool and phase 1 pins it. |
| 2 | `test/…/internal/extension_test.clj` | `flat-native-tool` fixture (`:22`), `flat-native-tool-spec-test` (`:28-234`, 44 assertions), `constrained-native-tool` (`:519`), `wire-schema-constraints-test` (`:530-551`) | ~230 of 741 lines: they validate `:ext.symbol/schema` shapes that the spec no longer defines. |
| 2 | `test/…/internal/loop_test.clj` | `native-tool-timeout-settings-test` (`:449`), `native-handler-timeout-test` (`:509`), `native-tool-call-execution-test` (`:701`), `native-introspection-tools-test` (`:3719-3819`), `real-call-shapes` (`:3712`), `tool-call->python-source-test` (`:3823-3841`), `native-tool-call-block` + `native-tool-call-block-test` (`:4028-4063`) | Handler dispatch, call shapes and the tool-call→Python transcription all disappear with the machinery. |
| 2 | `test/…/internal/foundation/language_surface_test.clj` | `render-test-call-test` (`:442`), `format-schema-advertises-recursion-test` (`:591`) | One reads `native-tool-start-call-renderers`, the other asserts a JSON-Schema recursion depth. |
| 2 | `test/…/internal/foundation/shell_test.clj` | `shell-native-contract-test` (`:1469`), `shell-pending-call-render-test` (`:1604`) | "No shell symbol is a native tool" becomes vacuous; the pending-call renderer no longer exists. |
| 2 | `test/…/internal/python_extensions_test.clj` | `python-native-tool-test` (`:1672-1694`) and the `weather-py` schema half of the fixture (`:1601`) | A Python extension can no longer declare a native tool; it declares a sandbox function. |
| 3 | `extensions/channels/vis-channel-tui/test/…/render_test.clj` | `native-tool-error-compact-test` (`:150`) | Compacting a native-tool error card: no such card. |
| 4 | `test/…/internal/foundation/self_docs_test.clj` | WHOLE NAMESPACE, 146 lines, 8 describes: `vis-docs-listing-test` (`:17`), `vis-docs-blurb-test` (`:45`), `vis-docs-fetch-test` (`:72`), `vis-docs-forgiving-slug-test` (`:108`), `vis-docs-blank-slug-test` (`:121`), `vis-docs-unknown-slug-test` (`:131`), `vis-docs-symbol-test` (`:139`) | `vis_docs` is deleted. Its three real behaviours — forgiving slug, blank slug lists, unknown slug helps — are RE-EXPRESSED against `doc` in `env_python_test` (see ADD), so nothing is lost but the verb. |
| 4 | `test/…/internal/env_python_test.clj` | `apropos-groups-and-bare-verb-docs-test` (`:966-999`) | `group` and `__vis_groups__` are deleted; the family listing it protected is replaced by the curated `doc()` index. |
| 2 (G) | `test/…/internal/loop_test.clj` | `large-arg-replay-compaction-test` (`:1799-1846`, 48 lines) | The only test of the `:elide-args` replay policy, which no production symbol ever declared. Behaviour deleted, not moved. |
| 5 | `extensions/persistance/vis-persistance-sqlite/test/…/core_test.clj` | `native-results-for-tool-ids-test` (`:2562-2678`), `native-result-ids-for-session-test` (`:2681-2735`), `native-result-index-for-session-test` (`:2738-2814`), `native-result-index-for-scope-test` (`:2820-2925`) | ~364 lines over four describes covering five DB query fns that are deleted with the store. |
| 5 | `test/…/internal/env_python_test.clj` | `ntr-browse-test` (`:680-685`), `ntr-coordinate-test` (`:723-834`) | `ntr` no longer exists in the sandbox. |
| 5 | `test/…/internal/compaction_verbs_test.clj` | `session-fold-native-tool-test` (`:574-613`) | Folding a native-tool call: there are none. The receipt's whole recovery half (`recover …`, `· more results:`) goes with it. |
| 6 | `test/…/internal/foundation/editing/core_test.clj` | `native-tools-flat-spec-guard` helper (`:78`, 6 call sites) and every `cat`/`patch` describe | The guard checks a spec that is gone; the verbs are gone. |
| 3 (§L) | `test/…/internal/foundation/language_surface_test.clj` | `render-test-result-test` (`:323`), `render-test-call-test` (`:439`), `render-repl-start-result-test` (`:462`), `render-repl-eval-result-test` (`:482`), `render-lint-result-names-target-test` (`:604`), `render-lint-snippet-test` (`:746`), plus `format-schema-advertises-recursion-test` (`:552`) | Seven describes, ~415 lines. The six render describes pin layout the human declined to keep (§L); the schema describe pins a `:schema` literal deleted in phase 2. |

**REWRITE — the behaviour survives, the vocabulary does not.**

| Phase | File | From (line) | To |
| --- | --- | --- | --- |
| 1 | `loop_test.clj` | `tool-result-display-timeout-test` (`:462`) | `python-block-timeout-display-test` — same assertion, one tool. |
| 2 | `loop_test.clj` | `tool-call-protocol-leak-test` (`:5916-5965`, 10 sites) | Keep the leak assertions, drop the per-tool matrix: the only door is `python_execution`. |
| 2 | `loop_test.clj` | `normalize-tool-input-strings-only-test` (`:5857`), `tool-call-door-strings-only-test` (`:5903`) | Both survive and TIGHTEN: the door now has exactly one argument, `code`. |
| 2 | `loop_test.clj` | `human-input-parks-native-tool-wall-test` (`:5813`) | `human-input-parks-python-block-wall-test` — the parking behaviour is real and stays. |
| 2 | `loop_test.clj` | `native-tools-results-e2e-test` (`:2482`), `native-tool-result-pairing-test` (`:2636`) | Result PAIRING (call id ↔ result block) survives the machinery; re-expressed for the single tool. |
| 2 | `test/…/foundation/surface_contract_test.clj` | `surface-contract-test` (`:43`) | Becomes the registry gate: every symbol is Python-bound and carries `:description` + `:result`, no symbol declares a schema key. |
| 2 | `extensions/common/vis-foundation-search/test/…/core_test.clj` | `unified-search-test` (`:586-590`) | Two `:ext.symbol/native-tool?` expectations deleted; the rest of the describe stands. |
| 2 | `test/…/foundation/shell_test.clj` | `shell-one-wait-test` (`:1200`), `no-wait-knob-test` (`:2157`) | Drop the `:native-tool?` predicate/filter; keep the one-wait and no-knob claims. |
| 3 | `render_test.clj` | `result-summary-color-test` (`:75`) | "native-tool headlines" → "the python card headline". |
| 3 | `chat_test.clj` | `restore-block-record-test` (`:619-650`) | Keep the `:cards` restore half, delete the native-tool card identity half. |
| 4 | `env_python_test.clj` | `doc-apropos-surface-test` (`:328-413`, 19 sites) | Split into `apropos-searches-every-document-body-test` and `doc-retrieves-one-entry-test`; forgiving/blank/unknown slug cases arrive here from `self_docs_test`. |
| 4 | `test/…/foundation/harness/core_test.clj` | `skill-native-tool-test` (`:149-214`, 11 sites) | `skill-activation-is-a-session-effect-test` — activation still marks, resolves project root and resources; the schema half goes. |
| 5 | `compaction_verbs_test.clj` | the `tool-call->python-source` reference (`:40`) and the fold receipts | Fold assertions stop naming a recovery coordinate. |
| 7 | `prompt_test.clj` | `prompt-core-test` (`:100-180`) | Asserts the new sentences and, negatively, every removed term. |

**ADJUST — one line or one word, no structural change.**

`posix_refusal_shim_test/shell-toggle-off-test` (`:78`, 5 sites — a shell-shaped query is answered by
apropos TEXT, not by a vanished `shell` group) and `process-surface-is-said-once-test` (`:139`, drop
`apropos_table.py` from the scanned file list once that file is deleted);
`shim_attach_test/attach-discovery-test` (`:313`); `shim_ls_test/ls-shim-listing-test` (`:87`);
`sandbox_shim_contract_test/shim-source-test` (`:120` — "one `doc()`/`apropos` gist" becomes "the
first line of its `doc` text"); `env_python_form_eval_test/sandbox-auto-import-test` (`:168`) and
`run-python-block-form-eval-test` (`:1134`); `introspection_test/introspection-public-surface-test`
(`:49`); `editing/core_test/editing-extension-loads-test` (`:373`, drop the `:native-tool?` filter),
`find-fuzzy-fallback-test` (`:5064`, a fixture FILENAME mentions native tools) and
`grep-large-file-and-deadline-test` (`:6305`, a comment names the native-tool wall);
`human_input_test/blocking-wall-park-test` (`:709`, comment); `tool_surface_boundary_test` (`:75`,
unchanged claims, new tool count); `apps/vis-companion/src/components/ChatContent.tsx:534` (a comment
about `ntr[…]`-carrying metric bullets).

**ADD — 14 new describes, one per claim the change makes.**

`loop_test/only-python-execution-is-advertised-test` (1) · `surface_contract_test/every-function-is-a-python-name-test` (2) ·
`render_test/one-card-shape-test` and `render_test/python-block-error-row-test` (3) ·
`env_python_test/apropos-searches-every-document-body-test`, `…/apropos-does-not-activate-a-skill-test`,
`…/doc-resolves-function-before-page-test`, `…/doc-returns-whole-skill-body-test`,
`…/doc-index-is-curated-test`, `…/every-entry-has-a-usable-first-line-test` (4) ·
`env_python_test/ntr-is-gone-test`, `compaction_verbs_test/fold-receipt-has-no-recovery-coordinate-test` (5) ·
`editing/core_test/anchored-verbs-are-gone-test` (6) · `prompt_test/prompt-mentions-one-tool-test` (7).

## State of the plan

**REQUIRES WORK** — written, not yet reviewed; no phase has landed. Nothing is done.

TODO — one checkable step per line, in order. Each step names the file, the thing it does to it, and
the test that flips. A step is done when its own tests are green; a PHASE is done when its whole
block is green, lint and format are clean, and it is committed with its tests.

**Phase 1 — advertise only `python_execution`.**

1. `loop.clj:4843` — `native-tools` returns `[(finalize-engine-native-tool (python-execution-tool caps))]`; delete the `apropos`, `doc` and `session_fold` entries and the `native-tools-for` splice.
2. `loop.clj:4856` + call site `:5371` — delete `advertised-native-capability-names` and the `env/set-advertised-native-tools!` call.
3. `env_python.clj:939,945` — delete `set-advertised-native-tools!` and the `__vis_advertised_native_tools__` global.
4. `resources/vis-python/apropos_table.py` — delete the "already advertised, suppress it" branch that read that global.
5. ADD `loop_test/only-python-execution-is-advertised-test`; run `…loop-test`. Commit.

**Phase 2 — delete the declaration and dispatch machinery.**

6. `extension.clj:421-500` — delete the `:ext.symbol/native-tool?` and `:ext.symbol/schema` specs; `:440-448` `native-tool-root-union-keys` / `portable-native-tool-schema?`.
7. `extension.clj:993,1027,1036-1146` — delete `native-tools-for`, `native-tool-replay-policies`, `wire-schema`, `advertise-tool`, `native-tool-schemas`.
8. `extension.clj:1148-1224` — delete `native-tool-handlers`, `native-tool-call-shapes`, `native-tool-finish-call-renderers`, `native-tool-start-call-renderers`, `native-tool-tags`; `:2157` `native-tool-finish-call-renderers-by-op`; `:1226` `symbol-bound?` (every symbol is bound now).
9. `extension.clj:1495-1553` — delete the six refusal rules that only guarded schemas; `:1805` the `remove :ext.symbol/native-tool?` filter becomes an unconditional pass; `:2766-2769` loses the `symbol-bound?` filter.
10. `loop.clj:978-1060` — delete native handler execution and the `:vis/native-tool-timeout` error type; `:4707` `finalize-engine-native-tool`; `:4838` `engine-native-tool-call-shapes`; `:4867+` `py-literal` and `tool-call->python-source`; `:5169` `native-tool-call-block`; the lookups at `:5516-5527`, `:8257`, `:5565-5622`, `:9413`. The block builder becomes: one call, name `python_execution`, `:lang "python"`, `:source` = `code`.
11. `runtime_settings.clj:17-51` — `NATIVE_TOOL_TIMEOUT_MS` / `native-tool-timeout-ms` renamed to the python-block budget they now exclusively serve.
12. Strip `:native-tool?`, `:schema`, `:call`, `:render-start-call-fn`, `:render-finish-call-fn`, `:replay` from every registration: `foundation/editing/core.clj:6180,6272,6296,6341,6746,6966`; `foundation/language_surface.clj:1081,1111,1135,1178,1208`; `foundation/harness/core.clj:180`; `foundation/mcp/core.clj:1114`; `vis-foundation-search/.../core.clj:1848,1873,1898,1924,1948`. `:description` and `:result` REMAIN — they are the `doc(name)` body.
13. Tests: delete `native_tool_provider_contract_test.clj` (whole file); delete the four `extension_test` items; delete the seven `loop_test` items; delete `language_surface_test:442,591`, `shell_test:1469,1604`, `python_extensions_test:1672`; rewrite the six loop/shell/search entries listed in the REWRITE table.
14. ADD `surface_contract_test/every-function-is-a-python-name-test` — walk the live registry: no schema key survives, every symbol resolves to a bound sandbox name. Run `…extension-test`, `…loop-test`, `…python-extensions-test`, `…foundation.surface-contract-test`, `…foundation.shell-test`, `…foundation.language-surface-test`, the search extension test. Commit.

**Phase 3 — one card.**

15. `form.clj:43-63` — delete the badge identity block and the wire-name → label table; `:158-180` `native-tool-form?` collapses to "a form is a python block".
16. `channel_tui/render.clj` — delete the headline band branch (`:2570`), the tool identity in the cache key (`:3624`), `native-tool-error?` and its compaction (`:5201-5298`), the per-tool op-card path (`:5382-5399`), and the `(not= … "python_execution")` guards at `:5330,5482,5544,5598,5626,5645`.
17. `channel_tui/chat.clj:214,918` — drop the pre-rendered native-tool card path and the `:vis/tool-name` badge read-back.
18. `loop.clj:3854-4290` — the tool-result display path keeps `:stdout` verbatim, the "printed nothing" hint (`:4283`) and the error row.
19. Tests: delete `render_test/native-tool-error-compact-test`; rewrite `render_test:75` and `chat_test:619`; ADD `render_test/one-card-shape-test` and `render_test/python-block-error-row-test`. Run the two TUI namespaces in the `vis-channel-tui` REPL against `DefaultVirtualTerminal`, eyeball one `cap/shot!` PNG, then commit.

**Phase 4 — `apropos` searches, `doc` retrieves.**

20. Write the `clojure.spec.alpha` block (`:vis.doc/entry` = `name` + `text` + optional `call`; `:vis.apropos/hit` = entry + `score`) into `env_python.clj` BEFORE the implementation — it crosses into GraalPy.
21. `env_python.clj` — one corpus builder, four seeders (function registrations, `self_docs/pages`, `harness/discovery` `:content`, MCP tool descriptions), assembled once per env and cached with the docs registry.
22. `env_python.clj:1050-1080` — `apropos` becomes ranked full text over name/aliases + whole `text`; delete the group column and `__vis_groups__`; delete `resources/vis-python/apropos_table.py`'s group machinery (or the file, if nothing else remains).
23. `env_python.clj:1092-1116` — `doc(target)` resolves function → documentation page slug → skill name, returns the WHOLE text; bare `doc()` returns the curated index (one hand-ordered name vector beside the seeders); a miss answers with top `apropos` hits.
24. `extension.clj:3822-3939` — delete group assembly from `symbol-doc-text` / `sandbox-symbol-docs`; the first line of `:description` becomes the gist.
25. `foundation/self_docs.clj:52,94` and its symbol in `foundation/core.clj:100` — delete `vis_docs`; `pages`/`listing` survive as a seeding source only.
26. Tests: delete `self_docs_test.clj` (whole file) and `env_python_test/apropos-groups-and-bare-verb-docs-test`; rewrite `doc-apropos-surface-test` and `harness/core_test/skill-native-tool-test`; adjust `shim_attach_test:313`, `shim_ls_test:87`, `sandbox_shim_contract_test:120`, `posix_refusal_shim_test:78,139`; ADD the six phase-4 describes. Run `…env-python-test`, `…foundation.harness.core-test`, `…posix-refusal-shim-test`, the two shim tests. Commit.

**Phase 5 — retire `ntr`.**

27. `resources/vis-python/async_runtime.py` — delete `ntr` / `native_tools_results`: prime-on-scan (`:1884-1890`), the mapping + describe/scope/index helpers (`:2103-2550`), `__vis_native_result_scan__` (`:2542`), stored-result dict normalization (`:678, 852`), the print()s nudge (`:2220-2230`) and the `# saved:` sentences (`:2312, 2333, 2527-2530`). KEEP the direct-kwargs list at `:2004-2006` — `session_fold` still takes `gist=`.
28. `loop.clj:11156-11214` — delete the five `__vis_native_result_*` host callbacks.
29. `env_python.clj:886, 997-1002, 2117-2122` — drop `ntr` / `native_tools_results` / `__vis_native_result_*` from the protected-name, non-deferred and defer-exclusion sets; `:2696` loses the "re-read stored results with `ntr[…]`" nudge.
30. `persistance.clj:616-634` — delete the five `defdelegate`s; `vis-persistance-sqlite/.../core.clj:4487-4740` — delete the `ntr[tool_id]` index section, the five `db-native-result-*` fns and the result-body persistence that fed them, columns included (no compatibility layer).
31. **Fold surgery in `loop.clj`, in this order** (each step leaves the file compiling): delete `ntr-entries-of` (`:2017-2043`) and its caller branch in `stamp-iter-universe!` (`:2104-2105`, docstring `:2046`); delete `(declare ntr-recover-hint)` (`:2734`, AGENTS.md `declare` ban) together with `ntr-recover-hint` (`:3290-3296`) and its two call sites (`:3163`, `:4094`); drop `recover-hint` from the receipt builder (`:3140, 3244, 3270`); delete `code-ntr` (`:3384`), `recover-bullet` (`:3426-3460`) and the `recover `/`more results:` branches in `session-fold-card` (`:3514-3517`), leaving `markup-bullet` = `bold-lead-word`; delete `rec-ntr-ids` / `:summary-ntr-ids` (`:3568-3569, 3660-3665, 3690`) and the `recover` metric in the rendered summary (`:4091-4100`); delete the `# saved:` stamp emitter (`:4187`).
32. Descriptions and docs: `loop.clj:4775-4777` — `session_fold` says folding changes rendering, not storage, and a folded step is readable only via `await session_state()`; `loop.clj:4748` — `python_execution` loses the `# saved:`/`ntr` clause; `env_python.clj:1141` — sandbox `session_fold` docstring loses both `ntr` sentences; `resources/vis-docs/token-optimization.md:111,116` — delete the coordinate bullet and the introspection-OFF claim; `ChatContent.tsx:534` — reword the comment.
33. Tests: delete the four sqlite describes (`:2562-2925`), both `env_python_test` `ntr` describes (`:680, 723`), `compaction_verbs_test/session-fold-native-tool-test` (`:574-613`); rewrite the receipt-shape describes without `recover`/`more results:`; ADD `env_python_test/ntr-is-gone-test` (`NameError` for `ntr`, `native_tools_results`, every `__vis_native_result_*`) and `compaction_verbs_test/fold-receipt-has-no-recovery-coordinate-test`. Run `…env-python-test`, `…compaction-verbs-test`, the sqlite namespace. Commit.


**Phase 6 — remove `cat` and `patch`.**

34. `foundation/editing/core.clj:6272,6341` — delete the `cat` and `patch` registrations, then delete every internal fn no surviving verb calls (anchor hashing, range assembly, atomic multi-edit apply).
35. `prompt.clj:215-233` — replace the `cat`/`patch` sentences with "read with `grep`/`struct_*` or plain Python; write with `struct_patch` or plain Python".
36. `AGENTS.md` — update any rule naming `cat`/`patch`, in this same commit.
37. Tests: delete the `cat`/`patch` describes and `native-tools-flat-spec-guard` in `editing/core_test.clj`; ADD `anchored-verbs-are-gone-test`. Run `…foundation.editing.core-test` and confirm the `struct_patch` suite still carries structural editing. Commit.

**Phase 7 — prompt and measurement.**

38. **CORE prompt** — `prompt.clj:209-269`, in the order of §7.2 so the file compiles after each: (a) §1 `:214-215` drop `vis_docs()`, `:217-218` drop the JSON-Schema clause, ADD the three-line `apropos`/`doc`/`skill` discovery contract; (b) §2 `:219-220` delete the native-vs-Python routing fork, `:225-228` delete the four `ntr` lines keeping "Inspect shape before indexing", `:229-230` delete "Call advertised native tools directly", ADD "ONE call exists", "Batch independent work in ONE block" and the print-discipline sentence ending "a result you did not print is gone when the block ends"; (c) §3 `:232-235` → "**Filesystem work is Python**", `:239-241` drop `cat`/`patch` keeping the `grep`→`struct_index`→`struct_nodes`→`struct_patch` order, `:244` "BATCH every tool" → "BATCH inside one block"; (d) §6 `:261` drop "recovery IDs", ADD "a folded step is NOT re-readable". §4, §5, §7 untouched.
39. **Engine descriptions** — `loop.clj:4744-4758`: delete the `# saved:`/`ntr` sentence and the "engine-bound natives are bare snake_case" clause, open with "the only call", keep the shell-handle and file-descriptor paragraphs verbatim and the `caps` line. Then `loop.clj:4765-4797`: `session_fold`'s `:schema` argument prose folds into its `doc` text as the argument paragraph. Verify `apropos`/`doc` texts from Phase 4 pass the first-line lint.
40. **The four small blocks** — `language_surface.clj:110` docstring drops "call via the facade"; `harness/core.clj:351-353` → "`doc(\"name\")` reads a skill, `await skill(\"name\")` activates it (session effect)", `[project]` clause and the 180-char clip kept; `introspection.clj:1308` → "Folded content is readable ONLY here"; `prompt.clj:558-640` unchanged (verified, not edited).
41. **The anti-regrowth rule** — write the `:ext/prompt-fn` routing-and-policy-only rule into `extension.clj`'s prompt docstring and `extending.md`; make `symbol-doc-text` (`extension.clj:1785-1830`) the single renderer and drop its "Native tools use the separate contract below" split.
42. **Docs in the tree** — `AGENTS.md` (three edits per §7.6), `vis-docs/index.md:37-46` "Two gears" → "One gear", `vis-docs/token-optimization.md:111,116` `ntr` advice → print discipline. Same commit as the code.
42a. **Tests** — `prompt_test.clj` surgery exactly as listed in Phase 7's acceptance criteria (`:59-63`, `:76`, `:79-81`, `:91`, `:100`, `:112-115`, `:122`, `:145-147`, `:149-155`), ADD `prompt-names-one-tool-test` and `extension-fragments-do-not-restate-doc-text-test`. Run `…prompt-test`, `…loop-test`, `…env-python-test`, `…foundation.harness.core-test`, `…foundation.language-surface-test`, `…foundation.introspection-test`.
42b. **Measure** — clean REPL, fixture env (this repo, default extensions, `introspection` OFF), the seven metrics of Phase 7's measurement table on `main` and on the branch. Record the table HERE and in the commit message; if total first-request overhead did not drop by ≥40 %, find the miss before marking done. Then `lint_code`, `format_code`, commit, push, mark this plan **DONE**.

43. **A.** Delete `:ext.symbol/render-start-call-fn` / `:ext.symbol/render-finish-call-fn` spec keys, the `render-start-call-adapter` / `render-finish-call-adapter` in `python_extensions.clj`, and all 21 renderer defns plus the `shell-start-renderer` factory in `foundation/{editing/core,language_surface,shell,mcp/core,harness/core}.clj` and `vis-foundation-search/.../core.clj`. Keep `:ticker-fn` and `progress.clj:340-352`.
44. **A.** Read `shell-on-error`, `mcp-on-error`, `tool-failure-on-error`; delete `:on-error-fn` only if it is display-only, otherwise strip its display half. Record the verdict in this line.
45. **B.** Delete every `:schema {…}` literal in the seven files of the table (371 lines) in the same commit as the `:ext.symbol/schema` spec key.
46. **C.** Reduce `form.clj` to `display-fields` (python-block keys), `display-keys`, `with-display-code`, `->display`, `<-wire` — and `result-card`/`result-cards`, which §K keeps for PRINTED results, minus their `:vis/tool-name` lookup; drop the 14 identity/coalescing defs, the `core.clj` re-exports of `coalesce-forms`/`hide-tool-code?`, and the TUI call sites. DELETE `form_test.clj`'s `hide-tool-code?` describes and the coalescing describes in TUI `render_test.clj`; keep the card-geometry ones, rewritten against printed results.
47. **D.** Cut `extension_bootstrap.py` to `symbol(fn, name=None, tag='observation', is_hidden=False)` — docstring is the doc; delete the ~40 lines of native validation and every `Raw result:` rule (`extension.clj`, `loop.clj`, `extending.md`, the 9 test sites). Run `…python-extensions-test`.
48. **E.** Drop `tool_name` / `result_render` from `gateway/state.clj:1214-1236`, `ctx_engine.clj:1035-1044`, the sqlite `V1__schema.sql` columns and `persistance_sqlite/core.clj`, then `lib/types.ts`, `ChatContent.tsx`, `SessionScreen.tsx` and `ChatContent.test.tsx`. `tool_call_id` STAYS. Run `…gateway.state-test`, `…persistance-sqlite`, `npm run lint && npm run build`.
49. **F.** Delete `runtime_settings.clj:17-53` and all 6 `:vis/outside-tool-wall` sites plus the language-clojure `test_runner.clj` declarations; the Python eval watchdog is the only wall. Run `…language-surface-test`, `…loop-test`.
50. **G.** Delete the replay-policy path end to end — it has **no production producer**, so this removes behaviour that never ran. In order: `extension.clj:479-493` (spec), `:503` (`:opt` key), `:997,1021` (projection), `:1027-1034` (`native-tool-replay-policies`), `:1456-1457` (builder passthrough); then `loop.clj:4296-4376` (`successful-tool-call?`, `oversized-arg-receipts`, `compactable-native-call`, `plain-tool-results`, `compacted-native-replay`), `:4565` (drop the `& [replay-policies]` tail arg), `:4598-4599,4631` (binding + `cond` branch), `:7430,7459,7494,7556,7576` (emergency-fold parameter lists), `:8257,8270,8422-8423` (call site + request key). Keep `replay-target` — it decides thinking preservation and vision image replay. Delete `loop_test/large-arg-replay-compaction-test:1799-1846`; drop the `oversized-arg-receipts` assertion at `loop_test:5897`. Run `…loop-test`, `…extension-test`, `…ctx-engine-test`.
51. **H.** Remove `ntr`/`native_tools_results` from `env_python.clj:886` and `:1001-1002`, and delete `set-advertised-native-tools!`, `__vis_advertised_native_tools__` and the `apropos` suppression branch in `install-introspection!`.
52. **I.** Trace `:tag`'s consumers; delete it or record why it stays.
53. **J.** Rewrite `resources/vis-docs/extending.md` to the post-change authoring surface and fix `index.md:55` — before Phase 4 seeds pages into the `apropos` corpus.

**Presentation (K) — after step 15, before the rest of Phase 3.**

54. **K, the decision first.** In `loop.clj:5600-5620` replace `extension/native-tool-finish-call-renderers-by-op` with ONE data renderer: headline = the printed result's own `:op` plus a size the value already carries (`(count …)` of the dominant collection, else nothing), body = the pretty-printed value. It reads the RESULT, never a symbol table, so §A's 21 renderer defns can go in the same commit without a printed `grep` result degrading to raw stdout. ADD `loop_test/printed-result-card-headline-test` — a printed `{:op "grep" :matches {…}}` yields a headline naming `grep` and a body, with no extension registered.

55. **K, TUI source.** `channel_tui/render.clj`: KEEP `compact-tool-card-body-entries` (`:4769-4831`) and `tool-card-entries` (`:4833-4946`) — they paint printed results, which survive; DELETE the tool-identity half around them (`:2570` headline band branch, `:3611-3632` the `:vis/tool-name` component of the render cache key, `:5201`+`:5205-5208`+`:5265-5267` `native-tool-error?`, `:5298`, `:5331`, `:5381-5409` the "prefer `:result-render` for NATIVE TOOL forms only" gate, `:5483`+`:5513` `tool-label`, `:5599-5604` the running-native headline-first branch, `:5625-5652` the code-less op-card and its adjacent-card stacking, `:5859`+`:5929` the progress label's tool name). `virtual.clj:440,566` keep `:result-render` (it is now stdout) and keep `(map :body (:cards f))`. `chat.clj:214,918` as already written. Comment-only: `screen.clj:995`, `theme.clj:128`. After the edit the whole extension tree must answer ZERO for `native-tool|native_tool|tool-label|:vis/tool-name` — that grep is the step's own exit check.

55b. **K, TUI tests.** Three namespaces, three different verdicts, so nobody deletes by keyword: `render_test.clj` (50 display/card sites) SPLITS — the ~33 tool-identity sites die, the card-geometry ones are rewritten against printed results; `chat_test.clj:548,553,619,637-657` REWRITES (nine sites; `:619` keeps its claim, loses the per-tool matrix); `virtual_test.clj:150,333,370,673,1140` is UNTOUCHED — those five fixtures set `:result-render` as *stdout of a python block*, which is exactly what survives, and deleting them by prefix match would drop the height-estimation and huge-output regressions. `block_fixtures_test.clj` and `parity_test.clj` carry no tool-identity vocabulary and need no edit; confirm with the same zero-grep before claiming the step.

56. **K, companion source.** `ChatContent.tsx`: DELETE `toolLabelOverrides` (`:118-122`), `toolLabel` (`:970-973`), `compactToolSummary` (`:1010-1020`), `RUNNING_CODE_TOOLS` (`:1229`, one member left), the `ntr[…]` comment (`:534`, Phase 5) and every `form.tool_name` branch (`:1007, 1099, 1122, 1195, 1233, 1245, 1348, 1374-1376, 2039, 2195`); KEEP `toolCards`/`ToolCard`/`CardGrid`/`FormTrace` — a card is now "a printed result", keyed by `card.scope` and titled from the card's own `op`. `toolCards` (`:999-1008`) stops gating on `form.tool_name` and returns `form.cards ?? []`. `lib/types.ts` drops `result_render` (`:708`) and `tool_name` (`:728`) and rewrites the `sections` docstring at `:719`, which currently defines that field by contrast with `result_render`; `cards`, `tool_call_id` and the phase fields stay. `SessionScreen.tsx:507,516,561,686-688` — the form kind is always `"code"`, the merge key loses `tool_name`. `lib/artifacts.ts:239` reads the iteration's `op`, not `tool_name`. No other file in `apps/vis-companion/src` or `scripts/` names the vocabulary; the exit check is a zero-grep for `tool_name|toolLabel|result_render|RUNNING_CODE_TOOLS|compactToolSummary` over both trees.

56b. **K, companion tests and proof.** Re-fixture `artifacts.test.ts:163-189` and `ChatContent.test.tsx:228-231` against a card that carries only `op`/`scope`. ADD one companion test — `ChatContent.test.tsx/a-card-titles-itself-from-its-op` — so the app's title path is pinned to the result rather than to a label table. Then `npm run lint && npm run build`, and the SHIPPED-UI proof per AGENTS.md: `npm run dev`, `spel open`, `spel snapshot -i -c` on a session with a multi-result python block, and `getBoundingClientRect()` figures for the card band **at 390 and 1440**, reported in the answer — the band must not change height when the badge text goes, and `spel errors` must be empty. `ui.tsx` is NOT touched by this plan (no control is added or removed); if an edit here reaches for one, that is the signal to stop and design it.

57. **K, evidence for the TUI.** Run the TUI namespaces in the `vis-channel-tui` REPL against Lanterna `DefaultVirtualTerminal`, ADD `render_test/one-card-shape-test` and `render_test/python-block-error-row-test`, keep every surviving grid assertion, and attach one `cap/shot!` PNG of an iteration with three printed results (the screenshot is the eyeball, the grid assertions are the gate). That `render_test.clj` splits is why the test map counts it in both DELETE and REWRITE.

58. **L, delete the language-pack renderers too.** No exception list: step 43's sweep takes
    `render-lint-result` + `findings->table` (`language_surface.clj:498-567`), `render-test-result` +
    `failures->table` (`:569-700`), `render-test-call` (`:797-836`), `render-repl-start-result` (`:750-786`),
    `render-repl-eval-result` + `sect` (`:788-950`) and `render-format-result` (`:1093`), together with the `:render-finish-call-fn` /
    `:render-start-call-fn` entries at `:1093, 1121, 1150` and the `repl_eval` one. `dispatch!`
    (`:172-280`) is left returning the pack's raw result map, unwrapped and unmerged. Reviewer's check:
    `language_surface.clj` greps ZERO for `render-`, `->table` and `sect` afterwards, and the namespace
    shrinks by ~413 lines.

58b. **L, keep exactly two survivors, and prove each.** `test-target` (`:662-673`) survives only as the
    `:ticker-fn` label source — ADD `language_surface_test/running-tests-ticker-names-the-selection-test`.
    `:on-error-fn` keeps its error→DATA half and loses its display half — ADD
    `language_surface_test/a-pack-failure-is-data-not-a-python-exception-test`: a failing
    `repl_eval("clojure", …)` returns a map the block can print (`ex`/`root_ex`/`err` present), never raises
    a Python traceback around a Java stack trace. The SIX existing render describes in that namespace are
    DELETED, not rewritten — `render-test-result-test` (`:323`), `render-test-call-test` (`:439`),
    `render-repl-start-result-test` (`:462`), `render-repl-eval-result-test` (`:482`),
    `render-lint-result-names-target-test` (`:604`), `render-lint-snippet-test` (`:746`) — because the
    behaviour they pinned is gone by decision, and `format-schema-advertises-recursion-test` (`:552`) goes
    with phase 2's schemas: +7 describes, ~415 lines in the test map's DELETE column.

58c. **L, check the raw result is still legible.** With Phase 3's card in place, run one failing
    `run_tests("clojure")`, one `lint_code` with findings and one `repl_eval("clojure")` that throws, from a
    python block, and record the printed card in the commit message. The bar is NOT prettiness — it is that
    `file`, `row`, `level`, `message`, `expected`, `actual`, `ex` and `root_ex` are all present and
    addressable in the value. If a key is only reachable from a deleted renderer, that key was never in the
    result map and the pack must put it there.
