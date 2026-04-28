# Meta extension

Package: `com.blockether/vis-meta`. Source: `extensions/vis-meta/`.
This opt-in extension lets the agent introspect its own state,
conversation history, and failure diagnostics from inside `:code`.
The functions bind under the `meta` alias and return plain Clojure
maps or vectors so the agent can manipulate the data structurally.

Every function is a pure read off the same persistence data the
projection layer reads from. Failures return `nil` / `[]`, never throw.
Use these functions instead of ad-hoc `sqlite3` calls from inside the
agent when triaging a stalled turn.

Add the jar to the classpath, or list `com.blockether/vis-meta` in
your `deps.edn`, to enable it.

## Current turn snapshot

Call: `(meta/turn)`. Snapshot of the current turn as a single map:

```clojure
{:id          "uuid"
 :goal        "user query text"
 :status      :running
 :plan        {:goal "..." :items [...] :open [...] :decided [...]}
 :breadcrumbs [{:position 0 :breadcrumb "..."}]
 :attempts    [{:iteration-id :iteration :code :result :error
                :stdout :stderr :duration-ms}]
 :errors      [...]
 :failures    [{:source :classification :iteration :message :advice}]
 :iteration   {:current N}
 :cost        {:input-tokens :output-tokens :reasoning-tokens
               :cached-tokens :total-cost :model}
 :redundancy  {:duplicate-count N :total-count N :fraction 0.0}
 :elapsed-ms  N}
```

Use this when you want to programmatically manipulate turn-level data.
For just reading the plan, breadcrumbs, or iteration pointer, the
projection (`<plan>`, `<breadcrumbs>`, `<system_state>`) already
delivers them in your prompt.

## Failure list

Call: `(meta/failures)` or `(meta/failures conversation-id)`. Returns
provider/schema and code/tool failures as normalized data:

```clojure
[{:source :provider
  :classification :provider-schema-rejected
  :iteration 4
  :reason :not-a-map
  :received-type "String"
  :raw-preview "Looking at what I have so far"
  :advice "..."}
 {:source :code
  :classification :regex-unsupported-escape
  :tool "vis/rg"
  :iteration 9
  :code "(vis/rg ...)"
  :message "Unsupported escape character: \\|"
  :advice "..."}]
```

Recognized classifications include:

- `:provider-schema-rejected` — provider returned prose/string instead
  of the svar iteration map.
- `:regex-unsupported-escape` — usually `\|` in a Clojure string.
- `:regex-unescaped-quote` — an inner quote split a regex string.
- `:patch-unbalanced-string` — malformed `vis/patch` EDN payload.
- `:patch-no-match` — `vis/patch` SEARCH block did not match exactly.

The one-argument form scans every turn in the given conversation and
adds `:turn-id` plus `:goal` to each failure.

## Turn diagnosis

Call: `(meta/diagnose)` or `(meta/diagnose conversation-id)`. Returns a
compact summary designed as the first stop for stalled-agent triage:

```clojure
{:turn-id "uuid"
 :goal "..."
 :status :running
 :failure-count 3
 :by-classification {:provider-schema-rejected 1
                     :patch-no-match 2}
 :failures [...]
 :next-actions ["..."]}
```

This is the supported replacement for raw SQLite checks from inside
`:code`. The function exposes provider previews, parse/tool error
messages, classifications, and next actions directly as Clojure data.

## Single conversation snapshot

Call: `(meta/conversation)` or `(meta/conversation conversation-id)`.
Snapshot of one conversation:

```clojure
{:id         "uuid"
 :channel    :vis
 :title      "..."
 :model      "claude-sonnet-4-5"
 :created-at #inst "..."
 :turns      [{:id :goal :outcome :answer :iterations :total-cost}]
 :turn-count N}
```

No-arg form returns the current conversation. Pass a UUID to inspect
any other conversation in the DB.

## Conversation list

Call: `(meta/conversations)` or `(meta/conversations channel)`. Vector
of every known conversation, newest-first:

```clojure
[{:id "..." :channel :vis :title "..." :created-at #inst "..."
  :turn-count N :external-id "..."}]
```

No-arg form scans every known channel (`:vis`, `:tui`, `:telegram`,
`:cli`). Pass a channel keyword to filter.

## Var history

Call: `(meta/var-history sym)` or `(meta/var-history sym conversation-id)`.
Full version timeline for a var:

```clojure
[{:value :code :version 0}
 {:value :code :version 1}]
```

Defaults to the current conversation. Pass a UUID to read from another
conversation. Accepts a symbol or string for `sym`.

## Search past attempts

Call: `(meta/find-attempts pattern)` or `(meta/find-attempts pattern conversation-id)`.
Regex search over executed `:code` strings:

```clojure
[{:turn-id :iteration-id :iteration :code :result :error}]
```

One-arg form searches only the current turn's attempts. Two-arg form
scans every turn of the given conversation. `pattern` accepts either a
string, compiled to a `Pattern`, or a `Pattern` directly.

## When to use it

- You need failure triage without leaving the sandbox:
  `(meta/diagnose)` or `(meta/failures)`.
- You need to programmatically manipulate state:
  `(filter :error (:attempts (meta/turn)))`.
- You want to inspect a different conversation than the one you're
  currently in.
- You want regex search over historical attempts.
- You want token / cost visibility.

## When not to use it

- For state already in the projection: `<plan>`, `<breadcrumbs>`,
  `<system_state>`, `<var_index>`. Reading the projection is free;
  calling `meta/*` costs an iteration round-trip via `:code`.
- As a substitute for setting `:doc` on code blocks. The right way to
  carry purpose into `<var_index>` is the spec field, not a runtime
  introspection call.
- For schema or migration debugging outside the agent. Host-side
  post-mortems can still inspect SQLite directly when needed.

## Implementation notes

- **Environment-aware via `:before-fn` injection.** Each implementation
  function takes the environment map as its first parameter. The shared
  `inject-environment` hook prepends that map to the args vector, so
  the model still calls `(meta/turn)` with zero arguments.
- **No side effects, never throws.** Every function is a pure read.
  Misconfigured DB or missing context returns `nil` / `[]`; the
  agent's iteration is unaffected.
- **No schema change.** The extension reads existing conversation,
  query, iteration, and expression rows through the persistence facade.
