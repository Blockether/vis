# vis-self-debug

Opt-in extension (`com.blockether/vis-self-debug`, ships from
`extensions/vis-self-debug/`) that lets the agent introspect its own
state and the underlying conversation/turn DB from inside `:code`.
Five functions, each returning a plain Clojure map or vector so the
agent can manipulate the data structurally with `(filter …)`,
`(get-in …)`, etc. — instead of making seven separate function calls.

Every function is a pure read off the same DB tables the projection
layer reads from. Failures return `nil` / `[]`, never throw.

Add the jar to the classpath (or list `com.blockether/vis-self-debug`
in your `deps.edn`) to enable. The functions bind under the `self`
alias.

## API

### Current turn snapshot

Call: `(self/turn)`. Snapshot of the current turn as a single map:

```clojure
{:id          "uuid"
 :goal        "user query text"
 :status      :running                     ;; | :done | :error | :interrupted
 :plan        {:goal "..." :items [...] :open [...] :decided [...]}  ;; or nil
 :breadcrumbs [{:position 0 :breadcrumb "..."} ...]
 :attempts    [{:iteration-id :iteration :code :result :error
                :stdout :stderr :duration-ms} ...]
 :errors      [...]                                    ;; subset of :attempts where :error is set
 :iteration   {:current N}                            ;; current iteration (1-indexed)
 :cost        {:input-tokens :output-tokens :reasoning-tokens
               :cached-tokens :total-cost :model}
 :redundancy  {:duplicate-count N :total-count N
               :fraction 0.0}                          ;; how often the agent repeated itself this turn
 :elapsed-ms  N}                                       ;; wall-clock for the turn
```

Use this when you want to programmatically manipulate turn-level
data. For just reading the plan / breadcrumbs / iteration pointer,
the projection (`<plan>`, `<breadcrumbs>`, `<system_state>`) already
delivers them in your prompt — no `self/turn` call needed.

### Single conversation snapshot

Call: `(self/conversation)` or `(self/conversation conversation-id)`.
Snapshot of one conversation:

```clojure
{:id         "uuid"
 :channel    :vis
 :title      "..."
 :model      "claude-sonnet-4-5"
 :created-at #inst "..."
 :turns      [{:id :goal :outcome :answer :iterations :total-cost} ...]
 :turn-count N}
```

No-arg form returns the **current** conversation. Pass a UUID
to inspect any other conversation in the DB.

### Conversation list

Call: `(self/conversations)` or `(self/conversations channel)`.
Vector of every known conversation, newest-first:

```clojure
[{:id "..." :channel :vis :title "..." :created-at #inst "..."
  :turn-count N :external-id "..."}                     ;; :external-id only for telegram etc.
 ...]
```

No-arg form scans every channel (`:vis`, `:tui`, `:telegram`,
`:cli`). Pass a channel keyword to filter:

```clojure
(self/conversations :telegram)        ;; → only :telegram conversations
(filter #(= "Bug fix" (:title %))
  (self/conversations))               ;; → cross-channel search by title
```

### Var history

Call: `(self/var-history sym)` or `(self/var-history sym conversation-id)`.
Full version timeline for a var:

```clojure
[{:value :code :version 0}
 {:value :code :version 1}
 ...]                                  ;; oldest-first
```

Defaults to the **current** conversation. Pass a UUID to read from
another conversation. Accepts symbol or string for `sym`.

### Search past attempts

Call: `(self/find-attempts pattern)` or `(self/find-attempts pattern conversation-id)`.
Regex search over executed `:code` strings:

```clojure
[{:turn-id :iteration-id :iteration :code :result :error} ...]
```

One-arg form searches only the **current turn**'s attempts. Two-arg
form scans every turn of the given conversation. `pattern` accepts
either a string (compiled to a `Pattern`) or a `Pattern` directly.

```clojure
(self/find-attempts "grep")           ;; current turn
(self/find-attempts #"\bdefn\b")      ;; current turn, compiled regex
(self/find-attempts "foo-fn" prior)   ;; any conversation
```

## When to use it

- You need to programmatically manipulate state (`(filter :error
  (:attempts (self/turn)))`, `(count (:turns (self/conversation)))`).
- You want to inspect a different conversation than the one you're
  currently in.
- You want regex search over historical attempts.
- You want token / cost visibility (the projection has none).

## When NOT to use it

- For state already in the projection: `<plan>`, `<breadcrumbs>`,
  `<system_state>`, `<var_index>`. Reading the projection is free —
  calling `self/*` costs an iteration round-trip via `:code`.
- As a substitute for setting `:doc` on code blocks. The right way to
  carry purpose into `<var_index>` is the spec field, not a runtime
  introspection call.

## Implementation notes

- **Env-aware via `:before-fn` injection.** Each impl fn is an
  ordinary `(defn- self-foo [env & rest])` taking the environment map
  as its first parameter. A shared `:before-fn`
  (`inject-environment`) prepends `env` to the args vector, so the
  model still calls e.g. `(self/turn)` with zero arguments.
- **No side effects, never throws.** Every fn is a pure read.
  Misconfigured DB or missing context returns `nil` / `[]`; the
  agent's iteration is unaffected.
- **Reads the same tables the projection reads.** No new persistence
  surface, no schema change.
