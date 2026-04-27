# vis-ext-self-debug

Opt-in extension that lets the agent introspect its own state from
inside `:code`. Every function is a pure read off the same DB tables
the projection layer reads from. Failures return `nil` / `[]`, never
throw.

Add the jar to the classpath (or list the package in your
`deps.edn` `:test` / `:dev` aliases) to enable. The extension binds
its functions under the `self` alias.

## Functions

| Form                              | Returns                                                                          |
| --------------------------------- | -------------------------------------------------------------------------------- |
| `(self/plan)`                     | Current sticky `plan-state` map for this query, or `nil`.                        |
| `(self/breadcrumbs [n])`          | Vector of `{:position int :breadcrumb str}`, oldest-first. `n` clips to last N.  |
| `(self/attempts [n])`             | Every code-block executed in this query: `[{:iteration-id :iteration :code :result :error :stdout :stderr :duration-ms} …]`. Oldest-first. |
| `(self/errors [n])`               | `(self/attempts)` filtered to entries with `:error` set. Default cap 50.          |
| `(self/turn-history [n])`         | Every query in this conversation as `{:turn-id :goal :outcome :answer :iterations}`. Oldest-first. |
| `(self/iteration-budget)`         | `{:current N :budget M :remaining K}` — same shape as `<system_state>.ITERATION`. |
| `(self/var-history 'sym)`         | Full version timeline for `sym`: `[{:value :code :version} …]`. Oldest-first.    |

## When to use

The projection (`<plan>`, `<breadcrumbs>`, `<system_state>`,
`<var_index>`) carries the bulk of what the agent needs every
iteration. Reach for `self/*` only when:

- You need a deeper history than the projection caps (e.g. last 20
  breadcrumbs versus *every* breadcrumb so far).
- You want to query attempts programmatically (filter by
  `:error`, group by `:code`, etc.) instead of scanning the projection.
- You want cross-turn introspection (`(self/turn-history)`) without
  paying the projection cost every iteration.

## When NOT to use

- For state that already lives in `<system_state>` /
  `<plan>` / `<breadcrumbs>`. Reading the projection is free; calling
  `self/*` costs an iteration round-trip via `:code`.
- As a substitute for setting `:doc` on code blocks. The right way to
  carry purpose into `<var_index>` is the spec field, not a runtime
  introspection call.

## Implementation notes

- **Env-aware via `:before-fn` injection.** Each impl fn is an
  ordinary `(defn- self-foo [env & args])` taking the environment map
  as its first parameter. A shared `:before-fn`
  (`inject-environment`) prepends `env` to the args vector, so the
  model still calls e.g. `(self/plan)` with zero arguments.
- **No side effects.** Every fn is a pure read. Misconfigured DB or
  missing context returns `nil`/`[]`; the agent's iteration is
  unaffected.
- **Reads the same tables the projection reads.** No new persistence
  surface, no schema change.
