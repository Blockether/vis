# Environment Map

Every callback an extension receives — `:ext/activation-fn`, optional
`:ext/prompt`, `:ext/nudge-fn`, and the symbol hooks (`:before-fn`,
`:after-fn`, `:on-error-fn`) — operates on the **environment**. This
is the runtime map that represents one live conversation context.

## All keys

### Conversation-scoped keys

These keys exist on every environment for its entire lifetime:

| Key | Type | Description |
|-----|------|-------------|
| `:environment-id` | `string` | Unique UUID string. Stable for the conversation lifetime. Use for log correlation. |
| `:conversation-id` | `java.util.UUID` | Conversation entity ID in the DB (plain UUID, not a tagged pair). Every query/iteration/var is parented under this. |
| `:db-info` | `map` | Database connection handle (`{:datasource ds …}`). Pass to `persistance.core` functions for reads. **Do not close it.** |
| `:router` | `map` | svar LLM router. Provider configs, model list, routing rules. Read-only from extensions, but the runtime itself **reseats** this key when provider config changes mid-conversation — see [Router Lifecycle](../architecture/state.md#router-lifecycle). Always read it fresh from the env handed to your callback; never cache `(:router env)` across iterations. |
| `:sci-ctx` | `SCI context` | Live SCI sandbox context. Contains the `:env` atom with all namespace maps. Read sandbox state via `(get-in @(:env sci-ctx) [:namespaces 'sandbox])`. **Do not mutate directly** — use `bind-and-bump!`. |
| `:sandbox-ns` | `SCI ns` | The `'sandbox` namespace object. Used internally by `eval-string+`. |
| `:initial-ns-keys` | `set of symbols` | Symbols in the sandbox at creation time (tools, helpers, builtins). Distinguishes user vars from infrastructure. |
| `:var-index-atom` | `atom` | Cached `<var_index>` render. Shape: `{:index string, :revision int, :current-revision int}`. The rendered string is compact pseudo-source (`(def ^{:v 3 :s :l :t :map :n 12} foo ...)`, `(defn ^{:v 2 :s :l} f [x] ...)`). Bump via `bump-var-index!` after mutating sandbox bindings. |
| `:extensions` | `atom of vector` | All registered extensions. Managed by `register-extension!` (replaces by `:ext/namespace`). Read by the iteration loop for nudges. |
| `:state-atom` | `atom` | Internal: `{:custom-bindings {sym val}, :environment <self-ref>, :conversation-id uuid}`. Extensions should not poke this. |
| `:depth-atom` | `atom of int` | Sub-RLM recursion depth. 0 for top-level queries. |

### Query-scoped keys

These keys are `assoc`'d onto the environment map when a query starts
(`query/core.clj :: prepare-query-context`). They do **not** exist on
the base environment returned by `create-environment`.

| Key | Type | Description |
|-----|------|-------------|
| `:current-iteration-atom` | `atom of int` | Plain integer counter for the iteration in flight (0-indexed). Bumped before every `store-iteration!`. Read by extensions that want to know which iteration they're in. |
| `:current-iteration-id-atom` | `atom of UUID or nil` | Entity ID (UUID) of the most recent `store-iteration!`. Created by `prepare-query-context`, reset to `nil` at query start, updated after each `store-iteration!`. Used for sub-RLM parenting and for log correlation. |

## Safe operations

- **Read** any key for conditional logic (`:db-info`, `:conversation-id`,
  `:sci-ctx`, `:router`, `:initial-ns-keys`).
- **Call** `persistance.core` functions with `:db-info` for DB reads.
- **Bump** the var-index cache via `bump-var-index!` after your tool
  mutates sandbox state.
- **Read** sandbox vars via
  `(get-in @(:env (:sci-ctx env)) [:namespaces 'sandbox sym])`.

## Prohibited operations

- **Close** `:db-info` — the runtime owns the connection lifecycle.
- **Swap** `:extensions` directly — use `register-extension!`.
- **Reset** `:current-iteration-id-atom` — internal iteration-loop state.
- **Mutate** `:sci-ctx` namespace maps without calling `bump-var-index!`
  — the `<var_index>` cache will serve stale data.
