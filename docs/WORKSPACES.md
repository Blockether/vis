# Workspace capabilities

Vis core does not depend on a specific filesystem snapshot implementation.
Workspace extensions register backends through `:ext/workspace-backends`; core
selects an available backend by declared capabilities.

## Capability matrix

The closed capability vocabulary is:

- `:isolated-fork` - create a distinct working root from a source root.
- `:merge-back` - changes can be landed into the source/trunk root.
- `:rollback` - discarding the working root restores pre-fork filesystem state.
- `:retained-revisions` - accepted roots can remain addressable for undo/redo.
- `:parallel-safe` - independent forks can execute concurrently.

Use `vis/workspace-capability-matrix` for explicit source/store roots, or
`vis/workspace-capabilities-for` for a workspace/root using Vis's actual derived
storage location. Matrix entries include backend id, priority, capabilities,
availability, and optional unavailability reason/details.

Extension discovery owns registration. Capability reads never trigger extension
loading, so backend availability has no hidden load-order side effects.

## Backend contract

Each backend descriptor contains:

```clojure
{:workspace.backend/id :example
 :workspace.backend/priority 100
 :workspace.backend/capabilities #{:isolated-fork :rollback}
 :workspace.backend/available-fn
 (fn [{:keys [source-root store-root]}]
   {:available? true})
 :workspace.backend/fork-fn
 (fn [{:keys [source-root store-root name]}] ...)
 :workspace.backend/discard-fn
 (fn [{:keys [root]}] ...)}
```

Persisted workspace rows record the backend id so cleanup dispatches to the same
implementation that created the root. `:live` identifies shared, non-isolated
roots and is never deleted by workspace cleanup.

## Capability gating

- Full drafts require isolation, merge-back, rollback, and retained revisions.
  When no registered backend covers that set for the active root, `/draft new`
  reports the capability matrix instead of silently falling back to a shared
  root.
- `sub_loop` clones the parent workspace when an isolation backend is available
  for the parent root, and otherwise runs in a shared trunk row at that root
  (safety then rests on disjoint `:files`).

Rift is implemented by the `vis-workspace-rift` extension. Its availability
probe checks both the real source root and the real Vis workspace store, so a
supported source filesystem does not mask an unsupported destination.
