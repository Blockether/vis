# Bridge — Codebase Knowledge Graph Extension for Vis

> "Structure is authoritative. Semantics accelerate, never decide."
> — Codebase-Memory design principle

## 1. Rationale

### The problem

Vis agents operate on codebases through `v/cat`, `v/rg`, `v/patch`, `v/glob`. These are **flat file primitives** — grep a string, read a file, patch a line. The agent has no structural model of:

- What *calls* what (call graph)
- What *contains* what (hierarchy: project → namespace → defn → local)
- What *depends on* what (import graph)
- Which symbols are *public surface* vs implementation detail
- How a change in one node *impacts* its neighbors (blast radius)

Every query starts from zero. The agent re-discovers structure through repeated `v/rg` and `v/cat` calls across iterations — burning tokens, time, and context budget on what a single graph traversal would resolve in one hop.

### What exists

| System | Approach | Gap for Vis |
|--------|----------|-------------|
| **PageIndex** | Hierarchical JSON tree (AST-based). LLM navigates tree via reasoning. No edges, no call graph. | Tree-only, no relational queries. Good for docs, weak for code dynamics. |
| **CodexGraph** (NAACL 2025) | Property graph in Neo4j. Nodes: MODULE, CLASS, FUNCTION, METHOD, FIELD. Edges: CONTAINS, INHERITS, CALLS, USES. LLM writes Cypher. | Requires Neo4j. Schema is task-agnostic but heavyweight for Clojure-centric use. |
| **CGBridge** (arXiv 2512.07666) | GNN encoder + trainable bridge module. Pre-trains on 270K code graphs. Injects structure-informed prompts into frozen LLM. | Research artifact. Requires GNN training. Not a tool an agent calls. |
| **Codebase-Memory** (arXiv 2603.27277) | Tree-sitter → SQLite knowledge graph. 66 languages. 14 MCP tools. Property graph with typed nodes/edges. Louvain community detection. Zero-dep C binary. | Closest fit. But: C binary, MCP protocol, not a Vis extension. We steal the data model, not the binary. |
| **AOCI** (arXiv 2605.02421) | Symbolic-semantic index: one entry per file. Symbolic tag (path, layer, deps, public surface) + semantic description (role, invariants, error model). Query-independent blueprint. | Brilliant for *human-readable* orientation. Not a graph — no traversal. Good complement, not replacement. |
| **PyCodeKG** | AST → SQLite + LanceDB. Three-pass pipeline. Hybrid query (semantic seed + structural expand). | Python-only. Good hybrid pattern to steal. |

### What Vis needs

Vis runs in the JVM. It already has SQLite via `vis-persistance-sqlite`. It already has a Clojure-aware environment (`vis-lang-clojure`). It already has extension aggregates for durable state. It does **not** need a new database engine, a GNN, or a C binary.

It needs:

1. **A graph** — nodes for code entities, edges for relationships — stored in extension aggregates it already has.
2. **A builder** — one or more language-specific extractors that walk source and emit (node, edge) tuples. Clojure first; others later.
3. **Query tools** — SCI symbols under `bridge/` that let the agent traverse, search, and extract structural context without flat-file scanning.
4. **Semantic enrichment** — optional LLM-generated summaries per node (AOCI-style), cached in the graph.
5. **Incremental maintenance** — content-hash-based re-indexing of changed files only, not full rebuilds.

### The name

**Bridge** because it bridges the gap between flat-file agent operations and structural code understanding. Also a nod to CGBridge — the idea that structure-aware context makes LLMs better at code tasks.

---

## 2. Design Principles

1. **Structure is authoritative.** The graph is built from deterministic static analysis (AST, import resolution, call-site extraction). No hallucinated edges.
2. **Semantics accelerate, never decide.** LLM-generated summaries and embeddings are optional overlays. The graph is complete without them.
3. **Everything is traceable.** Every node maps to a file + line range. Every edge maps to a syntactic construct. No phantom nodes.
4. **Incremental, not rebuild.** Content hashes per file. Changed files are re-extracted; unchanged files are left alone. Graph stays warm across sessions.
5. **Meaningful-only indexing.** Not every file earns a rich graph. Thresholds and filters skip generated files, vendored deps, data files, and boilerplate. The builder emits a *relevance signal* per file; below threshold, the file gets a lightweight stub node (file exists, no children).
6. **Clojure-native.** First-class Clojure/JVM extraction. Clojure's `edamale` + `rewrite-clj` already in Vis. Other languages via pluggable extractors later.
7. **Aggregate-native.** Graph lives in Vis's existing extension aggregate store. No new tables, no DDL, no new database engine.

---

## 3. Data Model

### 3.1 Storage: Extension Aggregates

Bridge stores **everything** through Vis's extension aggregate API:

```clojure
(vis/ext-create! env row)
(vis/ext-put!    env row)   ;; upsert by key/kind/scope
(vis/ext-get     env query)
(vis/ext-list    env query)
(vis/ext-delete! env query)
(vis/ext-swap!   env query f & args)
```

Rows are scoped `:global` (the graph is project-wide, not per-conversation).
Each row is owned by the bridge extension — `:extension-id` is runtime-filled,
never supplied by caller code.

The aggregate table schema is:

```sql
-- Already exists, managed by vis-persistance-sqlite
CREATE TABLE extension_aggregate (
  id              TEXT PRIMARY KEY,
  extension_id    TEXT NOT NULL,
  aggregate_key   TEXT NOT NULL,
  kind            TEXT NOT NULL,
  metadata        TEXT,          -- JSON
  content         BLOB,          -- Nippy-encoded Clojure value
  -- ... scope FK columns (all NULL for :global scope)
  UNIQUE (extension_id, aggregate_key, kind, scope_key)
);
```

Bridge maps its graph concepts onto this KV shape:

| Graph concept | `aggregate_key` | `kind` | `content` (Nippy blob) |
|---------------|-----------------|--------|----------------------|
| Node | `"node:<qualified-name>"` | `:bridge/node` | Node map (see §3.2) |
| Edge | `"edge:<src-qname>::<kind>::<tgt-qname>"` | `:bridge/edge` | Edge map (see §3.3) |
| Index entry | `"idx:<file-path>"` | `:bridge/index` | Index map (see §3.4) |
| Summary | `"summary:<qualified-name>:<type>"` | `:bridge/summary` | Summary map (see §3.5) |

> **Why this works**: The aggregate UNIQUE constraint on
> `(extension_id, aggregate_key, kind, scope_key)` gives us dedup for free.
> `ext-put!` upserts by that tuple. `ext-list` with `:kind` filter gives us
> typed queries. `aggregate_key` encodes the graph identity (qualified names)
> so we can list by prefix pattern.

> **Limitation acknowledged**: This is a KV document store, not a relational
> graph engine. Traversal queries (callers-of, blast-radius) deserialize
> intermediate results in memory. For a codebase of Vis's size (~500 nodes,
> ~2000 edges), this is fine — the full graph fits in ~2 MB of Nippy blobs.
> If we ever hit scale limits, we add a dedicated SQLite table for edges
> and keep nodes in aggregates (see §3.7).

### 3.2 Nodes

Stored via `ext-put!`:

```clojure
(vis/ext-put! env
  {:key   "node:com.blockether.vis.core/run"  ;; unique aggregate_key per node
   :kind  :bridge/node
   :scope :global
   :metadata {:language  "clojure"
              :path "src/core.clj"
              :kind      "def"
              :relevance 1.0}
   :content {:name           "run"
             :qualified-name "com.blockether.vis.core/run"
             :file-path      "src/com/blockether/vis/core.clj"
             :line-start     142
             :line-end       168
             :signature      "([db-info opts])"
             :docstring      "Run the Vis iteration loop..."
             :kind           "def"
             :language       "clojure"
             :relevance      1.0
             :content-hash   "sha256:abc..."
             :metadata       {:private false :macro false}}})
```

#### Node `content` map shape

| Field | Type | Description |
|-------|------|-------------|
| `:name` | string | Short name (symbol, file name) |
| `:qualified-name` | string | Globally unique path-based name |
| `:file-path` | string \| nil | Source file path (nil for virtual/project-level) |
| `:line-start` | int \| nil | 1-based start line |
| `:line-end` | int \| nil | 1-based end line |
| `:signature` | string \| nil | Arglists, type signature, or interface signature |
| `:docstring` | string \| nil | Docstring from source |
| `:kind` | string | Node type discriminator |
| `:language` | string \| nil | "clojure", "java", "markdown", etc. |
| `:relevance` | float | 0.0–1.0 meaningfulness score |
| `:content-hash` | string \| nil | SHA-256 of the source span |
| `:metadata` | map | Extensible: `{:private true}`, `{:macro true}`, etc. |

#### Node kinds

| Kind | Description | Key properties |
|------|-------------|----------------|
| `project` | Root node, one per indexed project | `:qualified-name` = project name |
| `directory` | Filesystem directory | `:file-path` = dir path |
| `file` | Source file | `:file-path`, `:language`, `:relevance` |
| `namespace` | Clojure namespace (or Java package, etc.) | `:name` = ns symbol |
| `def` | `defn`, `def`, `defmacro`, `defmethod`, etc. | `:signature` = arglists, `:docstring` |
| `defn-private` | `defn-` / private def | Same as def, `:metadata` includes `{:private true}` |
| `protocol` | `defprotocol` | `:name`, methods in `:metadata` |
| `record` | `defrecord` | `:name`, fields in `:signature` |
| `type` | `deftype` | `:name`, fields in `:signature` |
| `multimethod` | `defmulti` / `defmethod` | `:name`, dispatch fn in `:signature` |
| `var` | Clojure var (dynamic, defonce, etc.) | `:name` |
| `class` | Java/interop class | `:name`, `:signature` = constructors |
| `interface` | Java interface | `:name`, methods in `:metadata` |
| `heading` | Markdown heading (for doc graphs) | `:name` = heading text, `:line-start` |
| `section` | Markdown section (heading + body) | `:name` derived from heading |

> **Extensibility**: `:kind` is a free-form string. Language extractors define
> their own kinds. The query layer is kind-agnostic.

### 3.3 Edges

Stored via `ext-put!`:

```clojure
(vis/ext-put! env
  {:key   "edge:com.blockether.vis.core/run::calls::com.blockether.vis.internal.loop-core/iterate!"
   :kind  :bridge/edge
   :scope :global
   :metadata {:edge-kind "calls"
              :source    "com.blockether.vis.core/run"
              :target    "com.blockether.vis.internal.loop-core/iterate!"
              :path "src/com/blockether/vis/core.clj"}
   :content {:source    "com.blockether.vis.core/run"
             :target    "com.blockether.vis.internal.loop-core/iterate!"
             :kind      "calls"
             :weight    1.0
             :metadata  {:call-sites [147 153]}}})
```

The `aggregate_key` for edges is `"edge:<source-qname>::<edge-kind>::<target-qname>"`.
This gives us uniqueness per (source, kind, target).

Edge queries use metadata filtering — no content deserialization needed:

```clojure
;; All edges from a node (upstream callers)
(vis/ext-list env {:kind :bridge/edge :metadata {:source "com.blockether.vis.core/run"}})

;; All edges to a node (downstream callees)
(vis/ext-list env {:kind :bridge/edge :metadata {:target "com.blockether.vis.internal.loop-core/iterate!"}})

;; All edges for a file (re-indexing)
(vis/ext-list env {:kind :bridge/edge :metadata {:path "src/com/blockether/vis/core.clj"}})
```

#### Edge `content` map shape

| Field | Type | Description |
|-------|------|-------------|
| `:source` | string | Qualified name of source node |
| `:target` | string | Qualified name of target node |
| `:kind` | string | Relationship type |
| `:weight` | float | Default 1.0, for ranking |
| `:metadata` | map | `{:call-sites [147 153]}`, `{:confidence 0.9}`, etc. |

#### Edge kinds

| Kind | Source → Target | Description |
|------|----------------|-------------|
| `contains` | parent → child | Structural nesting: project → dir, dir → file, file → ns, ns → def |
| `calls` | def → def | A calls B. `:metadata` includes `{:call-sites [line ...]}` |
| `imports` | namespace → namespace | `(:require ...)` / `import`. Cross-ns dependency. |
| `refers` | def → def | Symbol reference without invocation |
| `implements` | record/type → protocol | Record implements protocol |
| `extends` | type → type | Inheritance / protocol extension |
| `exports` | namespace → def | Public surface (non-private defs) |
| `documents` | heading/section → def/namespace | Doc heading references code entity |
| `depends-on` | namespace → namespace | Transitive import (derived, for blast-radius) |
| `tested-by` | def → def | Test function exercises source function |

### 3.4 Index state

Stored via `ext-put!`:

```clojure
(vis/ext-put! env
  {:key   "idx:src/core.clj"
   :kind  :bridge/index
   :scope :global
   :content {:file-path     "src/core.clj"
             :language      "clojure"
             :content-hash  "sha256:abc..."
             :extractor-version 1
             :node-count    12
             :edge-count    31
             :relevance     1.0
             :indexed-at    1715400000000}})
```

Tracks which files have been indexed, with what extractor version, and their
content hash. On re-index:
- Hash unchanged → skip
- Hash changed → delete old nodes/edges for this file, re-extract, insert new
- File deleted → delete old nodes/edges

### 3.5 Semantic summaries (optional)

Stored via `ext-put!`:

```clojure
(vis/ext-put! env
  {:key   "summary:com.blockether.vis.core/run:role"
   :kind  :bridge/summary
   :scope :global
   :content {:node-id      "com.blockether.vis.core/run"
             :summary-type :role
             :summary      "Sole entry point for the Vis iteration loop..."
             :model        "gpt-4o"
             :content-hash "sha256:abc..."}})
```

AOCI-inspired. Not required for graph operation. When available, the agent
can request a node's summary to get architectural intent without reading
full source.

### 3.6 Meaningfulness filtering

Not every file deserves deep indexing. The builder applies a **relevance filter** before extraction:

| Signal | Skip? | Relevance |
|--------|-------|-----------|
| File in `.git/`, `target/`, `.lsp/`, `.clj-kondo/` | Yes | 0.0 |
| File matches `.bridgeignore` patterns | Yes | 0.0 |
| File is generated (header comment `Auto-generated`, `DO NOT EDIT`) | Yes | 0.0 |
| File is binary (images, compiled classes, jars) | Yes | 0.0 |
| File is a data file (`.json`, `.edn` config without code structure) | Partial | 0.3 |
| File is a test file | No | 0.8 |
| File is a source file with `defn` / class definitions | No | 1.0 |
| File is a source file with only imports/requires (thin facade) | No | 0.5 |
| Markdown / doc file | No | 0.7 |

Files with relevance < 0.2 get a stub `file` node only (no children).
Files with relevance ≥ 0.2 get full extraction.

### 3.7 Scale escape hatch

The aggregate KV model works for codebases up to ~5K nodes / ~20K edges.
Beyond that, in-memory traversal becomes slow and we'd need a dedicated
relational table for edges with indexed `source` / `target` columns.

**If we hit this**, the migration path is:
1. Keep nodes in extension aggregates (they're looked up by key, which aggregates handle well).
2. Add a `bridge_edge` table via the V1 schema migration (DDL exception per AGENTS.md).
3. Edge traversal becomes indexed SQL JOINs instead of Nippy deserialization.

This is a future decision, not a v1 requirement. Vis's own codebase is ~500 nodes.

---

## 4. Extension Structure

```
extensions/common/vis-bridge/
├── BRIDGE.md                                    ← this document
├── deps.edn                                     ← {:paths ["src" "resources"]}
├── resources/
│   └── META-INF/
│       └── vis-extension/
│           └── vis.edn                          ← manifest
└── src/com/blockether/vis/ext/bridge/
    ├── core.clj                                 ← extension registration, SCI symbols, extract/fill/reindex
    ├── doctor.clj                               ← doctor checks (CommonMark + clojure-lsp)
    └── languages/
        ├── schema.clj                           ← normalized fact + aggregate-row schema
        ├── registry.clj                         ← language dispatch
        ├── clojure.clj                          ← Clojure extractor via external clojure-lsp
        └── markdown.clj                         ← CommonMark heading/section extractor
```

Test namespace:

```
test/com/blockether/vis/ext/bridge/
├── core_test.clj
├── doctor_test.clj
└── languages/
    ├── schema_test.clj
    ├── registry_test.clj
    ├── clojure_test.clj
    └── markdown_test.clj
```

---

## 5. SCI Surface (bridge/ alias)

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `bridge/extract` | `([] [opts])` | Generic extraction. `:path` for one file, `:paths` for many files, `:language` + `:project-root` for project extract. Relative paths resolve against Vis' active workspace root. Returns normalized facts only. |
| `bridge/extract-file` | `([path] [path opts])` | One-file extraction dispatched by path/language. Returns normalized facts only. |
| `bridge/aggregate-rows` | `([facts] [facts opts])` | Pure preview: normalized facts → extension aggregate rows. No writes. |
| `bridge/fill!` | `([facts] [facts opts])` | Persist facts via extension aggregates. Replacement modes: `:path`, `:language`, `:all`, `:none`. |
| `bridge/extract-and-fill!` | `([] [opts])` | Extract then fill. `{:path ...}` force-reindexes one file; `{:language ...}` refreshes a language. |
| `bridge/backfill!` | `([opts])` | Incremental path reindex. Compares current SHA-256 against stored `:bridge/index` hash; extracts/fills only changed paths. |
| `bridge/index-status` | `([])` | Show index state: file count, node/edge counts, last indexed, stale files. |
| `bridge/node` | `([qualified-name])` | Get node by qualified name. Returns full node map. |
| `bridge/neighbors` | `([qualified-name] [qualified-name opts])` | Get adjacent nodes. Opts: `:direction :out/:in/:both`, `:edge-kinds`, `:depth`, `:limit`. |
| `bridge/callers` | `([qualified-name])` | Who calls this? Upstream traversal along `calls` edges. |
| `bridge/callees` | `([qualified-name])` | What does this call? Downstream traversal along `calls` edges. |
| `bridge/blast-radius` | `([qualified-name])` | All nodes reachable from this one. For impact analysis before changes. |
| `bridge/path` | `([from-qname to-qname])` | Shortest path between two nodes. Returns edge sequence. |
| `bridge/search` | `([query] [query opts])` | Search nodes by name pattern, kind, file pattern. Opts: `:kinds`, `:file-pattern`, `:limit`. |
| `bridge/exports` | `([qualified-name])` | Public surface of a namespace (all `exports` edges). |
| `bridge/summary` | `([qualified-name])` | Get or generate LLM summary for a node. |
| `bridge/subgraph` | `([opts])` | Extract a subgraph as data (for context injection). Opts: `:roots`, `:depth`, `:edge-kinds`. |
| `bridge/invalidate` | `([file-paths])` | Mark files as needing re-index. Next `bridge/index` picks them up. |

---

## 6. Extraction Pipeline

### Phase 1: Discover

1. Walk Vis' active workspace root, enumerate files. Bridge resolves relative paths through `com.blockether.vis.internal.workspace-context/cwd`, the same dynamic workspace binding used by Foundation tools.
2. Apply `.bridgeignore` + built-in skip patterns.
3. Compute content hashes.
4. Compare against index aggregates; partition into `:unchanged`, `:stale`, `:new`, `:deleted`.

### Phase 2: Filter

1. Score each `:new`/`:stale` file for relevance.
2. Partition into `:full-extract` (relevance ≥ 0.2) and `:stub` (relevance < 0.2).

### Phase 3: Extract

For each `:full-extract` file, dispatch to language-specific extractor:

**Clojure extractor** (`languages/clojure.clj`):
- Run external `clojure-lsp dump --raw`.
- Convert namespace definitions to `:module` nodes.
- Convert var definitions to `:symbol` nodes.
- Put function/macro/protocol/record/var detail in `:metadata {:symbol-kind ...}`.
- Convert namespace usages / dep-graph entries to `:imports` edges.
- Convert var usages to `:uses` edges.
- Emit normalized facts only; no storage writes.

**Markdown extractor** (`languages/markdown.clj`):
- Parse CommonMark headings.
- Emit `:file` + `:doc-section` nodes.
- Keep fenced code blocks as doc-section payload metadata, not graph nodes.
- Inline-code references become `:mentions` edges.
- Markdown links become `:links-to` edges.

### Phase 4: Build / fill

Bridge separates extraction from filling:

```clojure
(bridge/extract {:path "README.md"})       ;; facts only
(bridge/aggregate-rows facts)              ;; pure facts -> rows preview
(bridge/fill! facts {:replace :path})      ;; write extension aggregates
(bridge/backfill! {:paths changed-paths})  ;; hash-guarded incremental fill
```

Each `:bridge/index` row stores `:hash-sha256`. Backfill computes current
SHA-256 for candidate paths, compares it to the index row, and skips unchanged
files. This is the source-change guard: extraction work happens only for paths
whose content hash changed or whose index row is missing/stale.


1. Delete old nodes/edges for stale/deleted files.
2. Insert new nodes. Collect generated IDs.
3. Insert new edges (resolve target qualified names → IDs).
4. Update index aggregates via `ext-put!`.
5. All via batched `ext-delete!` + `ext-put!` calls.

### Phase 5: Enrich (optional, deferred)

1. For nodes without summaries, queue LLM summary generation.
2. Run asynchronously (not in the agent's critical path).
3. Store in summary aggregates via `ext-put!`.

---

## 7. Integration with Vis

### 7.1 Extension registration

Standard Vis extension. `:ext/ns-alias {:ns vis.ext.bridge :alias bridge}`.

Requires `vis-foundation` (for `v/` tools).

### 7.2 Persistence

Bridge stores **everything** through extension aggregates (`vis/ext-put!`,
`vis/ext-list`, etc.). No custom SQL tables. No DDL migration.

The aggregate KV schema gives us:
- **Dedup** via `UNIQUE (extension_id, aggregate_key, kind, scope_key)`
- **Upsert** via `ext-put!`
- **Typed queries** via `:kind` filter (`:bridge/node`, `:bridge/edge`, etc.)
- **Ownership** via runtime-filled `extension_id`

#### 7.2.1 Required: metadata JSON filtering

Bridge needs one enhancement to the extension aggregate query layer
that does not exist today: **metadata JSON field filtering**.

Current `ext-list` / `ext-delete!` support filtering by `:id`, `:key`,
`:kind`, and scope FK columns. They do NOT support filtering by fields
inside the `metadata` JSON column.

Bridge needs this because:

| Bridge query | Required metadata filter |
|---|---|
| Edges from source X | `:metadata {:source "..."}` |
| Edges to target Y | `:metadata {:target "..."}` |
| Nodes in file F | `:metadata {:path "..."}` |
| Delete nodes for file | `:metadata {:path "..."}` |
| Nodes of kind "def" | `:metadata {:kind "def"}` |

**Implementation**: add one clause to `extension-aggregate-clauses` in
`persistance_sqlite/core.clj`, thread it through the persistance facade
(which already passes `:metadata` through via `dissoc`),
and add a metadata JSON index:

**Done.** Changes applied:

1. `extensions/persistance/vis-persistance-sqlite/src/.../core.clj` —
   `extension-aggregate-clauses` now accepts `:metadata` map in opts,
   generates `json_extract(metadata, '$.field') = value` WHERE conditions.

2. `extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql` —
   `idx_extension_aggregate_metadata` index on `(extension_id, kind, json_extract(metadata, '$.kind'))`.
   `idx_extension_aggregate_meta_source` on `(extension_id, kind, json_extract(metadata, '$.source'))`.
   `idx_extension_aggregate_meta_target` on `(extension_id, kind, json_extract(metadata, '$.target'))`.
   `idx_extension_aggregate_meta_path` on `(extension_id, kind, json_extract(metadata, '$.path'))`.

3. `src/.../extension_aggregate.clj` — **no change needed**. `normalize-query`
   already passes `:metadata` through via `(dissoc query :key :scope)`,
   and `normalize-row` passes it through the same way for writes.

Usage:

```clojure
;; Filter edges from a specific source node
(vis/ext-list env {:kind :bridge/edge :metadata {:source "com.blockether.vis.core/run"}})

;; Delete all nodes for a file during re-indexing
(vis/ext-delete! env {:kind :bridge/node :metadata {:path "src/core.clj"}})

;; Find nodes by their code kind
(vis/ext-list env {:kind :bridge/node :metadata {:kind "def"}})

;; Combined metadata + kind filter
(vis/ext-list env {:kind :bridge/node :metadata {:path "src/core.clj" :kind "def"}})
```

This is a **small, general-purpose enhancement** that benefits any
extension wanting structured queries over its metadata. Zero schema
changes. The `metadata` column is already JSON.

**Without this**: Bridge must load ALL `:bridge/edge` rows, deserialize
Nippy content, and filter in memory for every traversal query. This
works at ~500 nodes but defeats the purpose of structured storage.

**With this**: Bridge queries become indexed JSON lookups. No full-scan
overhead. Scales cleanly to 10K+ nodes.

#### 7.2.2 Design: searchable fields in metadata, not content

Bridge puts **searchable fields** in `metadata` (JSON, queryable) and
**full data** in `content` (Nippy blob, opaque but fast to deserialize).

```clojure
{:key    "edge:src::calls::tgt"
 :kind   :bridge/edge
 :scope  :global
 :metadata {:edge-kind "calls"       ;; searchable via json_extract
            :source    "core/run"     ;; searchable (indexed)
            :target    "lc/iterate!"   ;; searchable (indexed)
            :path "src/core.clj"} ;; searchable (indexed, enables re-index)
 :content  {:source    "core/run"     ;; full data, Nipy-encoded
            :target    "lc/iterate!"
            :kind      "calls"
            :weight    1.0
            :metadata  {:call-sites [147]}}}
```

Rule: if Bridge needs to filter on it, it goes in `metadata`.
If it's payload data, it goes in `content`. Some fields appear in both.

### 7.3 Environment info

`bridge/environment-info-fn` contributes:

```
bridge.indexed: true
bridge.nodes: 847
bridge.edges: 2103
bridge.languages: [clojure markdown]
bridge.stale_files: 3
```

### 7.4 Nudge

`bridge/nudge-fn` emits a nudge when:
- Stale files > 0 and agent is about to edit code: `"bridge/index has N stale files. Run (bridge/index) to refresh the graph."`

### 7.5 Prompt

`bridge/prompt` includes a compact guide:
```
bridge/ — codebase knowledge graph. Run (bridge/index) first.
(bridge/node "com.blockether.vis.core/run") — inspect a node.
(bridge/callers "com.blockether.vis.core/run") — who calls it.
(bridge/blast-radius "com.blockether.vis.core/run") — impact analysis.
(bridge/search "transcript" {:kinds ["def"]}) — find nodes by name.
(bridge/subgraph {:roots ["ns.core"] :depth 2}) — extract context.
```

---

## 8. Future directions (not in v1)

- **Language plugins**: Java extractor (via `javaparser`), TypeScript extractor (via tree-sitter), Python extractor.
- **Embeddings**: Optional vector index on node summaries for semantic search (LanceDB or SQLite-vec).
- **Community detection**: Louvain modularity on the call graph to identify module boundaries.
- **Git co-change**: `co-occurs` edges from git history. `FILE_CHANGES_WITH` from commit analysis.
- **Cross-project indexing**: Multiple Vis projects sharing a graph store.
- **ADR integration**: Architecture Decision Records as first-class nodes with `documents` edges.
- **MCP server**: Expose Bridge as an MCP server for non-Vis agents.
- **Visualization**: `vis-mermaid` integration to render subgraphs as Mermaid diagrams in the TUI.

---

## 9. Key references

| Paper / System | What we take from it |
|----------------|---------------------|
| **Codebase-Memory** (arXiv 2603.27277) | Property-graph data model, SQLite storage, incremental re-indexing via content hashes, 6-phase pipeline, Louvain communities. |
| **CodexGraph** (NAACL 2025) | Unified graph schema (MODULE, CLASS, FUNCTION), Cypher-style queries, LLM-as-query-builder pattern. |
| **CGBridge** (arXiv 2512.07666) | The bridge metaphor: structure-informed context injection into LLM reasoning. Cross-modal alignment. |
| **AOCI** (arXiv 2605.02421) | Symbolic-semantic indexing per file. Query-independent blueprint. Incremental entry maintenance. |
| **PageIndex** (github.com/arifkasim/PageIndex) | Hierarchical tree + LLM reasoning navigation. "Similarity ≠ relevance." |
| **PyCodeKG** (github.com/Flux-Frontiers/pycode_kg) | Three-pass AST pipeline, hybrid query (semantic seed + structural expand), MCP tool surface. |
