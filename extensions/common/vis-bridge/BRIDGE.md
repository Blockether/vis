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

1. **A graph** — nodes for code entities, edges for relationships — stored in SQLite tables it already owns.
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
7. **SQLite-native.** Graph lives in Vis's existing SQLite connection. No new database engine. HoneySQL for all queries.

---

## 3. Data Model

### 3.1 Overview

Property graph in two core tables: `bridge_node` and `bridge_edge`. One metadata table: `bridge_index`. All in the same SQLite database Vis already uses.

```
bridge_index        — indexing state (file hashes, extractor versions, timestamps)
bridge_node         — entities (files, namespaces, defs, protocols, records, ...)
bridge_edge         — relationships (contains, calls, imports, implements, ...)
bridge_node_summary — optional LLM-generated semantic summaries per node
```

### 3.2 Nodes

```sql
CREATE TABLE bridge_node (
  id              INTEGER PRIMARY KEY AUTOINCREMENT,
  kind            TEXT    NOT NULL,  -- node type discriminator
  name            TEXT    NOT NULL,  -- short name (symbol, file name)
  qualified_name  TEXT    NOT NULL,  -- globally unique path-based name
  file_path       TEXT,              -- source file (NULL for virtual/project-level nodes)
  line_start      INTEGER,          -- 1-based line where the entity begins
  line_end        INTEGER,          -- 1-based line where the entity ends
  signature       TEXT,             -- arglists / type signature / interface signature
  docstring       TEXT,             -- docstring from source, if present
  metadata_json   TEXT,             -- extensible JSON blob (decorators, tags, visibility, etc.)
  content_hash    TEXT,             -- hash of the source span this node was extracted from
  language        TEXT,             -- "clojure", "java", "markdown", etc.
  relevance       REAL DEFAULT 1.0, -- 0.0-1.0: how "meaningful" this node is
  created_at      TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at      TEXT NOT NULL DEFAULT (datetime('now')),

  UNIQUE(qualified_name)
);

CREATE INDEX idx_node_kind    ON bridge_node(kind);
CREATE INDEX idx_node_file    ON bridge_node(file_path);
CREATE INDEX idx_node_name    ON bridge_node(name);
CREATE INDEX idx_node_lang    ON bridge_node(language);
```

#### Node kinds

| Kind | Description | Key properties |
|------|-------------|----------------|
| `project` | Root node, one per indexed project | `qualified_name` = project name |
| `directory` | Filesystem directory | `file_path` = dir path |
| `file` | Source file | `file_path`, `language`, `relevance` |
| `namespace` | Clojure namespace (or Java package, etc.) | `name` = ns symbol |
| `def` | `defn`, `def`, `defmacro`, `defmethod`, etc. | `signature` = arglists, `docstring` |
| `defn-private` | `defn-` / private def | Same as def, `metadata_json` includes `{:private true}` |
| `protocol` | `defprotocol` | `name`, methods listed in `metadata_json` |
| `record` | `defrecord` | `name`, fields in `signature` |
| `type` | `deftype` | `name`, fields in `signature` |
| `multimethod` | `defmulti` / `defmethod` | `name`, dispatch fn in `signature` |
| `var` | Clojure var (dynamic, defonce, etc.) | `name` |
| `class` | Java/interop class | `name`, `signature` = constructors |
| `interface` | Java interface | `name`, methods in `metadata_json` |
| `heading` | Markdown heading (for doc graphs) | `name` = heading text, `line_start` |
| `section` | Markdown section (heading + body) | `name` derived from heading |

> **Extensibility**: `kind` is a free-form string. Language extractors define their own kinds. The query layer is kind-agnostic — it filters on `kind` when asked, but doesn't hardcode the set.

### 3.3 Edges

```sql
CREATE TABLE bridge_edge (
  id          INTEGER PRIMARY KEY AUTOINCREMENT,
  source_id   INTEGER NOT NULL REFERENCES bridge_node(id),
  target_id   INTEGER NOT NULL REFERENCES bridge_node(id),
  kind        TEXT    NOT NULL,  -- relationship type
  weight      REAL    DEFAULT 1.0,
  metadata_json TEXT,           -- extensible JSON blob (call-site line, confidence, etc.)
  created_at  TEXT NOT NULL DEFAULT (datetime('now')),

  UNIQUE(source_id, target_id, kind)
);

CREATE INDEX idx_edge_source ON bridge_edge(source_id);
CREATE INDEX idx_edge_target ON bridge_edge(target_id);
CREATE INDEX idx_edge_kind   ON bridge_edge(kind);
```

#### Edge kinds

| Kind | Source → Target | Description |
|------|----------------|-------------|
| `contains` | parent → child | Structural nesting: project → dir, dir → file, file → namespace, namespace → def, etc. |
| `calls` | def → def | A calls B. `metadata_json` includes `{:call-sites [line ...]}` |
| `imports` | namespace → namespace | `(:require ...)` / `import`. Cross-ns dependency. |
| `refers` | def → def | Symbol reference without invocation (var usage, deref, etc.) |
| `implements` | record/type → protocol | Record implements protocol |
| `extends` | type → type | Inheritance / protocol extension |
| `exports` | namespace → def | Public surface of a namespace (only `def`s without `^:private`) |
| `documents` | heading/section → def/namespace | Doc heading references code entity |
| `depends-on` | namespace → namespace | Transitive import dependency (derived, for blast-radius analysis) |
| `tested-by` | def → def | Test function exercises source function |
| `co-occurs` | def → def | Changed together in git history (optional, later) |
| `member-of` | def → community | Louvain/structural community membership (optional, later) |

> **Weight**: Default 1.0. Used for ranking and filtering. `calls` edges from hot paths get higher weight. Low-relevance nodes get edges with lower weight.

### 3.4 Index state

```sql
CREATE TABLE bridge_index (
  file_path       TEXT    NOT NULL,
  language        TEXT    NOT NULL,
  content_hash    TEXT    NOT NULL,
  extractor_version TEXT  NOT NULL DEFAULT '1',
  node_count      INTEGER NOT NULL DEFAULT 0,
  edge_count      INTEGER NOT NULL DEFAULT 0,
  relevance       REAL    DEFAULT 1.0,
  indexed_at      TEXT    NOT NULL DEFAULT (datetime('now')),

  UNIQUE(file_path)
);
```

Tracks which files have been indexed, with what version of the extractor, and what their content hash was. On re-index:
- Hash unchanged → skip
- Hash changed → delete old nodes/edges for this file, re-extract, insert new
- File deleted → delete old nodes/edges

### 3.5 Semantic summaries (optional)

```sql
CREATE TABLE bridge_node_summary (
  node_id         INTEGER NOT NULL REFERENCES bridge_node(id),
  summary_type    TEXT    NOT NULL, -- "role", "invariant", "intent", "full"
  summary         TEXT    NOT NULL,
  model           TEXT,             -- which LLM generated this
  content_hash    TEXT,             -- hash of source at generation time
  created_at      TEXT NOT NULL DEFAULT (datetime('now')),

  UNIQUE(node_id, summary_type)
);
```

AOCI-inspired. Not required for graph operation. When available, the agent can request a node's summary to get architectural intent without reading the full source.

### 3.6 Meaningfulness filtering

Not every file deserves deep indexing. The builder applies a **relevance filter** before extraction:

| Signal | Skip? | Relevance |
|--------|-------|-----------|
| File in `.git/`, `target/`, `.lsp/`, `.clj-kondo/` | Yes | 0.0 |
| File matches `.cbmignore` / `.bridgeignore` patterns | Yes | 0.0 |
| File is generated (header comment `Auto-generated`, `DO NOT EDIT`) | Yes | 0.0 |
| File is binary (images, compiled classes, jars) | Yes | 0.0 |
| File is a data file (`.json`, `.edn` config without code structure) | Partial | 0.3 |
| File is a test file | No | 0.8 |
| File is a source file with `defn` / class definitions | No | 1.0 |
| File is a source file with only imports/requires (thin facade) | No | 0.5 |
| Markdown / doc file | No | 0.7 |

Files with relevance < 0.2 get a stub `file` node only (no children). Files with relevance ≥ 0.2 get full extraction.

The relevance score propagates to child nodes: a `def` inside a relevance-0.5 file inherits 0.5 unless the def itself is especially significant (exported, heavily called, etc.).

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
    ├── core.clj                                 ← extension registration, SCI symbols
    ├── extract.clj                              ← extraction pipeline coordinator
    ├── extract_clojure.clj                      ← Clojure-specific AST extractor
    ├── extract_markdown.clj                     ← Markdown heading/section extractor
    ├── graph.clj                                ← graph CRUD (nodes, edges, traversals)
    ├── query.clj                                ← query builders (traverse, neighbors, blast-radius)
    ├── relevance.clj                            ← meaningfulness filtering
    └── summary.clj                              ← LLM summary generation (optional)
```

Test namespace:

```
test/com/blockether/vis/ext/bridge/
├── core_test.clj
├── extract_clojure_test.clj
├── extract_markdown_test.clj
├── graph_test.clj
├── query_test.clj
└── relevance_test.clj
```

---

## 5. SCI Surface (bridge/ alias)

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `bridge/index` | `([] [opts])` | Run extraction pipeline on workspace. Opts: `:languages`, `:force?`, `:paths`. Returns stats. |
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

1. Walk workspace root, enumerate files.
2. Apply `.bridgeignore` + built-in skip patterns.
3. Compute content hashes.
4. Compare against `bridge_index`; partition into `:unchanged`, `:stale`, `:new`, `:deleted`.

### Phase 2: Filter

1. Score each `:new`/`:stale` file for relevance.
2. Partition into `:full-extract` (relevance ≥ 0.2) and `:stub` (relevance < 0.2).

### Phase 3: Extract

For each `:full-extract` file, dispatch to language-specific extractor:

**Clojure extractor** (`extract_clojure.clj`):
- Parse with `edamale` (already in Vis).
- Walk top-level forms:
  - `ns` → extract `:require`, `:import` → `namespace` node + `imports` edges
  - `defn` / `defn-` → `def` / `defn-private` node with arglists, docstring
  - `defmacro` → `def` node with `:macro true` in metadata
  - `defprotocol` → `protocol` node, child `def` nodes for methods
  - `defrecord` → `record` node, `implements` edges to protocols
  - `deftype` → `type` node
  - `defmulti` / `defmethod` → `multimethod` node
  - `def` (data vars) → `var` node
- Walk function bodies for call sites:
  - Symbol in call position → resolve against namespace imports → `calls` edge
  - Symbol in reference position → `refers` edge
- Emit `(node, edge)` tuples.

**Markdown extractor** (`extract_markdown.clj`):
- Parse headings → `heading` nodes
- Sections between headings → `section` nodes
- Symbol references in backticks → `documents` edges to matching nodes (best-effort)

### Phase 4: Build

1. Delete old nodes/edges for stale/deleted files.
2. Insert new nodes. Collect generated IDs.
3. Insert new edges (resolve target qualified names → IDs).
4. Update `bridge_index` rows.
5. All in a single SQLite transaction.

### Phase 5: Enrich (optional, deferred)

1. For nodes without summaries, queue LLM summary generation.
2. Run asynchronously (not in the agent's critical path).
3. Store in `bridge_node_summary`.

---

## 7. Integration with Vis

### 7.1 Extension registration

Standard Vis extension. `:ext/ns-alias {:ns vis.ext.bridge :alias bridge}`.

Requires `vis-foundation` (for `v/` tools).

### 7.2 Persistence

Uses the **same SQLite connection** as `vis-persistance-sqlite`. Bridge tables are created by the extension's own migration (run at first `bridge/index` or via the extension's `:ext/activation-fn`).

This respects Vis's inline-schema convention: one V1 schema file that the extension owns.

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
