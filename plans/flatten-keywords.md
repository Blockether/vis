# Plan: Flatten Namespaced Keywords in vis RLM

## Problem

The codebase uses Datomic-style triple-namespace keywords (`:document.toc/target-page`,
`:page.node/content`, `:entity/document-id`) inherited from a schema-registry architecture.
In the current SQLite-backed system these add:

- **LLM token waste** — every SCI code block reads/writes `(:entity/name x)` instead of `(:name x)`.
  The LLM sees these in system prompts, tool docs, and iteration receipts.
- **Redundant prefixes** — when you hold a TOC entry map, `:document.toc/title` tells you nothing
  that context doesn't already provide. `(:title entry)` is clearer.
- **Ceremony tax** — 20+ prefix namespaces, 150+ unique keys, each needing row↔ns converter
  pairs, schema spec entries, and split-entity-attrs case branches.

## Scope

**In scope:** All namespaced keywords used as *map keys* in data flowing through the RLM —
documents, pages, page-nodes, TOC entries, entities, relationships, commits, repos, claims,
queries, iterations, conversation attrs.

**Out of scope:**
- `:rlm/*`, `:rlm.error/*`, `:rlm.status/*`, `:svar.pageindex/*`, `:svar.vision/*`, `:svar.pdf/*`, `:svar/*`
  — these are **error/anomaly discriminators** used in `ex-info`, not data map keys. Leave them as-is.
- `:document.type/skill` — single enum value, leave as-is.
- Internal svar framework code (anything under `com.blockether.svar.*`).

## Key Mapping

### New flat key convention

| Old (namespaced) | New (flat) | Notes |
|---|---|---|
| `:entity/id` | `:id` | Universal — every entity has this |
| `:entity/type` | `:type` | |
| `:entity/name` | `:name` | |
| `:entity/description` | `:description` | |
| `:entity/parent-id` | `:parent-id` | |
| `:entity/document-id` | `:document-id` | |
| `:entity/page` | `:page` | |
| `:entity/section` | `:section` | |
| `:entity/canonical-id` | `:canonical-id` | |
| `:entity/created-at` | `:created-at` | |
| `:entity/updated-at` | `:updated-at` | |
| `:document/id` | `:id` | Context-dependent — documents are maps with `:id` |
| `:document/name` | `:name` | |
| `:document/type` | `:type` | |
| `:document/title` | `:title` | |
| `:document/abstract` | `:abstract` | |
| `:document/extension` | `:extension` | |
| `:document/author` | `:author` | |
| `:document/pages` | `:pages` | |
| `:document/toc` | `:toc` | |
| `:document/page-count` | `:page-count` | |
| `:document/created-at` | `:created-at` | |
| `:document/updated-at` | `:updated-at` | |
| `:document/certainty-alpha` | `:certainty-alpha` | |
| `:document/certainty-beta` | `:certainty-beta` | |
| `:document.toc/id` | `:id` | TOC entry map |
| `:document.toc/type` | `:type` | |
| `:document.toc/title` | `:title` | |
| `:document.toc/description` | `:description` | |
| `:document.toc/target-page` | `:target-page` | |
| `:document.toc/target-section-id` | `:target-section-id` | |
| `:document.toc/level` | `:level` | |
| `:document.toc/parent-id` | `:parent-id` | |
| `:document.toc/document-id` | `:document-id` | |
| `:document.toc/created-at` | `:created-at` | |
| `:page/id` | `:id` | Page map |
| `:page/index` | `:index` | |
| `:page/nodes` | `:nodes` | |
| `:page/document-id` | `:document-id` | |
| `:page/created-at` | `:created-at` | |
| `:page/last-accessed` | `:last-accessed` | |
| `:page/access-count` | `:access-count` | |
| `:page/q-value` | `:q-value` | |
| `:page/q-update-count` | `:q-update-count` | |
| `:page.node/id` | `:id` | Page node map |
| `:page.node/type` | `:type` | |
| `:page.node/parent-id` | `:parent-id` | |
| `:page.node/level` | `:level` | |
| `:page.node/content` | `:content` | |
| `:page.node/image-data` | `:image-data` | |
| `:page.node/description` | `:description` | |
| `:page.node/continuation?` | `:continuation?` | |
| `:page.node/caption` | `:caption` | |
| `:page.node/kind` | `:kind` | |
| `:page.node/group-id` | `:group-id` | |
| `:page.node/bbox` | `:bbox` | |
| `:page.node/image-index` | `:image-index` | |
| `:page.node/image-path` | `:image-path` | |
| `:page.node/page-id` | `:page-id` | |
| `:page.node/document-id` | `:document-id` | |
| `:page.node/local-id` | `:local-id` | |
| `:relationship/id` | `:id` | |
| `:relationship/type` | `:type` | |
| `:relationship/source-entity-id` | `:source-id` | Shortened |
| `:relationship/target-entity-id` | `:target-id` | Shortened |
| `:relationship/description` | `:description` | |
| `:relationship/document-id` | `:document-id` | |
| `:commit/sha` | `:sha` | |
| `:commit/category` | `:category` | |
| `:commit/date` | `:date` | |
| `:commit/prefix` | `:prefix` | |
| `:commit/scope` | `:scope` | |
| `:commit/author-email` | `:author-email` | |
| `:commit/ticket-refs` | `:ticket-refs` | |
| `:commit/file-paths` | `:file-paths` | |
| `:commit/parents` | `:parents` | |
| `:repo/name` | `:name` | |
| `:repo/path` | `:path` | |
| `:repo/head-sha` | `:head-sha` | |
| `:repo/head-short` | `:head-short` | |
| `:repo/branch` | `:branch` | |
| `:repo/commits-ingested` | `:commits-ingested` | |
| `:repo/ingested-at` | `:ingested-at` | |
| `:claim/id` | `:id` | |
| `:claim/text` | `:text` | |
| `:claim/document-id` | `:document-id` | |
| `:claim/page` | `:page` | |
| `:claim/section` | `:section` | |
| `:claim/quote` | `:quote` | |
| `:claim/confidence` | `:confidence` | |
| `:claim/query-id` | `:query-id` | |
| `:claim/verified?` | `:verified?` | |
| `:claim/verification-verdict` | `:verification-verdict` | |
| `:claim/created-at` | `:created-at` | |
| `:person/email` | `:email` | |
| `:conversation/env-id` | `:env-id` | |
| `:conversation/name` | `:name` | |
| `:conversation/system-prompt` | `:system-prompt` | |
| `:conversation/model` | `:model` | |
| `:query/messages` | `:messages` | |
| `:query/text` | `:text` | |
| `:query/answer` | `:answer` | |
| `:query/iterations` | `:iterations` | |
| `:query/duration-ms` | `:duration-ms` | |
| `:query/status` | `:status` | |
| `:query/eval-score` | `:eval-score` | |
| `:iteration/code` | `:code` | |
| `:iteration/results` | `:results` | |
| `:iteration/vars` | `:vars` | |
| `:iteration/answer` | `:answer` | |
| `:iteration/thinking` | `:thinking` | |
| `:iteration/duration-ms` | `:duration-ms` | |
| `:iteration.var/name` | `:name` | |
| `:iteration.var/value` | `:value` | |
| `:iteration.var/code` | `:code` | |

### Entity lookup refs

| Old | New |
|---|---|
| `[:entity/id uuid]` | `[:id uuid]` |
| `[:page.node/id "abc"]` | `[:node/id "abc"]` |
| `[:document/id "doc-1"]` | `[:doc/id "doc-1"]` |
| `[:document.toc/id "toc-1"]` | `[:toc/id "toc-1"]` |

Lookup refs keep a short disambiguating prefix (`:doc/id`, `:node/id`, `:toc/id`) because
they're the one place where context is lost — they're passed as bare vectors to
`fetch-document-content`.

## Implementation Phases

### Phase 0: Add presentation layer (non-breaking)

**Goal:** Add a thin `present` namespace that strips namespaced keys from maps before
returning them to the LLM. Internal storage and all existing code keep using namespaced keys.
SCI tools call `present` on their return values.

**Files changed:**
- NEW `src/com/blockether/vis/rlm/present.clj` — pure functions:
  - `(present-entity m)` — strips `:entity/` prefix
  - `(present-node m)` — strips `:page.node/` prefix
  - `(present-toc m)` — strips `:document.toc/` prefix
  - `(present-document m)` — strips `:document/` prefix
  - `(present-relationship m)` — strips `:relationship/`, shortens `source-entity-id` → `source-id`
  - `(present-commit m)` — strips `:commit/` + `:entity/`
  - `(present-repo m)` — strips `:repo/`
  - `(present-claim m)` — strips `:claim/`
  - Each function: `(into {} (map (fn [[k v]] [(->flat-key k) v])) m)`
  - Handles lookup refs: `[:entity/id uuid]` → `[:id uuid]`
- `src/com/blockether/vis/rlm/tools.clj` — import `present`, wrap all SCI tool return values:
  - `search-documents` → already returns markdown (uses `format-docs`)
  - `fetch-document-content` → present entity/node/toc
  - `get-entity` → present entity
  - `find-related` → present entities
  - `list-relationships` → present relationships
  - `search-batch` → already returns markdown
  - Git tools: `git-search-commits` → present commits
- `src/com/blockether/vis/rlm/core.clj` — update system prompt to show flat keys in examples
- Tests — update expectations in `entity-bindings-test`, `git-wiring-test`, etc.

**Why this phase first:** Zero internal disruption. All existing code, storage, row↔ns
converters stay untouched. Only the LLM sees flat keys immediately.

### Phase 1: Flatten schema.clj clojure.spec definitions

**Goal:** Change all `s/def` keywords from `:document.toc/title` to `:title` etc.
Update `::spec/key-ns` values in LLM structured output specs to emit flat keys.

**Files changed:**
- `src/com/blockether/vis/rlm/schema.clj`
  - Change `s/def :page.node/type ...` → `s/def :type ...`
  - Change `s/def :document.toc/title ...` → `s/def :title ...`
  - Change all 50+ `s/def` entries
  - Update `::page`, `::page-list`, `::document`, `::documents` specs
  - Change `{::spec/key-ns "page.node"}` → `{::spec/key-ns ""}` or remove key-ns
- `src/com/blockether/vis/rlm/pageindex/vision.clj`
  - All LLM output specs (`section-spec`, `heading-spec`, etc.) — change `::spec/key-ns "page.node"` to emit flat keys
  - `toc-entry-spec` — change `::spec/key-ns "document.toc"` to flat
  - Response parsing: adjust destructuring to use flat keys
  - `vision-response-spec` — update key extraction
- `src/com/blockether/vis/rlm/pageindex/markdown.clj`
  - All `:page.node/*` keys → flat (`:type`, `:id`, `:content`, `:level`, `:parent-id`)
  - All `:page/*` keys → flat (`:index`, `:nodes`)
- `src/com/blockether/vis/rlm/pageindex.clj`
  - `build-index`, `inspect`, `load-index`, `write-document-edn!`, `read-document-edn`
  - All `:document/*`, `:page/*`, `:page.node/*` keys
  - Continuation grouping (`group-continuations`) — uses `:page.node/group-id`, `:page.node/continuation?`
- All pageindex tests

### Phase 2: Flatten sqlite.clj row↔ns converters and storage

**Goal:** Change the internal storage layer to use flat keys throughout.

**Files changed:**
- `src/com/blockether/vis/rlm/sqlite.clj`
  - **Row→NS functions** — simplify to identity or near-identity:
    - `entity-base` → return flat keys (`:id`, `:type`, `:name`, etc.)
    - `document-row->ns` → flat keys
    - `node-row->ns` → flat keys
    - `toc-row->ns` → flat keys
    - `repo-row->ns` → flat keys
    - `commit-row->ns` → flat keys
    - `conversation-attrs->ns`, `query-attrs->ns`, `iteration-attrs->ns`, `iteration-var-attrs->ns` → flat
  - **NS→Cols functions** — simplify:
    - `split-entity-attrs` — remove `case` dispatch, just filter known column names
    - `doc->cols`, `page->cols`, `node->cols`, `toc->cols` — direct map to snake_case SQL cols
  - **Lookup ref helpers:**
    - `entity-ref->id` → accept `[:id uuid]` in addition to `[:entity/id uuid]` (backward compat)
    - `id->entity-ref` → return `[:id uuid]`
  - All query functions that build result maps
  - All insert functions that accept maps
- `src/com/blockether/vis/rlm/db.clj`
  - Update re-exports (no key changes needed since fns just delegate)
  - Remove `->id`, `->kw`, `->epoch-ms` re-exports if Phase 2 makes them unnecessary externally
- `src/com/blockether/vis/rlm/tools.clj`
  - Remove `present.clj` import (no longer needed — everything is flat internally)
  - Update `format-docs` to use flat keys
  - Update tool spec docstrings
  - Update lookup ref examples in doc metadata

### Phase 3: Flatten upstream consumers

**Goal:** Update all callers that construct or destructure namespaced-key maps.

**Files changed:**
- `src/com/blockether/vis/rlm/core.clj`
  - `build-system-prompt` — all key references in prompt text
  - `build-document-summary` — `:document/title` → `:title` etc.
  - `format-git-context` — `:repo/name` → `:name` etc.
  - Code execution receipt formatting — entity key references
- `src/com/blockether/vis/rlm/data.clj`
  - Entity/relationship extraction — uses `:entity/*`, `:relationship/*`
- `src/com/blockether/vis/rlm/git.clj`
  - `commit->entity`, `author->person-entity`, `file->file-entity` — all `:entity/*`, `:commit/*`, `:person/*`
  - `ingest-commits!` — entity construction and relationship building
  - Relationship maps — `:relationship/*`
- `src/com/blockether/vis/rlm/qa.clj`
  - QA generation pipeline — uses `:page.node/*` keys
- `src/com/blockether/vis/rlm/qa_manifest.clj`
  - Corpus snapshot — may reference document keys
- `src/com/blockether/vis/rlm/query.clj`
  - Query result formatting — `:query/*`, claim keys
- `src/com/blockether/vis/rlm/trajectory.clj`
  - Trajectory export — `:conversation/system-prompt`
- `src/com/blockether/vis/rlm/env.clj`
  - Entity ref handling — `[:entity/id uuid]`
- `src/com/blockether/vis/rlm/skills.clj`
  - Skill document construction — `:document/*`
- DELETE `src/com/blockether/vis/rlm/present.clj` — no longer needed

### Phase 4: Flatten tests

**Goal:** Update all test files to use flat keys.

**Files changed (all in `test/com/blockether/vis/rlm/`):**
- `rlm_test.clj` — largest test file, ~2200 lines. Every `:entity/*`, `:document/*`,
  `:page.node/*`, `:relationship/*` reference.
- `git_wiring_test.clj` — `:commit/*`, `:entity/*`, `:repo/*`
- `context_tools_test.clj` — `:entity/*`
- `markdown_test.clj` — `:page/*`, `:page.node/*`
- `skills_test.clj` — `:document/*`
- `pageindex_test.clj` — `:document/*`, `:page/*`, `:page.node/*`, `:document.toc/*`
- `data_test.clj` — `:entity/*`, `:relationship/*`
- `schema_test.clj` — `:page.node/*`, `:document.toc/*`
- Any other test files referencing namespaced keys

## PageIndex Deep Dive

The pageindex subsystem is the heaviest user of nested namespace keywords and needs special attention.

### Current flow

```
PDF file
  → pageindex/pdf.clj (PDFBox → BufferedImage per page)
  → pageindex/vision.clj (LLM vision call → structured JSON with :page.node/* keys)
  → pageindex.clj (group continuations, normalize, validate via schema.clj specs)
  → sqlite.clj (store-document!, store pages + page_nodes)
```

### Changes per file

#### `pageindex/vision.clj` (~1093 lines)

**Current:** LLM returns JSON with keys like `type`, `content`, `level`. The `::spec/key-ns "page.node"`
setting in the spec causes svar to namespace them as `:page.node/type`, `:page.node/content`, etc.
TOC entries get `::spec/key-ns "document.toc"` → `:document.toc/title`, etc.

**Change:**
- All specs (`section-spec`, `heading-spec`, `paragraph-spec`, `list-item-spec`, `toc-entry-spec`,
  `image-spec`, `table-spec`, `header-spec`, `footer-spec`, `metadata-spec`, `vision-response-spec`):
  - Remove `{::spec/key-ns "page.node"}` → the LLM's JSON keys (`type`, `content`, `level`)
    will stay flat when parsed.
  - Remove `{::spec/key-ns "document.toc"}` → TOC keys (`title`, `level`, `target-page`) stay flat.
- Response handling:
  - `(get m :page.node/type)` → `(get m :type)` or `(:type m)`
  - `(get m :page.node/content)` → `(:content m)`
  - `(get m :page.node/level)` → `(:level m)`
  - All destructuring and `cond->` chains that add `:page.node/*` keys
- The `extract-text-from-pdf` function builds page maps:
  - `{:page/index idx :page/nodes [...]}` → `{:index idx :nodes [...]}`
- Node ID generation: `(:page.node/id node)` → `(:id node)`

**Approximate change count:** ~150 key references across the file.

#### `pageindex/markdown.clj` (~383 lines)

**Current:** `markdown->pages` produces `[{:page/index 0 :page/nodes [{:page.node/type :heading :page.node/content "Title" :page.node/id "md-1"} ...]}]`

**Change:**
- `{:page/index idx :page/nodes nodes}` → `{:index idx :nodes nodes}`
- Node construction: `{:page.node/type :heading :page.node/content "Title" :page.node/id id}` → `{:type :heading :content "Title" :id id}`
- `:page.node/parent-id` → `:parent-id`
- `:page.node/level` → `:level`
- All `assoc` calls adding `:page.node/*` keys → flat keys

**Approximate change count:** ~40 key references.

#### `pageindex.clj` (~371 lines)

**Current:** `build-index` takes a document spec map with `:document/*` keys, calls vision.clj,
groups continuations, stores to DB.

**Change:**
- Input validation: `(:document/name doc)` → `(:name doc)`, `(:document/extension doc)` → `(:extension doc)`
- `store-document!` call: pass flat-key map (sqlite.clj's `doc->cols` handles the mapping)
- `group-continuations`: `(:page.node/group-id n)` → `(:group-id n)`, `(:page.node/continuation? n)` → `(:continuation? n)`
- `inspect`: all key references in output formatting
- `load-index` / `read-document-edn`: expects flat keys in stored EDN
- Page filtering: `(:page/index page)` → `(:index page)`
- `normalize-page-spec` / `filter-pages`: operate on `:index` not `:page/index`

**Approximate change count:** ~60 key references.

#### `pageindex/pdf.clj` (~319 lines)

**Current:** Returns raw PDF data (page count, metadata, images). No namespaced keys in return values —
uses plain strings/ints. **Minimal changes needed.** Only error anomaly keys (`:svar.pdf/*`) which
are out of scope.

## Migration Strategy

### Backward compatibility during transition

1. **Phase 0 is non-breaking** — internal storage keeps namespaced keys, only LLM output changes.
2. **Phases 1-3 are done atomically** — once we start flattening internal storage, everything flips together.
3. **EDN serialization** — `read-document-edn` can read both old (namespaced) and new (flat) keys
   during transition. Add a migration step that re-indexes stored EDN files.
4. **SQLite data** — no schema changes! SQL columns are already `snake_case` (`document_id`, `page_count`).
   The namespaced keywords only exist in the Clojure map layer. Flattening keywords does NOT require
   a DB migration.

### Commit strategy

Each phase = one commit:
1. `feat(rlm): add present layer for flat LLM-facing keys`
2. `refactor(rlm): flatten schema.clj and pageindex specs`
3. `refactor(rlm): flatten sqlite.clj row↔ns converters`
4. `refactor(rlm): flatten upstream consumers`
5. `refactor(rlm): flatten tests, remove present.clj`

### Risk

- **Phase 0** — Zero risk. Presentation-only layer.
- **Phases 1-4** — Medium risk. Single atomic change across ~15 files. All tests must pass
  before committing. The key insight: **no SQLite schema changes**, so we can always roll back
  to namespaced keys by reverting the Clojure code.

## Estimated Effort

| Phase | Files | Key Changes | Effort |
|---|---|---|---|
| Phase 0: Present layer | 3 (new present.clj + tools.clj + core.clj) | ~100 | 2h |
| Phase 1: Schema + pageindex | 5 (schema.clj + vision.clj + markdown.clj + pageindex.clj + tests) | ~250 | 4h |
| Phase 2: SQLite converters | 2 (sqlite.clj + db.clj) | ~200 | 3h |
| Phase 3: Upstream consumers | 8-10 files | ~200 | 3h |
| Phase 4: Tests | 8-10 test files | ~300 | 3h |
| **Total** | **~25 files** | **~1050 key references** | **~15h** |
