-- =============================================================================
-- V1 - vis schema (SQLite)
--
-- Hierarchy:
--
--   session_soul (identity)
--     └─ session_state (branch/fork)
--          ├─ session_turn_soul (user ask, branch-local)
--          │    └─ session_turn_state (one run/retry of the turn)
--          │         └─ session_turn_iteration (one LLM round-trip)
--          │              │
--          │              └─ code/result/error/duration_ms columns
--          │                   Executed form plus result/error timing live inline on
--          │                   the iteration row.
--          │
--          └─ definition_soul (branch-local var identities)
--               ├─ definition_state (var versions; each row points at the
--               │                    iteration that produced it)
--               └─ definition_dependency (var dep graph, soul-level)
--
-- Naming convention:
--   *_soul   = immutable identity, branch-local
--   *_state  = mutable snapshot; retry/fork = new state row
--   parent_table_child = nested concept (session_turn_iteration
--                        = iteration nested under session_turn_state)
--
-- Position columns (1-based int) live alongside UUID PKs at every
-- level. UUIDs are the join key; position is the public/agent-
-- facing identifier (see PLAN.md §2.9, §2.10).
--
-- Flow per turn:
--   user request
--     -> session_turn_soul + session_turn_state
--     -> session_turn_iteration(s)
--          each session_turn_iteration records executed code inline in
--          code/result/error/duration_ms and writes one
--          definition_soul + definition_state row per named var touched by
--          `(def ...)` / `(defn ...)`
--     -> session_turn_state done/error
--     -> next turn (or branch/fork to new session_state)
--
-- Fork flow:
--   session_state(v1)
--        └─ fork -> session_state(v2, parent_state_id=v1)
--   Each fork keeps isolated branch-local session_turn_soul +
--   definition_soul identity.
-- =============================================================================

PRAGMA foreign_keys = ON;

-- =============================================================================
-- Session soul - pure identity.
-- metadata is a JSON blob for channel bindings and external IDs.
-- =============================================================================
CREATE TABLE session_soul (
  id           TEXT PRIMARY KEY NOT NULL,
  metadata     TEXT,                       -- JSON-encoded object/string
  created_at   INTEGER NOT NULL
);

CREATE INDEX idx_session_soul_created
  ON session_soul(created_at DESC);

-- =============================================================================
-- Session state - forkable mutable snapshot.
-- =============================================================================
CREATE TABLE session_state (
  id                    TEXT PRIMARY KEY NOT NULL,
  session_soul_id  TEXT NOT NULL
                        REFERENCES session_soul(id) ON DELETE CASCADE,
  parent_state_id       TEXT
                        REFERENCES session_state(id) ON DELETE CASCADE,
  title                 TEXT,
  version               INTEGER NOT NULL CHECK (version >= 0),
  metadata              TEXT,              -- JSON-encoded object/string
  created_at            INTEGER NOT NULL,

  UNIQUE (session_soul_id, version)
);

CREATE INDEX idx_session_state_soul
  ON session_state(session_soul_id, version);

CREATE INDEX idx_session_state_parent
  ON session_state(parent_state_id);

-- =============================================================================
-- Turn soul - immutable identity of a user request, branch-local.
-- =============================================================================
CREATE TABLE session_turn_soul (
  id                     TEXT PRIMARY KEY NOT NULL,
  session_state_id  TEXT NOT NULL
                         REFERENCES session_state(id) ON DELETE CASCADE,
  position               INTEGER NOT NULL CHECK (position >= 1),
  -- Session title lives on `session_state.title` (set via
  -- `(set-session-title! ...)` and mirrored to the SESSION_TITLE
  -- SCI var). Turn rows carry `user_request` for display and replay.
  user_request           TEXT,
  metadata               TEXT,             -- JSON-encoded object/string
  created_at             INTEGER NOT NULL,

  UNIQUE (session_state_id, position)
);

CREATE INDEX idx_session_turn_soul_state
  ON session_turn_soul(session_state_id, position);

CREATE TRIGGER trg_session_turn_soul_position_ai
BEFORE INSERT ON session_turn_soul
BEGIN
  SELECT CASE
    WHEN NOT EXISTS (
           SELECT 1 FROM session_turn_soul s
           WHERE s.session_state_id = NEW.session_state_id)
         AND NEW.position <> 1
    THEN RAISE(ABORT, 'first session_turn_soul position must be 1')
  END;

  SELECT CASE
    WHEN EXISTS (
           SELECT 1 FROM session_turn_soul s
           WHERE s.session_state_id = NEW.session_state_id)
         AND NEW.position <> (
           SELECT max(s.position) + 1 FROM session_turn_soul s
           WHERE s.session_state_id = NEW.session_state_id)
    THEN RAISE(ABORT, 'session_turn_soul position must increment by 1')
  END;
END;

CREATE TRIGGER trg_session_turn_soul_position_au
BEFORE UPDATE ON session_turn_soul
BEGIN
  SELECT CASE
    WHEN NEW.session_state_id <> OLD.session_state_id OR NEW.position <> OLD.position
    THEN RAISE(ABORT, 'session_turn_soul state/position are immutable')
  END;
END;

-- =============================================================================
-- Session turn state - one run of session_turn_soul.
-- Retry = new state version.
-- =============================================================================
CREATE TABLE session_turn_state (
  id                           TEXT PRIMARY KEY NOT NULL,
  session_turn_soul_id                TEXT NOT NULL
                               REFERENCES session_turn_soul(id) ON DELETE CASCADE,
  forked_from_session_turn_state_id   TEXT
                               REFERENCES session_turn_state(id) ON DELETE SET NULL,
  version                      INTEGER NOT NULL CHECK (version >= 0),
  llm_root_provider            TEXT,    -- provider id (e.g. 'openai', 'github-copilot')
  llm_root_model               TEXT,
  status                       TEXT NOT NULL
                               CHECK (status IN ('running', 'done', 'error', 'interrupted')),
  metadata                     TEXT,        -- JSON-encoded object/string
  answer                       BLOB,        -- Nippy-frozen IR `[:ir & nodes]`. NULL while running.
  -- Per-turn final outcome, derived at turn end. Lets the next turn's
  -- handover digest say "previous turn complete | cancelled | error"
  -- without scanning every session_turn_iteration. Set by the session_turn_iteration
  -- loop on the terminal session_turn_iteration and by
  -- sweep-orphaned-running-turns! for cancelled / orphaned turns.
  prior_outcome                TEXT
                               CHECK (prior_outcome IS NULL OR
                                      prior_outcome IN ('complete', 'cancelled', 'error')),
  created_at                   INTEGER NOT NULL,

  UNIQUE (session_turn_soul_id, version)
);

CREATE INDEX idx_session_turn_state_soul
  ON session_turn_state(session_turn_soul_id, version);

CREATE INDEX idx_session_turn_state_forked_from
  ON session_turn_state(forked_from_session_turn_state_id);

-- =============================================================================
-- Iteration - one LLM round-trip within a session_turn_state.
-- =============================================================================
CREATE TABLE session_turn_iteration (
  id                              TEXT PRIMARY KEY NOT NULL,
  session_turn_state_id                  TEXT NOT NULL
                                  REFERENCES session_turn_state(id) ON DELETE CASCADE,
  position                        INTEGER NOT NULL CHECK (position >= 1),

  status                          TEXT NOT NULL
                                  CHECK (status IN ('running', 'done', 'error', 'interrupted')),

  llm_system_prompt               TEXT,
  llm_user_prompt                 TEXT,    -- JSON envelope for multimodal user input (text/images/audio/files)
  llm_provider                    TEXT,    -- provider id used for this session_turn_iteration (e.g. 'openai', 'github-copilot')
  llm_model                       TEXT,
  llm_selected_provider           TEXT,
  llm_selected_model              TEXT,
  llm_actual_provider             TEXT,
  llm_actual_model                TEXT,
  llm_fallback                    INTEGER NOT NULL DEFAULT 0
                                  CHECK (llm_fallback IN (0, 1)),

  llm_full_duration_ms            INTEGER CHECK (
                                    llm_full_duration_ms IS NULL OR llm_full_duration_ms >= 0
                                  ),       -- total duration across all traced attempts
  llm_thinking                    TEXT,
  llm_error                       TEXT,
  llm_returned_empty_code         INTEGER NOT NULL DEFAULT 0
                                  CHECK (llm_returned_empty_code IN (0, 1)),

  -- Raw-response diagnostics for provider / extraction forensics.
  -- Full raw text is persisted so investigations can query the DB
  -- instead of grepping process logs. Preview / length / hash keep
  -- cheap list views and diagnostics available too.
  llm_raw_response                TEXT,
  llm_raw_response_preview        TEXT,
  llm_raw_response_length         INTEGER CHECK (
                                    llm_raw_response_length IS NULL OR llm_raw_response_length >= 0
                                  ),
  llm_raw_response_sha256         TEXT,
  llm_executable_code_blocks      TEXT,    -- JSON vec of executable Markdown code blocks selected by svar: [{:lang :source} ...]

  -- svar canonical assistant message persisted so preserved-thinking
  -- replay survives a vis restart. JSON-encoded
  -- `{:role "assistant" :content [<canonical-blocks>]}` exactly as
  -- svar's `:assistant-message` returns it; canonical thinking blocks
  -- carry per-provider preserved-reasoning state under
  -- `:thinking-signature`. Cross-turn / cross-restart resume reads
  -- this column to rebuild the per-turn replay buffer.
  llm_assistant_message           TEXT,

  -- Per-session_turn_iteration token accounting + estimated USD cost. NULL when
  -- the provider response did not surface usage (e.g. the LLM call
  -- itself failed before a response was returned). Reasoning /
  -- cached tokens are subsets of completion / prompt respectively;
  -- callers that want "input minus cached" compute it themselves.
  -- Cost is provider-side estimated USD via the router's pricing
  -- table - recorded so historical rows survive future price
  -- changes.
  llm_input_tokens                INTEGER CHECK (
                                    llm_input_tokens IS NULL OR llm_input_tokens >= 0
                                  ),
  llm_output_tokens               INTEGER CHECK (
                                    llm_output_tokens IS NULL OR llm_output_tokens >= 0
                                  ),
  llm_reasoning_tokens            INTEGER CHECK (
                                    llm_reasoning_tokens IS NULL OR llm_reasoning_tokens >= 0
                                  ),
  llm_cached_tokens               INTEGER CHECK (
                                    llm_cached_tokens IS NULL OR llm_cached_tokens >= 0
                                  ),
  llm_cost_usd                    REAL    CHECK (
                                    llm_cost_usd IS NULL OR llm_cost_usd >= 0
                                  ),

  metadata                        TEXT,    -- JSON-encoded per-session_turn_iteration context (active extensions, etc.)

  -- Single-form iteration payload. `result` and `error` are Nippy-encoded
  -- Clojure values.
  code                            TEXT NOT NULL,
  result                          BLOB,
  error                           BLOB,
  duration_ms                     INTEGER CHECK (duration_ms IS NULL OR duration_ms >= 0),

  created_at                      INTEGER NOT NULL,
  finished_at                     INTEGER,

  UNIQUE (session_turn_state_id, position)
);

CREATE INDEX idx_session_turn_iteration_session_turn_state
  ON session_turn_iteration(session_turn_state_id, position);

CREATE INDEX idx_session_turn_iteration_session_turn_state_created
  ON session_turn_iteration(session_turn_state_id, created_at);

CREATE TABLE llm_routing_event (
  id                             TEXT PRIMARY KEY NOT NULL,
  session_turn_iteration_id TEXT NOT NULL
                                 REFERENCES session_turn_iteration(id) ON DELETE CASCADE,
  position                       INTEGER NOT NULL CHECK (position >= 0),
  event_type                     TEXT NOT NULL,
  provider                       TEXT,
  model                          TEXT,
  from_provider                  TEXT,
  from_model                     TEXT,
  to_provider                    TEXT,
  to_model                       TEXT,
  status                         INTEGER,
  reason                         TEXT,
  error                          TEXT,
  attempt                        INTEGER CHECK (attempt IS NULL OR attempt >= 0),
  delay_ms                       INTEGER CHECK (delay_ms IS NULL OR delay_ms >= 0),
  elapsed_ms                     INTEGER CHECK (elapsed_ms IS NULL OR elapsed_ms >= 0),
  at_ms                          INTEGER,
  event_json                     TEXT NOT NULL,
  created_at                     INTEGER NOT NULL,

  UNIQUE (session_turn_iteration_id, position)
);

CREATE INDEX idx_llm_routing_event_iteration_position
  ON llm_routing_event(session_turn_iteration_id, position);

CREATE TRIGGER trg_session_turn_iteration_position_ai
BEFORE INSERT ON session_turn_iteration
BEGIN
  SELECT CASE
    WHEN NOT EXISTS (
           SELECT 1 FROM session_turn_iteration i
           WHERE i.session_turn_state_id = NEW.session_turn_state_id)
         AND NEW.position <> 1
    THEN RAISE(ABORT, 'first session_turn_iteration position must be 1')
  END;

  SELECT CASE
    WHEN EXISTS (
           SELECT 1 FROM session_turn_iteration i
           WHERE i.session_turn_state_id = NEW.session_turn_state_id)
         AND NEW.position <> (
           SELECT max(i.position) + 1 FROM session_turn_iteration i
           WHERE i.session_turn_state_id = NEW.session_turn_state_id)
    THEN RAISE(ABORT, 'session_turn_iteration position must increment by 1')
  END;
END;

CREATE TRIGGER trg_session_turn_iteration_position_au
BEFORE UPDATE ON session_turn_iteration
BEGIN
  SELECT CASE
    WHEN NEW.session_turn_state_id <> OLD.session_turn_state_id OR NEW.position <> OLD.position
    THEN RAISE(ABORT, 'session_turn_iteration turn-state/position are immutable')
  END;
END;

-- =============================================================================
-- Definition soul - branch-local identity for persistent user vars.
--
-- One row per var name in a session_state. Definitions include
-- normal vars and function vars, e.g. `(def x 42)` and
-- `(defn sum [a b] (+ a b))`.
--
-- Execution payload lives on session_turn_iteration. Var history
-- lives in definition_state: one versioned row per iteration that writes
-- the var. Dependency edges live in definition_dependency.
-- =============================================================================
CREATE TABLE definition_soul (
  id                     TEXT PRIMARY KEY NOT NULL,
  session_state_id  TEXT NOT NULL
                         REFERENCES session_state(id) ON DELETE CASCADE,

  name                   TEXT NOT NULL CHECK (trim(name) <> ''),
  created_at             INTEGER NOT NULL
);

CREATE UNIQUE INDEX uq_definition_soul_state_name
  ON definition_soul(session_state_id, name);

CREATE INDEX idx_definition_soul_state_created
  ON definition_soul(session_state_id, created_at);

-- =============================================================================
-- Definition dependency - downstream depends on upstream (soul-level graph).
--
-- Direction:
--   upstream_definition_soul_id -> downstream_definition_soul_id
--
-- This table stores pure recalculation edges.
-- If edge (u -> d) exists, d must be recalculated when u changes.
-- =============================================================================
CREATE TABLE definition_dependency (
  id                             TEXT PRIMARY KEY NOT NULL,
  session_state_id          TEXT NOT NULL
                                 REFERENCES session_state(id) ON DELETE CASCADE,
  downstream_definition_soul_id  TEXT NOT NULL
                                 REFERENCES definition_soul(id) ON DELETE CASCADE,
  upstream_definition_soul_id    TEXT NOT NULL
                                 REFERENCES definition_soul(id) ON DELETE CASCADE,
  created_at                     INTEGER NOT NULL,

  CHECK (downstream_definition_soul_id <> upstream_definition_soul_id),
  UNIQUE (downstream_definition_soul_id, upstream_definition_soul_id)
);

CREATE INDEX idx_def_dep_downstream
  ON definition_dependency(downstream_definition_soul_id);

CREATE INDEX idx_def_dep_upstream
  ON definition_dependency(upstream_definition_soul_id);

CREATE INDEX idx_def_dep_state
  ON definition_dependency(session_state_id);

CREATE TRIGGER trg_def_dep_same_state_ai
BEFORE INSERT ON definition_dependency
BEGIN
  SELECT
    CASE
      WHEN (SELECT session_state_id FROM definition_soul WHERE id = NEW.downstream_definition_soul_id) IS NULL
        OR (SELECT session_state_id FROM definition_soul WHERE id = NEW.upstream_definition_soul_id) IS NULL
      THEN RAISE(ABORT, 'definition_dependency endpoint not found')
      WHEN (SELECT session_state_id FROM definition_soul WHERE id = NEW.downstream_definition_soul_id) <>
           (SELECT session_state_id FROM definition_soul WHERE id = NEW.upstream_definition_soul_id)
      THEN RAISE(ABORT, 'definition_dependency endpoints must be in same session_state')
      WHEN (SELECT session_state_id FROM definition_soul WHERE id = NEW.downstream_definition_soul_id) <> NEW.session_state_id
      THEN RAISE(ABORT, 'definition_dependency session_state_id mismatch')
    END;
END;

CREATE TRIGGER trg_def_dep_same_state_au
BEFORE UPDATE ON definition_dependency
BEGIN
  SELECT
    CASE
      WHEN (SELECT session_state_id FROM definition_soul WHERE id = NEW.downstream_definition_soul_id) <>
           (SELECT session_state_id FROM definition_soul WHERE id = NEW.upstream_definition_soul_id)
      THEN RAISE(ABORT, 'definition_dependency endpoints must be in same session_state')
      WHEN (SELECT session_state_id FROM definition_soul WHERE id = NEW.downstream_definition_soul_id) <> NEW.session_state_id
      THEN RAISE(ABORT, 'definition_dependency session_state_id mismatch')
    END;
END;

-- =============================================================================
-- Definition state - versioned durable state per var. Each row carries
-- the source form (the smallest `(def NAME …)` shape that introduced
-- this version) and the Nippy-frozen result value, freeze-safe so fn /
-- lazy-seq / runtime-object values land as `{:vis/ref :expr}` and the
-- caller re-evals `expression` to reconstitute them on restore.
-- =============================================================================
CREATE TABLE definition_state (
  id                              TEXT PRIMARY KEY NOT NULL,
  definition_soul_id              TEXT NOT NULL
                                  REFERENCES definition_soul(id) ON DELETE CASCADE,
  session_turn_iteration_id  TEXT NOT NULL
                                  REFERENCES session_turn_iteration(id) ON DELETE CASCADE,

  version                         INTEGER NOT NULL CHECK (version >= 0),

  -- `expression` is the precise per-var source (e.g. "(def NAME ...)");
  -- caller re-evals it on restore. Nullable for callers that only
  -- want value persistence (no re-eval round-trip). NOT NULL would
  -- enforce the restoration contract but breaks every direct-write
  -- test fixture that skips the source-tracking step.
  expression                      TEXT CHECK (expression IS NULL OR trim(expression) <> ''),
  result                          BLOB,
  created_at                      INTEGER NOT NULL,

  UNIQUE (definition_soul_id, version)
);

CREATE INDEX idx_definition_state_soul
  ON definition_state(definition_soul_id, version);

CREATE INDEX idx_definition_state_iteration
  ON definition_state(session_turn_iteration_id);

-- First row for a definition_soul must start at version = 0.
CREATE TRIGGER trg_definition_state_first_version_ai
BEFORE INSERT ON definition_state
BEGIN
  SELECT CASE
    WHEN NOT EXISTS (
           SELECT 1
           FROM definition_state es
           WHERE es.definition_soul_id = NEW.definition_soul_id)
         AND NEW.version <> 0
    THEN RAISE(ABORT, 'first definition_state version must be 0')
  END;
END;

-- =============================================================================
-- Extension aggregate - extension-owned durable sidecar state.
--
-- extension_id is filled by runtime extension helpers from the registered
-- extension identity. Extension callers should not supply or spoof it.
--
-- Optional block scope belongs to a session_turn_iteration. Current
-- iteration rows store one executed form, so block_index is normally 0
-- when present.
-- =============================================================================
CREATE TABLE extension_aggregate (
  id                          TEXT PRIMARY KEY NOT NULL,

  extension_id                TEXT NOT NULL CHECK (trim(extension_id) <> ''),
  aggregate_key               TEXT NOT NULL CHECK (trim(aggregate_key) <> ''),
  kind                        TEXT NOT NULL CHECK (trim(kind) <> ''),

  metadata                    TEXT,            -- JSON-encoded object/string
  content                     BLOB,            -- Nippy-encoded extension-owned payload

  session_soul_id        TEXT
                              REFERENCES session_soul(id) ON DELETE CASCADE,
  session_state_id       TEXT
                              REFERENCES session_state(id) ON DELETE CASCADE,
  session_turn_state_id  TEXT
                              REFERENCES session_turn_state(id) ON DELETE CASCADE,
  session_turn_iteration_id                TEXT
                              REFERENCES session_turn_iteration(id) ON DELETE CASCADE,
  session_turn_iteration_block_index       INTEGER CHECK (
                                session_turn_iteration_block_index IS NULL OR session_turn_iteration_block_index >= 0
                              ),
  session_turn_iteration_block_id          TEXT CHECK (
                                session_turn_iteration_block_id IS NULL OR trim(session_turn_iteration_block_id) <> ''
                              ),

  -- Singleton dedupe key for upsert. Generated from FK scope columns
  -- because SQLite treats NULLs in UNIQUE as distinct, which would
  -- otherwise defeat ON CONFLICT against the FK columns directly.
  scope_key                   TEXT GENERATED ALWAYS AS (
                                CASE
                                  WHEN session_turn_iteration_block_id IS NOT NULL
                                    THEN 'block-id:' || session_turn_iteration_block_id
                                  WHEN session_turn_iteration_id IS NOT NULL AND session_turn_iteration_block_index IS NOT NULL
                                    THEN 'block:' || session_turn_iteration_id || ':' || session_turn_iteration_block_index
                                  WHEN session_turn_iteration_id IS NOT NULL
                                    THEN 'session_turn_iteration:' || session_turn_iteration_id
                                  WHEN session_turn_state_id IS NOT NULL
                                    THEN 'turn-state:' || session_turn_state_id
                                  WHEN session_state_id IS NOT NULL
                                    THEN 'session-state:' || session_state_id
                                  WHEN session_soul_id IS NOT NULL
                                    THEN 'session-soul:' || session_soul_id
                                  ELSE 'global'
                                END
                              ) STORED NOT NULL,

  created_at                  INTEGER NOT NULL,
  updated_at                  INTEGER NOT NULL CHECK (updated_at >= created_at),

  CHECK ((session_turn_iteration_block_index IS NULL AND session_turn_iteration_block_id IS NULL)
         OR session_turn_iteration_id IS NOT NULL),
  UNIQUE (extension_id, aggregate_key, kind, scope_key)
);

CREATE INDEX idx_extension_aggregate_ext_kind
  ON extension_aggregate(extension_id, kind);

CREATE INDEX idx_extension_aggregate_ext_key
  ON extension_aggregate(extension_id, aggregate_key);

CREATE INDEX idx_extension_aggregate_ext_kind_updated
  ON extension_aggregate(extension_id, kind, updated_at);

CREATE INDEX idx_extension_aggregate_session_soul
  ON extension_aggregate(session_soul_id)
  WHERE session_soul_id IS NOT NULL;

CREATE INDEX idx_extension_aggregate_session_state
  ON extension_aggregate(session_state_id)
  WHERE session_state_id IS NOT NULL;

CREATE INDEX idx_extension_aggregate_session_turn_state
  ON extension_aggregate(session_turn_state_id)
  WHERE session_turn_state_id IS NOT NULL;

CREATE INDEX idx_extension_aggregate_iteration
  ON extension_aggregate(session_turn_iteration_id)
  WHERE session_turn_iteration_id IS NOT NULL;

CREATE INDEX idx_extension_aggregate_iteration_block
  ON extension_aggregate(session_turn_iteration_id, session_turn_iteration_block_index)
  WHERE session_turn_iteration_id IS NOT NULL AND session_turn_iteration_block_index IS NOT NULL;

CREATE INDEX idx_extension_aggregate_iteration_block_id
  ON extension_aggregate(session_turn_iteration_block_id)
  WHERE session_turn_iteration_block_id IS NOT NULL;

-- Supports metadata JSON field filtering for extension aggregate queries.
-- Extensions pass {:metadata {:field value}} to extension-list-aggregates/extension-delete-aggregate!;
-- the clause layer generates json_extract WHERE conditions on these paths.
CREATE INDEX idx_extension_aggregate_metadata
  ON extension_aggregate(extension_id, kind, json_extract(metadata, '$.kind'))
  WHERE metadata IS NOT NULL;

-- Supports graph-traversal-style metadata queries (Bridge edges by source/target).
-- Without these, "find all edges FROM x" or "find all edges TO y" scans all rows
-- of that kind.
CREATE INDEX idx_extension_aggregate_meta_source
  ON extension_aggregate(extension_id, kind, json_extract(metadata, '$.source'))
  WHERE metadata IS NOT NULL;

CREATE INDEX idx_extension_aggregate_meta_target
  ON extension_aggregate(extension_id, kind, json_extract(metadata, '$.target'))
  WHERE metadata IS NOT NULL;

-- Supports "all nodes/edges for a file" queries (Bridge re-indexing).
CREATE INDEX idx_extension_aggregate_meta_path
  ON extension_aggregate(extension_id, kind, json_extract(metadata, '$.path'))
  WHERE metadata IS NOT NULL;

-- =============================================================================
-- Log - structured logs.
-- Event envelope:
--   - event: machine-stable event key (e.g. "session_turn_iteration.llm.error")
--   - data: JSON-encoded object/string with all event payload fields
-- =============================================================================
CREATE TABLE log (
  id                     TEXT PRIMARY KEY NOT NULL,
  level                  TEXT NOT NULL
                         CHECK (level IN ('trace', 'debug', 'info', 'warn', 'error', 'fatal')),
  event                  TEXT NOT NULL,
  data                   TEXT,  -- JSON-encoded object/string

  session_soul_id   TEXT
                         REFERENCES session_soul(id) ON DELETE CASCADE,
  session_state_id  TEXT
                         REFERENCES session_state(id) ON DELETE CASCADE,
  session_turn_soul_id          TEXT
                         REFERENCES session_turn_soul(id) ON DELETE CASCADE,
  session_turn_state_id         TEXT
                         REFERENCES session_turn_state(id) ON DELETE CASCADE,
  session_turn_iteration_id           TEXT
                         REFERENCES session_turn_iteration(id) ON DELETE CASCADE,
  definition_soul_id     TEXT
                         REFERENCES definition_soul(id) ON DELETE CASCADE,
  definition_state_id    TEXT
                         REFERENCES definition_state(id) ON DELETE CASCADE,

  created_at             INTEGER NOT NULL
);

CREATE INDEX idx_log_level
  ON log(level, created_at);

CREATE INDEX idx_log_created
  ON log(created_at);

CREATE INDEX idx_log_event_created
  ON log(event, created_at);

CREATE INDEX idx_log_session_soul
  ON log(session_soul_id, created_at)
  WHERE session_soul_id IS NOT NULL;

CREATE INDEX idx_log_session_state
  ON log(session_state_id, created_at)
  WHERE session_state_id IS NOT NULL;

CREATE INDEX idx_log_session_turn_soul
  ON log(session_turn_soul_id, created_at)
  WHERE session_turn_soul_id IS NOT NULL;

CREATE INDEX idx_log_session_turn_state
  ON log(session_turn_state_id, created_at)
  WHERE session_turn_state_id IS NOT NULL;

CREATE INDEX idx_log_iteration
  ON log(session_turn_iteration_id, created_at)
  WHERE session_turn_iteration_id IS NOT NULL;

CREATE INDEX idx_log_definition_soul
  ON log(definition_soul_id, created_at)
  WHERE definition_soul_id IS NOT NULL;

CREATE INDEX idx_log_definition_state
  ON log(definition_state_id, created_at)
  WHERE definition_state_id IS NOT NULL;

-- =============================================================================
-- FTS5 - full-text search over user requests + iteration code + definition state expression snapshots.
-- =============================================================================
CREATE VIRTUAL TABLE search USING fts5(
  owner_table  UNINDEXED,
  owner_id     UNINDEXED,
  field        UNINDEXED,
  text,
  tokenize='porter unicode61 remove_diacritics 2'
);

-- Turn soul indexing
CREATE TRIGGER trg_session_turn_soul_ai AFTER INSERT ON session_turn_soul BEGIN
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'session_turn_soul', new.id, 'user_request', new.user_request
    WHERE new.user_request IS NOT NULL AND new.user_request <> '';
END;

CREATE TRIGGER trg_session_turn_soul_au AFTER UPDATE ON session_turn_soul BEGIN
  DELETE FROM search WHERE owner_table='session_turn_soul' AND owner_id=old.id;
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'session_turn_soul', new.id, 'user_request', new.user_request
    WHERE new.user_request IS NOT NULL AND new.user_request <> '';
END;

CREATE TRIGGER trg_session_turn_soul_ad AFTER DELETE ON session_turn_soul BEGIN
  DELETE FROM search WHERE owner_table='session_turn_soul' AND owner_id=old.id;
END;

-- Definition state indexing

-- Iteration code indexing
CREATE TRIGGER trg_session_turn_iteration_ai AFTER INSERT ON session_turn_iteration BEGIN
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'session_turn_iteration', new.id, 'code', new.code
    WHERE new.code IS NOT NULL AND new.code <> '';
END;

CREATE TRIGGER trg_session_turn_iteration_au AFTER UPDATE ON session_turn_iteration BEGIN
  DELETE FROM search WHERE owner_table='session_turn_iteration' AND owner_id=old.id AND field='code';
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'session_turn_iteration', new.id, 'code', new.code
    WHERE new.code IS NOT NULL AND new.code <> '';
END;

CREATE TRIGGER trg_session_turn_iteration_ad AFTER DELETE ON session_turn_iteration BEGIN
  DELETE FROM search WHERE owner_table='session_turn_iteration' AND owner_id=old.id AND field='code';
END;

CREATE TRIGGER trg_definition_state_ai AFTER INSERT ON definition_state BEGIN
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'definition_state', new.id, 'expression', new.expression
    WHERE new.expression IS NOT NULL AND new.expression <> '';
END;

CREATE TRIGGER trg_definition_state_au AFTER UPDATE ON definition_state BEGIN
  DELETE FROM search WHERE owner_table='definition_state' AND owner_id=old.id;
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'definition_state', new.id, 'expression', new.expression
    WHERE new.expression IS NOT NULL AND new.expression <> '';
END;

CREATE TRIGGER trg_definition_state_ad AFTER DELETE ON definition_state BEGIN
  DELETE FROM search WHERE owner_table='definition_state' AND owner_id=old.id;
END;
