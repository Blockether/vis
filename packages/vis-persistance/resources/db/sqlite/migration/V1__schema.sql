-- =============================================================================
-- V1 — vis schema (SQLite)
--
-- Runtime process diagram
--
--   conversation_soul (identity)
--     └─ conversation_state (branch/fork)
--          ├─ query_soul (user ask identity, branch-local)
--          │    └─ query_state (one run/retry)
--          │         └─ iteration (one LLM round-trip)
--          │              └─ expression_state (durable state versions; stateless uses single v0)
--          └─ expression_soul (branch-local expression identities)
--               └─ expression_dependency (expression graph, soul-level)
--
-- Flow per turn:
--   user ask -> query_soul/query_state -> iterations -> expression rows emitted
--   -> query_state done/error -> next turn (or branch/fork to new conversation_state)
--
-- Fork flow:
--   conversation_state(v1)
--        └─ fork -> conversation_state(v2, parent_state_id=v1)
--
--   Each fork keeps isolated branch-local query_soul + expression_soul identity.
-- =============================================================================

PRAGMA foreign_keys = ON;

-- =============================================================================
-- Conversation soul — pure identity.
-- metadata is a JSON blob for channel bindings and external IDs.
-- =============================================================================
CREATE TABLE conversation_soul (
  id           TEXT PRIMARY KEY NOT NULL,
  metadata     TEXT,                       -- JSON-encoded object/string
  created_at   INTEGER NOT NULL
);

CREATE INDEX idx_conv_soul_created
  ON conversation_soul(created_at DESC);

-- =============================================================================
-- Conversation state — forkable mutable snapshot.
-- =============================================================================
CREATE TABLE conversation_state (
  id                    TEXT PRIMARY KEY NOT NULL,
  conversation_soul_id  TEXT NOT NULL
                        REFERENCES conversation_soul(id) ON DELETE CASCADE,
  parent_state_id       TEXT
                        REFERENCES conversation_state(id) ON DELETE CASCADE,
  title                 TEXT,
  version               INTEGER NOT NULL CHECK (version >= 0),
  metadata              TEXT,              -- JSON-encoded object/string
  created_at            INTEGER NOT NULL,

  UNIQUE (conversation_soul_id, version)
);

CREATE INDEX idx_conv_state_soul
  ON conversation_state(conversation_soul_id, version);

CREATE INDEX idx_conv_state_parent
  ON conversation_state(parent_state_id);

-- =============================================================================
-- Query soul — immutable identity of a user ask, branch-local.
-- =============================================================================
CREATE TABLE query_soul (
  id                     TEXT PRIMARY KEY NOT NULL,
  conversation_state_id  TEXT NOT NULL
                         REFERENCES conversation_state(id) ON DELETE CASCADE,
  title                  TEXT,
  query                  TEXT,
  metadata               TEXT,             -- JSON-encoded object/string
  created_at             INTEGER NOT NULL
);

CREATE INDEX idx_query_soul_state
  ON query_soul(conversation_state_id, created_at);

-- =============================================================================
-- Query state — one run of query_soul. Retry = new state version.
-- =============================================================================
CREATE TABLE query_state (
  id                           TEXT PRIMARY KEY NOT NULL,
  query_soul_id                TEXT NOT NULL
                               REFERENCES query_soul(id) ON DELETE CASCADE,
  forked_from_query_state_id   TEXT
                               REFERENCES query_state(id) ON DELETE SET NULL,
  version                      INTEGER NOT NULL CHECK (version >= 0),
  llm_provider                 TEXT,
  llm_root_model               TEXT,
  prompt_enrichment            TEXT,
  subtitle                     TEXT,
  run_label                    TEXT,
  status                       TEXT NOT NULL
                               CHECK (status IN ('running', 'done', 'error', 'interrupted')),
  metadata                     TEXT,        -- JSON-encoded object/string
  created_at                   INTEGER NOT NULL,

  UNIQUE (query_soul_id, version)
);

CREATE INDEX idx_query_state_soul
  ON query_state(query_soul_id, version);

CREATE INDEX idx_query_state_forked_from
  ON query_state(forked_from_query_state_id);

-- =============================================================================
-- Iteration — one LLM round-trip within a query_state.
-- =============================================================================
CREATE TABLE iteration (
  id                              TEXT PRIMARY KEY NOT NULL,
  query_state_id                  TEXT NOT NULL
                                  REFERENCES query_state(id) ON DELETE CASCADE,
  position                        INTEGER NOT NULL CHECK (position >= 0),

  status                          TEXT NOT NULL
                                  CHECK (status IN ('running', 'done', 'error', 'interrupted')),

  llm_system_prompt               TEXT,
  llm_user_prompt                 TEXT,    -- JSON envelope for multimodal user input (text/images/audio/files)
  llm_provider                    TEXT,
  llm_model                       TEXT,

  llm_response                    TEXT,    -- final selected LLM response for this iteration
  llm_traces                      TEXT,    -- all LLM attempts/traces (fallback chain, failures, timings)
  llm_full_duration_ms            INTEGER CHECK (
                                    llm_full_duration_ms IS NULL OR llm_full_duration_ms >= 0
                                  ),       -- total duration across all traced attempts
  llm_thinking                    TEXT,
  llm_error                       TEXT,
  llm_returned_empty_expressions  INTEGER NOT NULL DEFAULT 0
                                  CHECK (llm_returned_empty_expressions IN (0, 1)),

  metadata                        TEXT,    -- JSON-encoded per-iteration context (active extensions, etc.)

  created_at                      INTEGER NOT NULL,
  finished_at                     INTEGER,

  UNIQUE (query_state_id, position)
);

CREATE INDEX idx_iteration_query_state
  ON iteration(query_state_id, position);

CREATE INDEX idx_iteration_query_state_created
  ON iteration(query_state_id, created_at);

-- =============================================================================
-- Expression soul — unified identity for var/call/literal.
-- Branch-local: belongs to conversation_state.
--
-- kind:
--   var      - variable identity / binding target (includes function vars)
--              e.g. (def x 42), (def user-name "Ana"), (defn sum [a b] (+ a b))
--   call     - executable expression/invocation
--              e.g. (sum 1 2), (map inc [1 2 3]), (println "hi")
--                   (if (> x 10) :big :small), (-> data :user :email clojure.string/lower-case)
--   literal  - constant/literal data node (always stateless)
--              e.g. 42, "hello", :ok, [1 2 3], {:a 1}
--
-- state_mode:
--   stateless | stateful
-- =============================================================================
CREATE TABLE expression_soul (
  id                     TEXT PRIMARY KEY NOT NULL,
  conversation_state_id  TEXT NOT NULL
                         REFERENCES conversation_state(id) ON DELETE CASCADE,

  kind                   TEXT NOT NULL
                         CHECK (kind IN ('var', 'call', 'literal')),

  state_mode             TEXT NOT NULL
                         CHECK (state_mode IN ('stateless', 'stateful')),

  name                   TEXT,
  metadata               TEXT,             -- JSON-encoded object/string
  created_at             INTEGER NOT NULL,

  CHECK (kind <> 'literal' OR state_mode = 'stateless')
);

CREATE INDEX idx_expression_soul_state_kind
  ON expression_soul(conversation_state_id, kind, created_at);

CREATE UNIQUE INDEX uq_expression_soul_state_name
  ON expression_soul(conversation_state_id, name)
  WHERE name IS NOT NULL;

-- =============================================================================
-- Expression dependency — downstream depends on upstream (soul-level graph).
--
-- Direction:
--   upstream_expression_soul_id -> downstream_expression_soul_id
--
-- This table stores pure recalculation edges.
-- If edge (u -> d) exists, d must be recalculated when u changes.
-- =============================================================================
CREATE TABLE expression_dependency (
  id                             TEXT PRIMARY KEY NOT NULL,
  conversation_state_id          TEXT NOT NULL
                                 REFERENCES conversation_state(id) ON DELETE CASCADE,
  downstream_expression_soul_id  TEXT NOT NULL
                                 REFERENCES expression_soul(id) ON DELETE CASCADE,
  upstream_expression_soul_id    TEXT NOT NULL
                                 REFERENCES expression_soul(id) ON DELETE CASCADE,
  metadata                       TEXT,      -- JSON-encoded object/string
  created_at                     INTEGER NOT NULL,

  CHECK (downstream_expression_soul_id <> upstream_expression_soul_id),
  UNIQUE (downstream_expression_soul_id, upstream_expression_soul_id)
);

CREATE INDEX idx_expr_dep_downstream
  ON expression_dependency(downstream_expression_soul_id);

CREATE INDEX idx_expr_dep_upstream
  ON expression_dependency(upstream_expression_soul_id);

CREATE INDEX idx_expr_dep_state
  ON expression_dependency(conversation_state_id);

CREATE TRIGGER trg_expr_dep_same_state_ai
BEFORE INSERT ON expression_dependency
BEGIN
  SELECT
    CASE
      WHEN (SELECT conversation_state_id FROM expression_soul WHERE id = NEW.downstream_expression_soul_id) IS NULL
        OR (SELECT conversation_state_id FROM expression_soul WHERE id = NEW.upstream_expression_soul_id) IS NULL
      THEN RAISE(ABORT, 'expression_dependency endpoint not found')
      WHEN (SELECT conversation_state_id FROM expression_soul WHERE id = NEW.downstream_expression_soul_id) <>
           (SELECT conversation_state_id FROM expression_soul WHERE id = NEW.upstream_expression_soul_id)
      THEN RAISE(ABORT, 'expression_dependency endpoints must be in same conversation_state')
      WHEN (SELECT conversation_state_id FROM expression_soul WHERE id = NEW.downstream_expression_soul_id) <> NEW.conversation_state_id
      THEN RAISE(ABORT, 'expression_dependency conversation_state_id mismatch')
    END;
END;

CREATE TRIGGER trg_expr_dep_same_state_au
BEFORE UPDATE ON expression_dependency
BEGIN
  SELECT
    CASE
      WHEN (SELECT conversation_state_id FROM expression_soul WHERE id = NEW.downstream_expression_soul_id) <>
           (SELECT conversation_state_id FROM expression_soul WHERE id = NEW.upstream_expression_soul_id)
      THEN RAISE(ABORT, 'expression_dependency endpoints must be in same conversation_state')
      WHEN (SELECT conversation_state_id FROM expression_soul WHERE id = NEW.downstream_expression_soul_id) <> NEW.conversation_state_id
      THEN RAISE(ABORT, 'expression_dependency conversation_state_id mismatch')
    END;
END;

-- =============================================================================
-- Expression state — versioned durable state emitted by expression_soul.
-- NOTE: write rows here only for expression_soul.state_mode='stateful'.
-- =============================================================================
CREATE TABLE expression_state (
  id                  TEXT PRIMARY KEY NOT NULL,
  expression_soul_id  TEXT NOT NULL
                      REFERENCES expression_soul(id) ON DELETE CASCADE,
  iteration_id        TEXT NOT NULL
                      REFERENCES iteration(id) ON DELETE CASCADE,

  version             INTEGER NOT NULL CHECK (version >= 0),

  success             INTEGER NOT NULL DEFAULT 1 CHECK (success IN (0, 1)),
  expr                TEXT CHECK (expr IS NULL OR trim(expr) <> ''), -- expected to be valid Clojure expression text
  result              BLOB,               -- Nippy-encoded result bytes
  error               BLOB,               -- Nippy-encoded error bytes
  stdout              TEXT,
  stderr              TEXT,
  duration_ms         INTEGER CHECK (duration_ms IS NULL OR duration_ms >= 0),
  metadata            TEXT,               -- JSON-encoded object/string
  created_at          INTEGER NOT NULL,

  UNIQUE (expression_soul_id, version),
  CHECK ((success = 1 AND error IS NULL) OR (success = 0 AND error IS NOT NULL))
);

CREATE INDEX idx_expression_state_soul
  ON expression_state(expression_soul_id, version);

CREATE INDEX idx_expression_state_iteration
  ON expression_state(iteration_id);

-- Version constraints for expression_state:
-- - first row for an expression_soul must start at version = 0
-- - stateless expressions: exactly one row, fixed at version = 0
CREATE TRIGGER trg_expression_state_stateless_ai
BEFORE INSERT ON expression_state
BEGIN
  SELECT CASE
    WHEN NOT EXISTS (
           SELECT 1
           FROM expression_state es
           WHERE es.expression_soul_id = NEW.expression_soul_id)
         AND NEW.version <> 0
    THEN RAISE(ABORT, 'first expression_state version must be 0')
  END;

  SELECT CASE
    WHEN (SELECT state_mode FROM expression_soul WHERE id = NEW.expression_soul_id) = 'stateless'
         AND NEW.version <> 0
    THEN RAISE(ABORT, 'stateless expression_state must use version 0')
  END;

  SELECT CASE
    WHEN (SELECT state_mode FROM expression_soul WHERE id = NEW.expression_soul_id) = 'stateless'
         AND EXISTS (
           SELECT 1
           FROM expression_state es
           WHERE es.expression_soul_id = NEW.expression_soul_id)
    THEN RAISE(ABORT, 'stateless expression_state may have only one row')
  END;
END;

CREATE TRIGGER trg_expression_state_stateless_au
BEFORE UPDATE ON expression_state
BEGIN
  SELECT CASE
    WHEN (SELECT state_mode FROM expression_soul WHERE id = NEW.expression_soul_id) = 'stateless'
         AND NEW.version <> 0
    THEN RAISE(ABORT, 'stateless expression_state must use version 0')
  END;

  SELECT CASE
    WHEN (SELECT state_mode FROM expression_soul WHERE id = NEW.expression_soul_id) = 'stateless'
         AND EXISTS (
           SELECT 1
           FROM expression_state es
           WHERE es.expression_soul_id = NEW.expression_soul_id
             AND es.id <> NEW.id)
    THEN RAISE(ABORT, 'stateless expression_state may have only one row')
  END;
END;

-- =============================================================================
-- Log — structured logs.
-- Event envelope:
--   - event: machine-stable event key (e.g. "iteration.llm.error")
--   - data: JSON-encoded object/string with all event payload fields
-- =============================================================================
CREATE TABLE log (
  id                     TEXT PRIMARY KEY NOT NULL,
  level                  TEXT NOT NULL
                         CHECK (level IN ('trace', 'debug', 'info', 'warn', 'error', 'fatal')),
  event                  TEXT NOT NULL,
  data                   TEXT,  -- JSON-encoded object/string

  conversation_soul_id   TEXT
                         REFERENCES conversation_soul(id) ON DELETE CASCADE,
  conversation_state_id  TEXT
                         REFERENCES conversation_state(id) ON DELETE CASCADE,
  query_soul_id          TEXT
                         REFERENCES query_soul(id) ON DELETE CASCADE,
  query_state_id         TEXT
                         REFERENCES query_state(id) ON DELETE CASCADE,
  iteration_id           TEXT
                         REFERENCES iteration(id) ON DELETE CASCADE,
  expression_soul_id     TEXT
                         REFERENCES expression_soul(id) ON DELETE CASCADE,
  expression_state_id    TEXT
                         REFERENCES expression_state(id) ON DELETE CASCADE,

  created_at             INTEGER NOT NULL
);

CREATE INDEX idx_log_level
  ON log(level, created_at);

CREATE INDEX idx_log_created
  ON log(created_at);

CREATE INDEX idx_log_event_created
  ON log(event, created_at);

CREATE INDEX idx_log_conv_soul
  ON log(conversation_soul_id, created_at)
  WHERE conversation_soul_id IS NOT NULL;

CREATE INDEX idx_log_conv_state
  ON log(conversation_state_id, created_at)
  WHERE conversation_state_id IS NOT NULL;

CREATE INDEX idx_log_query_soul
  ON log(query_soul_id, created_at)
  WHERE query_soul_id IS NOT NULL;

CREATE INDEX idx_log_query_state
  ON log(query_state_id, created_at)
  WHERE query_state_id IS NOT NULL;

CREATE INDEX idx_log_iteration
  ON log(iteration_id, created_at)
  WHERE iteration_id IS NOT NULL;

CREATE INDEX idx_log_expression_soul
  ON log(expression_soul_id, created_at)
  WHERE expression_soul_id IS NOT NULL;

CREATE INDEX idx_log_expression_state
  ON log(expression_state_id, created_at)
  WHERE expression_state_id IS NOT NULL;

-- =============================================================================
-- FTS5 — full-text search over query text + expression state expr snapshots.
-- =============================================================================
CREATE VIRTUAL TABLE search USING fts5(
  owner_table  UNINDEXED,
  owner_id     UNINDEXED,
  field        UNINDEXED,
  text,
  tokenize='porter unicode61 remove_diacritics 2'
);

-- Query soul indexing
CREATE TRIGGER trg_query_soul_ai AFTER INSERT ON query_soul BEGIN
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'query_soul', new.id, 'query', new.query
    WHERE new.query IS NOT NULL AND new.query <> '';
END;

CREATE TRIGGER trg_query_soul_au AFTER UPDATE ON query_soul BEGIN
  DELETE FROM search WHERE owner_table='query_soul' AND owner_id=old.id;
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'query_soul', new.id, 'query', new.query
    WHERE new.query IS NOT NULL AND new.query <> '';
END;

CREATE TRIGGER trg_query_soul_ad AFTER DELETE ON query_soul BEGIN
  DELETE FROM search WHERE owner_table='query_soul' AND owner_id=old.id;
END;

-- Expression state indexing
CREATE TRIGGER trg_expression_state_ai AFTER INSERT ON expression_state BEGIN
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'expression_state', new.id, 'expr', new.expr
    WHERE new.expr IS NOT NULL AND new.expr <> '';
END;

CREATE TRIGGER trg_expression_state_au AFTER UPDATE ON expression_state BEGIN
  DELETE FROM search WHERE owner_table='expression_state' AND owner_id=old.id;
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'expression_state', new.id, 'expr', new.expr
    WHERE new.expr IS NOT NULL AND new.expr <> '';
END;

CREATE TRIGGER trg_expression_state_ad AFTER DELETE ON expression_state BEGIN
  DELETE FROM search WHERE owner_table='expression_state' AND owner_id=old.id;
END;
