-- =============================================================================
-- V1 — vis schema (SQLite) — CONSOLIDATED
--
-- Soul/state pattern:
--   conversation_soul -> conversation_state   (forkable)
--   query_soul        -> query_state          (retriable)
--   expression_soul   -> expression_state     (versioned only when stateful)
--
-- Main model decisions:
--   1) execution + iteration_var_* are unified as expression_*
--   2) expression dependencies are modeled on soul level (many-to-many)
--   3) stateful/stateless is explicit on expression_soul.state_mode
--   4) only stateful expressions emit durable expression_state versions
--   5) optional expression_event captures stateless runtime observations
--
-- Branch-local semantics:
--   query_soul and expression_soul belong to conversation_state
--   (not conversation_soul) so branches stay isolated.
-- =============================================================================

PRAGMA foreign_keys = ON;

-- =============================================================================
-- Conversation soul — pure identity.
-- metadata is a JSON blob for channel bindings and external IDs.
-- =============================================================================
CREATE TABLE conversation_soul (
  id           TEXT PRIMARY KEY NOT NULL,
  metadata     TEXT,
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
  metadata              TEXT,
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
  query                  TEXT,
  metadata               TEXT,
  created_at             INTEGER NOT NULL
);

CREATE INDEX idx_query_soul_state
  ON query_soul(conversation_state_id, created_at);

-- =============================================================================
-- Query state — one run of query_soul. Retry = new state version.
-- =============================================================================
CREATE TABLE query_state (
  id                TEXT PRIMARY KEY NOT NULL,
  query_soul_id     TEXT NOT NULL
                    REFERENCES query_soul(id) ON DELETE CASCADE,
  version           INTEGER NOT NULL CHECK (version >= 0),
  llm_provider      TEXT,
  llm_root_model    TEXT,
  status            TEXT NOT NULL
                    CHECK (status IN ('running', 'done', 'error', 'interrupted')),
  metadata          TEXT,
  created_at        INTEGER NOT NULL,

  UNIQUE (query_soul_id, version)
);

CREATE INDEX idx_query_state_soul
  ON query_state(query_soul_id, version);

-- =============================================================================
-- Iteration — one LLM round-trip within a query_state.
-- =============================================================================
CREATE TABLE iteration (
  id                       TEXT PRIMARY KEY NOT NULL,
  query_state_id           TEXT NOT NULL
                           REFERENCES query_state(id) ON DELETE CASCADE,
  position                 INTEGER NOT NULL CHECK (position >= 0),

  system_prompt            TEXT,
  llm_provider             TEXT,
  llm_model                TEXT,

  llm_final_response       TEXT,
  llm_traces               TEXT,
  llm_router_duration_ms   INTEGER CHECK (
                             llm_router_duration_ms IS NULL OR llm_router_duration_ms >= 0
                           ),

  thinking                 TEXT,
  error                    TEXT,
  duration_ms              INTEGER CHECK (duration_ms IS NULL OR duration_ms >= 0),
  created_at               INTEGER NOT NULL,

  UNIQUE (query_state_id, position)
);

CREATE INDEX idx_iteration_query_state
  ON iteration(query_state_id, position);

CREATE INDEX idx_iteration_query_state_created
  ON iteration(query_state_id, created_at);

-- =============================================================================
-- Expression soul — unified identity for var/function/call/op/literal.
-- Branch-local: belongs to conversation_state.
--
-- kind:
--   var | function | call | op | literal
--
-- state_mode:
--   stateless | stateful
-- =============================================================================
CREATE TABLE expression_soul (
  id                     TEXT PRIMARY KEY NOT NULL,
  conversation_state_id  TEXT NOT NULL
                         REFERENCES conversation_state(id) ON DELETE CASCADE,

  kind                   TEXT NOT NULL
                         CHECK (kind IN ('var', 'function', 'call', 'op', 'literal')),

  state_mode             TEXT NOT NULL
                         CHECK (state_mode IN ('stateless', 'stateful')),

  name                   TEXT,
  metadata               TEXT,
  created_at             INTEGER NOT NULL
);

CREATE INDEX idx_expression_soul_state_kind
  ON expression_soul(conversation_state_id, kind, created_at);

CREATE INDEX idx_expression_soul_state_name
  ON expression_soul(conversation_state_id, name);

-- Optional branch-local uniqueness for named identities.
-- Keep commented out if you want shadowing/scoping duplicates.
-- CREATE UNIQUE INDEX uq_expression_soul_state_kind_name
--   ON expression_soul(conversation_state_id, kind, name)
--   WHERE name IS NOT NULL;

-- =============================================================================
-- Expression dependency — downstream depends on upstream (soul-level graph).
-- =============================================================================
CREATE TABLE expression_dependency (
  id                             TEXT PRIMARY KEY NOT NULL,
  downstream_expression_soul_id  TEXT NOT NULL
                                 REFERENCES expression_soul(id) ON DELETE CASCADE,
  upstream_expression_soul_id    TEXT NOT NULL
                                 REFERENCES expression_soul(id) ON DELETE CASCADE,
  position                       INTEGER,
  dependency_kind                TEXT NOT NULL DEFAULT 'value'
                                 CHECK (dependency_kind IN ('value', 'call', 'read', 'write', 'control')),
  metadata                       TEXT,
  created_at                     INTEGER NOT NULL,

  CHECK (downstream_expression_soul_id <> upstream_expression_soul_id),
  UNIQUE (downstream_expression_soul_id, upstream_expression_soul_id, dependency_kind, position)
);

CREATE INDEX idx_expression_dependency_downstream
  ON expression_dependency(downstream_expression_soul_id, position);

CREATE INDEX idx_expression_dependency_upstream
  ON expression_dependency(upstream_expression_soul_id);

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
  status              TEXT NOT NULL
                      CHECK (status IN ('materialized', 'error', 'invalidated')),

  expression_text     TEXT,
  value               TEXT,
  error               TEXT,
  stdout              TEXT,
  stderr              TEXT,
  duration_ms         INTEGER CHECK (duration_ms IS NULL OR duration_ms >= 0),
  metadata            TEXT,
  created_at          INTEGER NOT NULL,

  UNIQUE (expression_soul_id, version)
);

CREATE INDEX idx_expression_state_soul
  ON expression_state(expression_soul_id, version);

CREATE INDEX idx_expression_state_iteration
  ON expression_state(iteration_id);

-- =============================================================================
-- Expression event — optional runtime events for stateless expressions.
-- Use when execution happened but no durable state should be versioned.
-- =============================================================================
CREATE TABLE expression_event (
  id                  TEXT PRIMARY KEY NOT NULL,
  expression_soul_id  TEXT NOT NULL
                      REFERENCES expression_soul(id) ON DELETE CASCADE,
  iteration_id        TEXT NOT NULL
                      REFERENCES iteration(id) ON DELETE CASCADE,

  position            INTEGER NOT NULL CHECK (position >= 0),
  outcome             TEXT NOT NULL
                      CHECK (outcome IN ('observed', 'error')),

  expression_text     TEXT,
  result              TEXT,
  error               TEXT,
  stdout              TEXT,
  stderr              TEXT,
  duration_ms         INTEGER CHECK (duration_ms IS NULL OR duration_ms >= 0),
  metadata            TEXT,
  created_at          INTEGER NOT NULL,

  UNIQUE (iteration_id, position)
);

CREATE INDEX idx_expression_event_expression
  ON expression_event(expression_soul_id, created_at);

CREATE INDEX idx_expression_event_iteration
  ON expression_event(iteration_id, position);

-- =============================================================================
-- Log — structured logs.
-- Convention: set only the deepest applicable FK.
-- =============================================================================
CREATE TABLE log (
  id                     TEXT PRIMARY KEY NOT NULL,
  level                  TEXT NOT NULL
                         CHECK (level IN ('trace', 'debug', 'info', 'warn', 'error', 'fatal')),
  ns                     TEXT,
  message                TEXT,
  data                   TEXT,

  conversation_soul_id   TEXT
                         REFERENCES conversation_soul(id) ON DELETE CASCADE,
  conversation_state_id  TEXT
                         REFERENCES conversation_state(id) ON DELETE CASCADE,
  query_id               TEXT
                         REFERENCES query_soul(id) ON DELETE CASCADE,
  iteration_id           TEXT
                         REFERENCES iteration(id) ON DELETE CASCADE,
  expression_state_id    TEXT
                         REFERENCES expression_state(id) ON DELETE CASCADE,
  expression_event_id    TEXT
                         REFERENCES expression_event(id) ON DELETE CASCADE,

  created_at             INTEGER NOT NULL
);

CREATE INDEX idx_log_level
  ON log(level, created_at);

CREATE INDEX idx_log_created
  ON log(created_at);

CREATE INDEX idx_log_conv_soul
  ON log(conversation_soul_id)
  WHERE conversation_soul_id IS NOT NULL;

CREATE INDEX idx_log_conv_state
  ON log(conversation_state_id)
  WHERE conversation_state_id IS NOT NULL;

CREATE INDEX idx_log_query
  ON log(query_id)
  WHERE query_id IS NOT NULL;

CREATE INDEX idx_log_iteration
  ON log(iteration_id)
  WHERE iteration_id IS NOT NULL;

CREATE INDEX idx_log_expression_state
  ON log(expression_state_id)
  WHERE expression_state_id IS NOT NULL;

CREATE INDEX idx_log_expression_event
  ON log(expression_event_id)
  WHERE expression_event_id IS NOT NULL;

-- =============================================================================
-- FTS5 — full-text search over query text + expression state snapshots.
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
    SELECT 'expression_state', new.id, 'expression_text', new.expression_text
    WHERE new.expression_text IS NOT NULL AND new.expression_text <> '';
END;

CREATE TRIGGER trg_expression_state_au AFTER UPDATE ON expression_state BEGIN
  DELETE FROM search WHERE owner_table='expression_state' AND owner_id=old.id;
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'expression_state', new.id, 'expression_text', new.expression_text
    WHERE new.expression_text IS NOT NULL AND new.expression_text <> '';
END;

CREATE TRIGGER trg_expression_state_ad AFTER DELETE ON expression_state BEGIN
  DELETE FROM search WHERE owner_table='expression_state' AND owner_id=old.id;
END;
