-- =============================================================================
-- V1 - vis schema (SQLite)
--
-- Runtime process diagram
--
--   conversation_soul (identity)
--     └─ conversation_state (branch/fork)
--          ├─ conversation_turn_soul (user ask identity, branch-local)
--          │    └─ conversation_turn_state (one run/retry)
--          │         ├─ iteration (one LLM round-trip)
--          │         │    │
--          │         │    ├─ iteration.blocks BLOB
--          │         │    │    Nippy-encoded vec of per-block maps
--          │         │    │    {:idx :code :comment :result :error
--          │         │    │     :stdout :stderr :duration-ms
--          │         │    │     :timeout? :repaired?}
--          │         │    │
--          │         │    └─ expression_state (var versions only)
--          │         │
--          │
--          └─ expression_soul (branch-local var identities, kind='var')
--               └─ expression_dependency (var dependency graph, soul-level)
--
-- Flow per turn:
--   user ask -> conversation_turn_soul/conversation_turn_state -> iterations
--     each iteration writes its full block log inline into
--     iteration.blocks plus one expression_soul + expression_state
--     row per `(def ...)` it executed
--   -> conversation_turn_state done/error -> next turn (or branch/fork to new conversation_state)
--
-- Fork flow:
--   conversation_state(v1)
--        └─ fork -> conversation_state(v2, parent_state_id=v1)
--
--   Each fork keeps isolated branch-local conversation_turn_soul + expression_soul identity.
-- =============================================================================

PRAGMA foreign_keys = ON;

-- =============================================================================
-- Conversation soul - pure identity.
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
-- Conversation state - forkable mutable snapshot.
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
-- Turn soul - immutable identity of a user request, branch-local.
-- =============================================================================
CREATE TABLE conversation_turn_soul (
  id                     TEXT PRIMARY KEY NOT NULL,
  conversation_state_id  TEXT NOT NULL
                         REFERENCES conversation_state(id) ON DELETE CASCADE,
  position               INTEGER NOT NULL CHECK (position >= 1),
  title                  TEXT,
  user_request           TEXT,
  metadata               TEXT,             -- JSON-encoded object/string
  created_at             INTEGER NOT NULL,

  UNIQUE (conversation_state_id, position)
);

CREATE INDEX idx_conversation_turn_soul_state
  ON conversation_turn_soul(conversation_state_id, position);

CREATE TRIGGER trg_conversation_turn_soul_position_ai
BEFORE INSERT ON conversation_turn_soul
BEGIN
  SELECT CASE
    WHEN NOT EXISTS (
           SELECT 1 FROM conversation_turn_soul s
           WHERE s.conversation_state_id = NEW.conversation_state_id)
         AND NEW.position <> 1
    THEN RAISE(ABORT, 'first conversation_turn_soul position must be 1')
  END;

  SELECT CASE
    WHEN EXISTS (
           SELECT 1 FROM conversation_turn_soul s
           WHERE s.conversation_state_id = NEW.conversation_state_id)
         AND NEW.position <> (
           SELECT max(s.position) + 1 FROM conversation_turn_soul s
           WHERE s.conversation_state_id = NEW.conversation_state_id)
    THEN RAISE(ABORT, 'conversation_turn_soul position must increment by 1')
  END;
END;

CREATE TRIGGER trg_conversation_turn_soul_position_au
BEFORE UPDATE ON conversation_turn_soul
BEGIN
  SELECT CASE
    WHEN NEW.conversation_state_id <> OLD.conversation_state_id OR NEW.position <> OLD.position
    THEN RAISE(ABORT, 'conversation_turn_soul state/position are immutable')
  END;
END;

-- =============================================================================
-- Conversation turn state - one run of conversation_turn_soul.
-- Retry = new state version.
-- =============================================================================
CREATE TABLE conversation_turn_state (
  id                           TEXT PRIMARY KEY NOT NULL,
  conversation_turn_soul_id                TEXT NOT NULL
                               REFERENCES conversation_turn_soul(id) ON DELETE CASCADE,
  forked_from_conversation_turn_state_id   TEXT
                               REFERENCES conversation_turn_state(id) ON DELETE SET NULL,
  version                      INTEGER NOT NULL CHECK (version >= 0),
  llm_root_provider            TEXT,    -- provider id (e.g. 'openai', 'github-copilot')
  llm_root_model               TEXT,
  prompt_enrichment            TEXT,
  subtitle                     TEXT,
  run_label                    TEXT,
  status                       TEXT NOT NULL
                               CHECK (status IN ('running', 'done', 'error', 'interrupted')),
  metadata                     TEXT,        -- JSON-encoded object/string
  -- Per-turn final outcome, derived at turn end. Lets the next turn's
  -- handover digest say "previous turn complete | cancelled | error"
  -- without scanning every iteration. Set by the iteration
  -- loop on the terminal iteration and by
  -- sweep-orphaned-running-turns! for cancelled / orphaned turns.
  prior_outcome                TEXT
                               CHECK (prior_outcome IS NULL OR
                                      prior_outcome IN ('complete', 'cancelled', 'error')),
  created_at                   INTEGER NOT NULL,

  UNIQUE (conversation_turn_soul_id, version)
);

CREATE INDEX idx_conversation_turn_state_soul
  ON conversation_turn_state(conversation_turn_soul_id, version);

CREATE INDEX idx_conversation_turn_state_forked_from
  ON conversation_turn_state(forked_from_conversation_turn_state_id);

-- =============================================================================
-- Iteration - one LLM round-trip within a conversation_turn_state.
-- =============================================================================
CREATE TABLE iteration (
  id                              TEXT PRIMARY KEY NOT NULL,
  conversation_turn_state_id                  TEXT NOT NULL
                                  REFERENCES conversation_turn_state(id) ON DELETE CASCADE,
  position                        INTEGER NOT NULL CHECK (position >= 1),

  status                          TEXT NOT NULL
                                  CHECK (status IN ('running', 'done', 'error', 'interrupted')),

  llm_system_prompt               TEXT,
  llm_user_prompt                 TEXT,    -- JSON envelope for multimodal user input (text/images/audio/files)
  llm_provider                    TEXT,    -- provider id used for this iteration (e.g. 'openai', 'github-copilot')
  llm_model                       TEXT,

  llm_full_duration_ms            INTEGER CHECK (
                                    llm_full_duration_ms IS NULL OR llm_full_duration_ms >= 0
                                  ),       -- total duration across all traced attempts
  llm_thinking                    TEXT,
  llm_error                       TEXT,
  llm_returned_empty_blocks  INTEGER NOT NULL DEFAULT 0
                                  CHECK (llm_returned_empty_blocks IN (0, 1)),

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
  llm_executable_code             TEXT,
  llm_executable_blocks           TEXT,    -- JSON vec of executable fenced blocks selected by svar: [{:lang :source} ...]

  -- svar canonical assistant message persisted so preserved-thinking
  -- replay survives a vis restart. JSON-encoded
  -- `{:role "assistant" :content [<canonical-blocks>]}` exactly as
  -- svar's `:assistant-message` returns it; canonical thinking blocks
  -- carry per-provider preserved-reasoning state under
  -- `:thinking-signature`. Cross-turn / cross-restart resume reads
  -- this column to rebuild the per-turn replay buffer.
  llm_assistant_message           TEXT,

  -- Per-iteration token accounting + estimated USD cost. NULL when
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

  metadata                        TEXT,    -- JSON-encoded per-iteration context (active extensions, etc.)

  -- Per-iteration code-block log as ONE Nippy-encoded vec of
  -- block maps:
  --   [{:idx N
  --     :code              "(some-code)"
  --     :comment           ";; leading comment" | absent
  --     :result            <Nippy-frozen value> | absent
  --     :error             "ERROR string" | absent
  --     :stdout            non-blank | absent
  --     :stderr            non-blank | absent
  --     :duration-ms       N
  --     :timeout?          true | absent
  --     :repaired?         true | absent}
  --    ...]
  --
  -- \"Block\" matches the LLM-facing prompt vocabulary (each
  -- top-level form inside a fenced ```clojure ... ``` block). The
  -- per-call `expression_soul` rows are gone: every reader iterates
  -- per-iteration anyway, so per-call rows added cost without index
  -- value. Var rows stay
  -- first-class (expression_soul kind='var') because var-history is
  -- the keystone of the data model - versioned, branched,
  -- dependency-graphed.
  blocks                          BLOB,

  -- Index of the form that called `(answer ...)` when the iteration
  -- produced a final answer. Channels render the answer text below;
  -- this slot lets readers ELIDE that form from the displayed call
  -- log without re-walking the source. NULL for non-terminal
  -- iterations.
  answer_form_idx                 INTEGER
                                  CHECK (answer_form_idx IS NULL
                                         OR answer_form_idx >= 0),

  created_at                      INTEGER NOT NULL,
  finished_at                     INTEGER,

  UNIQUE (conversation_turn_state_id, position)
);

CREATE INDEX idx_iteration_conversation_turn_state
  ON iteration(conversation_turn_state_id, position);

CREATE INDEX idx_iteration_conversation_turn_state_created
  ON iteration(conversation_turn_state_id, created_at);

CREATE TRIGGER trg_iteration_position_ai
BEFORE INSERT ON iteration
BEGIN
  SELECT CASE
    WHEN NOT EXISTS (
           SELECT 1 FROM iteration i
           WHERE i.conversation_turn_state_id = NEW.conversation_turn_state_id)
         AND NEW.position <> 1
    THEN RAISE(ABORT, 'first iteration position must be 1')
  END;

  SELECT CASE
    WHEN EXISTS (
           SELECT 1 FROM iteration i
           WHERE i.conversation_turn_state_id = NEW.conversation_turn_state_id)
         AND NEW.position <> (
           SELECT max(i.position) + 1 FROM iteration i
           WHERE i.conversation_turn_state_id = NEW.conversation_turn_state_id)
    THEN RAISE(ABORT, 'iteration position must increment by 1')
  END;
END;

CREATE TRIGGER trg_iteration_position_au
BEFORE UPDATE ON iteration
BEGIN
  SELECT CASE
    WHEN NEW.conversation_turn_state_id <> OLD.conversation_turn_state_id OR NEW.position <> OLD.position
    THEN RAISE(ABORT, 'iteration turn-state/position are immutable')
  END;
END;

-- =============================================================================
-- Expression soul - identity for var bindings (and reserved literal
-- slot). Branch-local: belongs to conversation_state.
--
-- kind:
--   var      - variable identity / binding target (includes function vars)
--              e.g. (def x 42), (def user-name "Ana"), (defn sum [a b] (+ a b))
--   literal  - constant/literal data node (always stateless). Reserved;
--              the loop does not write literal rows today, but the
--              kind enum stays open so a future RLM extension can
--              pin literals without another schema migration.
--
-- Per-call rows were removed. Per-call data now lives in the
-- Nippy-encoded `iteration.blocks` BLOB; nothing else queried call rows
-- by id, so the indirection added cost without value.
--
-- state_mode:
--   stateless | stateful
-- =============================================================================
CREATE TABLE expression_soul (
  id                     TEXT PRIMARY KEY NOT NULL,
  conversation_state_id  TEXT NOT NULL
                         REFERENCES conversation_state(id) ON DELETE CASCADE,

  kind                   TEXT NOT NULL
                         CHECK (kind IN ('var', 'literal')),

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
-- Expression dependency - downstream depends on upstream (soul-level graph).
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
-- Expression state - versioned durable state emitted by expression_soul.
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
-- - stateless blocks: exactly one row, fixed at version = 0
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
-- Extension aggregate - extension-owned durable sidecar state.
--
-- extension_id is filled by runtime extension helpers from the registered
-- extension identity. Extension callers should not supply or spoof it.
--
-- iteration_block_id is a logical block id for future first-class block rows. Current block payloads still live in iteration.blocks BLOB, so
-- iteration_block_id is intentionally not a foreign key yet. Use
-- iteration_block_index with iteration_id for current block-scoped state.
-- =============================================================================
CREATE TABLE extension_aggregate (
  id                          TEXT PRIMARY KEY NOT NULL,

  extension_id                TEXT NOT NULL CHECK (trim(extension_id) <> ''),
  aggregate_key               TEXT NOT NULL CHECK (trim(aggregate_key) <> ''),
  kind                        TEXT NOT NULL CHECK (trim(kind) <> ''),

  metadata                    TEXT,            -- JSON-encoded object/string
  content                     BLOB,            -- Nippy-encoded extension-owned payload
  scope_key                   TEXT NOT NULL CHECK (trim(scope_key) <> ''),
                                -- Runtime-normalized singleton key for upsert.
                                -- Examples: global, conversation-soul:<id>,
                                -- iteration:<id>, block:<iteration-id>:<index>.

  conversation_soul_id        TEXT
                              REFERENCES conversation_soul(id) ON DELETE CASCADE,
  conversation_state_id       TEXT
                              REFERENCES conversation_state(id) ON DELETE CASCADE,
  conversation_turn_state_id  TEXT
                              REFERENCES conversation_turn_state(id) ON DELETE CASCADE,
  iteration_id                TEXT
                              REFERENCES iteration(id) ON DELETE CASCADE,
  iteration_block_index       INTEGER CHECK (
                                iteration_block_index IS NULL OR iteration_block_index >= 0
                              ),
  iteration_block_id          TEXT CHECK (
                                iteration_block_id IS NULL OR trim(iteration_block_id) <> ''
                              ),

  created_at                  INTEGER NOT NULL,
  updated_at                  INTEGER NOT NULL CHECK (updated_at >= created_at),

  CHECK ((iteration_block_index IS NULL AND iteration_block_id IS NULL)
         OR iteration_id IS NOT NULL),
  UNIQUE (extension_id, aggregate_key, kind, scope_key)
);

CREATE INDEX idx_extension_aggregate_ext_kind
  ON extension_aggregate(extension_id, kind);

CREATE INDEX idx_extension_aggregate_ext_key
  ON extension_aggregate(extension_id, aggregate_key);

CREATE INDEX idx_extension_aggregate_ext_kind_updated
  ON extension_aggregate(extension_id, kind, updated_at);

CREATE INDEX idx_extension_aggregate_conversation_soul
  ON extension_aggregate(conversation_soul_id)
  WHERE conversation_soul_id IS NOT NULL;

CREATE INDEX idx_extension_aggregate_conversation_state
  ON extension_aggregate(conversation_state_id)
  WHERE conversation_state_id IS NOT NULL;

CREATE INDEX idx_extension_aggregate_conversation_turn_state
  ON extension_aggregate(conversation_turn_state_id)
  WHERE conversation_turn_state_id IS NOT NULL;

CREATE INDEX idx_extension_aggregate_iteration
  ON extension_aggregate(iteration_id)
  WHERE iteration_id IS NOT NULL;

CREATE INDEX idx_extension_aggregate_iteration_block
  ON extension_aggregate(iteration_id, iteration_block_index)
  WHERE iteration_id IS NOT NULL AND iteration_block_index IS NOT NULL;

CREATE INDEX idx_extension_aggregate_iteration_block_id
  ON extension_aggregate(iteration_block_id)
  WHERE iteration_block_id IS NOT NULL;

-- =============================================================================
-- Log - structured logs.
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
  conversation_turn_soul_id          TEXT
                         REFERENCES conversation_turn_soul(id) ON DELETE CASCADE,
  conversation_turn_state_id         TEXT
                         REFERENCES conversation_turn_state(id) ON DELETE CASCADE,
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

CREATE INDEX idx_log_conversation_turn_soul
  ON log(conversation_turn_soul_id, created_at)
  WHERE conversation_turn_soul_id IS NOT NULL;

CREATE INDEX idx_log_conversation_turn_state
  ON log(conversation_turn_state_id, created_at)
  WHERE conversation_turn_state_id IS NOT NULL;

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
-- FTS5 - full-text search over user requests + expression state expr snapshots.
-- =============================================================================
CREATE VIRTUAL TABLE search USING fts5(
  owner_table  UNINDEXED,
  owner_id     UNINDEXED,
  field        UNINDEXED,
  text,
  tokenize='porter unicode61 remove_diacritics 2'
);

-- Turn soul indexing
CREATE TRIGGER trg_conversation_turn_soul_ai AFTER INSERT ON conversation_turn_soul BEGIN
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'conversation_turn_soul', new.id, 'user_request', new.user_request
    WHERE new.user_request IS NOT NULL AND new.user_request <> '';
END;

CREATE TRIGGER trg_conversation_turn_soul_au AFTER UPDATE ON conversation_turn_soul BEGIN
  DELETE FROM search WHERE owner_table='conversation_turn_soul' AND owner_id=old.id;
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'conversation_turn_soul', new.id, 'user_request', new.user_request
    WHERE new.user_request IS NOT NULL AND new.user_request <> '';
END;

CREATE TRIGGER trg_conversation_turn_soul_ad AFTER DELETE ON conversation_turn_soul BEGIN
  DELETE FROM search WHERE owner_table='conversation_turn_soul' AND owner_id=old.id;
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
