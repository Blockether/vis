-- =============================================================================
-- V1 — vis schema (SQLite)
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
--          ├─ conversation_intent / plan / gate (conversation-scoped)
--          │    └─ conversation_intent_focus (turn-state sidecar)
--          │         refs into iteration.blocks provenance timeline
--          │
--          └─ expression_soul (branch-local var identities, kind='var')
--               └─ expression_dependency (var dependency graph, soul-level)
--
-- Flow per turn:
--   user ask -> conversation_turn_soul/conversation_turn_state -> iterations
--     each iteration writes its full block log inline into
--     iteration.blocks plus one expression_soul + expression_state
--     row per `(def …)` it executed
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
-- Turn soul — immutable identity of a user request, branch-local.
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
-- Conversation turn state — one run of conversation_turn_soul.
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
  -- handover digest say "previous turn complete | abandoned | cancelled
  -- | error" without scanning every iteration. Set by the iteration
  -- loop on the terminal iteration and by
  -- sweep-orphaned-running-turns! for cancelled / orphaned turns.
  prior_outcome                TEXT
                               CHECK (prior_outcome IS NULL OR
                                      prior_outcome IN ('complete', 'abandoned',
                                                        'cancelled', 'error')),
  created_at                   INTEGER NOT NULL,

  UNIQUE (conversation_turn_soul_id, version)
);

CREATE INDEX idx_conversation_turn_state_soul
  ON conversation_turn_state(conversation_turn_soul_id, version);

CREATE INDEX idx_conversation_turn_state_forked_from
  ON conversation_turn_state(forked_from_conversation_turn_state_id);

-- =============================================================================
-- Iteration — one LLM round-trip within a conversation_turn_state.
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
  -- cheap list views and evidence comparison available too.
  llm_raw_response                TEXT,
  llm_raw_response_preview        TEXT,
  llm_raw_response_length         INTEGER CHECK (
                                    llm_raw_response_length IS NULL OR llm_raw_response_length >= 0
                                  ),
  llm_raw_response_sha256         TEXT,
  llm_executable_code             TEXT,
  llm_executable_blocks           TEXT,    -- JSON vec of executable fenced blocks selected by svar: [{:lang :source} ...]

  -- Per-iteration token accounting + estimated USD cost. NULL when
  -- the provider response did not surface usage (e.g. the LLM call
  -- itself failed before a response was returned). Reasoning /
  -- cached tokens are subsets of completion / prompt respectively;
  -- callers that want "input minus cached" compute it themselves.
  -- Cost is provider-side estimated USD via the router's pricing
  -- table — recorded so historical rows survive future price
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
  --    …]
  --
  -- \"Block\" matches the LLM-facing prompt vocabulary (each
  -- top-level form inside a fenced ```clojure ... ``` block). The
  -- per-call `expression_soul` rows are gone: every reader iterates
  -- per-iteration anyway, so per-call rows added cost without index
  -- value. Var rows stay
  -- first-class (expression_soul kind='var') because var-history is
  -- the keystone of the data model — versioned, branched,
  -- dependency-graphed.
  blocks                          BLOB,

  -- Index of the form that called `(answer …)` when the iteration
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
-- Conversation-scoped intents, plans, blocking gates, and turn focus.
--
-- Intents belong to conversation_soul. Focus belongs to one
-- conversation_turn_state so an old unrelated active intent does not block
-- every later branch/run. Provenance refs cite canonical iteration block paths.
-- =============================================================================
CREATE TABLE conversation_intent (
  id TEXT PRIMARY KEY NOT NULL,
  conversation_soul_id TEXT NOT NULL REFERENCES conversation_soul(id) ON DELETE CASCADE,
  title TEXT NOT NULL CHECK (trim(title) <> ''),
  rationale TEXT NOT NULL CHECK (trim(rationale) <> ''),
  status TEXT NOT NULL CHECK (status IN ('active', 'fulfilled', 'abandoned')),
  fulfillment_summary TEXT CHECK (fulfillment_summary IS NULL OR trim(fulfillment_summary) <> ''),
  abandonment_reason TEXT CHECK (abandonment_reason IS NULL OR trim(abandonment_reason) <> ''),
  created_conversation_turn_id TEXT REFERENCES conversation_turn_soul(id) ON DELETE SET NULL,
  resolved_conversation_turn_id TEXT REFERENCES conversation_turn_soul(id) ON DELETE SET NULL,
  created_ref TEXT CHECK (created_ref IS NULL OR trim(created_ref) <> ''),
  resolved_ref TEXT CHECK (resolved_ref IS NULL OR trim(resolved_ref) <> ''),
  metadata TEXT,
  created_at INTEGER NOT NULL,
  resolved_at INTEGER,

  CHECK (status <> 'fulfilled' OR
         (fulfillment_summary IS NOT NULL AND abandonment_reason IS NULL AND resolved_at IS NOT NULL)),
  CHECK (status <> 'abandoned' OR
         (abandonment_reason IS NOT NULL AND fulfillment_summary IS NULL AND resolved_at IS NOT NULL)),
  CHECK (status <> 'active' OR
         (fulfillment_summary IS NULL AND abandonment_reason IS NULL AND resolved_at IS NULL))
);

CREATE INDEX idx_conversation_intent_soul_status
  ON conversation_intent(conversation_soul_id, status, created_at);

CREATE TABLE conversation_intent_ref (
  id TEXT PRIMARY KEY NOT NULL,
  intent_id TEXT NOT NULL REFERENCES conversation_intent(id) ON DELETE CASCADE,
  ref TEXT NOT NULL CHECK (trim(ref) <> ''),
  role TEXT NOT NULL CHECK (role IN ('fulfillment-evidence', 'abandonment-evidence', 'context')),
  metadata TEXT,
  created_at INTEGER NOT NULL,
  UNIQUE (intent_id, ref, role)
);

CREATE INDEX idx_conversation_intent_ref_intent ON conversation_intent_ref(intent_id);
CREATE INDEX idx_conversation_intent_ref_ref ON conversation_intent_ref(ref);

CREATE TABLE conversation_intent_relation (
  id TEXT PRIMARY KEY NOT NULL,
  from_intent_id TEXT NOT NULL REFERENCES conversation_intent(id) ON DELETE CASCADE,
  to_intent_id TEXT NOT NULL REFERENCES conversation_intent(id) ON DELETE CASCADE,
  relation TEXT NOT NULL CHECK (relation IN ('subintent', 'related', 'supports', 'blocks')),
  rationale TEXT CHECK (rationale IS NULL OR trim(rationale) <> ''),
  metadata TEXT,
  created_at INTEGER NOT NULL,
  CHECK (from_intent_id <> to_intent_id),
  UNIQUE (from_intent_id, to_intent_id, relation)
);

CREATE TRIGGER trg_conversation_intent_relation_same_soul_ai
BEFORE INSERT ON conversation_intent_relation
BEGIN
  SELECT CASE
    WHEN (SELECT conversation_soul_id FROM conversation_intent WHERE id = NEW.from_intent_id) IS NULL
      OR (SELECT conversation_soul_id FROM conversation_intent WHERE id = NEW.to_intent_id) IS NULL
    THEN RAISE(ABORT, 'conversation_intent_relation endpoint not found')
    WHEN (SELECT conversation_soul_id FROM conversation_intent WHERE id = NEW.from_intent_id) <>
         (SELECT conversation_soul_id FROM conversation_intent WHERE id = NEW.to_intent_id)
    THEN RAISE(ABORT, 'conversation_intent_relation endpoints must belong to same conversation_soul')
  END;
END;

CREATE TRIGGER trg_conversation_intent_relation_same_soul_au
BEFORE UPDATE ON conversation_intent_relation
BEGIN
  SELECT CASE
    WHEN (SELECT conversation_soul_id FROM conversation_intent WHERE id = NEW.from_intent_id) IS NULL
      OR (SELECT conversation_soul_id FROM conversation_intent WHERE id = NEW.to_intent_id) IS NULL
    THEN RAISE(ABORT, 'conversation_intent_relation endpoint not found')
    WHEN (SELECT conversation_soul_id FROM conversation_intent WHERE id = NEW.from_intent_id) <>
         (SELECT conversation_soul_id FROM conversation_intent WHERE id = NEW.to_intent_id)
    THEN RAISE(ABORT, 'conversation_intent_relation endpoints must belong to same conversation_soul')
  END;
END;

CREATE TABLE conversation_intent_plan (
  id TEXT PRIMARY KEY NOT NULL,
  intent_id TEXT NOT NULL REFERENCES conversation_intent(id) ON DELETE CASCADE,
  status TEXT NOT NULL CHECK (status IN ('active', 'completed', 'superseded', 'abandoned')),
  summary TEXT NOT NULL CHECK (trim(summary) <> ''),
  plan_dsl BLOB,
  steps BLOB,
  supersedes_plan_id TEXT REFERENCES conversation_intent_plan(id) ON DELETE SET NULL,
  created_conversation_turn_id TEXT REFERENCES conversation_turn_soul(id) ON DELETE SET NULL,
  created_ref TEXT CHECK (created_ref IS NULL OR trim(created_ref) <> ''),
  metadata BLOB,
  created_at INTEGER NOT NULL
);

CREATE INDEX idx_conversation_intent_plan_intent
  ON conversation_intent_plan(intent_id, created_at);

CREATE UNIQUE INDEX uq_conversation_intent_plan_one_active
  ON conversation_intent_plan(intent_id)
  WHERE status = 'active';

CREATE TRIGGER trg_conversation_intent_plan_supersedes_ai
BEFORE INSERT ON conversation_intent_plan
BEGIN
  SELECT CASE
    WHEN NEW.supersedes_plan_id IS NOT NULL
         AND (SELECT intent_id FROM conversation_intent_plan WHERE id = NEW.supersedes_plan_id) <> NEW.intent_id
    THEN RAISE(ABORT, 'conversation_intent_plan supersedes must target same intent')
  END;
END;

CREATE TRIGGER trg_conversation_intent_plan_supersedes_au
BEFORE UPDATE ON conversation_intent_plan
BEGIN
  SELECT CASE
    WHEN NEW.intent_id <> OLD.intent_id
    THEN RAISE(ABORT, 'conversation_intent_plan intent is immutable')
  END;

  SELECT CASE
    WHEN NEW.supersedes_plan_id IS NOT NULL
         AND (SELECT intent_id FROM conversation_intent_plan WHERE id = NEW.supersedes_plan_id) <> NEW.intent_id
    THEN RAISE(ABORT, 'conversation_intent_plan supersedes must target same intent')
  END;
END;

CREATE TABLE conversation_intent_gate (
  id TEXT PRIMARY KEY NOT NULL,
  plan_id TEXT NOT NULL REFERENCES conversation_intent_plan(id) ON DELETE CASCADE,
  status TEXT NOT NULL DEFAULT 'open' CHECK (status IN ('open', 'proven', 'impeded')),
  required INTEGER NOT NULL DEFAULT 1 CHECK (required IN (0, 1)),
  proposition TEXT NOT NULL CHECK (trim(proposition) <> ''),
  expected_proof BLOB NOT NULL,
  candidate_proof BLOB,
  proof BLOB,
  impediment BLOB,
  metadata BLOB,
  created_ref TEXT CHECK (created_ref IS NULL OR trim(created_ref) <> ''),
  resolved_ref TEXT CHECK (resolved_ref IS NULL OR trim(resolved_ref) <> ''),
  created_at INTEGER NOT NULL,
  resolved_at INTEGER,

  CHECK (status <> 'proven' OR
         (proof IS NOT NULL AND impediment IS NULL AND resolved_at IS NOT NULL)),
  CHECK (status <> 'impeded' OR
         (impediment IS NOT NULL AND proof IS NULL AND resolved_at IS NOT NULL)),
  CHECK (status <> 'open' OR
         (proof IS NULL AND impediment IS NULL AND resolved_at IS NULL))
);

CREATE INDEX idx_conversation_intent_gate_plan
  ON conversation_intent_gate(plan_id, created_at);

CREATE TABLE conversation_intent_gate_ref (
  id TEXT PRIMARY KEY NOT NULL,
  gate_id TEXT NOT NULL REFERENCES conversation_intent_gate(id) ON DELETE CASCADE,
  ref TEXT NOT NULL CHECK (trim(ref) <> ''),
  role TEXT NOT NULL DEFAULT 'proof' CHECK (role IN ('proof', 'impediment', 'context')),
  intent_id TEXT,
  slot TEXT,
  metadata BLOB,
  created_at INTEGER NOT NULL,
  UNIQUE (gate_id, ref, role, intent_id, slot)
);

CREATE INDEX idx_conversation_intent_gate_ref_gate ON conversation_intent_gate_ref(gate_id);
CREATE INDEX idx_conversation_intent_gate_ref_ref ON conversation_intent_gate_ref(ref);

CREATE TRIGGER trg_conversation_intent_gate_ref_terminal_au
BEFORE UPDATE ON conversation_intent_gate_ref
BEGIN
  SELECT CASE
    WHEN (SELECT status FROM conversation_intent_gate WHERE id = OLD.gate_id) IN ('proven', 'impeded')
    THEN RAISE(ABORT, 'conversation_intent_gate refs are immutable after gate is proven or impeded')
  END;
END;

CREATE TRIGGER trg_conversation_intent_gate_ref_terminal_ad
BEFORE DELETE ON conversation_intent_gate_ref
BEGIN
  SELECT CASE
    WHEN (SELECT status FROM conversation_intent_gate WHERE id = OLD.gate_id) IN ('proven', 'impeded')
    THEN RAISE(ABORT, 'conversation_intent_gate refs cannot be deleted after gate is proven or impeded')
  END;
END;

CREATE TABLE conversation_intent_focus (
  id TEXT PRIMARY KEY NOT NULL,
  conversation_turn_state_id TEXT NOT NULL REFERENCES conversation_turn_state(id) ON DELETE CASCADE,
  intent_id TEXT NOT NULL REFERENCES conversation_intent(id) ON DELETE CASCADE,
  source TEXT NOT NULL CHECK (source IN ('created', 'touched', 'inferred')),
  metadata TEXT,
  created_at INTEGER NOT NULL,
  UNIQUE (conversation_turn_state_id, intent_id)
);

CREATE INDEX idx_conversation_intent_focus_turn_state
  ON conversation_intent_focus(conversation_turn_state_id, created_at);

CREATE INDEX idx_conversation_intent_focus_intent
  ON conversation_intent_focus(intent_id);

CREATE TRIGGER trg_conversation_intent_focus_same_soul_ai
BEFORE INSERT ON conversation_intent_focus
BEGIN
  SELECT CASE
    WHEN (SELECT cs.conversation_soul_id
          FROM conversation_turn_state cts
          JOIN conversation_turn_soul cturn ON cturn.id = cts.conversation_turn_soul_id
          JOIN conversation_state cs ON cs.id = cturn.conversation_state_id
          WHERE cts.id = NEW.conversation_turn_state_id) IS NULL
      OR (SELECT conversation_soul_id FROM conversation_intent WHERE id = NEW.intent_id) IS NULL
    THEN RAISE(ABORT, 'conversation_intent_focus endpoint not found')
    WHEN (SELECT cs.conversation_soul_id
          FROM conversation_turn_state cts
          JOIN conversation_turn_soul cturn ON cturn.id = cts.conversation_turn_soul_id
          JOIN conversation_state cs ON cs.id = cturn.conversation_state_id
          WHERE cts.id = NEW.conversation_turn_state_id) <>
         (SELECT conversation_soul_id FROM conversation_intent WHERE id = NEW.intent_id)
    THEN RAISE(ABORT, 'conversation_intent_focus intent must belong to turn-state conversation_soul')
  END;
END;

CREATE TRIGGER trg_conversation_intent_focus_same_soul_au
BEFORE UPDATE ON conversation_intent_focus
BEGIN
  SELECT CASE
    WHEN (SELECT cs.conversation_soul_id
          FROM conversation_turn_state cts
          JOIN conversation_turn_soul cturn ON cturn.id = cts.conversation_turn_soul_id
          JOIN conversation_state cs ON cs.id = cturn.conversation_state_id
          WHERE cts.id = NEW.conversation_turn_state_id) IS NULL
      OR (SELECT conversation_soul_id FROM conversation_intent WHERE id = NEW.intent_id) IS NULL
    THEN RAISE(ABORT, 'conversation_intent_focus endpoint not found')
    WHEN (SELECT cs.conversation_soul_id
          FROM conversation_turn_state cts
          JOIN conversation_turn_soul cturn ON cturn.id = cts.conversation_turn_soul_id
          JOIN conversation_state cs ON cs.id = cturn.conversation_state_id
          WHERE cts.id = NEW.conversation_turn_state_id) <>
         (SELECT conversation_soul_id FROM conversation_intent WHERE id = NEW.intent_id)
    THEN RAISE(ABORT, 'conversation_intent_focus intent must belong to turn-state conversation_soul')
  END;
END;

-- =============================================================================
-- Expression soul — identity for var bindings (and reserved literal
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
-- FTS5 — full-text search over user requests + expression state expr snapshots.
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
