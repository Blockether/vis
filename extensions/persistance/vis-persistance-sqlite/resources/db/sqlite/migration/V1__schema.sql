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
--          │         └─ intent_soul / intent_state (turn-run scoped)
--          │              └─ plan_soul / plan_state
--          │                   └─ gate_soul / gate_state
--          │                        └─ attestation
--          │                             └─ attestation_provenance_ref
--          │                                  refs into iteration.blocks provenance timeline
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
  title                  TEXT,
  user_request           TEXT,
  metadata               TEXT,             -- JSON-encoded object/string
  created_at             INTEGER NOT NULL
);

CREATE INDEX idx_conversation_turn_soul_state
  ON conversation_turn_soul(conversation_state_id, created_at);

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
  position                        INTEGER NOT NULL CHECK (position >= 0),

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
  -- legacy `expression_soul kind='call'` + `expression_state` per-
  -- call rows are gone: every reader iterates per-iteration anyway,
  -- so per-call rows added cost without index value. Var rows stay
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

-- =============================================================================
-- Work provenance domain — versioned intent -> plan -> gate -> attestation.
--
-- These tables make the completion contract durable instead of prompt-only:
--   intent_state(versioned)
--     -> plan_state(versioned)
--       -> gate_state(versioned)
--         -> exactly one attestation per gate_state
--           -> one or more provenance refs into the iteration timeline
--
-- A gate can be inserted as open first. To close/block it, callers must insert
-- the matching attestation and at least one attestation_provenance_ref, then
-- update gate_state.status to closed/blocked. Triggers below enforce that final
-- closed/blocked state.
-- =============================================================================
CREATE TABLE intent_soul (
  id                          TEXT PRIMARY KEY NOT NULL,
  conversation_turn_state_id  TEXT NOT NULL
                              REFERENCES conversation_turn_state(id) ON DELETE CASCADE,
  key                         TEXT NOT NULL CHECK (trim(key) <> ''),
  metadata                    TEXT,         -- JSON-encoded object/string
  created_at                  INTEGER NOT NULL,

  UNIQUE (conversation_turn_state_id, key)
);

CREATE INDEX idx_intent_soul_turn_state
  ON intent_soul(conversation_turn_state_id, created_at);

CREATE TABLE intent_state (
  id                         TEXT PRIMARY KEY NOT NULL,
  intent_soul_id             TEXT NOT NULL
                             REFERENCES intent_soul(id) ON DELETE CASCADE,
  version                    INTEGER NOT NULL CHECK (version >= 0),
  supersedes_intent_state_id TEXT
                             REFERENCES intent_state(id) ON DELETE SET NULL,
  status                     TEXT NOT NULL
                             CHECK (status IN ('active', 'satisfied', 'superseded', 'abandoned')),
  text                       TEXT NOT NULL CHECK (trim(text) <> ''),
  created_iteration_id       TEXT
                             REFERENCES iteration(id) ON DELETE SET NULL,
  created_ref                TEXT CHECK (created_ref IS NULL OR trim(created_ref) <> ''),
  metadata                   TEXT,          -- JSON-encoded object/string
  created_at                 INTEGER NOT NULL,

  UNIQUE (intent_soul_id, version)
);

CREATE INDEX idx_intent_state_soul
  ON intent_state(intent_soul_id, version);

CREATE INDEX idx_intent_state_created_iteration
  ON intent_state(created_iteration_id)
  WHERE created_iteration_id IS NOT NULL;

CREATE TRIGGER trg_intent_state_version_ai
BEFORE INSERT ON intent_state
BEGIN
  SELECT CASE
    WHEN NOT EXISTS (
           SELECT 1 FROM intent_state s
           WHERE s.intent_soul_id = NEW.intent_soul_id)
         AND NEW.version <> 0
    THEN RAISE(ABORT, 'first intent_state version must be 0')
  END;

  SELECT CASE
    WHEN EXISTS (
           SELECT 1 FROM intent_state s
           WHERE s.intent_soul_id = NEW.intent_soul_id)
         AND NEW.version <> (
           SELECT max(s.version) + 1 FROM intent_state s
           WHERE s.intent_soul_id = NEW.intent_soul_id)
    THEN RAISE(ABORT, 'intent_state version must increment by 1')
  END;

  SELECT CASE
    WHEN NEW.supersedes_intent_state_id IS NOT NULL
         AND (SELECT intent_soul_id FROM intent_state WHERE id = NEW.supersedes_intent_state_id) <> NEW.intent_soul_id
    THEN RAISE(ABORT, 'intent_state supersedes must target same intent_soul')
  END;
END;

CREATE TRIGGER trg_intent_state_version_au
BEFORE UPDATE ON intent_state
BEGIN
  SELECT CASE
    WHEN NEW.intent_soul_id <> OLD.intent_soul_id OR NEW.version <> OLD.version
    THEN RAISE(ABORT, 'intent_state identity/version are immutable')
  END;

  SELECT CASE
    WHEN NEW.supersedes_intent_state_id IS NOT NULL
         AND (SELECT intent_soul_id FROM intent_state WHERE id = NEW.supersedes_intent_state_id) <> NEW.intent_soul_id
    THEN RAISE(ABORT, 'intent_state supersedes must target same intent_soul')
  END;
END;

CREATE TABLE plan_soul (
  id              TEXT PRIMARY KEY NOT NULL,
  intent_soul_id  TEXT NOT NULL
                  REFERENCES intent_soul(id) ON DELETE CASCADE,
  key             TEXT NOT NULL CHECK (trim(key) <> ''),
  metadata        TEXT,                     -- JSON-encoded object/string
  created_at      INTEGER NOT NULL,

  UNIQUE (intent_soul_id, key)
);

CREATE INDEX idx_plan_soul_intent
  ON plan_soul(intent_soul_id, created_at);

CREATE TABLE plan_state (
  id                       TEXT PRIMARY KEY NOT NULL,
  plan_soul_id             TEXT NOT NULL
                           REFERENCES plan_soul(id) ON DELETE CASCADE,
  version                  INTEGER NOT NULL CHECK (version >= 0),
  intent_state_id          TEXT NOT NULL
                           REFERENCES intent_state(id) ON DELETE CASCADE,
  supersedes_plan_state_id TEXT
                           REFERENCES plan_state(id) ON DELETE SET NULL,
  status                   TEXT NOT NULL
                           CHECK (status IN ('active', 'completed', 'stale', 'superseded', 'abandoned')),
  summary                  TEXT NOT NULL CHECK (trim(summary) <> ''),
  steps                    TEXT,            -- JSON-encoded vector of plan steps
  created_iteration_id     TEXT
                           REFERENCES iteration(id) ON DELETE SET NULL,
  created_ref              TEXT CHECK (created_ref IS NULL OR trim(created_ref) <> ''),
  metadata                 TEXT,            -- JSON-encoded object/string
  created_at               INTEGER NOT NULL,

  UNIQUE (plan_soul_id, version)
);

CREATE INDEX idx_plan_state_soul
  ON plan_state(plan_soul_id, version);

CREATE INDEX idx_plan_state_intent_state
  ON plan_state(intent_state_id);

CREATE TRIGGER trg_plan_state_lineage_ai
BEFORE INSERT ON plan_state
BEGIN
  SELECT CASE
    WHEN (SELECT intent_soul_id FROM plan_soul WHERE id = NEW.plan_soul_id) <>
         (SELECT intent_soul_id FROM intent_state WHERE id = NEW.intent_state_id)
    THEN RAISE(ABORT, 'plan_state intent_state must belong to plan_soul intent_soul')
  END;

  SELECT CASE
    WHEN NOT EXISTS (
           SELECT 1 FROM plan_state s
           WHERE s.plan_soul_id = NEW.plan_soul_id)
         AND NEW.version <> 0
    THEN RAISE(ABORT, 'first plan_state version must be 0')
  END;

  SELECT CASE
    WHEN EXISTS (
           SELECT 1 FROM plan_state s
           WHERE s.plan_soul_id = NEW.plan_soul_id)
         AND NEW.version <> (
           SELECT max(s.version) + 1 FROM plan_state s
           WHERE s.plan_soul_id = NEW.plan_soul_id)
    THEN RAISE(ABORT, 'plan_state version must increment by 1')
  END;

  SELECT CASE
    WHEN NEW.supersedes_plan_state_id IS NOT NULL
         AND (SELECT plan_soul_id FROM plan_state WHERE id = NEW.supersedes_plan_state_id) <> NEW.plan_soul_id
    THEN RAISE(ABORT, 'plan_state supersedes must target same plan_soul')
  END;
END;

CREATE TRIGGER trg_plan_state_lineage_au
BEFORE UPDATE ON plan_state
BEGIN
  SELECT CASE
    WHEN NEW.plan_soul_id <> OLD.plan_soul_id OR NEW.version <> OLD.version
    THEN RAISE(ABORT, 'plan_state identity/version are immutable')
  END;

  SELECT CASE
    WHEN (SELECT intent_soul_id FROM plan_soul WHERE id = NEW.plan_soul_id) <>
         (SELECT intent_soul_id FROM intent_state WHERE id = NEW.intent_state_id)
    THEN RAISE(ABORT, 'plan_state intent_state must belong to plan_soul intent_soul')
  END;

  SELECT CASE
    WHEN NEW.supersedes_plan_state_id IS NOT NULL
         AND (SELECT plan_soul_id FROM plan_state WHERE id = NEW.supersedes_plan_state_id) <> NEW.plan_soul_id
    THEN RAISE(ABORT, 'plan_state supersedes must target same plan_soul')
  END;
END;

CREATE TABLE gate_soul (
  id            TEXT PRIMARY KEY NOT NULL,
  plan_soul_id  TEXT NOT NULL
                REFERENCES plan_soul(id) ON DELETE CASCADE,
  key           TEXT NOT NULL CHECK (trim(key) <> ''),
  metadata      TEXT,                       -- JSON-encoded object/string
  created_at    INTEGER NOT NULL,

  UNIQUE (plan_soul_id, key)
);

CREATE INDEX idx_gate_soul_plan
  ON gate_soul(plan_soul_id, created_at);

CREATE TABLE gate_state (
  id                       TEXT PRIMARY KEY NOT NULL,
  gate_soul_id             TEXT NOT NULL
                           REFERENCES gate_soul(id) ON DELETE CASCADE,
  version                  INTEGER NOT NULL CHECK (version >= 0),
  plan_state_id            TEXT NOT NULL
                           REFERENCES plan_state(id) ON DELETE CASCADE,
  supersedes_gate_state_id TEXT
                           REFERENCES gate_state(id) ON DELETE SET NULL,
  status                   TEXT NOT NULL
                           CHECK (status IN ('open', 'closed', 'blocked', 'superseded')),
  required                 INTEGER NOT NULL DEFAULT 1 CHECK (required IN (0, 1)),
  question                 TEXT NOT NULL CHECK (trim(question) <> ''),
  created_iteration_id     TEXT
                           REFERENCES iteration(id) ON DELETE SET NULL,
  created_ref              TEXT CHECK (created_ref IS NULL OR trim(created_ref) <> ''),
  metadata                 TEXT,            -- JSON-encoded object/string
  created_at               INTEGER NOT NULL,

  UNIQUE (gate_soul_id, version)
);

CREATE INDEX idx_gate_state_soul
  ON gate_state(gate_soul_id, version);

CREATE INDEX idx_gate_state_plan_state
  ON gate_state(plan_state_id);

CREATE TABLE attestation (
  id                    TEXT PRIMARY KEY NOT NULL,
  gate_state_id         TEXT NOT NULL
                        REFERENCES gate_state(id) ON DELETE CASCADE,
  status                TEXT NOT NULL
                        CHECK (status IN ('proven', 'blocked', 'rejected')),
  summary               TEXT CHECK (summary IS NULL OR trim(summary) <> ''),
  reason                TEXT CHECK (reason IS NULL OR trim(reason) <> ''),
  created_iteration_id  TEXT
                        REFERENCES iteration(id) ON DELETE SET NULL,
  created_ref           TEXT CHECK (created_ref IS NULL OR trim(created_ref) <> ''),
  metadata              TEXT,              -- JSON-encoded object/string
  created_at            INTEGER NOT NULL,

  UNIQUE (gate_state_id),
  CHECK (status <> 'proven' OR summary IS NOT NULL),
  CHECK (status <> 'blocked' OR reason IS NOT NULL),
  CHECK (status <> 'rejected' OR reason IS NOT NULL)
);

CREATE INDEX idx_attestation_gate_state
  ON attestation(gate_state_id);

CREATE INDEX idx_attestation_created_iteration
  ON attestation(created_iteration_id)
  WHERE created_iteration_id IS NOT NULL;

CREATE TABLE attestation_provenance_ref (
  id              TEXT PRIMARY KEY NOT NULL,
  attestation_id  TEXT NOT NULL
                  REFERENCES attestation(id) ON DELETE CASCADE,
  ref             TEXT NOT NULL CHECK (trim(ref) <> ''),
  role            TEXT NOT NULL DEFAULT 'evidence'
                  CHECK (role IN ('evidence', 'counter-evidence', 'context')),
  metadata        TEXT,                     -- JSON-encoded object/string
  created_at      INTEGER NOT NULL,

  UNIQUE (attestation_id, ref)
);

CREATE INDEX idx_attestation_ref_attestation
  ON attestation_provenance_ref(attestation_id);

CREATE INDEX idx_attestation_ref_ref
  ON attestation_provenance_ref(ref);

CREATE TRIGGER trg_gate_state_lineage_ai
BEFORE INSERT ON gate_state
BEGIN
  SELECT CASE
    WHEN (SELECT plan_soul_id FROM gate_soul WHERE id = NEW.gate_soul_id) <>
         (SELECT plan_soul_id FROM plan_state WHERE id = NEW.plan_state_id)
    THEN RAISE(ABORT, 'gate_state plan_state must belong to gate_soul plan_soul')
  END;

  SELECT CASE
    WHEN NOT EXISTS (
           SELECT 1 FROM gate_state s
           WHERE s.gate_soul_id = NEW.gate_soul_id)
         AND NEW.version <> 0
    THEN RAISE(ABORT, 'first gate_state version must be 0')
  END;

  SELECT CASE
    WHEN EXISTS (
           SELECT 1 FROM gate_state s
           WHERE s.gate_soul_id = NEW.gate_soul_id)
         AND NEW.version <> (
           SELECT max(s.version) + 1 FROM gate_state s
           WHERE s.gate_soul_id = NEW.gate_soul_id)
    THEN RAISE(ABORT, 'gate_state version must increment by 1')
  END;

  SELECT CASE
    WHEN NEW.supersedes_gate_state_id IS NOT NULL
         AND (SELECT gate_soul_id FROM gate_state WHERE id = NEW.supersedes_gate_state_id) <> NEW.gate_soul_id
    THEN RAISE(ABORT, 'gate_state supersedes must target same gate_soul')
  END;

  SELECT CASE
    WHEN NEW.status = 'closed'
         AND NOT EXISTS (
           SELECT 1
           FROM attestation a
           WHERE a.gate_state_id = NEW.id
             AND a.status = 'proven'
             AND EXISTS (SELECT 1 FROM attestation_provenance_ref r WHERE r.attestation_id = a.id))
    THEN RAISE(ABORT, 'closed gate_state requires one proven attestation with provenance refs')
  END;

  SELECT CASE
    WHEN NEW.status = 'blocked'
         AND NOT EXISTS (
           SELECT 1
           FROM attestation a
           WHERE a.gate_state_id = NEW.id
             AND a.status = 'blocked'
             AND a.reason IS NOT NULL
             AND EXISTS (SELECT 1 FROM attestation_provenance_ref r WHERE r.attestation_id = a.id))
    THEN RAISE(ABORT, 'blocked gate_state requires one blocked attestation with reason and provenance refs')
  END;
END;

CREATE TRIGGER trg_gate_state_lineage_au
BEFORE UPDATE ON gate_state
BEGIN
  SELECT CASE
    WHEN NEW.gate_soul_id <> OLD.gate_soul_id OR NEW.version <> OLD.version
    THEN RAISE(ABORT, 'gate_state identity/version are immutable')
  END;

  SELECT CASE
    WHEN (SELECT plan_soul_id FROM gate_soul WHERE id = NEW.gate_soul_id) <>
         (SELECT plan_soul_id FROM plan_state WHERE id = NEW.plan_state_id)
    THEN RAISE(ABORT, 'gate_state plan_state must belong to gate_soul plan_soul')
  END;

  SELECT CASE
    WHEN NEW.supersedes_gate_state_id IS NOT NULL
         AND (SELECT gate_soul_id FROM gate_state WHERE id = NEW.supersedes_gate_state_id) <> NEW.gate_soul_id
    THEN RAISE(ABORT, 'gate_state supersedes must target same gate_soul')
  END;

  SELECT CASE
    WHEN NEW.status = 'closed'
         AND NOT EXISTS (
           SELECT 1
           FROM attestation a
           WHERE a.gate_state_id = NEW.id
             AND a.status = 'proven'
             AND EXISTS (SELECT 1 FROM attestation_provenance_ref r WHERE r.attestation_id = a.id))
    THEN RAISE(ABORT, 'closed gate_state requires one proven attestation with provenance refs')
  END;

  SELECT CASE
    WHEN NEW.status = 'blocked'
         AND NOT EXISTS (
           SELECT 1
           FROM attestation a
           WHERE a.gate_state_id = NEW.id
             AND a.status = 'blocked'
             AND a.reason IS NOT NULL
             AND EXISTS (SELECT 1 FROM attestation_provenance_ref r WHERE r.attestation_id = a.id))
    THEN RAISE(ABORT, 'blocked gate_state requires one blocked attestation with reason and provenance refs')
  END;
END;

CREATE TRIGGER trg_attestation_immutable_when_gate_terminal_au
BEFORE UPDATE ON attestation
BEGIN
  SELECT CASE
    WHEN (SELECT status FROM gate_state WHERE id = OLD.gate_state_id) IN ('closed', 'blocked')
    THEN RAISE(ABORT, 'attestation is immutable after gate_state is closed or blocked')
  END;
END;

CREATE TRIGGER trg_attestation_immutable_when_gate_terminal_ad
BEFORE DELETE ON attestation
BEGIN
  SELECT CASE
    WHEN (SELECT status FROM gate_state WHERE id = OLD.gate_state_id) IN ('closed', 'blocked')
    THEN RAISE(ABORT, 'attestation cannot be deleted after gate_state is closed or blocked')
  END;
END;

CREATE TRIGGER trg_attestation_ref_immutable_when_gate_terminal_au
BEFORE UPDATE ON attestation_provenance_ref
BEGIN
  SELECT CASE
    WHEN (SELECT gs.status
          FROM gate_state gs
          JOIN attestation a ON a.gate_state_id = gs.id
          WHERE a.id = OLD.attestation_id) IN ('closed', 'blocked')
    THEN RAISE(ABORT, 'attestation provenance refs are immutable after gate_state is closed or blocked')
  END;
END;

CREATE TRIGGER trg_attestation_ref_immutable_when_gate_terminal_ad
BEFORE DELETE ON attestation_provenance_ref
BEGIN
  SELECT CASE
    WHEN (SELECT gs.status
          FROM gate_state gs
          JOIN attestation a ON a.gate_state_id = gs.id
          WHERE a.id = OLD.attestation_id) IN ('closed', 'blocked')
    THEN RAISE(ABORT, 'attestation provenance refs cannot be deleted after gate_state is closed or blocked')
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
-- The legacy 'call' kind was REMOVED. Per-call data now lives in the
-- Nippy-encoded `iteration.blocks` BLOB; nothing else queried
-- call rows by id, so the indirection added cost without value.
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
