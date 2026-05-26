-- =============================================================================
-- V1 - vis schema (SQLite)
--
-- Hierarchy:
--
--   workspace (git worktree-backed work unit; one branch per row,
--              trunk-kind rows have no worktree dir)
--     └─ session_state (binds to exactly one workspace via workspace_id)
--
--   session_soul (identity)
--     └─ session_state (branch/fork; pinned to workspace 1:1)
--          └─ session_turn_soul (user ask, branch-local)
--               └─ session_turn_state (one run/retry of the turn)
--                    └─ session_turn_iteration (one LLM round-trip)
--                         │
--                         └─ code / forms BLOB / result / error / duration_ms columns
--                              Executed forms (per-form envelopes) live on
--                              the iteration row. There is no per-var sidecar
--                              table: SCI defs are intra-turn scratch only,
--                              never persisted as cross-turn state.
--                              Cross-turn memory rides on session_turn_state.ctx
--                              (specs / tasks / facts / trailer) and on the
--                              forms BLOB (forensic source-of-truth for
--                              introspect-form / introspect-iter / introspect-turn).
--
-- Naming convention:
--   *_soul   = immutable identity, branch-local
--   *_state  = mutable snapshot; retry/fork = new state row
--   parent_table_child = nested concept (session_turn_iteration
--                        = iteration nested under session_turn_state)
--
-- Position columns (1-based int) live alongside UUID PKs at every
-- level. UUIDs are the join key; position is the public/agent-
-- facing identifier.
--
-- Flow per turn:
--   user request
--     -> session_turn_soul + session_turn_state
--     -> session_turn_iteration(s)
--          each session_turn_iteration records executed code in `code`,
--          the per-form envelope vec in `forms` (BLOB), and result /
--          error / duration_ms columns. SCI defs live ONLY for the
--          duration of the SCI sandbox (intra-turn). No cross-turn
--          var rehydration: the model reaches earlier forms via
--          introspect-form / introspect-iter / introspect-turn (DB
--          reads against the forms BLOB) or via :session/facts.
--     -> session_turn_state done/error
--     -> next turn (or branch/fork to new session_state)
--
-- Fork flow:
--   session_state(v1)
--        └─ fork -> session_state(v2, parent_state_id=v1)
--   Each fork keeps isolated branch-local session_turn_soul identity.
-- =============================================================================

PRAGMA foreign_keys = ON;

-- =============================================================================
-- Session soul - pure identity.
-- =============================================================================
CREATE TABLE session_soul (
  id           TEXT PRIMARY KEY NOT NULL,
  channel      TEXT NOT NULL DEFAULT 'tui',
  external_id  TEXT,
  created_at   INTEGER NOT NULL
);

CREATE INDEX idx_session_soul_channel_created
  ON session_soul(channel, created_at DESC);

CREATE UNIQUE INDEX idx_session_soul_channel_external
  ON session_soul(channel, external_id)
  WHERE external_id IS NOT NULL;

-- =============================================================================
-- Workspace - git worktree-backed work unit.
--
-- One row = one branch = one worktree on disk = one session-binding.
-- Each session_state pins to exactly one workspace (1:1 invariant).
--
--   kind='trunk'   - original repo checkout. root = repo_root. No worktree
--                    materialised on disk. merge/discard refused by API.
--   kind='branch'  - managed worktree under
--                    ~/.vis/workspaces/<repo_id>/<workspace_id>/.
--
-- state lifecycle (branch-kind only):
--   active --> merging --> merged
--   active --> discarded
--
-- UNIQUE(repo_id, branch) is partial: only enforced for kind='branch',
-- so multiple trunk-kind rows (one per session-on-trunk) coexist.
-- =============================================================================
CREATE TABLE workspace (
  id                   TEXT PRIMARY KEY NOT NULL,
  repo_id              TEXT NOT NULL,
  repo_root            TEXT NOT NULL,

  kind                 TEXT NOT NULL
                       CHECK (kind IN ('trunk', 'branch')),
  branch               TEXT,
  root                 TEXT NOT NULL,

  parent_workspace_id  TEXT
                       REFERENCES workspace(id) ON DELETE SET NULL,

  state                TEXT NOT NULL
                       CHECK (state IN ('active', 'merging', 'merged', 'discarded')),

  commit_id            TEXT,              -- repo HEAD sha at worktree creation

  -- Human-friendly label that overrides the default
  -- (session.title / branch name) in the TUI strip + Telegram
  -- switcher. NULL falls back to the heuristic. Set via
  -- `/workspace label "…"` / `workspace/set-label!`.
  label                TEXT,
  -- Monotonic timestamp of the last `workspace/focus!`.
  -- Drives TUI strip ordering (most-recent first) and tab restore
  -- (`workspace/last-focused`). NULL falls back to `created_at`.
  last_focused_at_ms   INTEGER,

  created_at           INTEGER NOT NULL,
  merged_at            INTEGER,
  discarded_at         INTEGER,

  CHECK (kind = 'trunk' OR branch IS NOT NULL),
  CHECK (kind = 'branch' OR state = 'active')
);

CREATE UNIQUE INDEX uq_workspace_repo_branch
  ON workspace(repo_id, branch) WHERE kind = 'branch';

CREATE INDEX idx_workspace_repo_state
  ON workspace(repo_id, state);

-- =============================================================================
-- repo_focus — per-repo last-active workspace pointer.
-- One row per repo; updated by `workspace/focus!`. The TUI uses this to
-- restore the active tab across restarts; the Telegram switcher uses it
-- as the default landing for `/workspace switch` without an argument.
-- =============================================================================
CREATE TABLE repo_focus (
  repo_id        TEXT PRIMARY KEY NOT NULL,
  workspace_id   TEXT NOT NULL
                 REFERENCES workspace(id) ON DELETE CASCADE,
  updated_at_ms  INTEGER NOT NULL
);

-- =============================================================================
-- Session state - forkable mutable snapshot. Pinned 1:1 to a workspace.
-- =============================================================================
CREATE TABLE session_state (
  id                    TEXT PRIMARY KEY NOT NULL,
  session_soul_id  TEXT NOT NULL
                        REFERENCES session_soul(id) ON DELETE CASCADE,
  parent_state_id       TEXT
                        REFERENCES session_state(id) ON DELETE CASCADE,
  workspace_id          TEXT NOT NULL
                        REFERENCES workspace(id) ON DELETE RESTRICT,
  title                 TEXT,
  version               INTEGER NOT NULL CHECK (version >= 0),
  system_prompt         TEXT,
  llm_root_provider     TEXT,
  llm_root_model        TEXT,
  created_at            INTEGER NOT NULL,

  -- When this session_state is the engine-spawned merge-resolve
  -- sub-session, this column points at the parent
  -- session_state that initiated the merge. The sub-session's
  -- prompt + permitted op set are gated on the column being
  -- non-NULL; completion deletes the sub-session and the
  -- parent resumes. Distinct from `parent_state_id` which is
  -- the fork lineage column.
  merge_resolve_parent_id  TEXT
                        REFERENCES session_state(id) ON DELETE SET NULL,

  UNIQUE (session_soul_id, version)
);

-- The 1:1 workspace-session_state invariant is partial. A
-- merge-resolve sub-session pins to its parent's workspace so the
-- conflict state in the worktree index is visible to both sessions.
-- We DON'T want that to violate the UNIQUE: the
-- constraint applies only to rows where merge_resolve_parent_id IS
-- NULL (i.e. the canonical session_state of a workspace). Resolve
-- sub-session rows can coexist on the same workspace_id.
CREATE UNIQUE INDEX uq_session_state_workspace
  ON session_state(workspace_id)
  WHERE merge_resolve_parent_id IS NULL;

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
  iteration_count              INTEGER NOT NULL DEFAULT 0 CHECK (iteration_count >= 0),
  duration_ms                  INTEGER NOT NULL DEFAULT 0 CHECK (duration_ms >= 0),
  -- Phase B canonical token shape. `input_tokens` is ALWAYS TOTAL
  -- (Anthropic-additive raw values are summed at the canonical
  -- normalizer boundary; OpenAI / Gemini / Z.ai already report total).
  -- The detail columns are SUBSETS of `input_tokens` and obey the
  -- invariant:
  --   input_regular_tokens + input_cache_write_tokens
  --     + input_cache_read_tokens = input_tokens
  input_tokens                 INTEGER NOT NULL DEFAULT 0 CHECK (input_tokens >= 0),
  input_regular_tokens         INTEGER NOT NULL DEFAULT 0 CHECK (input_regular_tokens >= 0),
  input_cache_write_tokens     INTEGER NOT NULL DEFAULT 0 CHECK (input_cache_write_tokens >= 0),
  input_cache_read_tokens      INTEGER NOT NULL DEFAULT 0 CHECK (input_cache_read_tokens >= 0),
  output_tokens                INTEGER NOT NULL DEFAULT 0 CHECK (output_tokens >= 0),
  output_reasoning_tokens      INTEGER NOT NULL DEFAULT 0 CHECK (output_reasoning_tokens >= 0),
  total_cost_usd               REAL NOT NULL DEFAULT 0 CHECK (total_cost_usd >= 0),
  answer_markdown              TEXT,        -- Raw Markdown source the model emitted via `(done {:answer ...})`.
                                            -- Channels parse via `render/markdown->ir` at render time.
                                            -- NULL while the turn is still running.
  -- Per-turn final outcome, derived at turn end. Lets the next turn's
  -- handover digest say "previous turn complete | cancelled | error"
  -- without scanning every session_turn_iteration. Set by the session_turn_iteration
  -- loop on the terminal session_turn_iteration and by
  -- sweep-orphaned-running-turns! for cancelled / orphaned turns.
  prior_outcome                TEXT
                               CHECK (prior_outcome IS NULL OR
                                      prior_outcome IN ('complete', 'cancelled', 'error')),

  -- Nippy-encoded CTX snapshot as of the END of this turn version.
  -- Live CTX = ctx on the latest turn-state for the latest turn-soul
  -- of a session_state. History = walk the soul chain and decode each
  -- row's ctx in turn-position order. No parallel history table;
  -- per-turn snapshots come for free.
  -- NULL while the turn is still running; written by the engine's
  -- (done ...) handler in the same transaction that flips status.
  -- Forks: copy parent's latest ctx into the new state's turn-1 row
  -- so branched timelines keep their pre-fork history.
  ctx                          BLOB,

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

  -- Phase B canonical token shape per iteration. NULL columns when the
  -- provider response did not surface usage (e.g. LLM call failed
  -- before a response landed). `input_tokens` is TOTAL, the detail
  -- columns are subsets obeying the invariant:
  --   input_regular + input_cache_write + input_cache_read = input_tokens
  -- `output_reasoning_tokens` is a subset of `output_tokens`.
  -- Cost is provider-side estimated USD via the router's pricing
  -- table at write time — historical rows survive future price changes.
  input_tokens                    INTEGER CHECK (
                                    input_tokens IS NULL OR input_tokens >= 0
                                  ),
  input_regular_tokens            INTEGER CHECK (
                                    input_regular_tokens IS NULL OR input_regular_tokens >= 0
                                  ),
  input_cache_write_tokens        INTEGER CHECK (
                                    input_cache_write_tokens IS NULL OR input_cache_write_tokens >= 0
                                  ),
  input_cache_read_tokens         INTEGER CHECK (
                                    input_cache_read_tokens IS NULL OR input_cache_read_tokens >= 0
                                  ),
  output_tokens                   INTEGER CHECK (
                                    output_tokens IS NULL OR output_tokens >= 0
                                  ),
  output_reasoning_tokens         INTEGER CHECK (
                                    output_reasoning_tokens IS NULL OR output_reasoning_tokens >= 0
                                  ),
  cost_usd                        REAL    CHECK (
                                    cost_usd IS NULL OR cost_usd >= 0
                                  ),


  -- The verbatim fence body the model emitted this iter (the entire
  -- ```clojure ... ``` block as one string). Forensics + transcript replay.
  -- All per-form structure (scope, source, result, error, tag) lives in
  -- `forms` below — there are NO `result` / `error` columns on this row.
  -- The model emits N top-level forms per fence; the per-form envelope
  -- in `forms` is the canonical shape.
  code                            TEXT NOT NULL,

  -- Nippy-encoded vec of per-form envelopes captured during the iter:
  --   [{:scope "tN/iM/fK"
  --     :tag   :observation | :mutation
  --     :src   <source string of one top-level form>
  --     :result <any>                       -- present when the form returned
  --     :error  {:message :data?}}          -- present when the form threw
  --    ...]
  -- `introspect-form` decodes this vec; validator-fn evaluation reads
  -- `:result` from the matching entry. NULL or empty vec on iters that
  -- contained only `(done ...)`.
  forms                           BLOB,
  -- SCI sandbox eval wall time for this iteration's block. The LLM
  -- call time lives on llm_full_duration_ms; the turn total wall time
  -- lives on session_turn_state.duration_ms. Per-iteration wall time
  -- approximates eval_duration_ms + llm_full_duration_ms.
  eval_duration_ms                INTEGER CHECK (
                                    eval_duration_ms IS NULL OR eval_duration_ms >= 0
                                  ),

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
-- Definitions (defs / defns) are intra-turn SCI scratch only. No
-- definition_* sidecar tables: cross-turn references happen through
-- :session/facts and introspect-form / introspect-iter / introspect-turn
-- (DB reads against session_turn_iteration.forms). Restore re-evaluates
-- nothing.

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

  index_data                  TEXT,            -- JSON-encoded extension-owned query/index fields
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

-- Supports index_data JSON field filtering for extension aggregate queries.
-- Extensions pass {:index-data {:field value}} to extension-list-aggregates/extension-delete-aggregate!;
-- the clause layer generates json_extract WHERE conditions on these paths.
CREATE INDEX idx_extension_aggregate_index_data_kind
  ON extension_aggregate(extension_id, kind, json_extract(index_data, '$.kind'))
  WHERE index_data IS NOT NULL;

-- Supports graph-traversal-style index queries (Bridge edges by source/target).
-- Without these, "find all edges FROM x" or "find all edges TO y" scans all rows
-- of that kind.
CREATE INDEX idx_extension_aggregate_index_source
  ON extension_aggregate(extension_id, kind, json_extract(index_data, '$.source'))
  WHERE index_data IS NOT NULL;

CREATE INDEX idx_extension_aggregate_index_target
  ON extension_aggregate(extension_id, kind, json_extract(index_data, '$.target'))
  WHERE index_data IS NOT NULL;

-- Supports "all nodes/edges for a file" queries (Bridge re-indexing).
CREATE INDEX idx_extension_aggregate_index_path
  ON extension_aggregate(extension_id, kind, json_extract(index_data, '$.path'))
  WHERE index_data IS NOT NULL;

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

-- =============================================================================
-- FTS5 - full-text search over user requests + iteration code.
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
