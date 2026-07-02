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
--                         └─ code / tool_calls BLOB / duration_ms columns
--                              The executed TOOL-CALL records (one per call) live
--                              on the iteration row. Sandbox state is NOT persisted:
--                              one persistent interpreter holds globals in-process,
--                              never as cross-turn DB state.
--                              Cross-turn memory rides on session_turn_state.ctx
--                              — the bare per-turn context snapshot (identity,
--                              workspace, env, routing, resources, symbols) as
--                              one Nippy blob on this row. The tool_calls BLOB is
--                              the forensic source-of-truth for executed code.
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
--          each session_turn_iteration records executed code in `code`
--          and the executed TOOL-CALL records in `tool_calls` (BLOB), plus
--          duration_ms. Sandbox state is NOT persisted: one persistent
--          interpreter holds the model's globals in-process across turns.
--          Prior tool-call outcomes live in the tool_calls BLOB (DB reads
--          against session_turn_iteration.tool_calls).
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
  -- A sub_loop CHILD session is a WHOLE separate soul whose work belongs to a
  -- parent turn: this points at the parent's `session_state.id` (CROSS-soul).
  -- Distinct from `session_state.parent_state_id`, which is WITHIN-soul fork/
  -- retry versioning — so forks are untouched. NULL = a normal top-level
  -- session (the only kind `db-list-sessions` shows); children hang off their
  -- parent for a queryable sub-tree and cascade-delete with it.
  parent_state_id TEXT REFERENCES session_state(id) ON DELETE CASCADE,
  -- Per-session model preference: the PROVIDER + MODEL this session routes
  -- through, set via the web picker or the TUI (Ctrl+T). Mirrors how a turn
  -- records its route (session_state.llm_root_provider + llm_root_model) so
  -- the preference is unambiguous (a model name can exist under >1 provider).
  -- Channel-neutral and read by the engine at turn start
  -- (prepare-turn-context) so every channel routes the same way. Both NULL =
  -- router default.
  llm_pref_provider TEXT,
  llm_pref_model    TEXT,
  created_at   INTEGER NOT NULL
);

CREATE INDEX idx_session_soul_parent ON session_soul(parent_state_id)
  WHERE parent_state_id IS NOT NULL;

CREATE INDEX idx_session_soul_channel_created
  ON session_soul(channel, created_at DESC);

CREATE UNIQUE INDEX idx_session_soul_channel_external
  ON session_soul(channel, external_id)
  WHERE external_id IS NOT NULL;

-- =============================================================================
-- Workspace — a rift copy-on-write clone of cwd (a "draft").
--
-- One row = one draft clone on disk = one session-binding (1:1 invariant).
-- The user's real cwd is *trunk* (`repo_root`) and is never mutated; `root`
-- is the rift clone path. `fork_ms` is the clone timestamp — the baseline
-- for the mtime since-fork diff that `apply` lands back into cwd. Drafts
-- carry no git: no branch/commit/merge, only active -> discarded.
-- =============================================================================
CREATE TABLE workspace (
  id                   TEXT PRIMARY KEY NOT NULL,
  repo_id              TEXT NOT NULL,
  repo_root            TEXT NOT NULL,     -- trunk: the real cwd, where apply lands
  root                 TEXT NOT NULL,     -- the rift clone path

  state                TEXT NOT NULL DEFAULT 'active'
                       CHECK (state IN ('active', 'discarded')),

  fork_ms              INTEGER,           -- clone timestamp; mtime since-fork baseline

  -- Human-friendly label overriding the default (session.title) in the
  -- TUI strip. NULL falls back to the heuristic. Set via `workspace/set-label!`.
  label                TEXT,
  -- Monotonic timestamp of the last `workspace/focus!` (TUI strip ordering /
  -- tab restore). NULL falls back to `created_at`.
  last_focused_at_ms   INTEGER,

  -- Additional filesystem roots (opencode-style "add folders/repos"): extra
  -- directories the session may operate on IN ADDITION to its primary `root`.
  -- JSON array of canonical absolute path strings, e.g.
  -- '["/Users/me/lib-a","/Users/me/lib-b"]'. NULL / absent = no extra roots
  -- (the single-root default). Mutated via `workspace/add-filesystem-root!` /
  -- `remove-filesystem-root!` (the `/fs add|remove` slash commands).
  filesystem_roots     TEXT,

  -- Workspace kind: 'trunk' = the user's real cwd (never cloned), 'draft' =
  -- an isolated backend working copy that `apply!` lands back into trunk.
  -- ('checkpoint' is reserved in the constraint for forward-compatibility but
  -- is not minted by core.)
  workspace_kind       TEXT NOT NULL DEFAULT 'trunk'
                       CHECK (workspace_kind IN ('trunk', 'draft', 'checkpoint')),

  -- Which registered workspace backend supplies this workspace's isolation.
  -- 'live' = no backend (trunk, edits in place); e.g. 'rift' for the CoW
  -- backend extension. Persisted so discard routes to the right backend.
  workspace_backend    TEXT NOT NULL DEFAULT 'live',

  -- The workspace this one forked from (draft → its trunk parent). Lineage
  -- only; NULL for trunks. RESTRICT so a parent can't vanish under a child.
  parent_workspace_id  TEXT REFERENCES workspace(id) ON DELETE RESTRICT,

  -- Cumulative since-fork baseline. For ordinary drafts this equals fork_ms;
  -- the column exists so apply! can read one baseline uniformly.
  apply_fork_ms        INTEGER,

  created_at           INTEGER NOT NULL,
  discarded_at         INTEGER
);

CREATE INDEX idx_workspace_repo_state
  ON workspace(repo_id, state);

CREATE INDEX idx_workspace_parent
  ON workspace(parent_workspace_id);

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

  UNIQUE (session_soul_id, version)
);

-- Each workspace pins to exactly one session_state (1:1 invariant).
CREATE UNIQUE INDEX uq_session_state_workspace
  ON session_state(workspace_id);

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
  answer_markdown              TEXT,        -- Raw Markdown source of the model's plain-prose answer.
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
  -- Model markdown PROSE returned ALONGSIDE a tool call (its commentary while
  -- acting, native tool calling) — persisted so a resumed turn / web history
  -- still shows what the model SAID, not only what it ran. Null = no prose.
  llm_assistant_prose             TEXT,
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


  -- The code this iteration executed, as one string: the synthesized native
  -- tool calls (cat/rg/patch/…) and/or the python_execution program. Forensics
  -- + transcript replay. Per-call outcome (result / stdout / error) lives in
  -- `tool_calls` below — there are NO `result` / `error` columns on this row.
  code                            TEXT NOT NULL,

  -- Nippy-encoded vec of the TOOL-CALL records executed this iteration. An
  -- iteration IS a list of tool calls (one of which may be python_execution);
  -- one record per call:
  --   [{:scope             "tN/iM"       -- iteration handle (no per-form index)
  --     :tag               :observation | :mutation
  --     :src               <the call's source code>
  --     :svar/tool-call-id <id>             -- the tool_use this record answers
  --     :vis/tool-name     <name>           -- cat / rg / python_execution / …
  --     :result <any>                       -- a NATIVE tool's returned value
  --     :stdout <string>                    -- python_execution's PRINTED text
  --     :error  {:message :data?}}          -- present when the call threw
  --    ...]
  -- :result and :stdout are EXCLUSIVE (the engine emits ONE per call): a native
  -- tool carries :result, a python_execution call carries :stdout; a failure
  -- carries :error (+ any partial :stdout). A DB read decodes this vec to
  -- recover a prior call's outcome. NULL or empty vec on empty iters.
  tool_calls                      BLOB,
  -- sandbox eval wall time for this iteration's block. The LLM
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
-- No sandbox-var sidecar tables. The model's Python globals live in ONE
-- persistent in-process interpreter per session; they are not persisted. Prior
-- tool-call outcomes live in session_turn_iteration.tool_calls (DB reads).

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

