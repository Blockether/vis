-- =============================================================================
-- V1__schema.sql — THE single unified vis SQLite schema (snake_string epoch).
-- One consolidated baseline: clean CREATEs only, no historical ALTER chain.
-- All persisted payloads are canonical snake_case STRING-keyed JSON/Nippy;
-- pre-epoch (keyword-era) databases are not migrated — start a fresh DB.
-- SQLite resolves the circular session_soul <-> session_state refs at row
-- time, so the forward reference is fine.
-- =============================================================================

PRAGMA foreign_keys = ON;

-- =============================================================================
-- owner — ownership tag (the "who" axis). Single-user seam: one `local` row.
--
-- =============================================================================
CREATE TABLE owner (
  id           TEXT PRIMARY KEY NOT NULL,
  name         TEXT NOT NULL CHECK (trim(name) <> ''),
  created_at   INTEGER NOT NULL
);

-- The single implicit owner every session belongs to until real accounts land.
-- created_at = 0 marks it as the schema-seeded default.
INSERT INTO owner (id, name, created_at) VALUES ('local', 'local', 0);

-- =============================================================================
-- project — the CROSS-CHANNEL "what belongs together" axis. A project OWNS its
-- member sessions (the TUI calls them TABS), ordered by
-- session_soul.project_position. `workspace_root` binds a project to a
-- directory so "workspace = project = tab set". No channel column: every
-- channel sees the same projects.
-- =============================================================================
CREATE TABLE project (
  id             TEXT PRIMARY KEY NOT NULL,
  owner_id       TEXT NOT NULL REFERENCES owner(id),
  name           TEXT NOT NULL CHECK (trim(name) <> ''),
  color          TEXT,                        -- optional TUI/web accent
  position       INTEGER NOT NULL DEFAULT 0,  -- manual ordering
  created_at     INTEGER NOT NULL,
  archived_at    INTEGER,                     -- soft-hide without deleting

  -- Canonical absolute path of the workspace root this project is bound to.
  -- NULL = a LOOSE project (hand-created via the move-to-project picker), not
  -- tied to a directory. NOTE: this is `workspace.repo_root` (the real cwd),
  -- NOT `workspace.root` (the rift clone path) — named `workspace_root` so the
  -- two `root`s one join apart never collide. Callers store a
  -- java.io.File/getCanonicalPath (case-normalized so a dir never fragments
  -- into two projects). The CHECK forbids a blank binding posing as "bound".
  workspace_root TEXT CHECK (workspace_root IS NULL OR trim(workspace_root) <> '')
);

CREATE INDEX idx_project_owner
  ON project(owner_id, position, created_at);

-- One project per (owner, workspace_root): partial-unique makes get-or-create
-- race-safe (a losing concurrent insert fails and the caller re-reads).
CREATE UNIQUE INDEX idx_project_owner_workspace_root
  ON project(owner_id, workspace_root)
  WHERE workspace_root IS NOT NULL;

-- =============================================================================
-- session_soul — pure identity.
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
  parent_state_id   TEXT REFERENCES session_state(id) ON DELETE CASCADE,
  -- Per-session model preference: the PROVIDER + MODEL this session routes
  -- through, set via the web picker or the TUI (Ctrl+T). Both NULL = router
  -- default.
  llm_pref_provider TEXT,
  llm_pref_model    TEXT,
  created_at        INTEGER NOT NULL,

  -- Adoption stamp. NULL = unclaimed warm-pool scaffolding (hidden from
  -- db-list-sessions); non-NULL = a real conversation (user-created or received
  -- its first turn). Direct resume by id stays unfiltered.
  claimed_at        INTEGER,

  -- Ownership tag.
  owner_id          TEXT REFERENCES owner(id),

  -- Exclusive project membership pointer. A soul
  -- is in 0..1 projects; deleting the project SCATTERS members back to loose
  -- (SET NULL), never cascade-deletes conversations.
  project_id        TEXT REFERENCES project(id) ON DELETE SET NULL,

  -- Manual order of this soul within its project (the movable TAB order, V7).
  -- Held gap-free & unique per project by idx_project_position.
  project_position  INTEGER NOT NULL DEFAULT 0
);

CREATE INDEX idx_session_soul_parent ON session_soul(parent_state_id)
  WHERE parent_state_id IS NOT NULL;

CREATE INDEX idx_session_soul_channel_created
  ON session_soul(channel, created_at DESC);

CREATE UNIQUE INDEX idx_session_soul_channel_external
  ON session_soul(channel, external_id)
  WHERE external_id IS NOT NULL;

-- Claimed-only listing hot path.
CREATE INDEX idx_session_soul_claimed
  ON session_soul(channel, claimed_at, created_at DESC)
  WHERE claimed_at IS NOT NULL;

CREATE INDEX idx_session_soul_owner ON session_soul(owner_id);

-- Members of a project in tab order.
CREATE INDEX idx_session_soul_project ON session_soul(project_id, project_position)
  WHERE project_id IS NOT NULL;

-- One slot per (project, position), forever — position integrity is structural,
-- not application convention. The reorder fn parks members in negative
-- temp slots first so a row-by-row renumber never transiently collides.
CREATE UNIQUE INDEX idx_project_position
  ON session_soul(project_id, project_position)
  WHERE project_id IS NOT NULL;

-- =============================================================================
-- workspace — a rift copy-on-write clone of cwd (a "draft"). One row = one
-- draft clone on disk = one session-binding (1:1).
-- =============================================================================
CREATE TABLE workspace (
  id                   TEXT PRIMARY KEY NOT NULL,
  repo_id              TEXT NOT NULL,
  repo_root            TEXT NOT NULL,     -- trunk: the real cwd, where apply lands
  root                 TEXT NOT NULL,     -- the rift clone path

  state                TEXT NOT NULL DEFAULT 'active'
                       CHECK (state IN ('active', 'discarded')),

  fork_ms              INTEGER,           -- clone timestamp; mtime since-fork baseline

  label                TEXT,              -- human label overriding session.title in the TUI strip
  last_focused_at_ms   INTEGER,           -- last workspace/focus! (NULL -> created_at)

  -- Extra filesystem roots the session may operate on (JSON array of canonical
  -- absolute paths). NULL/absent = single-root default.
  filesystem_roots     TEXT,

  workspace_kind       TEXT NOT NULL DEFAULT 'trunk'
                       CHECK (workspace_kind IN ('trunk', 'draft', 'checkpoint')),

  workspace_backend    TEXT NOT NULL DEFAULT 'live',

  -- Lineage: draft -> its trunk parent. RESTRICT so a parent can't vanish under
  -- a child. NULL for trunks.
  parent_workspace_id  TEXT REFERENCES workspace(id) ON DELETE RESTRICT,

  apply_fork_ms        INTEGER,           -- cumulative since-fork baseline for apply!

  created_at           INTEGER NOT NULL,
  discarded_at         INTEGER
);

CREATE INDEX idx_workspace_repo_state
  ON workspace(repo_id, state);

CREATE INDEX idx_workspace_parent
  ON workspace(parent_workspace_id);

-- =============================================================================
-- repo_focus — per-repo last-active workspace pointer.
-- =============================================================================
CREATE TABLE repo_focus (
  repo_id        TEXT PRIMARY KEY NOT NULL,
  workspace_id   TEXT NOT NULL
                 REFERENCES workspace(id) ON DELETE CASCADE,
  updated_at_ms  INTEGER NOT NULL
);

-- =============================================================================
-- session_state — forkable mutable snapshot. Pinned 1:1 to a workspace.
-- =============================================================================
CREATE TABLE session_state (
  id                    TEXT PRIMARY KEY NOT NULL,
  session_soul_id       TEXT NOT NULL
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
-- session_turn_soul — immutable identity of a user request, branch-local.
-- =============================================================================
CREATE TABLE session_turn_soul (
  id                     TEXT PRIMARY KEY NOT NULL,
  session_state_id       TEXT NOT NULL
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
-- session_turn_state — one run of session_turn_soul. Retry = new version.
-- =============================================================================
CREATE TABLE session_turn_state (
  id                           TEXT PRIMARY KEY NOT NULL,
  session_turn_soul_id                TEXT NOT NULL
                               REFERENCES session_turn_soul(id) ON DELETE CASCADE,
  forked_from_session_turn_state_id   TEXT
                               REFERENCES session_turn_state(id) ON DELETE SET NULL,
  version                      INTEGER NOT NULL CHECK (version >= 0),
  llm_root_provider            TEXT,
  llm_root_model               TEXT,
  status                       TEXT NOT NULL
                               CHECK (status IN ('running', 'done', 'error', 'interrupted')),
  iteration_count              INTEGER NOT NULL DEFAULT 0 CHECK (iteration_count >= 0),
  duration_ms                  INTEGER NOT NULL DEFAULT 0 CHECK (duration_ms >= 0),
  -- Canonical token shape. input_tokens is ALWAYS TOTAL; the detail columns are
  -- SUBSETS: input_regular + input_cache_write + input_cache_read = input_tokens.
  input_tokens                 INTEGER NOT NULL DEFAULT 0 CHECK (input_tokens >= 0),
  input_regular_tokens         INTEGER NOT NULL DEFAULT 0 CHECK (input_regular_tokens >= 0),
  input_cache_write_tokens     INTEGER NOT NULL DEFAULT 0 CHECK (input_cache_write_tokens >= 0),
  input_cache_read_tokens      INTEGER NOT NULL DEFAULT 0 CHECK (input_cache_read_tokens >= 0),
  output_tokens                INTEGER NOT NULL DEFAULT 0 CHECK (output_tokens >= 0),
  output_reasoning_tokens      INTEGER NOT NULL DEFAULT 0 CHECK (output_reasoning_tokens >= 0),
  total_cost_usd               REAL NOT NULL DEFAULT 0 CHECK (total_cost_usd >= 0),
  content_json                 TEXT NOT NULL DEFAULT '[]' CHECK (json_valid(content_json)),
  -- Per-turn final outcome, derived at turn end.
  prior_outcome                TEXT
                               CHECK (prior_outcome IS NULL OR
                                      prior_outcome IN ('complete', 'cancelled', 'error')),

  -- Nippy-encoded CTX snapshot as of the END of this turn version. NULL while
  -- running; written by the (done ...) handler in the status-flip transaction.
  ctx                          BLOB,

  -- Nippy-encoded STRUCTURED terminal error for a status='error' turn. NULL for
  -- a successful turn.
  error                        BLOB,

  created_at                   INTEGER NOT NULL,

  UNIQUE (session_turn_soul_id, version)
);

CREATE INDEX idx_session_turn_state_soul
  ON session_turn_state(session_turn_soul_id, version);

CREATE INDEX idx_session_turn_state_forked_from
  ON session_turn_state(forked_from_session_turn_state_id);

-- =============================================================================
-- session_turn_iteration — one LLM round-trip within a session_turn_state.
-- =============================================================================
CREATE TABLE session_turn_iteration (
  id                              TEXT PRIMARY KEY NOT NULL,
  session_turn_state_id                  TEXT NOT NULL
                                  REFERENCES session_turn_state(id) ON DELETE CASCADE,
  position                        INTEGER NOT NULL CHECK (position >= 1),

  status                          TEXT NOT NULL
                                  CHECK (status IN ('running', 'done', 'error', 'interrupted')),

  llm_selected_provider           TEXT,
  llm_selected_model              TEXT,
  llm_actual_provider             TEXT,
  llm_actual_model                TEXT,
  is_llm_fallback                    INTEGER NOT NULL DEFAULT 0
                                  CHECK (is_llm_fallback IN (0, 1)),

  llm_full_duration_ms            INTEGER CHECK (
                                    llm_full_duration_ms IS NULL OR llm_full_duration_ms >= 0
                                  ),
  llm_thinking                    TEXT,
  -- Model markdown PROSE returned ALONGSIDE a tool call. NULL = no prose.
  llm_assistant_prose             TEXT,
  is_llm_returned_empty_code         INTEGER NOT NULL DEFAULT 0
                                  CHECK (is_llm_returned_empty_code IN (0, 1)),

  -- svar canonical assistant message (JSON) — preserved-thinking replay survives
  -- a restart.
  llm_assistant_message           TEXT,

  -- Canonical per-iteration token shape (NULL when the response surfaced no
  -- usage). input_tokens is TOTAL; detail columns are subsets; cost is
  -- provider-estimated USD at write time.
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

  -- The code this iteration executed (synthesized native tool calls and/or the
  -- python_execution program). Per-call outcome lives in tool_calls below.
  code                            TEXT NOT NULL,

  -- Nippy-encoded vec of the TOOL-CALL records executed this iteration
  -- (:result for native tools, :stdout for python_execution, :error on throw).
  -- NULL/empty on empty iters.
  tool_calls                      BLOB,
  -- Sandbox eval wall time for this iteration's block.
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
-- llm_routing_event — per-iteration provider/model routing trace.
-- =============================================================================
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

-- =============================================================================
-- extension_aggregate — extension-owned durable sidecar state.
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

  -- Singleton dedupe key for upsert (SQLite treats NULLs in UNIQUE as distinct,
  -- which would otherwise defeat ON CONFLICT against the FK columns directly).
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

CREATE INDEX idx_extension_aggregate_index_data_kind
  ON extension_aggregate(extension_id, kind, json_extract(index_data, '$.kind'))
  WHERE index_data IS NOT NULL;

CREATE INDEX idx_extension_aggregate_index_source
  ON extension_aggregate(extension_id, kind, json_extract(index_data, '$.source'))
  WHERE index_data IS NOT NULL;

CREATE INDEX idx_extension_aggregate_index_target
  ON extension_aggregate(extension_id, kind, json_extract(index_data, '$.target'))
  WHERE index_data IS NOT NULL;

CREATE INDEX idx_extension_aggregate_index_path
  ON extension_aggregate(extension_id, kind, json_extract(index_data, '$.path'))
  WHERE index_data IS NOT NULL;

-- =============================================================================
-- log — structured logs.
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
-- session_attachment — unified attachment table.
--
-- Every row carries session_turn_soul_id (ALWAYS set: the turn it belongs to).
-- A row with session_turn_iteration_id set is a TOOL artifact (OUTBOUND); NULL
-- means a USER image (INBOUND). `source` is DERIVED from that, never stored.
-- Storage: inline `bytes` now, `storage_uri` reserved for externalization;
-- exactly one is set.
-- =============================================================================
CREATE TABLE session_attachment (
  id                        TEXT PRIMARY KEY NOT NULL,

  session_turn_soul_id      TEXT NOT NULL
                            REFERENCES session_turn_soul(id) ON DELETE CASCADE,

  session_turn_iteration_id TEXT
                            REFERENCES session_turn_iteration(id) ON DELETE CASCADE,

  tool_call_id              TEXT,

  position                  INTEGER NOT NULL CHECK (position >= 0),
  kind                      TEXT NOT NULL DEFAULT 'image',
  media_type                TEXT NOT NULL,
  filename                  TEXT,
  size_bytes                INTEGER NOT NULL CHECK (size_bytes >= 0),

  bytes                     BLOB,
  storage_uri               TEXT,

  created_at                INTEGER NOT NULL,

  -- Exactly one payload location: never both, never neither.
  CHECK (
    (bytes IS NOT NULL AND storage_uri IS NULL)
    OR
    (bytes IS NULL AND storage_uri IS NOT NULL)
  ),

  -- Tool grain (iteration, tool_call_id, position). User rows have a NULL
  -- iteration and SQLite treats NULLs as distinct in UNIQUE, so user images
  -- stay unconstrained.
  UNIQUE (session_turn_iteration_id, tool_call_id, position)
);

-- Per-turn roll-up (user + tool together): the introspection hot path.
CREATE INDEX idx_attachment_soul
  ON session_attachment(session_turn_soul_id, position);

-- Per-iteration roll-up (tool artifacts), ordered by (call, position).
CREATE INDEX idx_attachment_iteration
  ON session_attachment(session_turn_iteration_id, tool_call_id, position);
