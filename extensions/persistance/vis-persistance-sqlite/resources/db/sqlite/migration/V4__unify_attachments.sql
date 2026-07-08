-- =============================================================================
-- V4 - unify the two attachment rails into ONE `session_attachment` table.
--
-- V2 introduced `session_iteration_attachment` (OUTBOUND tool artifacts, owned
-- by an iteration); V1/V3 kept `session_turn_attachment` (INBOUND user images,
-- owned by the turn message). Two tables meant read-back had to know WHICH one
-- to hit — first via a sequential fallback, then via a `source:` handle prefix.
-- Both are complexity that a single table erases: knock on ONE table by id.
--
-- The insight (see the FK chain below): BOTH owners resolve to the same turn.
-- A tool artifact's iteration -> turn_state -> turn_soul is the very turn the
-- user image already hangs on. So every row can carry `session_turn_soul_id`
-- (ALWAYS set: the turn it belongs to) plus, ONLY for tool artifacts, the finer
-- `session_turn_iteration_id` + `tool_call_id`.
--
--   session_soul -> session_state -> session_turn_soul (TURN)
--                                 -> session_turn_state -> session_turn_iteration
--
-- `source` is DERIVED, never stored: a row with `session_turn_iteration_id`
-- set is a `tool` artifact; NULL means a `user` image. No enum column can drift
-- out of sync with the FK, because there is no enum column.
--
-- Introspection then collapses to plain indexed filters:
--   * all for an ITERATION      -> WHERE session_turn_iteration_id = ?
--   * all for a TURN (user+tool) -> WHERE session_turn_soul_id = ?   (tool rows
--                                    denormalize soul, so this ONE filter sees
--                                    both rails — the roll-up we care about most)
--   * split user vs tool         -> session_turn_iteration_id IS [NOT] NULL
--
-- Storage contract is unchanged from V2/V3: inline `bytes` now, `storage_uri`
-- (s3://…, file://…) reserved for later externalization, CHECK enforcing that
-- exactly one is set.
--
-- Both source tables are leaves (nothing FKs TO them), so dropping them after
-- the copy carries no dependents. Existing rows all have inline bytes, so they
-- satisfy the CHECK unchanged.
-- =============================================================================

CREATE TABLE session_attachment (
  id                        TEXT PRIMARY KEY NOT NULL,

  -- The TURN this artifact belongs to - ALWAYS set (user images AND tool
  -- artifacts). Tool rows denormalize it from their iteration's turn_state so a
  -- per-turn roll-up is one indexed filter instead of a soul-OR-subquery.
  session_turn_soul_id      TEXT NOT NULL
                            REFERENCES session_turn_soul(id) ON DELETE CASCADE,

  -- The finer owner for tool artifacts; NULL for user images. Its presence IS
  -- the `source`: set => tool, NULL => user.
  session_turn_iteration_id TEXT
                            REFERENCES session_turn_iteration(id) ON DELETE CASCADE,

  -- Which tool call produced it (tool artifacts only); NULL for user images and
  -- whole-iteration artifacts.
  tool_call_id              TEXT,

  position                  INTEGER NOT NULL CHECK (position >= 0),
  kind                      TEXT NOT NULL DEFAULT 'image',
  media_type                TEXT NOT NULL,
  filename                  TEXT,
  size_bytes                INTEGER NOT NULL CHECK (size_bytes >= 0),

  -- Payload: inline bytes now (bytes NOT NULL, storage_uri NULL) OR external
  -- reference later (bytes NULL, storage_uri 's3://…'). Exactly one is set.
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
  -- iteration, and SQLite treats NULLs as distinct in UNIQUE, so this leaves
  -- user images unconstrained - matching their V1/V3 behaviour.
  UNIQUE (session_turn_iteration_id, tool_call_id, position)
);

-- OUTBOUND tool artifacts: carry the iteration + call, and denormalize the turn
-- soul via iteration -> session_turn_state -> session_turn_soul_id.
INSERT INTO session_attachment
  (id, session_turn_soul_id, session_turn_iteration_id, tool_call_id,
   position, kind, media_type, filename, size_bytes, bytes, storage_uri, created_at)
SELECT
  ia.id, sts.session_turn_soul_id, ia.session_turn_iteration_id, ia.tool_call_id,
  ia.position, ia.kind, ia.media_type, ia.filename, ia.size_bytes,
  ia.bytes, ia.storage_uri, ia.created_at
FROM session_iteration_attachment ia
JOIN session_turn_iteration it ON it.id = ia.session_turn_iteration_id
JOIN session_turn_state sts    ON sts.id = it.session_turn_state_id;

-- INBOUND user images: turn soul only; no iteration, no call.
INSERT INTO session_attachment
  (id, session_turn_soul_id, session_turn_iteration_id, tool_call_id,
   position, kind, media_type, filename, size_bytes, bytes, storage_uri, created_at)
SELECT
  ta.id, ta.session_turn_soul_id, NULL, NULL,
  ta.position, ta.kind, ta.media_type, ta.filename, ta.size_bytes,
  ta.bytes, ta.storage_uri, ta.created_at
FROM session_turn_attachment ta;

DROP TABLE session_iteration_attachment;
DROP TABLE session_turn_attachment;

-- Per-turn roll-up (user + tool together): the introspection hot path.
CREATE INDEX idx_attachment_soul
  ON session_attachment(session_turn_soul_id, position);

-- Per-iteration roll-up (tool artifacts), ordered by (call, position).
CREATE INDEX idx_attachment_iteration
  ON session_attachment(session_turn_iteration_id, tool_call_id, position);
