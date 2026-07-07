-- =============================================================================
-- V2 - iteration attachments (SQLite)
--
-- Adds `session_iteration_attachment`: binary artifacts (images now, any
-- MIME later) PRODUCED BY a tool call inside one LLM round-trip. The
-- canonical example is a `matplotlib` figure emitted by `plt.show()` /
-- `plt.savefig()` from a `python_execution` call — V1 only kept a text
-- fence pointing at a temp file, so the bytes vanished on restart. Here the
-- bytes are OWNED by the DB and survive restarts / session replay.
--
-- Grain: (iteration, tool_call_id, position).
--   * session_turn_iteration_id  = the LLM round-trip that ran the call
--   * tool_call_id               = the :svar/tool-call-id that produced it
--                                  (nullable: a whole-iteration artifact not
--                                   tied to a single call keeps it NULL)
--   * position                   = 0-based order within (iteration, call),
--                                  since ONE call may emit MANY images —
--                                  even same-named ones. `id` is the join
--                                  key; position is the public ordinal.
--
-- Contrast with V1's `session_turn_attachment` (kept intact): that table is
-- keyed by (turn_soul, position) and holds INBOUND images the USER attached
-- to a turn (web/API upload or terminal drop). This table is OUTBOUND:
-- artifacts the assistant's tools GENERATED, keyed one level deeper at the
-- iteration + call.
--
-- Storage: `bytes` is inline BLOB for now. `storage_uri` reserves the future
-- externalization path (s3://…, file://…) — exactly one of the two is
-- populated (CHECK below), so a reader always knows where the payload lives.
-- The `id` is MINTED at emit time (a fresh UUID), NEVER derived from the
-- filename, so collisions across calls are impossible.
-- =============================================================================

CREATE TABLE session_iteration_attachment (
  id                        TEXT PRIMARY KEY NOT NULL,

  session_turn_iteration_id TEXT NOT NULL
                            REFERENCES session_turn_iteration(id) ON DELETE CASCADE,

  -- The :svar/tool-call-id that produced this artifact. NULL for an
  -- iteration-level artifact not attributable to a single call.
  tool_call_id              TEXT,

  -- 0-based order within (iteration, tool_call_id). One call may emit
  -- several images (possibly same filename), so ordinal disambiguates.
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

  UNIQUE (session_turn_iteration_id, tool_call_id, position),

  -- Exactly one payload location: never both, never neither.
  CHECK (
    (bytes IS NOT NULL AND storage_uri IS NULL)
    OR
    (bytes IS NULL AND storage_uri IS NOT NULL)
  )
);

CREATE INDEX idx_iteration_attachment_iteration
  ON session_iteration_attachment(session_turn_iteration_id, position);

CREATE INDEX idx_iteration_attachment_call
  ON session_iteration_attachment(session_turn_iteration_id, tool_call_id, position);
