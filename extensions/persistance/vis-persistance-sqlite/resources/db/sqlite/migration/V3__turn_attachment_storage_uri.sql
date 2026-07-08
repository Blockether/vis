-- =============================================================================
-- V3 - bring `session_turn_attachment` (V1, INBOUND user images) up to the
-- same payload contract as `session_iteration_attachment` (V2, OUTBOUND tool
-- artifacts), so both attachment rails tell ONE consistent storage story.
--
-- V1 shipped `session_turn_attachment` with `bytes BLOB NOT NULL` and NO
-- `storage_uri`: inline-only, never externalizable. V2's iteration rail was
-- born S3-ready — `bytes` nullable, a `storage_uri` reserving the future
-- externalization path (s3://…, file://…), and a CHECK enforcing that exactly
-- one of the two is populated. This migration retrofits that same shape onto
-- the user-image rail:
--   * add `storage_uri TEXT`          (NULL = payload is inline)
--   * loosen `bytes` to nullable      (NULL = payload lives at storage_uri)
--   * add exactly-one(bytes, storage_uri) CHECK
--
-- SQLite can't ALTER a column's NOT NULL away nor ADD a table CHECK, so this
-- is the canonical table-rebuild: create the new shape, copy rows, drop old,
-- rename. `session_turn_attachment` is a leaf (nothing FKs TO it — it only
-- REFERENCES session_turn_soul), so the rebuild carries no dependents. Every
-- existing row has inline bytes, so it satisfies the new CHECK unchanged.
-- =============================================================================

CREATE TABLE session_turn_attachment_new (
  id                     TEXT PRIMARY KEY NOT NULL,
  session_turn_soul_id   TEXT NOT NULL
                         REFERENCES session_turn_soul(id) ON DELETE CASCADE,
  position               INTEGER NOT NULL CHECK (position >= 0),
  kind                   TEXT NOT NULL DEFAULT 'image',
  media_type             TEXT NOT NULL,
  filename               TEXT,
  size_bytes             INTEGER NOT NULL CHECK (size_bytes >= 0),

  -- Payload: inline bytes now (bytes NOT NULL, storage_uri NULL) OR external
  -- reference later (bytes NULL, storage_uri 's3://…'). Exactly one is set.
  bytes                  BLOB,
  storage_uri            TEXT,

  created_at             INTEGER NOT NULL,

  -- Exactly one payload location: never both, never neither.
  CHECK (
    (bytes IS NOT NULL AND storage_uri IS NULL)
    OR
    (bytes IS NULL AND storage_uri IS NOT NULL)
  )
);

INSERT INTO session_turn_attachment_new
  (id, session_turn_soul_id, position, kind, media_type, filename,
   size_bytes, bytes, storage_uri, created_at)
SELECT
  id, session_turn_soul_id, position, kind, media_type, filename,
  size_bytes, bytes, NULL, created_at
FROM session_turn_attachment;

DROP TABLE session_turn_attachment;

ALTER TABLE session_turn_attachment_new RENAME TO session_turn_attachment;

CREATE INDEX idx_turn_attachment_soul
  ON session_turn_attachment(session_turn_soul_id, position);
