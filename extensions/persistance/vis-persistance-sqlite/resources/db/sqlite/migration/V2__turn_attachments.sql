-- =============================================================================
-- Turn attachments - durable image bytes for a user request, branch-local.
--
-- Images referenced by an on-disk path (terminal drop) are re-derivable from
-- the stored `user_request` text and never land here. INLINE uploads (web/API
-- base64 payloads) have no durable disk path, so their validated bytes persist
-- here, keyed to the turn soul, so resume + history re-render survive a restart.
-- Kept OFF the ctx / event-log path (bytes are big) - same ethos as the
-- gateway's bounded-pr "big values stay off the log".
-- =============================================================================
CREATE TABLE session_turn_attachment (
  id                     TEXT PRIMARY KEY NOT NULL,
  session_turn_soul_id   TEXT NOT NULL
                         REFERENCES session_turn_soul(id) ON DELETE CASCADE,
  position               INTEGER NOT NULL CHECK (position >= 0),
  kind                   TEXT NOT NULL DEFAULT 'image',
  media_type             TEXT NOT NULL,
  filename               TEXT,
  size_bytes             INTEGER NOT NULL CHECK (size_bytes >= 0),
  bytes                  BLOB NOT NULL,
  created_at             INTEGER NOT NULL
);

CREATE INDEX idx_turn_attachment_soul
  ON session_turn_attachment(session_turn_soul_id, position);
