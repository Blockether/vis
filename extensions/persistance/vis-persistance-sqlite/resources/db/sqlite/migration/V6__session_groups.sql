-- =============================================================================
-- V6 - session ownership (`owner`) + session groups (folders).
--
-- Two related identity axes that the post-gateway schema was missing, modelled
-- the way desktop/editor tools (Cursor, Zed, ChatGPT "Projects", …) already do:
--
--   owner          - the "who" axis. An OWNERSHIP TAG, not auth yet: a session
--                    belongs to one owner. A single backfilled `local` owner
--                    keeps today's single-user behaviour intact and reserves
--                    the seam for real accounts/tokens in a later migration
--                    (which touches the HTTP layer, not just this table).
--
--   session_group  - the "what belongs together" axis. A named, persisted,
--                    place-INDEPENDENT folder a session can live in. Membership
--                    is EXCLUSIVE (a soul is in 0..1 groups) via a nullable
--                    pointer on the soul - the folder/tab mental model, and it
--                    keeps `db-list-sessions` a single LEFT JOIN (no N-per-row
--                    workspace round-trips to reconstruct a group key).
--
-- Deleting a group SCATTERS its sessions back to ungrouped (ON DELETE SET NULL)
-- - it NEVER cascade-deletes real conversations (contrast parent_state_id,
-- whose cascade is deliberate for sub_loop children).
--
-- No auto-seed: groups ship empty and are built by the client. (A later,
-- optional backfill could seed one group per distinct workspace repo_root to
-- reconstruct "grouped-by-place" as real rows - kept out of here to stay clean.)
-- =============================================================================

-- --- owner (ownership tag) ---------------------------------------------------
CREATE TABLE owner (
  id           TEXT PRIMARY KEY NOT NULL,
  name         TEXT NOT NULL CHECK (trim(name) <> ''),
  created_at   INTEGER NOT NULL
);

-- The single implicit owner every pre-existing session is retroactively stamped
-- with. `created_at = 0` marks it as the migration-seeded default.
INSERT INTO owner (id, name, created_at) VALUES ('local', 'local', 0);

ALTER TABLE session_soul ADD COLUMN owner_id TEXT REFERENCES owner(id);

UPDATE session_soul SET owner_id = 'local' WHERE owner_id IS NULL;

CREATE INDEX idx_session_soul_owner ON session_soul(owner_id);

-- --- session_group (folder) --------------------------------------------------
CREATE TABLE session_group (
  id           TEXT PRIMARY KEY NOT NULL,
  owner_id     TEXT NOT NULL REFERENCES owner(id),
  -- NULL = a cross-channel group (matches db-list-sessions' :all view);
  -- non-NULL scopes the group to one channel ('tui'/'telegram'/…).
  channel      TEXT,
  name         TEXT NOT NULL CHECK (trim(name) <> ''),
  color        TEXT,                        -- optional TUI/web accent
  position     INTEGER NOT NULL DEFAULT 0,  -- manual ordering within a channel
  created_at   INTEGER NOT NULL,
  archived_at  INTEGER                      -- soft-hide without deleting
);

CREATE INDEX idx_session_group_owner_channel
  ON session_group(owner_id, channel, position, created_at);

-- --- membership (exclusive pointer on the soul) ------------------------------
ALTER TABLE session_soul ADD COLUMN group_id TEXT
  REFERENCES session_group(id) ON DELETE SET NULL;

CREATE INDEX idx_session_soul_group ON session_soul(group_id)
  WHERE group_id IS NOT NULL;
