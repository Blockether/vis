-- Rooms credentials and membership are separate from sealed Push grants.
PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS room_machines (
  machine_id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  credential_hash TEXT NOT NULL UNIQUE,
  can_create_rooms INTEGER NOT NULL DEFAULT 0 CHECK (can_create_rooms IN (0, 1)),
  created_at INTEGER NOT NULL
);

CREATE TABLE IF NOT EXISTS council_rooms (
  room_id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  owner_machine_id TEXT NOT NULL REFERENCES room_machines(machine_id),
  created_at INTEGER NOT NULL,
  deleted_at INTEGER
);

CREATE TABLE IF NOT EXISTS room_memberships (
  room_id TEXT NOT NULL REFERENCES council_rooms(room_id),
  machine_id TEXT NOT NULL REFERENCES room_machines(machine_id),
  role TEXT NOT NULL CHECK (role IN ('owner', 'member')),
  joined_at INTEGER NOT NULL,
  revoked_at INTEGER,
  PRIMARY KEY (room_id, machine_id)
);

CREATE TABLE IF NOT EXISTS room_invites (
  invite_id TEXT PRIMARY KEY,
  room_id TEXT NOT NULL REFERENCES council_rooms(room_id),
  token_hash TEXT NOT NULL UNIQUE,
  expires_at INTEGER NOT NULL,
  max_uses INTEGER NOT NULL,
  uses INTEGER NOT NULL DEFAULT 0,
  revoked_at INTEGER
);

CREATE TABLE IF NOT EXISTS room_redemptions (
  invite_id TEXT NOT NULL REFERENCES room_invites(invite_id),
  request_id TEXT NOT NULL,
  machine_id TEXT NOT NULL REFERENCES room_machines(machine_id),
  credential_hash TEXT NOT NULL,
  created_at INTEGER NOT NULL,
  PRIMARY KEY (invite_id, request_id)
);

-- The insert, capacity check, use count and membership form one transaction.
CREATE TRIGGER IF NOT EXISTS room_redeem_check BEFORE INSERT ON room_redemptions
WHEN NOT EXISTS (
  SELECT 1 FROM room_redemptions WHERE invite_id = NEW.invite_id AND request_id = NEW.request_id
)
BEGIN
  SELECT RAISE(ABORT, 'invite_unavailable') WHERE NOT EXISTS (
    SELECT 1 FROM room_invites i JOIN council_rooms r ON r.room_id = i.room_id
    JOIN room_machines m ON m.machine_id = NEW.machine_id
    WHERE i.invite_id = NEW.invite_id AND i.revoked_at IS NULL AND r.deleted_at IS NULL
      AND i.expires_at > NEW.created_at AND i.uses < i.max_uses
      AND m.credential_hash = NEW.credential_hash
  );
END;

CREATE TRIGGER IF NOT EXISTS room_redeem_apply AFTER INSERT ON room_redemptions
BEGIN
  UPDATE room_invites SET uses = uses + 1 WHERE invite_id = NEW.invite_id;
  INSERT INTO room_memberships (room_id, machine_id, role, joined_at)
    SELECT room_id, NEW.machine_id, 'member', NEW.created_at FROM room_invites WHERE invite_id = NEW.invite_id
    ON CONFLICT (room_id, machine_id) DO UPDATE SET revoked_at = NULL, joined_at = NEW.created_at;
END;

CREATE TABLE IF NOT EXISTS room_sessions (
  session_id TEXT PRIMARY KEY,
  machine_id TEXT NOT NULL REFERENCES room_machines(machine_id)
);

CREATE TABLE IF NOT EXISTS room_presence (
  room_id TEXT NOT NULL REFERENCES council_rooms(room_id),
  session_id TEXT NOT NULL REFERENCES room_sessions(session_id),
  title TEXT NOT NULL,
  state TEXT NOT NULL CHECK (state IN ('running', 'queued', 'held', 'idle')),
  wake_allowed INTEGER NOT NULL CHECK (wake_allowed IN (0, 1)),
  expires_at INTEGER NOT NULL,
  PRIMARY KEY (room_id, session_id)
);

CREATE TABLE IF NOT EXISTS room_entries (
  entry_id INTEGER PRIMARY KEY AUTOINCREMENT,
  room_id TEXT NOT NULL REFERENCES council_rooms(room_id),
  author_session_id TEXT NOT NULL REFERENCES room_sessions(session_id),
  kind TEXT NOT NULL,
  content TEXT NOT NULL,
  title TEXT,
  thread_id INTEGER REFERENCES room_entries(entry_id),
  reply_to INTEGER REFERENCES room_entries(entry_id),
  reply_required INTEGER NOT NULL DEFAULT 0,
  self_wake INTEGER NOT NULL DEFAULT 0 CHECK (self_wake IN (0, 1)),
  created_at INTEGER NOT NULL,
  idempotency_key TEXT NOT NULL,
  fingerprint TEXT NOT NULL,
  deliveries_json TEXT NOT NULL,
  UNIQUE (room_id, author_session_id, idempotency_key)
);

CREATE INDEX IF NOT EXISTS room_entries_page ON room_entries(room_id, entry_id);

CREATE UNIQUE INDEX IF NOT EXISTS room_terminal_reply ON room_entries(reply_to, author_session_id)
  WHERE reply_to IS NOT NULL;

CREATE TABLE IF NOT EXISTS room_deliveries (
  entry_id INTEGER NOT NULL REFERENCES room_entries(entry_id),
  session_id TEXT NOT NULL REFERENCES room_sessions(session_id),
  state TEXT NOT NULL CHECK (state IN ('pending', 'delivered', 'replied', 'unavailable', 'interrupted')),
  reply_entry_id INTEGER REFERENCES room_entries(entry_id),
  PRIMARY KEY (entry_id, session_id)
);
CREATE INDEX IF NOT EXISTS room_pending ON room_deliveries(session_id, state, entry_id);

CREATE TABLE IF NOT EXISTS room_limits (name TEXT PRIMARY KEY, value INTEGER NOT NULL);

CREATE TRIGGER IF NOT EXISTS room_session_identity BEFORE UPDATE ON room_sessions
WHEN OLD.machine_id != NEW.machine_id
BEGIN
  SELECT RAISE(ABORT, 'room_identity');
END;

CREATE TRIGGER IF NOT EXISTS room_member_capacity BEFORE INSERT ON room_memberships
WHEN NOT EXISTS (SELECT 1 FROM room_memberships WHERE room_id = NEW.room_id AND machine_id = NEW.machine_id AND revoked_at IS NULL)
BEGIN
  SELECT RAISE(ABORT, 'room_capacity') WHERE (SELECT count(*) FROM room_memberships m JOIN council_rooms r USING(room_id)
    WHERE m.machine_id = NEW.machine_id AND m.revoked_at IS NULL AND r.deleted_at IS NULL)
    >= (SELECT value FROM room_limits WHERE name = 'rooms_per_machine');
  SELECT RAISE(ABORT, 'room_capacity') WHERE (SELECT count(*) FROM room_memberships WHERE room_id = NEW.room_id AND revoked_at IS NULL)
    >= (SELECT value FROM room_limits WHERE name = 'machines_per_room');
END;

CREATE TRIGGER IF NOT EXISTS room_presence_guard BEFORE INSERT ON room_presence
BEGIN
  SELECT RAISE(ABORT, 'room_forbidden') WHERE NOT EXISTS (
    SELECT 1 FROM room_memberships m JOIN council_rooms r USING(room_id)
    JOIN room_sessions s ON s.machine_id = m.machine_id
    WHERE m.room_id = NEW.room_id AND s.session_id = NEW.session_id AND m.revoked_at IS NULL AND r.deleted_at IS NULL
  );
  SELECT RAISE(ABORT, 'room_capacity') WHERE (SELECT count(*) FROM room_presence WHERE room_id = NEW.room_id)
    >= (SELECT value FROM room_limits WHERE name = 'sessions_per_room');
END;

CREATE TRIGGER IF NOT EXISTS room_entry_guard BEFORE INSERT ON room_entries
BEGIN
  SELECT RAISE(ABORT, 'room_forbidden') WHERE NOT EXISTS (
    SELECT 1 FROM room_presence p JOIN room_sessions s USING(session_id)
    JOIN room_memberships m ON m.room_id = p.room_id AND m.machine_id = s.machine_id
    JOIN council_rooms r ON r.room_id = m.room_id
    WHERE p.room_id = NEW.room_id AND p.session_id = NEW.author_session_id
      AND p.expires_at > NEW.created_at AND (p.state IN ('running', 'queued')
        OR (NEW.self_wake = 1 AND p.state = 'idle' AND p.wake_allowed = 1))
      AND m.revoked_at IS NULL AND r.deleted_at IS NULL
  );
  SELECT RAISE(ABORT, 'room_identity') WHERE EXISTS (
    SELECT 1 FROM room_entries WHERE room_id = NEW.room_id AND author_session_id = NEW.author_session_id
      AND idempotency_key = NEW.idempotency_key AND fingerprint != NEW.fingerprint
  );
END;

CREATE TRIGGER IF NOT EXISTS room_entry_apply AFTER INSERT ON room_entries
BEGIN
  UPDATE room_entries SET thread_id = entry_id WHERE entry_id = NEW.entry_id AND thread_id IS NULL;
  INSERT INTO room_deliveries (entry_id, session_id, state)
    SELECT NEW.entry_id, json_extract(value, '$.sid'), json_extract(value, '$.state') FROM json_each(NEW.deliveries_json);
  UPDATE room_deliveries SET state = 'replied', reply_entry_id = NEW.entry_id
    WHERE entry_id = NEW.reply_to AND session_id = NEW.author_session_id AND state != 'replied';
END;
