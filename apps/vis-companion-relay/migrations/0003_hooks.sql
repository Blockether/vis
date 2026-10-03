-- Webhook inboxes hold requests for gateways without a public address.
-- An inbox token is stored only as its SHA-256 digest.
CREATE TABLE IF NOT EXISTS hook_inboxes (
  id TEXT PRIMARY KEY,
  token_hash TEXT NOT NULL UNIQUE,
  created_at INTEGER NOT NULL,
  polled_at INTEGER NOT NULL
);

-- A request keeps its raw body in base64 and only the allowlisted headers.
CREATE TABLE IF NOT EXISTS hook_requests (
  id TEXT PRIMARY KEY,
  inbox_id TEXT NOT NULL REFERENCES hook_inboxes(id) ON DELETE CASCADE,
  automation_id TEXT NOT NULL,
  received_at INTEGER NOT NULL,
  headers TEXT NOT NULL,
  body TEXT NOT NULL,
  body_bytes INTEGER NOT NULL CHECK (body_bytes >= 0)
);

CREATE INDEX IF NOT EXISTS hook_requests_by_inbox ON hook_requests (inbox_id, received_at, id);

CREATE INDEX IF NOT EXISTS hook_requests_by_age ON hook_requests (received_at);

CREATE INDEX IF NOT EXISTS hook_inboxes_by_poll ON hook_inboxes (polled_at);
