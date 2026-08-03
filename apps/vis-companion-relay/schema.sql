-- vis-companion-relay — the entire persistent state of the relay.
--
-- A GRANT is an opaque bearer capability that lets ONE gateway wake ONE device.
-- The relay stores sha256(grant), never the grant itself: a database dump is
-- not a set of push capabilities. Revoking is deleting one row, which is the
-- property an APNs signing key can never have.

CREATE TABLE IF NOT EXISTS grants (
  id           TEXT    PRIMARY KEY,          -- sha256(grant), hex
  device_token TEXT    NOT NULL,             -- APNs / FCM registration token
  platform     TEXT    NOT NULL,             -- ios | ipados | android
  environment  TEXT    NOT NULL DEFAULT 'production',
  label        TEXT,                         -- free-form, for the owner's UI
  created_at   INTEGER NOT NULL,
  last_push_at INTEGER,
  push_count   INTEGER NOT NULL DEFAULT 0
);

CREATE INDEX IF NOT EXISTS grants_device_idx ON grants (platform, device_token, created_at);
-- The cron sweep asks exactly one question: which grants were minted and never
-- used? Anyone can mint one, so that question must stay cheap forever.
CREATE INDEX IF NOT EXISTS grants_unused_idx ON grants (push_count, created_at);

-- One fixed-window counter per subject: `grant:<id>` for pushes, `ip:<addr>`
-- for grant creation. One row per subject, so a busy relay stays inside D1's
-- free write budget.
CREATE TABLE IF NOT EXISTS quota (
  subject      TEXT    PRIMARY KEY,
  window_start INTEGER NOT NULL,
  count        INTEGER NOT NULL
);

CREATE INDEX IF NOT EXISTS quota_window_idx ON quota (window_start);
