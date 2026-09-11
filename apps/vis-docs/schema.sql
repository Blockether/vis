-- Only explicitly approved metadata is public. No repository code is stored.
CREATE TABLE IF NOT EXISTS extensions (
  id TEXT PRIMARY KEY CHECK(length(id) = 24),
  metadata TEXT NOT NULL CHECK(json_valid(metadata)),
  added_at TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS submissions (
  id TEXT PRIMARY KEY CHECK(length(id) = 24),
  extension_id TEXT NOT NULL CHECK(length(extension_id) = 24),
  revision TEXT NOT NULL CHECK(length(revision) = 40),
  metadata TEXT NOT NULL CHECK(json_valid(metadata)),
  submitted_at TEXT NOT NULL,
  UNIQUE(extension_id, revision)
);
-- Version identities are permanent, including rejected releases (discovery must not requeue them).
CREATE TABLE IF NOT EXISTS releases (
  extension_id TEXT NOT NULL CHECK(length(extension_id) = 24),
  version TEXT NOT NULL,
  revision TEXT NOT NULL CHECK(length(revision) = 40),
  metadata TEXT NOT NULL CHECK(json_valid(metadata)),
  status TEXT NOT NULL CHECK(status IN ('approved','rejected')),
  reviewed_at TEXT NOT NULL,
  PRIMARY KEY(extension_id,version)
);
CREATE UNIQUE INDEX IF NOT EXISTS release_tags ON releases(extension_id,json_extract(metadata,'$.release_tag'))
  WHERE json_extract(metadata,'$.release_tag') IS NOT NULL;
CREATE TABLE IF NOT EXISTS release_sync (
  extension_id TEXT PRIMARY KEY REFERENCES extensions(id) ON DELETE CASCADE,
  page INTEGER NOT NULL DEFAULT 1,
  position INTEGER NOT NULL DEFAULT 0,
  checked_at TEXT NOT NULL,
  error TEXT
);

-- Only moderated comments are public. Voter is a private HMAC, never a raw IP.
CREATE TABLE IF NOT EXISTS comments (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  extension_id TEXT NOT NULL REFERENCES extensions(id) ON DELETE CASCADE,
  voter TEXT NOT NULL,
  name TEXT NOT NULL CHECK(length(name) BETWEEN 1 AND 60),
  body TEXT NOT NULL CHECK(length(body) BETWEEN 1 AND 2000),
  status TEXT NOT NULL DEFAULT 'pending' CHECK(status IN ('pending','approved','rejected')),
  created_at TEXT NOT NULL
);
CREATE INDEX IF NOT EXISTS comments_public ON comments(extension_id,status,id DESC);
CREATE INDEX IF NOT EXISTS comments_rate ON comments(voter,created_at);
CREATE TABLE IF NOT EXISTS package_votes (
  extension_id TEXT NOT NULL REFERENCES extensions(id) ON DELETE CASCADE,
  voter TEXT NOT NULL,
  value INTEGER NOT NULL CHECK(value IN (-1,1)),
  PRIMARY KEY(extension_id,voter)
);
CREATE TABLE IF NOT EXISTS comment_votes (
  comment_id INTEGER NOT NULL REFERENCES comments(id) ON DELETE CASCADE,
  voter TEXT NOT NULL,
  value INTEGER NOT NULL CHECK(value IN (-1,1)),
  PRIMARY KEY(comment_id,voter)
);
