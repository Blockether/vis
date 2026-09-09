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
