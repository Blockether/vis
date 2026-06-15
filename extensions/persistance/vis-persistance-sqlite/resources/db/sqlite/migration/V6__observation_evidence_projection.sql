CREATE TABLE IF NOT EXISTS observation_event (
  id                         TEXT PRIMARY KEY NOT NULL,
  session_turn_iteration_id   TEXT NOT NULL
                             REFERENCES session_turn_iteration(id) ON DELETE CASCADE,
  position                   INTEGER NOT NULL CHECK (position >= 0),
  form_scope                 TEXT,
  form_index                 INTEGER CHECK (form_index IS NULL OR form_index >= 1),
  op                         TEXT NOT NULL CHECK (trim(op) <> ''),
  fingerprint                TEXT NOT NULL CHECK (trim(fingerprint) <> ''),
  path                       TEXT,
  query                      TEXT,
  range_start                INTEGER,
  range_end                  INTEGER,
  mtime                      INTEGER,
  size                       INTEGER,
  line_count                 INTEGER CHECK (line_count IS NULL OR line_count >= 0),
  result_summary             TEXT,
  payload_scope              TEXT,
  repeat_of_scope            TEXT,
  covered_by_scope           TEXT,
  stale                      INTEGER NOT NULL DEFAULT 0 CHECK (stale IN (0, 1)),
  metadata_json              TEXT,
  created_at                 INTEGER NOT NULL,

  UNIQUE (session_turn_iteration_id, position)
);

CREATE INDEX IF NOT EXISTS idx_observation_event_iteration
  ON observation_event(session_turn_iteration_id, position);

CREATE INDEX IF NOT EXISTS idx_observation_event_fingerprint
  ON observation_event(fingerprint);

CREATE INDEX IF NOT EXISTS idx_observation_event_path_stale
  ON observation_event(path, stale)
  WHERE path IS NOT NULL;

CREATE TABLE IF NOT EXISTS evidence_event (
  id                         TEXT PRIMARY KEY NOT NULL,
  session_turn_iteration_id   TEXT NOT NULL
                             REFERENCES session_turn_iteration(id) ON DELETE CASCADE,
  position                   INTEGER NOT NULL CHECK (position >= 0),
  task_key                   TEXT,
  evidence_id                TEXT NOT NULL CHECK (trim(evidence_id) <> ''),
  evidence_kind              TEXT,
  status                     TEXT,
  payload_scope              TEXT,
  summary                    TEXT,
  observation_ids_json       TEXT,
  created_at                 INTEGER NOT NULL,

  UNIQUE (session_turn_iteration_id, position)
);

CREATE INDEX IF NOT EXISTS idx_evidence_event_iteration
  ON evidence_event(session_turn_iteration_id, position);

CREATE INDEX IF NOT EXISTS idx_evidence_event_task
  ON evidence_event(task_key, created_at)
  WHERE task_key IS NOT NULL;
