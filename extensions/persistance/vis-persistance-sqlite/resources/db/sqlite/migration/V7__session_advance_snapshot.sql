-- Lightweight advance lineage keyed by session state and execution scope.
--
-- Unlike workspace_graph_revision, these rows do not represent filesystem
-- checkpoints. They are append-only CTX/receipt snapshots for resume, logs, and
-- DAG inspection while advance mode mutates the currently pinned workspace.

CREATE TABLE session_advance_snapshot (
  id                 TEXT PRIMARY KEY NOT NULL,
  session_state_id   TEXT NOT NULL
                     REFERENCES session_state(id) ON DELETE CASCADE,
  parent_snapshot_id TEXT
                     REFERENCES session_advance_snapshot(id) ON DELETE SET NULL,
  scope              TEXT NOT NULL,
  workspace_id       TEXT
                     REFERENCES workspace(id) ON DELETE SET NULL,
  workspace_kind     TEXT,
  workspace_root     TEXT,
  ctx_before         BLOB,
  ctx                BLOB NOT NULL,
  advance            BLOB,
  receipt            BLOB,
  created_at         INTEGER NOT NULL
);

CREATE INDEX idx_session_advance_snapshot_state_created
  ON session_advance_snapshot(session_state_id, created_at);

CREATE INDEX idx_session_advance_snapshot_parent
  ON session_advance_snapshot(parent_snapshot_id);
