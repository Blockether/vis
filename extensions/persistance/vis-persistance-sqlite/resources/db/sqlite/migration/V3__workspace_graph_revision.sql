-- Graph snapshots keyed to rift workspace tips.
--
-- Checkpoint acceptance writes parent + child CTX revisions in the same
-- transaction as the session workspace pointer. Undo/redo can therefore move
-- filesystem and graph state together without reverse patches.

CREATE TABLE workspace_graph_revision (
  workspace_id        TEXT PRIMARY KEY NOT NULL
                      REFERENCES workspace(id) ON DELETE CASCADE,
  session_state_id    TEXT NOT NULL
                      REFERENCES session_state(id) ON DELETE CASCADE,
  parent_workspace_id TEXT
                      REFERENCES workspace(id) ON DELETE RESTRICT,
  ctx                 BLOB NOT NULL,
  settlement          BLOB,
  receipt             BLOB,
  created_at          INTEGER NOT NULL,
  updated_at          INTEGER NOT NULL
);

CREATE INDEX idx_workspace_graph_revision_session
  ON workspace_graph_revision(session_state_id, updated_at);
