-- Workspace-chain checkpoints for single-expression undo/redo.
--
-- fork_ms remains the immediate parent baseline. apply_fork_ms is inherited
-- from the first isolated fork and therefore describes the cumulative diff
-- that eventually lands in trunk. Existing drafts start with both baselines
-- equal; existing trunk rows remain baseline-free.

ALTER TABLE workspace ADD COLUMN workspace_kind TEXT NOT NULL DEFAULT 'trunk'
  CHECK (workspace_kind IN ('trunk', 'draft', 'checkpoint'));

ALTER TABLE workspace ADD COLUMN parent_workspace_id TEXT
  REFERENCES workspace(id) ON DELETE RESTRICT;

ALTER TABLE workspace ADD COLUMN apply_fork_ms INTEGER;

UPDATE workspace
SET workspace_kind = 'draft',
    apply_fork_ms = fork_ms
WHERE fork_ms IS NOT NULL;

CREATE INDEX idx_workspace_parent
  ON workspace(parent_workspace_id);
