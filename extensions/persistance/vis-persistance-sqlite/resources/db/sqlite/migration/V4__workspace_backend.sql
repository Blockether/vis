-- Record which workspace backend owns each derived root.
-- `live` means the root is shared and must never be deleted by workspace
-- teardown. Existing isolated rows predate backend abstraction and were Rift.

ALTER TABLE workspace ADD COLUMN workspace_backend TEXT NOT NULL DEFAULT 'live';

UPDATE workspace
SET workspace_backend = 'rift'
WHERE fork_ms IS NOT NULL;
