-- =============================================================================
-- V7 - rename session groups -> PROJECTS + movable project sessions.
--
-- The V6 "session_group / folder" notion is renamed to PROJECT everywhere, with
-- NO backwards-compatibility shims. A project is the cross-channel "what belongs
-- together" axis (owner + color + optional channel scope). A session belongs to
-- 0..1 project via the pointer on the soul.
--
-- Sessions inside a project are the "project sessions" (the TUI calls them TABS).
-- They are MOVABLE: `project_position` gives each soul a manual order within its
-- project, so a client can reorder tabs/sessions and have it persist.
--
-- Renames rely on modern SQLite (>= 3.25) auto-updating FK / index / trigger
-- references when a table or column is renamed (legacy_alter_table OFF, default).
-- =============================================================================

-- --- session_group -> project ------------------------------------------------
ALTER TABLE session_group RENAME TO project;

-- --- membership pointer session_soul.group_id -> project_id -------------------
ALTER TABLE session_soul RENAME COLUMN group_id TO project_id;

-- --- movable ordering of sessions within a project ---------------------------
ALTER TABLE session_soul ADD COLUMN project_position INTEGER NOT NULL DEFAULT 0;

-- --- refresh indexes to the new names ----------------------------------------
DROP INDEX IF EXISTS idx_session_group_owner_channel;
CREATE INDEX idx_project_owner_channel
  ON project(owner_id, channel, position, created_at);

DROP INDEX IF EXISTS idx_session_soul_group;
CREATE INDEX idx_session_soul_project ON session_soul(project_id, project_position)
  WHERE project_id IS NOT NULL;
