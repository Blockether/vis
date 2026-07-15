-- =============================================================================
-- V9 - bind a PROJECT to a workspace ROOT so a project IS a tab set.
--
-- The TUI's tab set used to live in a per-launch-directory EDN sidecar
-- (`~/.vis/tabs/<place>.edn`), redundant with the DB which already stores
-- project membership + `project_position` order. We remove the sidecar and let
-- a project OWN its tabs: launch vis in a directory -> get/create the project
-- bound to that canonical root -> its member sessions ARE the open tabs.
--
--   root  - the canonical absolute path of the workspace root a project is
--           bound to. NULL = a LOOSE project (created by hand via the move-to-
--           project picker), not tied to any directory. A non-NULL root is the
--           "workspace = project = tab set" binding the TUI resolves on launch
--           and on switch-project.
--
-- One project per (owner, root): the partial UNIQUE index makes get-or-create
-- race-safe (a losing concurrent insert fails and the caller re-reads).
-- =============================================================================

ALTER TABLE project ADD COLUMN root TEXT;

CREATE UNIQUE INDEX idx_project_owner_root
  ON project(owner_id, root)
  WHERE root IS NOT NULL;
