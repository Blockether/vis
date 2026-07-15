-- =============================================================================
-- V9 - bind a PROJECT to its workspace ROOT so a project IS a tab set.
--
-- The TUI's tab set used to live in a per-launch-directory EDN sidecar
-- (`~/.vis/tabs/<place>.edn`), redundant with the DB which already stores
-- project membership + `project_position` order. We remove the sidecar and let
-- a project OWN its tabs: launch vis in a directory -> get/create the project
-- bound to that canonical root -> its member sessions ARE the open tabs.
--
--   workspace_root - the canonical absolute path of the workspace root a
--           project is bound to. NULL = a LOOSE project (created by hand via
--           the move-to-project picker), not tied to any directory. A non-NULL
--           value is the "workspace = project = tab set" binding the TUI
--           resolves on launch and on switch-project.
--
--           NOTE the deliberately-explicit name: `workspace.root` is the CLONE
--           path and `workspace.repo_root` the real cwd; a project binds to the
--           cwd, i.e. `workspace.repo_root`. Naming it `workspace_root` (not a
--           bare `root`) keeps the two `root`s one join apart from colliding.
--           Callers store a canonical path (java.io.File/getCanonicalPath),
--           which also case-normalizes on case-insensitive filesystems so the
--           same directory never fragments into two projects.
--
-- One project per (owner, workspace_root): the partial UNIQUE index makes
-- get-or-create race-safe (a losing concurrent insert fails and the caller
-- re-reads). The CHECK forbids a blank binding masquerading as "bound".
-- =============================================================================

ALTER TABLE project
  ADD COLUMN workspace_root TEXT
  CHECK (workspace_root IS NULL OR trim(workspace_root) <> '');

CREATE UNIQUE INDEX idx_project_owner_workspace_root
  ON project(owner_id, workspace_root)
  WHERE workspace_root IS NOT NULL;
