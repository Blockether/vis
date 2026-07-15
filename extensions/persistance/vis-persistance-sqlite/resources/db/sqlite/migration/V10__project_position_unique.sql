-- =============================================================================
-- V10 - make `project_position` a HARD invariant, not a hope.
--
-- V7 added `project_position INTEGER NOT NULL DEFAULT 0`, so EVERY soul always
-- has a position (there is no NULL position) - but the DEFAULT 0 means every
-- member of a project that was backfilled by V7 (or otherwise never explicitly
-- ordered) SHARES slot 0. Tab order then silently collapses to the
-- `created_at DESC` tiebreak instead of the manual order, and nothing at the DB
-- level stops two members of one project from occupying the same slot. Position
-- integrity was pure application convention; here we make it structural.
--
--   1. Collapse the ties: renumber every project member to a gap-free 0-based
--      `project_position`, PARTITION BY project_id, preserving the order the
--      app already displays (existing position, then `created_at DESC` - the
--      same tiebreak `db-list-*` and the reorder fn use). Souls with no project
--      (project_id IS NULL) are untouched: their position is meaningless.
--
--   2. Enforce it forever: a partial UNIQUE index on (project_id,
--      project_position) forbids two members of the same project from sharing a
--      slot. The reorder fn parks members in negative temp slots first, so a
--      row-by-row renumber never transiently collides with this index.
--
-- No soul is ever deleted or moved between projects here - only the intra-project
-- ordinal is normalized.
-- =============================================================================

-- --- 1. collapse DEFAULT-0 ties into a contiguous 0..n-1 per project ----------
WITH ranked AS (
  SELECT id,
         row_number() OVER (
           PARTITION BY project_id
           ORDER BY project_position ASC, created_at DESC, id ASC
         ) - 1 AS rn
    FROM session_soul
   WHERE project_id IS NOT NULL
)
UPDATE session_soul
   SET project_position = (SELECT rn FROM ranked WHERE ranked.id = session_soul.id)
 WHERE project_id IS NOT NULL;

-- --- 2. one slot per (project, position), forever -----------------------------
CREATE UNIQUE INDEX idx_project_position
  ON session_soul(project_id, project_position)
  WHERE project_id IS NOT NULL;
