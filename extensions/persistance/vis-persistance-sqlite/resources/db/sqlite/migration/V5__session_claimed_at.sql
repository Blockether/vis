-- =============================================================================
-- V5 - session adoption marker (`claimed_at`).
--
-- The TUI keeps a warm POOL of empty `:tui` sessions ahead of demand (a Ctrl+N
-- opens instantly by popping a pre-built one). Each pool entry is a REAL
-- persisted `session_soul`, previously indistinguishable from a user-created
-- conversation - so unadopted pool rows LEAKED into the cross-channel
-- `db-list-sessions` view as untitled "ghost" sessions in the web/TUI pickers.
--
-- `claimed_at` is the adoption stamp:
--   NULL     = unclaimed scaffolding: a warm pool entry nobody has bound yet.
--   non-NULL = a real conversation - user-created (any non-prewarm channel) or
--              one that received its first turn (stamped by db-store-session-turn!).
--
-- `db-list-sessions` shows ONLY claimed souls. Direct resume by id
-- (`db-get-session`) stays UNFILTERED, so a claimed-but-empty tab still restores
-- and a pooled soul is still reachable by the TUI that owns it.
--
-- Backfill: a PRE-EXISTING soul counts as a real conversation (claimed) when
-- it has at least one turn OR a non-empty title on any of its states. Turnless,
-- untitled souls are exactly the warm-pool GHOSTS this column exists to hide -
-- so they stay unclaimed, retroactively cleaning up pickers that already
-- accumulated pool leakage. (Sub_loop children are excluded from the list by
-- parent_state_id regardless, so their claim state is irrelevant.)
-- =============================================================================

ALTER TABLE session_soul ADD COLUMN claimed_at INTEGER;

UPDATE session_soul
   SET claimed_at = created_at
 WHERE claimed_at IS NULL
   AND (EXISTS (SELECT 1
                  FROM session_state ss
                  JOIN session_turn_soul ts ON ts.session_state_id = ss.id
                 WHERE ss.session_soul_id = session_soul.id)
        OR EXISTS (SELECT 1
                     FROM session_state ss
                    WHERE ss.session_soul_id = session_soul.id
                      AND ss.title IS NOT NULL
                      AND trim(ss.title) <> ''));

CREATE INDEX idx_session_soul_claimed
  ON session_soul(channel, claimed_at, created_at DESC)
  WHERE claimed_at IS NOT NULL;
