-- =============================================================================
-- V8 - durable per-session TURN QUEUE.
--
-- A submitted turn that lands while its session is already running one turn is
-- QUEUED. Until now the queue lived ONLY in the gateway's in-memory registry, so
-- a daemon restart dropped every queued turn on the floor. This table persists
-- the queued REQUEST (the user's prompt text + FIFO position) so daemon-boot
-- auto-resume can re-run it, exactly like it re-runs an interrupted `:running`
-- turn.
--
-- A queued turn is NOT yet a `session_turn_state` (no engine run, no iterations)
-- - it is pure gateway scheduling intent. Hence its OWN table rather than a bogus
-- `queued` status the `session_turn_state.status` CHECK constraint would reject,
-- and rather than a placeholder turn row that would duplicate / reorder history.
--
-- Rows are short-lived: the gateway deletes one the instant it DRAINS (the turn
-- starts and becomes a real `session_turn_soul`), is CANCELLED, and it updates
-- the request in place when the queued prompt is EDITED. ON DELETE CASCADE drops
-- any leftover when the whole conversation soul is deleted.
-- =============================================================================

CREATE TABLE session_turn_queue (
  id               TEXT PRIMARY KEY NOT NULL,   -- gateway registry turn id (tid)
  session_soul_id  TEXT NOT NULL,
  request          TEXT NOT NULL,
  position         INTEGER NOT NULL,            -- FIFO order within the session
  queued_at        INTEGER NOT NULL,
  FOREIGN KEY (session_soul_id) REFERENCES session_soul(id) ON DELETE CASCADE
);

CREATE INDEX idx_session_turn_queue_session
  ON session_turn_queue(session_soul_id, position, queued_at);
