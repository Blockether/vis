-- Phase 1: Per-page concepts grounded to actual page nodes (extracted at index time)
CREATE TABLE IF NOT EXISTS page_concept (
  id           TEXT PRIMARY KEY,
  document_id  TEXT NOT NULL,
  page_id      TEXT NOT NULL,
  node_id      TEXT,
  term         TEXT NOT NULL,
  definition   TEXT,
  excerpt      TEXT,
  page_sha     TEXT,
  created_at   INTEGER NOT NULL DEFAULT (strftime('%s','now') * 1000)
);

CREATE INDEX IF NOT EXISTS idx_page_concept_doc  ON page_concept(document_id);
CREATE INDEX IF NOT EXISTS idx_page_concept_page ON page_concept(page_id);
CREATE INDEX IF NOT EXISTS idx_page_concept_term ON page_concept(term);

-- Add content_sha to page table for change detection
ALTER TABLE page ADD COLUMN content_sha TEXT;

-- Add status to concept table for user refinement
ALTER TABLE concept ADD COLUMN status TEXT NOT NULL DEFAULT 'active';
-- status: active | removed | user_edited
