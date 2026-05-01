-- V2 — per-iteration raw LLM response diagnostics.
--
-- Stores bounded forensic data for debugging extraction/rendering bugs
-- without requiring ~/.vis/vis.log access. The full raw response is not
-- persisted here; we keep a preview + length + SHA-256 hash so callers can
-- compare evidence while bounding storage and privacy exposure.

ALTER TABLE iteration
  ADD COLUMN llm_raw_response_preview TEXT;

ALTER TABLE iteration
  ADD COLUMN llm_raw_response_length INTEGER CHECK (
    llm_raw_response_length IS NULL OR llm_raw_response_length >= 0
  );

ALTER TABLE iteration
  ADD COLUMN llm_raw_response_sha256 TEXT;

ALTER TABLE iteration
  ADD COLUMN llm_selected_block_count INTEGER CHECK (
    llm_selected_block_count IS NULL OR llm_selected_block_count >= 0
  );

ALTER TABLE iteration
  ADD COLUMN llm_selected_block_langs TEXT;
