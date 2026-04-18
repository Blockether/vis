-- Rationale is mandatory for concept removal. Stored on the concept row.
ALTER TABLE concept ADD COLUMN removal_rationale TEXT;
