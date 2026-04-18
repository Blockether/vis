-- Concept graph: cross-document ontology / ubiquitous language
--
-- concept           — canonical term with definition
-- concept_alias     — alternative names to avoid
-- concept_source    — which document/page a concept was extracted from
-- concept_edge      — directed relationship between two concepts

CREATE TABLE IF NOT EXISTS concept (
  id          TEXT PRIMARY KEY,
  term        TEXT NOT NULL,
  definition  TEXT,
  group_name  TEXT,
  created_at  INTEGER NOT NULL DEFAULT (strftime('%s','now') * 1000)
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_concept_term ON concept(term);

CREATE TABLE IF NOT EXISTS concept_alias (
  id          TEXT PRIMARY KEY,
  concept_id  TEXT NOT NULL REFERENCES concept(id) ON DELETE CASCADE,
  alias       TEXT NOT NULL,
  reason      TEXT
);

CREATE INDEX IF NOT EXISTS idx_concept_alias_concept ON concept_alias(concept_id);

CREATE TABLE IF NOT EXISTS concept_source (
  id           TEXT PRIMARY KEY,
  concept_id   TEXT NOT NULL REFERENCES concept(id) ON DELETE CASCADE,
  document_id  TEXT,
  page_index   INTEGER,
  excerpt      TEXT
);

CREATE INDEX IF NOT EXISTS idx_concept_source_concept ON concept_source(concept_id);

CREATE TABLE IF NOT EXISTS concept_edge (
  id                 TEXT PRIMARY KEY,
  source_concept_id  TEXT NOT NULL REFERENCES concept(id) ON DELETE CASCADE,
  target_concept_id  TEXT NOT NULL REFERENCES concept(id) ON DELETE CASCADE,
  relationship_type  TEXT NOT NULL,
  description        TEXT,
  cardinality        TEXT
);

CREATE INDEX IF NOT EXISTS idx_concept_edge_source ON concept_edge(source_concept_id);
CREATE INDEX IF NOT EXISTS idx_concept_edge_target ON concept_edge(target_concept_id);

-- FTS5 for concept search
INSERT INTO search(owner_table, owner_id, field, text)
  SELECT 'concept', id, 'term', term FROM concept WHERE term IS NOT NULL AND term <> '';
INSERT INTO search(owner_table, owner_id, field, text)
  SELECT 'concept', id, 'definition', definition FROM concept WHERE definition IS NOT NULL AND definition <> '';

CREATE TRIGGER IF NOT EXISTS trg_concept_ai
AFTER INSERT ON concept BEGIN
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'concept', new.id, 'term', new.term WHERE new.term IS NOT NULL AND new.term <> '';
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'concept', new.id, 'definition', new.definition WHERE new.definition IS NOT NULL AND new.definition <> '';
END;

CREATE TRIGGER IF NOT EXISTS trg_concept_au
AFTER UPDATE ON concept BEGIN
  DELETE FROM search WHERE owner_table='concept' AND owner_id=old.id;
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'concept', new.id, 'term', new.term WHERE new.term IS NOT NULL AND new.term <> '';
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'concept', new.id, 'definition', new.definition WHERE new.definition IS NOT NULL AND new.definition <> '';
END;

CREATE TRIGGER IF NOT EXISTS trg_concept_ad
AFTER DELETE ON concept BEGIN
  DELETE FROM search WHERE owner_table='concept' AND owner_id=old.id;
END;
