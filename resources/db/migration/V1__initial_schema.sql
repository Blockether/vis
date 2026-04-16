-- =============================================================================
-- V1 — initial RLM schema (entities, per-type attrs, corpus, FTS, triggers)
-- =============================================================================
-- Single versioned migration that creates every table, index, FTS virtual
-- table, and trigger the runtime expects. Flyway applies this once on a
-- fresh database and records it in flyway_schema_history; later migrations
-- live beside this file as V2__…, V3__…, etc.
--
-- NOTE: we do NOT set PRAGMA foreign_keys here — that's a per-connection
-- setting applied by the SQLite connection pool at datasource open time.

-- -----------------------------------------------------------------------------
-- Core unified entity table
-- -----------------------------------------------------------------------------
CREATE TABLE entity (
  id            TEXT PRIMARY KEY NOT NULL,
  type          TEXT NOT NULL,
  name          TEXT,
  description   TEXT,
  parent_id     TEXT,
  document_id   TEXT,
  page          INTEGER,
  section       TEXT,
  canonical_id  TEXT,
  created_at    INTEGER,
  updated_at    INTEGER
);
CREATE INDEX idx_entity_type          ON entity(type);
CREATE INDEX idx_entity_parent        ON entity(parent_id);
CREATE INDEX idx_entity_document      ON entity(document_id);
CREATE INDEX idx_entity_canonical     ON entity(canonical_id);
CREATE INDEX idx_entity_type_doc_page ON entity(type, document_id, page);
CREATE INDEX idx_entity_created       ON entity(created_at);

-- -----------------------------------------------------------------------------
-- Per-type entity attribute tables
-- -----------------------------------------------------------------------------
CREATE TABLE conversation_attrs (
  entity_id      TEXT PRIMARY KEY NOT NULL REFERENCES entity(id) ON DELETE CASCADE,
  system_prompt  TEXT,
  model          TEXT
);

CREATE TABLE query_attrs (
  entity_id        TEXT PRIMARY KEY NOT NULL REFERENCES entity(id) ON DELETE CASCADE,
  messages         TEXT,
  text             TEXT,
  answer           TEXT,
  iterations       INTEGER,
  duration_ms      INTEGER,
  status           TEXT,
  eval_score       REAL,
  model            TEXT,
  input_tokens     INTEGER,
  output_tokens    INTEGER,
  reasoning_tokens INTEGER,
  cached_tokens    INTEGER,
  total_cost       REAL
);

CREATE TABLE iteration_attrs (
  entity_id    TEXT PRIMARY KEY NOT NULL REFERENCES entity(id) ON DELETE CASCADE,
  code         TEXT,
  results      TEXT,
  vars         TEXT,
  answer       TEXT,
  thinking     TEXT,
  error        TEXT,
  duration_ms  INTEGER
);

CREATE TABLE iteration_var_attrs (
  entity_id    TEXT PRIMARY KEY NOT NULL REFERENCES entity(id) ON DELETE CASCADE,
  name         TEXT,
  value        TEXT,
  code         TEXT
);

CREATE TABLE repo_attrs (
  entity_id         TEXT PRIMARY KEY NOT NULL REFERENCES entity(id) ON DELETE CASCADE,
  name              TEXT UNIQUE NOT NULL,
  path              TEXT,
  head_sha          TEXT,
  head_short        TEXT,
  branch            TEXT,
  commits_ingested  INTEGER,
  ingested_at       INTEGER
);

CREATE TABLE commit_attrs (
  entity_id      TEXT PRIMARY KEY NOT NULL REFERENCES entity(id) ON DELETE CASCADE,
  category       TEXT,
  sha            TEXT,
  date           TEXT,
  prefix         TEXT,
  scope          TEXT,
  author_email   TEXT
);
CREATE INDEX idx_commit_sha   ON commit_attrs(sha);
CREATE INDEX idx_commit_date  ON commit_attrs(date);
CREATE INDEX idx_commit_email ON commit_attrs(author_email);

CREATE TABLE commit_ticket_ref (
  entity_id    TEXT NOT NULL REFERENCES entity(id) ON DELETE CASCADE,
  ticket       TEXT NOT NULL,
  PRIMARY KEY (entity_id, ticket)
);
CREATE INDEX idx_commit_ticket ON commit_ticket_ref(ticket);

CREATE TABLE commit_file_path (
  entity_id    TEXT NOT NULL REFERENCES entity(id) ON DELETE CASCADE,
  path         TEXT NOT NULL,
  PRIMARY KEY (entity_id, path)
);
CREATE INDEX idx_commit_path ON commit_file_path(path);

CREATE TABLE commit_parent (
  entity_id    TEXT NOT NULL REFERENCES entity(id) ON DELETE CASCADE,
  parent_sha   TEXT NOT NULL,
  PRIMARY KEY (entity_id, parent_sha)
);

CREATE TABLE person_attrs (
  entity_id    TEXT PRIMARY KEY NOT NULL REFERENCES entity(id) ON DELETE CASCADE,
  email        TEXT
);
CREATE INDEX idx_person_email ON person_attrs(email);

-- -----------------------------------------------------------------------------
-- Non-entity data (corpus)
-- -----------------------------------------------------------------------------
CREATE TABLE document (
  id                TEXT PRIMARY KEY NOT NULL,
  name              TEXT,
  type              TEXT,
  title             TEXT,
  abstract          TEXT,
  extension         TEXT,
  author            TEXT,
  page_count        INTEGER,
  created_at        INTEGER,
  updated_at        INTEGER,
  certainty_alpha   REAL,
  certainty_beta    REAL
);
CREATE INDEX idx_document_type ON document(type);

CREATE TABLE skill_attrs (
  document_id   TEXT PRIMARY KEY NOT NULL REFERENCES document(id) ON DELETE CASCADE,
  body          TEXT,
  source_path   TEXT,
  agent_config  TEXT,
  requires      TEXT,
  version       TEXT,
  content_hash  TEXT
);

CREATE TABLE page (
  id               TEXT PRIMARY KEY NOT NULL,
  document_id      TEXT REFERENCES document(id) ON DELETE CASCADE,
  idx              INTEGER,
  created_at       INTEGER,
  last_accessed    INTEGER,
  access_count     REAL,
  q_value          REAL,
  q_update_count   INTEGER
);
CREATE INDEX idx_page_document      ON page(document_id);
CREATE INDEX idx_page_doc_idx       ON page(document_id, idx);
CREATE INDEX idx_page_last_accessed ON page(last_accessed);

CREATE TABLE page_node (
  id               TEXT PRIMARY KEY NOT NULL,
  page_id          TEXT REFERENCES page(id) ON DELETE CASCADE,
  document_id      TEXT,
  local_id         TEXT,
  type             TEXT,
  content          TEXT,
  description      TEXT,
  level            TEXT,
  parent_id        TEXT,
  image_data       BLOB,
  continuation     INTEGER,
  caption          TEXT,
  kind             TEXT,
  bbox             TEXT,
  group_id         TEXT
);
CREATE INDEX idx_node_page     ON page_node(page_id);
CREATE INDEX idx_node_document ON page_node(document_id);
CREATE INDEX idx_node_type     ON page_node(type);

CREATE TABLE document_toc (
  id                  TEXT PRIMARY KEY NOT NULL,
  document_id         TEXT REFERENCES document(id) ON DELETE CASCADE,
  type                TEXT,
  title               TEXT,
  description         TEXT,
  target_page         INTEGER,
  target_section_id   TEXT,
  level               TEXT,
  parent_id           TEXT,
  created_at          INTEGER
);
CREATE INDEX idx_toc_document ON document_toc(document_id);

CREATE TABLE page_cooccurrence (
  id           TEXT PRIMARY KEY NOT NULL,
  page_a       TEXT NOT NULL,
  page_b       TEXT NOT NULL,
  strength     REAL,
  last_seen    INTEGER
);
CREATE INDEX idx_cooc_a ON page_cooccurrence(page_a);
CREATE INDEX idx_cooc_b ON page_cooccurrence(page_b);

CREATE TABLE relationship (
  id                  TEXT PRIMARY KEY NOT NULL,
  type                TEXT NOT NULL,
  source_entity_id    TEXT,
  target_entity_id    TEXT,
  description         TEXT,
  document_id         TEXT
);
CREATE INDEX idx_rel_source ON relationship(source_entity_id);
CREATE INDEX idx_rel_target ON relationship(target_entity_id);
CREATE INDEX idx_rel_type   ON relationship(type);

CREATE TABLE claim (
  id                      TEXT PRIMARY KEY NOT NULL,
  text                    TEXT,
  document_id             TEXT,
  page                    INTEGER,
  section                 TEXT,
  quote                   TEXT,
  confidence              REAL,
  query_id                TEXT,
  verified                INTEGER,
  verification_verdict    TEXT,
  created_at              INTEGER
);
CREATE INDEX idx_claim_document ON claim(document_id);
CREATE INDEX idx_claim_query    ON claim(query_id);

CREATE TABLE raw_document (
  id        TEXT PRIMARY KEY NOT NULL,
  content   TEXT
);

CREATE TABLE rlm_meta (
  id                TEXT PRIMARY KEY NOT NULL,
  corpus_revision   INTEGER,
  updated_at        INTEGER
);

-- =============================================================================
-- FTS5 unified search index
-- =============================================================================
-- One virtual table indexes ALL text columns from ALL source tables.
-- Maintained via per-source triggers below. Query via:
--   SELECT owner_table, owner_id, snippet(...), bm25(search) AS rank
--   FROM search WHERE search MATCH ? ORDER BY rank LIMIT ?
--
-- UNINDEXED columns don't bloat the FTS index; they're for filtering/joins.
CREATE VIRTUAL TABLE search USING fts5(
  owner_table  UNINDEXED,
  owner_id     UNINDEXED,
  field        UNINDEXED,
  text,
  tokenize='porter unicode61 remove_diacritics 2'
);

-- -----------------------------------------------------------------------------
-- Triggers — keep `search` in sync with source tables.
-- Pattern per source: AI inserts non-empty fields; AU deletes prior + re-INS;
-- AD deletes all rows for that owner_id.
-- -----------------------------------------------------------------------------

-- document → indexes title, abstract
CREATE TRIGGER trg_document_ai
AFTER INSERT ON document BEGIN
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'document', new.id, 'title', new.title WHERE new.title IS NOT NULL AND new.title <> '';
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'document', new.id, 'abstract', new.abstract WHERE new.abstract IS NOT NULL AND new.abstract <> '';
END;
CREATE TRIGGER trg_document_au
AFTER UPDATE ON document BEGIN
  DELETE FROM search WHERE owner_table='document' AND owner_id=old.id;
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'document', new.id, 'title', new.title WHERE new.title IS NOT NULL AND new.title <> '';
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'document', new.id, 'abstract', new.abstract WHERE new.abstract IS NOT NULL AND new.abstract <> '';
END;
CREATE TRIGGER trg_document_ad
AFTER DELETE ON document BEGIN
  DELETE FROM search WHERE owner_table='document' AND owner_id=old.id;
END;

-- skill_attrs → indexes body
CREATE TRIGGER trg_skill_ai
AFTER INSERT ON skill_attrs BEGIN
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'skill', new.document_id, 'body', new.body WHERE new.body IS NOT NULL AND new.body <> '';
END;
CREATE TRIGGER trg_skill_au
AFTER UPDATE ON skill_attrs BEGIN
  DELETE FROM search WHERE owner_table='skill' AND owner_id=old.document_id;
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'skill', new.document_id, 'body', new.body WHERE new.body IS NOT NULL AND new.body <> '';
END;
CREATE TRIGGER trg_skill_ad
AFTER DELETE ON skill_attrs BEGIN
  DELETE FROM search WHERE owner_table='skill' AND owner_id=old.document_id;
END;

-- page_node → indexes content, description
CREATE TRIGGER trg_page_node_ai
AFTER INSERT ON page_node BEGIN
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'page_node', new.id, 'content', new.content WHERE new.content IS NOT NULL AND new.content <> '';
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'page_node', new.id, 'description', new.description WHERE new.description IS NOT NULL AND new.description <> '';
END;
CREATE TRIGGER trg_page_node_au
AFTER UPDATE ON page_node BEGIN
  DELETE FROM search WHERE owner_table='page_node' AND owner_id=old.id;
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'page_node', new.id, 'content', new.content WHERE new.content IS NOT NULL AND new.content <> '';
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'page_node', new.id, 'description', new.description WHERE new.description IS NOT NULL AND new.description <> '';
END;
CREATE TRIGGER trg_page_node_ad
AFTER DELETE ON page_node BEGIN
  DELETE FROM search WHERE owner_table='page_node' AND owner_id=old.id;
END;

-- document_toc → indexes title, description
CREATE TRIGGER trg_toc_ai
AFTER INSERT ON document_toc BEGIN
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'document_toc', new.id, 'title', new.title WHERE new.title IS NOT NULL AND new.title <> '';
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'document_toc', new.id, 'description', new.description WHERE new.description IS NOT NULL AND new.description <> '';
END;
CREATE TRIGGER trg_toc_au
AFTER UPDATE ON document_toc BEGIN
  DELETE FROM search WHERE owner_table='document_toc' AND owner_id=old.id;
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'document_toc', new.id, 'title', new.title WHERE new.title IS NOT NULL AND new.title <> '';
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'document_toc', new.id, 'description', new.description WHERE new.description IS NOT NULL AND new.description <> '';
END;
CREATE TRIGGER trg_toc_ad
AFTER DELETE ON document_toc BEGIN
  DELETE FROM search WHERE owner_table='document_toc' AND owner_id=old.id;
END;

-- entity → indexes name, description
CREATE TRIGGER trg_entity_ai
AFTER INSERT ON entity BEGIN
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'entity', new.id, 'name', new.name WHERE new.name IS NOT NULL AND new.name <> '';
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'entity', new.id, 'description', new.description WHERE new.description IS NOT NULL AND new.description <> '';
END;
CREATE TRIGGER trg_entity_au
AFTER UPDATE ON entity BEGIN
  DELETE FROM search WHERE owner_table='entity' AND owner_id=old.id;
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'entity', new.id, 'name', new.name WHERE new.name IS NOT NULL AND new.name <> '';
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'entity', new.id, 'description', new.description WHERE new.description IS NOT NULL AND new.description <> '';
END;
CREATE TRIGGER trg_entity_ad
AFTER DELETE ON entity BEGIN
  DELETE FROM search WHERE owner_table='entity' AND owner_id=old.id;
END;

-- query_attrs → indexes text
CREATE TRIGGER trg_query_ai
AFTER INSERT ON query_attrs BEGIN
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'query', new.entity_id, 'text', new.text WHERE new.text IS NOT NULL AND new.text <> '';
END;
CREATE TRIGGER trg_query_au
AFTER UPDATE ON query_attrs BEGIN
  DELETE FROM search WHERE owner_table='query' AND owner_id=old.entity_id;
  INSERT INTO search(owner_table, owner_id, field, text)
    SELECT 'query', new.entity_id, 'text', new.text WHERE new.text IS NOT NULL AND new.text <> '';
END;
CREATE TRIGGER trg_query_ad
AFTER DELETE ON query_attrs BEGIN
  DELETE FROM search WHERE owner_table='query' AND owner_id=old.entity_id;
END;
