CREATE TABLE IF NOT EXISTS provider_request_zone (
  id                              TEXT PRIMARY KEY NOT NULL,
  session_turn_iteration_id        TEXT NOT NULL
                                  REFERENCES session_turn_iteration(id) ON DELETE CASCADE,
  message_index                   INTEGER NOT NULL CHECK (message_index >= 0),
  zone_index                      INTEGER NOT NULL CHECK (zone_index >= 0),
  zone                            TEXT NOT NULL,
  zone_id                         TEXT NOT NULL,
  cache_class                     TEXT NOT NULL,
  role                            TEXT NOT NULL,
  scope                           TEXT,
  source                          TEXT,
  content_sha256                  TEXT NOT NULL,
  char_count                      INTEGER NOT NULL CHECK (char_count >= 0),
  byte_count                      INTEGER NOT NULL CHECK (byte_count >= 0),
  estimated_tokens                INTEGER CHECK (
                                    estimated_tokens IS NULL OR estimated_tokens >= 0
                                  ),
  provider_input_tokens           INTEGER CHECK (
                                    provider_input_tokens IS NULL OR provider_input_tokens >= 0
                                  ),
  provider_cache_write_tokens     INTEGER CHECK (
                                    provider_cache_write_tokens IS NULL OR provider_cache_write_tokens >= 0
                                  ),
  provider_cache_read_tokens      INTEGER CHECK (
                                    provider_cache_read_tokens IS NULL OR provider_cache_read_tokens >= 0
                                  ),
  cost_usd                        REAL CHECK (
                                    cost_usd IS NULL OR cost_usd >= 0
                                  ),
  content                         TEXT NOT NULL,
  created_at                      INTEGER NOT NULL,

  UNIQUE (session_turn_iteration_id, message_index, zone_index)
);

CREATE INDEX IF NOT EXISTS idx_provider_request_zone_iteration
  ON provider_request_zone(session_turn_iteration_id, message_index, zone_index);

CREATE INDEX IF NOT EXISTS idx_provider_request_zone_zone
  ON provider_request_zone(zone, cache_class);
