-- Generated from rooms.json. Run npm run contracts.
INSERT INTO room_limits (name, value) VALUES ('request_bytes', 262144) ON CONFLICT(name) DO UPDATE SET value = excluded.value;

INSERT INTO room_limits (name, value) VALUES ('response_bytes', 262144) ON CONFLICT(name) DO UPDATE SET value = excluded.value;

INSERT INTO room_limits (name, value) VALUES ('invite_lifetime_ms', 604800000) ON CONFLICT(name) DO UPDATE SET value = excluded.value;

INSERT INTO room_limits (name, value) VALUES ('rooms_per_machine', 32) ON CONFLICT(name) DO UPDATE SET value = excluded.value;

INSERT INTO room_limits (name, value) VALUES ('machines_per_room', 256) ON CONFLICT(name) DO UPDATE SET value = excluded.value;

INSERT INTO room_limits (name, value) VALUES ('sessions_per_room', 256) ON CONFLICT(name) DO UPDATE SET value = excluded.value;
