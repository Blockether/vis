import {
  deletedMachine,
  digest,
  fail,
  machine,
  member,
  room,
  rows,
  limits,
} from "./protocol";
import type { Data, Database } from "./protocol";

export async function register(
  db: Database,
  body: Data,
  now: number,
): Promise<Data> {
  const hash = await digest(body.credential);
  const prior = await db
    .prepare("SELECT * FROM room_machines WHERE machine_id = ?")
    .bind(body.machine_id)
    .first<Data>();
  if (prior && (prior.credential_hash !== hash || prior.name !== body.name)) {
    fail(409, "conflict", "Machine identity already exists");
  }
  await db
    .prepare(
      `INSERT INTO room_machines (machine_id, name, credential_hash, can_create_rooms, created_at)
    VALUES (?, ?, ?, ?, ?) ON CONFLICT(machine_id) DO NOTHING`,
    )
    .bind(body.machine_id, body.name, hash, body.can_create_rooms ? 1 : 0, now)
    .run();
  const result = await db
    .prepare("SELECT * FROM room_machines WHERE machine_id = ?")
    .bind(body.machine_id)
    .first<Data>();
  if (!result || result.credential_hash !== hash)
    fail(409, "conflict", "Machine identity already exists");
  return machine(result);
}

export async function createRoom(
  db: Database,
  body: Data,
  actor: Data | null,
  now: number,
): Promise<Data> {
  if (
    actor &&
    (!actor.can_create_rooms || actor.machine_id !== body.owner_machine_id)
  ) {
    fail(
      403,
      "forbidden",
      "Room creation needs an administrator or a moderator",
    );
  }
  const owner = await db
    .prepare(
      `SELECT machine_id FROM room_machines WHERE machine_id = ? AND NOT ${deletedMachine}`,
    )
    .bind(body.owner_machine_id)
    .first();
  if (!owner) fail(404, "not_found", "Owner machine does not exist");
  const prior = await db
    .prepare("SELECT * FROM council_rooms WHERE room_id = ?")
    .bind(body.room_id)
    .first<Data>();
  if (prior) {
    if (
      prior.deleted_at !== null ||
      prior.name !== body.name ||
      prior.owner_machine_id !== body.owner_machine_id
    ) {
      fail(409, "conflict", "Room identity already exists");
    }
    return room(prior);
  }
  await db.batch([
    db
      .prepare(
        `INSERT INTO council_rooms (room_id, name, owner_machine_id, created_at) VALUES (?, ?, ?, ?)`,
      )
      .bind(body.room_id, body.name, body.owner_machine_id, now),
    db
      .prepare(
        `INSERT INTO room_memberships (room_id, machine_id, role, joined_at) VALUES (?, ?, 'owner', ?)`,
      )
      .bind(body.room_id, body.owner_machine_id, now),
  ]);
  return { ...body, created_at: now };
}

export async function createInvite(
  db: Database,
  roomId: string,
  body: Data,
  origin: string,
  now: number,
): Promise<Data> {
  if (
    body.expires_at <= now ||
    body.expires_at > now + limits.invite_lifetime_ms
  ) {
    fail(400, "invalid_request", "Invite expiry must be within seven days");
  }
  const hash = await digest(body.token);
  const maxUses = body.max_uses ?? 1;
  await db
    .prepare(
      `INSERT INTO room_invites (invite_id, room_id, token_hash, expires_at, max_uses)
    VALUES (?, ?, ?, ?, ?) ON CONFLICT(invite_id) DO NOTHING`,
    )
    .bind(body.invite_id, roomId, hash, body.expires_at, maxUses)
    .run();
  const found = await db
    .prepare("SELECT * FROM room_invites WHERE invite_id = ?")
    .bind(body.invite_id)
    .first<Data>();
  if (
    !found ||
    found.room_id !== roomId ||
    found.token_hash !== hash ||
    found.expires_at !== body.expires_at ||
    found.max_uses !== maxUses
  ) {
    fail(409, "conflict", "Invite identity already exists");
  }
  return {
    invite: {
      invite_id: found.invite_id,
      room_id: roomId,
      expires_at: found.expires_at,
      max_uses: maxUses,
      uses: found.uses,
      revoked: found.revoked_at !== null,
    },
    invite_url: `${origin}/rooms/join#invite=${body.token}`,
  };
}

export async function join(
  db: Database,
  body: Data,
  credential: string,
  now: number,
): Promise<Data> {
  const credentialHash = await digest(credential);
  const found = await db
    .prepare("SELECT * FROM room_invites WHERE token_hash = ?")
    .bind(await digest(body.invite_token))
    .first<Data>();
  if (!found) fail(410, "invite_unavailable", "Invite is unavailable");
  const existing = await db
    .prepare("SELECT * FROM room_machines WHERE machine_id = ?")
    .bind(body.machine_id)
    .first<Data>();
  if (existing && existing.credential_hash !== credentialHash)
    fail(403, "forbidden", "Machine identity already exists");
  const prior = await db
    .prepare(
      "SELECT * FROM room_redemptions WHERE invite_id = ? AND request_id = ?",
    )
    .bind(found.invite_id, body.request_id)
    .first<Data>();
  if (
    prior &&
    (prior.machine_id !== body.machine_id ||
      prior.credential_hash !== credentialHash)
  ) {
    fail(409, "conflict", "Redemption identity already exists");
  }
  if (!prior) {
    if (
      found.revoked_at !== null ||
      found.expires_at <= now ||
      found.uses >= found.max_uses
    ) {
      fail(410, "invite_unavailable", "Invite is unavailable");
    }
    await db.batch([
      db
        .prepare(
          `INSERT INTO room_machines (machine_id, name, credential_hash, created_at)
        VALUES (?, ?, ?, ?) ON CONFLICT(machine_id) DO NOTHING`,
        )
        .bind(body.machine_id, body.machine_name, credentialHash, now),
      db
        .prepare(
          `INSERT INTO room_redemptions (invite_id, request_id, machine_id, credential_hash, created_at)
        VALUES (?, ?, ?, ?, ?) ON CONFLICT(invite_id, request_id) DO NOTHING`,
        )
        .bind(
          found.invite_id,
          body.request_id,
          body.machine_id,
          credentialHash,
          now,
        ),
    ]);
  }
  const redemption = await db
    .prepare(
      "SELECT * FROM room_redemptions WHERE invite_id = ? AND request_id = ?",
    )
    .bind(found.invite_id, body.request_id)
    .first<Data>();
  if (
    !redemption ||
    redemption.machine_id !== body.machine_id ||
    redemption.credential_hash !== credentialHash
  ) {
    fail(409, "conflict", "Redemption identity already exists");
  }
  await member(db, found.room_id, body.machine_id);
  const [joinedRoom] = await rows(
    db,
    "SELECT * FROM council_rooms WHERE room_id = ?",
    found.room_id,
  );
  const [joinedMachine] = await rows(
    db,
    "SELECT * FROM room_machines WHERE machine_id = ?",
    body.machine_id,
  );
  return { room: room(joinedRoom), machine: machine(joinedMachine) };
}

export async function presence(
  db: Database,
  roomId: string,
  machineId: string,
  body: Data,
  now: number,
): Promise<Data> {
  const ids = body.sessions.map(
    (session: Data) => session.session_id,
  ) as string[];
  if (new Set(ids).size !== ids.length)
    fail(400, "invalid_request", "Session IDs must be unique");
  const expires = now + (body.lease_seconds ?? 60) * 1000;
  const statements = [
    db
      .prepare(
        `DELETE FROM room_presence WHERE room_id = ? AND session_id IN
    (SELECT session_id FROM room_sessions WHERE machine_id = ?)`,
      )
      .bind(roomId, machineId),
  ];
  for (const session of body.sessions as Data[]) {
    statements.push(
      db
        .prepare(
          `INSERT INTO room_sessions (session_id, machine_id) VALUES (?, ?)
        ON CONFLICT(session_id) DO UPDATE SET machine_id = excluded.machine_id`,
        )
        .bind(session.session_id, machineId),
      db
        .prepare(
          `INSERT INTO room_presence (room_id, session_id, title, state, wake_allowed, expires_at)
        VALUES (?, ?, ?, ?, ?, ?)`,
        )
        .bind(
          roomId,
          session.session_id,
          session.title,
          session.state,
          session.wake_allowed ? 1 : 0,
          expires,
        ),
    );
  }
  statements.push(
    db
      .prepare(
        `UPDATE room_deliveries SET state = 'unavailable' WHERE state IN ('pending', 'delivered')
    AND session_id IN (SELECT session_id FROM room_sessions WHERE machine_id = ?)
    AND entry_id IN (SELECT entry_id FROM room_entries WHERE room_id = ?)
    AND session_id NOT IN (SELECT session_id FROM room_presence WHERE room_id = ?)`,
      )
      .bind(machineId, roomId, roomId),
  );
  await db.batch(statements);
  return { expires_at: expires };
}

/** Delete children before parents, so that each statement keeps foreign keys valid. */
function purge(
  db: Database,
  rooms: string,
  value: string,
): D1PreparedStatement[] {
  const scoped = (sql: string) => db.prepare(sql).bind(value);
  return [
    scoped(
      `DELETE FROM room_deliveries WHERE entry_id IN (SELECT entry_id FROM room_entries WHERE room_id IN (${rooms}))`,
    ),
    scoped(`DELETE FROM room_entries WHERE room_id IN (${rooms})`),
    scoped(`DELETE FROM room_presence WHERE room_id IN (${rooms})`),
    scoped(
      `DELETE FROM room_redemptions WHERE invite_id IN (SELECT invite_id FROM room_invites WHERE room_id IN (${rooms}))`,
    ),
    scoped(`DELETE FROM room_invites WHERE room_id IN (${rooms})`),
    scoped(`DELETE FROM room_memberships WHERE room_id IN (${rooms})`),
    scoped(
      `DELETE FROM council_rooms WHERE room_id IN (${rooms}) RETURNING room_id`,
    ),
  ];
}

/** Remove deleted machines and their sessions when no history uses them. */
function collect(db: Database): D1PreparedStatement[] {
  return [
    db.prepare(
      `DELETE FROM room_sessions WHERE machine_id IN (SELECT machine_id FROM room_machines WHERE ${deletedMachine})
    AND session_id NOT IN (SELECT author_session_id FROM room_entries)
    AND session_id NOT IN (SELECT session_id FROM room_deliveries)
    AND session_id NOT IN (SELECT session_id FROM room_presence)`,
    ),
    db.prepare(
      `DELETE FROM room_machines WHERE ${deletedMachine}
    AND machine_id NOT IN (SELECT machine_id FROM room_sessions)
    AND machine_id NOT IN (SELECT machine_id FROM room_memberships)
    AND machine_id NOT IN (SELECT machine_id FROM room_redemptions)
    AND machine_id NOT IN (SELECT owner_machine_id FROM council_rooms)`,
    ),
  ];
}

export async function deleteRoom(db: Database, roomId: string): Promise<void> {
  await db.batch([...purge(db, "?", roomId), ...collect(db)]);
}

/** Delete a credential and its rooms. Entries in other rooms keep their author. */
export async function deleteMachine(
  db: Database,
  machineId: string,
): Promise<Data> {
  const found = await db
    .prepare(
      `SELECT machine_id FROM room_machines WHERE machine_id = ? AND NOT ${deletedMachine}`,
    )
    .bind(machineId)
    .first();
  if (!found) fail(404, "not_found", "Machine does not exist");
  const sessions = "SELECT session_id FROM room_sessions WHERE machine_id = ?";
  const owned = purge(
    db,
    "SELECT room_id FROM council_rooms WHERE owner_machine_id = ?",
    machineId,
  );
  const results = await db.batch<Data>([
    ...owned,
    db
      .prepare(`DELETE FROM room_presence WHERE session_id IN (${sessions})`)
      .bind(machineId),
    db
      .prepare("DELETE FROM room_memberships WHERE machine_id = ?")
      .bind(machineId),
    db
      .prepare("DELETE FROM room_redemptions WHERE machine_id = ?")
      .bind(machineId),
    db
      .prepare(
        `UPDATE room_deliveries SET state = 'unavailable' WHERE state IN ('pending', 'delivered')
    AND session_id IN (${sessions})`,
      )
      .bind(machineId),
    db
      .prepare(
        `UPDATE room_machines SET name = 'Deleted machine', credential_hash = 'deleted:' || machine_id,
    can_create_rooms = 0 WHERE machine_id = ?`,
      )
      .bind(machineId),
    ...collect(db),
    db
      .prepare("SELECT machine_id FROM room_machines WHERE machine_id = ?")
      .bind(machineId),
  ]);
  return {
    machine_id: machineId,
    deleted_rooms: results[owned.length - 1].results.length,
    retained_history: results[results.length - 1].results.length > 0,
  };
}
