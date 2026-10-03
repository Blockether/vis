import {
  bytes,
  canonical,
  clip,
  councilDefinitions,
  definitions,
  digest,
  fail,
  limits,
  ownedSession,
  rows,
  text,
  validate,
} from "./protocol";
import type { Data, Database } from "./protocol";

export async function getEntry(
  db: Database,
  roomId: string,
  id: number,
): Promise<Data> {
  const found = await db
    .prepare("SELECT * FROM room_entries WHERE room_id = ? AND entry_id = ?")
    .bind(roomId, id)
    .first<Data>();
  if (!found) fail(404, "not_found", "Entry does not exist in this room");
  const deliveries = await rows(
    db,
    "SELECT session_id, state, reply_entry_id FROM room_deliveries WHERE entry_id = ? ORDER BY session_id",
    id,
  );
  const result: Data = {
    entry_id: found.entry_id,
    thread_id: found.thread_id,
    group_id: roomId,
    kind: found.kind,
    content: found.content,
    author_session_id: found.author_session_id,
    created_at: found.created_at,
    source: "sdk",
    ping: deliveries.map((row) => row.session_id),
  };
  if (found.title !== null) result.title = found.title;
  if (found.reply_to !== null) result.reply_to = found.reply_to;
  if (found.reply_required) {
    result.reply_required = true;
    result.replies = deliveries.map((row) => ({
      session_id: row.session_id,
      state: row.state,
      ...(row.reply_entry_id === null
        ? {}
        : { reply_entry_id: row.reply_entry_id }),
    }));
  }
  return result;
}

export async function publish(
  db: Database,
  roomId: string,
  machineId: string,
  body: Data,
  now: number,
  selfWake = false,
): Promise<Data> {
  const sid = body.session_id as string;
  const author = await ownedSession(db, roomId, machineId, sid, now);
  if (
    selfWake
      ? !author.wake_allowed || author.state === "held"
      : !["running", "queued"].includes(author.state)
  )
    fail(409, "inactive_session", "Author session cannot publish this event");
  const input = { ...body.publication };
  if (input.reply_to !== undefined || input.thread_id !== undefined)
    delete input.title;
  validate("publication", { session_id: sid, publication: input });
  if (input.group_id !== undefined && input.group_id !== roomId)
    fail(403, "forbidden", "Group is not this room");
  text(
    input.content,
    councilDefinitions.publish.properties.content["x-vis-max-utf8-bytes"],
  );
  const key = input.idempotency_key ?? crypto.randomUUID();
  text(
    key,
    councilDefinitions.publish.properties.idempotency_key[
      "x-vis-max-utf8-bytes"
    ],
  );
  const normalized = {
    ...input,
    group_id: roomId,
    idempotency_key: key,
    self_wake: selfWake,
  };
  if (Array.isArray(input.ping))
    normalized.ping = [
      ...new Set(
        input.ping.map((id: string) => id.replace(/^vis_session_id#/, "")),
      ),
    ].sort();
  const fingerprint = await digest(canonical(normalized));
  const replay = await db
    .prepare(
      "SELECT * FROM room_entries WHERE room_id = ? AND author_session_id = ? AND idempotency_key = ?",
    )
    .bind(roomId, sid, key)
    .first<Data>();
  if (replay) {
    if (replay.fingerprint !== fingerprint)
      fail(409, "conflict", "Idempotency key names a different publication");
    return getEntry(db, roomId, replay.entry_id);
  }
  let replyTo: number | null = input.reply_to ?? null;
  let thread: number | null = input.thread_id ?? null;
  let targets: string[] = [];
  const required = input.reply_required === true;
  if (thread !== null) {
    const root = await db
      .prepare(
        "SELECT entry_id FROM room_entries WHERE room_id = ? AND entry_id = ? AND thread_id = entry_id",
      )
      .bind(roomId, thread)
      .first();
    if (!root)
      fail(400, "invalid_thread", "Thread must be a root in this room");
  }
  if (
    thread !== null &&
    replyTo === null &&
    !required &&
    (!input.ping || input.ping.length === 0)
  ) {
    const pending = await db
      .prepare(
        `SELECT e.entry_id FROM room_entries e JOIN room_deliveries d USING(entry_id)
      WHERE e.room_id = ? AND e.thread_id = ? AND e.reply_required = 1 AND d.session_id = ? AND d.state != 'replied'
      ORDER BY e.entry_id DESC LIMIT 1`,
      )
      .bind(roomId, thread, sid)
      .first<Data>();
    replyTo = pending?.entry_id ?? null;
  }
  if (replyTo !== null) {
    const request = await getEntry(db, roomId, replyTo);
    if (
      request.reply_to ||
      required ||
      !request.ping.includes(sid) ||
      (thread !== null && thread !== request.thread_id) ||
      (input.ping &&
        (input.ping === "all" ||
          canonical(normalized.ping) !==
            canonical([request.author_session_id])))
    ) {
      fail(
        400,
        "invalid_reply",
        "Reply must answer a request addressed to this session",
      );
    }
    thread = request.thread_id;
    targets = [request.author_session_id];
  } else if (input.ping === "all") {
    targets = (
      await rows(
        db,
        `SELECT p.session_id FROM room_presence p JOIN room_sessions s USING(session_id)
      JOIN room_memberships m ON m.machine_id = s.machine_id AND m.room_id = p.room_id
      WHERE p.room_id = ? AND p.expires_at > ? AND p.state != 'idle' AND m.revoked_at IS NULL AND p.session_id != ?
      ORDER BY p.session_id`,
        roomId,
        now,
        sid,
      )
    ).map((row) => row.session_id);
  } else {
    targets = normalized.ping ?? [];
  }
  if (
    (required && targets.length === 0) ||
    targets.length > councilDefinitions.entry.properties.ping.maxItems
  ) {
    fail(
      400,
      "invalid_recipient",
      "Publication needs a bounded recipient list",
    );
  }
  const deliveries: { sid: string; state: string }[] = [];
  for (const target of targets) {
    const [recipient] = await rows(
      db,
      `SELECT p.* FROM room_presence p JOIN room_sessions s USING(session_id)
      JOIN room_memberships m ON m.machine_id = s.machine_id AND m.room_id = p.room_id
      WHERE p.room_id = ? AND p.session_id = ? AND m.revoked_at IS NULL`,
      roomId,
      target,
    );
    if (!recipient || (target === sid && !selfWake))
      fail(
        400,
        "invalid_recipient",
        "Recipient is not another session in this room",
      );
    const available =
      recipient.expires_at > now &&
      (recipient.state !== "idle" || recipient.wake_allowed);
    deliveries.push({
      sid: target,
      state: available ? "pending" : "unavailable",
    });
  }
  const titleLimit =
    councilDefinitions.publish.properties.title["x-vis-max-utf8-bytes"];
  let title: string | null = null;
  if (thread === null) {
    title =
      input.title?.trim() ??
      clip(
        input.content
          .split(/\r?\n/)
          .map((line: string) => line.trim())
          .find(Boolean) ?? "",
        titleLimit,
      );
    text(title!, titleLimit);
    if (/[\r\n\t]/.test(title!))
      fail(400, "invalid_request", "Title must be one line");
  }
  await db
    .prepare(
      `INSERT INTO room_entries (room_id, author_session_id, kind, content, title, thread_id, reply_to, reply_required, created_at, idempotency_key, fingerprint, deliveries_json, self_wake)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT(room_id, author_session_id, idempotency_key) DO NOTHING`,
    )
    .bind(
      roomId,
      sid,
      input.kind,
      input.content,
      title,
      thread,
      replyTo,
      required ? 1 : 0,
      now,
      key,
      fingerprint,
      JSON.stringify(deliveries),
      selfWake ? 1 : 0,
    )
    .run();
  const [created] = await rows(
    db,
    "SELECT entry_id, fingerprint FROM room_entries WHERE room_id = ? AND author_session_id = ? AND idempotency_key = ?",
    roomId,
    sid,
    key,
  );
  if (created.fingerprint !== fingerprint)
    fail(409, "conflict", "Idempotency key names a different publication");
  return getEntry(db, roomId, created.entry_id);
}

export async function page(
  db: Database,
  roomId: string,
  query: URLSearchParams,
  roots: boolean,
): Promise<Data> {
  const allowed = new Set(["after", "limit", ...(roots ? [] : ["thread_id"])]);
  const request: Data = {};
  for (const [key, value] of query) {
    if (
      !allowed.has(key) ||
      !/^\d+$/.test(value) ||
      !Number.isSafeInteger(Number(value)) ||
      key in request
    ) {
      fail(400, "invalid_request", "Invalid page query");
    }
    request[key] = Number(value);
  }
  validate("page_request", request);
  const after = request.after ?? 0;
  const limit =
    request.limit ?? councilDefinitions.page_request.properties.limit.default;
  if (request.thread_id) {
    const root = await getEntry(db, roomId, request.thread_id);
    if (root.entry_id !== root.thread_id)
      fail(400, "invalid_thread", "Thread must be a root in this room");
  }
  const found = await rows(
    db,
    `SELECT entry_id, thread_id, kind, title, author_session_id, created_at FROM room_entries
    WHERE room_id = ? AND entry_id > ? ${roots ? "AND entry_id = thread_id" : ""}
    ${request.thread_id ? "AND thread_id = ?" : ""} ORDER BY entry_id LIMIT ?`,
    roomId,
    after,
    ...(request.thread_id ? [request.thread_id] : []),
    limit + 1,
  );
  const result: Data = { entries: [], after, has_more: false };
  for (const row of found) {
    const entry = roots
      ? {
          thread_id: row.thread_id,
          title: row.title,
          kind: row.kind,
          author_session_id: row.author_session_id,
          created_at: row.created_at,
        }
      : await getEntry(db, roomId, row.entry_id);
    const next = {
      entries: [...result.entries, entry],
      after: row.entry_id,
      has_more: false,
    };
    if (
      result.entries.length >= limit ||
      bytes(JSON.stringify(next)) > limits.response_bytes
    ) {
      result.has_more = true;
      break;
    }
    Object.assign(result, next);
  }
  return result;
}

export async function inbox(
  db: Database,
  roomId: string,
  machineId: string,
  query: URLSearchParams,
  now: number,
): Promise<Data> {
  const input: Data = {};
  for (const [key, value] of query) {
    if (
      key in input ||
      !["session_id", "after", "limit"].includes(key) ||
      (key !== "session_id" && !/^\d+$/.test(value))
    )
      fail(400, "invalid_request", "Invalid inbox query");
    input[key] = key === "session_id" ? value : Number(value);
  }
  validate("inbox_request", input);
  await ownedSession(db, roomId, machineId, input.session_id, now);
  const after =
    input.after ?? definitions.inbox_request.properties.after.default;
  const limit =
    input.limit ?? definitions.inbox_request.properties.limit.default;
  const found = await rows(
    db,
    `SELECT e.entry_id FROM room_entries e JOIN room_deliveries d USING(entry_id)
    WHERE e.room_id = ? AND d.session_id = ? AND e.entry_id > ? AND d.state IN ('pending', 'delivered')
    ORDER BY e.entry_id LIMIT ?`,
    roomId,
    input.session_id,
    after,
    limit + 1,
  );
  const result: Data = { entries: [], after, has_more: false };
  for (const row of found) {
    const entry = await getEntry(db, roomId, row.entry_id);
    const next = {
      entries: [...result.entries, entry],
      after: row.entry_id,
      has_more: false,
    };
    if (
      result.entries.length >= limit ||
      bytes(JSON.stringify(next)) > limits.response_bytes
    ) {
      result.has_more = true;
      break;
    }
    Object.assign(result, next);
  }
  return result;
}

export async function pending(
  db: Database,
  roomId: string,
  machineId: string,
  sid: string,
  now: number,
): Promise<Data[]> {
  await ownedSession(db, roomId, machineId, sid, now);
  const found = await rows(
    db,
    `SELECT e.entry_id FROM room_entries e JOIN room_deliveries d USING(entry_id)
    WHERE e.room_id = ? AND e.reply_required = 1 AND d.session_id = ? AND d.state IN ('pending', 'delivered')
    ORDER BY e.entry_id LIMIT ?`,
    roomId,
    sid,
    50,
  );
  const result: Data[] = [];
  for (const row of found) {
    const entry = await getEntry(db, roomId, row.entry_id);
    if (bytes(JSON.stringify([...result, entry])) > limits.response_bytes)
      break;
    result.push(entry);
  }
  return result;
}

export async function receipts(
  db: Database,
  roomId: string,
  machineId: string,
  body: Data,
  now: number,
): Promise<Data> {
  const statements: D1PreparedStatement[] = [];
  for (const receipt of body.receipts as Data[]) {
    await ownedSession(db, roomId, machineId, receipt.session_id, now);
    const found = await db
      .prepare(
        `SELECT d.state FROM room_deliveries d JOIN room_entries e USING(entry_id)
      WHERE e.room_id = ? AND e.entry_id = ? AND d.session_id = ?`,
      )
      .bind(roomId, receipt.entry_id, receipt.session_id)
      .first();
    if (!found)
      fail(404, "not_found", "Delivery does not exist for this session");
    statements.push(
      db
        .prepare(
          `UPDATE room_deliveries SET state = ? WHERE entry_id = ? AND session_id = ?
      AND state IN ('pending', 'delivered')`,
        )
        .bind(receipt.state, receipt.entry_id, receipt.session_id),
    );
  }
  if (statements.length) await db.batch(statements);
  return { ok: true };
}
