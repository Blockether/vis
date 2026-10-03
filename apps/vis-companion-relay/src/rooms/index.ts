import type { Deps, Env } from "../types";
import schema from "../../../../packages/vis-contract/resources/vis-contract/schema/rooms.json";
import {
  RoomError,
  definitions,
  digest,
  fail,
  headers,
  limits,
  machine,
  member,
  response,
  room,
  rows,
  validate,
} from "./protocol";
import type { Data } from "./protocol";
import {
  createInvite,
  createRoom,
  join,
  presence,
  register,
} from "./management";
import { getEntry, inbox, page, pending, publish, receipts } from "./entries";

const routes = schema["x-vis-http"].map((route) => ({
  ...route,
  pattern: new RegExp(
    `^${route.path.replace(/\{([^}]+)\}/g, "(?<$1>[^/]+)")}$`,
  ),
}));
type Reader = (request: Request, limit: number) => Promise<unknown>;

async function route(
  request: Request,
  env: Env,
  deps: Deps,
  readJson: Reader,
): Promise<Response> {
  const url = new URL(request.url);
  const path = url.pathname.replace(/\/+$/, "");
  if (request.method === "OPTIONS") {
    return new Response(null, {
      status: 204,
      headers: {
        ...headers(),
        "access-control-allow-methods": "GET, POST, PATCH, DELETE, OPTIONS",
        "access-control-allow-headers": "authorization, content-type",
        "access-control-max-age": "86400",
      },
    });
  }
  if (path === "/rooms/join" && request.method === "GET") {
    return new Response(
      `<!doctype html><html lang="en"><meta charset="utf-8"><meta name="referrer" content="no-referrer">
      <title>Join a Vis Council room</title><h1>Join a Vis Council room</h1>
      <p>Copy this complete link. In Vis Settings, open Council rooms and select Join room.</p>
      <p>Opening this page does not join a room. Confirm the machine and sharing scope in Vis.</p>
      <p>The relay operator can read room messages. Room membership does not give access to local files.</p></html>`,
      {
        headers: {
          ...headers(),
          "content-type": "text/html; charset=utf-8",
          "content-security-policy":
            "default-src 'none'; frame-ancestors 'none'",
        },
      },
    );
  }
  const found = routes.find(
    (candidate) =>
      candidate.method === request.method && candidate.pattern.test(path),
  );
  if (!found) fail(404, "not_found", "Unknown Rooms route");
  if (!env.ROOMS_DB || !env.ROOMS_ADDRESS_LIMIT || !env.ROOMS_MACHINE_LIMIT) {
    fail(
      503,
      "rooms_unconfigured",
      "Rooms storage and rate limits are not configured",
    );
  }
  const ip =
    request.headers.get("cf-connecting-ip") ??
    request.headers.get("x-forwarded-for") ??
    "anon";
  if (!(await env.ROOMS_ADDRESS_LIMIT.limit({ key: ip })).success)
    fail(429, "rate_limited", "Rooms address limit reached");
  if (Number(request.headers.get("content-length") ?? 0) > limits.request_bytes)
    fail(413, "too_large", "Rooms request is too large");
  const token =
    request.headers
      .get("authorization")
      ?.match(/^Bearer ([A-Za-z0-9_-]+)$/i)?.[1] ?? "";
  if (!new RegExp(definitions.secret.pattern).test(token))
    fail(401, "unauthorized", "Rooms credential is required");
  const hash = await digest(token);
  const isAdmin = Boolean(
    env.ROOMS_ADMIN_TOKEN && hash === (await digest(env.ROOMS_ADMIN_TOKEN)),
  );
  if (!(await env.ROOMS_MACHINE_LIMIT.limit({ key: hash })).success)
    fail(429, "rate_limited", "Rooms credential limit reached");
  const db = env.ROOMS_DB.withSession("first-primary");
  const actor = isAdmin
    ? null
    : await db
        .prepare("SELECT * FROM room_machines WHERE credential_hash = ?")
        .bind(hash)
        .first<Data>();
  if (found.auth === "admin" && !isAdmin)
    fail(403, "forbidden", "Rooms administrator is required");
  if (!isAdmin && !actor && found.auth !== "machine-or-new")
    fail(401, "unauthorized", "Rooms credential is not registered");
  if (isAdmin && !["admin", "creator", "owner"].includes(found.auth))
    fail(403, "forbidden", "Use a room machine credential");
  const params = found.pattern.exec(path)?.groups ?? {};
  for (const [name, value] of Object.entries(params)) {
    if (name === "entry_id") {
      if (!/^[1-9]\d*$/.test(value) || !Number.isSafeInteger(Number(value)))
        fail(400, "invalid_request", "Invalid entry ID");
    } else validate("id", value);
  }
  if (
    url.search &&
    !["entry_page", "thread_page", "pending"].includes(found.response)
  ) {
    fail(400, "invalid_request", "This Rooms route has no query parameters");
  }
  let body: Data = {};
  if (found.request) {
    if (
      !request.headers
        .get("content-type")
        ?.toLowerCase()
        .startsWith("application/json")
    ) {
      fail(400, "invalid_request", "Rooms requests need application/json");
    }
    const raw = await readJson(request, limits.request_bytes);
    if (typeof raw === "symbol")
      fail(413, "too_large", "Rooms request is too large");
    if (found.request === "publication" && raw && typeof raw === "object") {
      const publication = (raw as Data).publication;
      if (
        publication &&
        (publication.thread_id !== undefined ||
          publication.reply_to !== undefined)
      )
        delete publication.title;
    }
    body = validate(found.request as Parameters<typeof validate>[0], raw);
  }
  const now = deps.now();
  const roomId = params.room_id;
  if (roomId) {
    if (isAdmin) {
      const exists = await db
        .prepare(
          "SELECT room_id FROM council_rooms WHERE room_id = ? AND deleted_at IS NULL",
        )
        .bind(roomId)
        .first();
      if (!exists) fail(404, "not_found", "Room does not exist");
    } else {
      const membership = await member(db, roomId, actor!.machine_id);
      if (found.auth === "owner" && membership.role !== "owner")
        fail(403, "forbidden", "Room owner is required");
      if (
        found.auth === "owner-or-self" &&
        membership.role !== "owner" &&
        params.machine_id !== actor!.machine_id
      ) {
        fail(403, "forbidden", "Room owner or departing member is required");
      }
    }
  }
  let result: unknown;
  switch (`${found.method} ${found.path}`) {
    case "POST /v1/rooms/machines":
      result = await register(db, body, now);
      break;
    case "PATCH /v1/rooms/machines/{machine_id}": {
      const updated = await db
        .prepare(
          "UPDATE room_machines SET can_create_rooms = ? WHERE machine_id = ? RETURNING *",
        )
        .bind(body.can_create_rooms ? 1 : 0, params.machine_id)
        .first<Data>();
      if (!updated) fail(404, "not_found", "Machine does not exist");
      result = machine(updated);
      break;
    }
    case "GET /v1/rooms/machine":
      result = machine(actor!);
      break;
    case "POST /v1/rooms":
      result = await createRoom(db, body, actor, now);
      break;
    case "GET /v1/rooms":
      result = (
        await rows(
          db,
          `SELECT r.* FROM council_rooms r JOIN room_memberships m USING(room_id)
      WHERE m.machine_id = ? AND m.revoked_at IS NULL AND r.deleted_at IS NULL ORDER BY r.created_at, r.room_id`,
          actor!.machine_id,
        )
      ).map(room);
      break;
    case "POST /v1/rooms/join":
      result = await join(db, body, token, now);
      break;
    case "DELETE /v1/rooms/{room_id}":
      await db
        .prepare("UPDATE council_rooms SET deleted_at = ? WHERE room_id = ?")
        .bind(now, roomId)
        .run();
      result = { ok: true };
      break;
    case "POST /v1/rooms/{room_id}/invites":
      result = await createInvite(db, roomId, body, url.origin, now);
      break;
    case "DELETE /v1/rooms/{room_id}/invites/{invite_id}":
      await db
        .prepare(
          "UPDATE room_invites SET revoked_at = ? WHERE room_id = ? AND invite_id = ?",
        )
        .bind(now, roomId, params.invite_id)
        .run();
      result = { ok: true };
      break;
    case "GET /v1/rooms/{room_id}/members":
      result = await rows(
        db,
        `SELECT m.machine_id, a.name, m.role, m.joined_at
      FROM room_memberships m JOIN room_machines a USING(machine_id) WHERE m.room_id = ? AND m.revoked_at IS NULL ORDER BY m.machine_id`,
        roomId,
      );
      break;
    case "DELETE /v1/rooms/{room_id}/members/{machine_id}": {
      const target = await member(db, roomId, params.machine_id);
      if (target.role === "owner")
        fail(409, "conflict", "Delete the room instead of removing its owner");
      await db.batch([
        db
          .prepare(
            "UPDATE room_memberships SET revoked_at = ? WHERE room_id = ? AND machine_id = ?",
          )
          .bind(now, roomId, params.machine_id),
        db
          .prepare(
            `DELETE FROM room_presence WHERE room_id = ? AND session_id IN (SELECT session_id FROM room_sessions WHERE machine_id = ?)`,
          )
          .bind(roomId, params.machine_id),
        db
          .prepare(
            `UPDATE room_deliveries SET state = 'unavailable' WHERE state IN ('pending', 'delivered')
          AND entry_id IN (SELECT entry_id FROM room_entries WHERE room_id = ?)
          AND session_id IN (SELECT session_id FROM room_sessions WHERE machine_id = ?)`,
          )
          .bind(roomId, params.machine_id),
      ]);
      result = { ok: true };
      break;
    }
    case "POST /v1/rooms/{room_id}/presence":
      result = await presence(db, roomId, actor!.machine_id, body, now);
      break;
    case "GET /v1/rooms/{room_id}/sessions":
      result = await rows(
        db,
        `SELECT p.session_id, p.title, p.state FROM room_presence p
      JOIN room_sessions s USING(session_id) JOIN room_memberships m ON m.room_id = p.room_id AND m.machine_id = s.machine_id
      WHERE p.room_id = ? AND p.expires_at > ? AND p.state != 'idle' AND m.revoked_at IS NULL ORDER BY p.session_id`,
        roomId,
        now,
      );
      break;
    case "POST /v1/rooms/{room_id}/entries":
      result = await publish(db, roomId, actor!.machine_id, body, now);
      break;
    case "POST /v1/rooms/{room_id}/wake":
      result = await publish(
        db,
        roomId,
        actor!.machine_id,
        {
          session_id: body.session_id,
          publication: { ...body.event, ping: [body.session_id] },
        },
        now,
        true,
      );
      break;
    case "GET /v1/rooms/{room_id}/inbox":
      result = await inbox(
        db,
        roomId,
        actor!.machine_id,
        url.searchParams,
        now,
      );
      break;
    case "GET /v1/rooms/{room_id}/entries":
      result = await page(db, roomId, url.searchParams, false);
      break;
    case "GET /v1/rooms/{room_id}/threads":
      result = await page(db, roomId, url.searchParams, true);
      break;
    case "GET /v1/rooms/{room_id}/entries/{entry_id}":
      result = await getEntry(db, roomId, Number(params.entry_id));
      break;
    case "GET /v1/rooms/{room_id}/pending": {
      const values = [...url.searchParams];
      if (values.length !== 1 || values[0][0] !== "session_id")
        fail(400, "invalid_request", "Pending query needs one session ID");
      validate("id", values[0][1]);
      result = await pending(db, roomId, actor!.machine_id, values[0][1], now);
      break;
    }
    case "POST /v1/rooms/{room_id}/receipts":
      result = await receipts(db, roomId, actor!.machine_id, body, now);
      break;
    default:
      fail(404, "not_found", "Unknown Rooms operation");
  }
  return response(found.response as Parameters<typeof response>[0], result);
}

export async function handleRooms(
  request: Request,
  env: Env,
  deps: Deps,
  readJson: Reader,
): Promise<Response> {
  try {
    return await route(request, env, deps, readJson);
  } catch (error) {
    let failure =
      error instanceof RoomError
        ? error
        : new RoomError(
            503,
            "rooms_unavailable",
            "Rooms is temporarily unavailable",
          );
    if (!(error instanceof RoomError) && error instanceof Error) {
      if (error.message.includes("invite_unavailable"))
        failure = new RoomError(
          410,
          "invite_unavailable",
          "Invite is unavailable",
        );
      else if (error.message.includes("room_capacity"))
        failure = new RoomError(409, "capacity", "Room capacity reached");
      else if (error.message.includes("room_forbidden"))
        failure = new RoomError(403, "forbidden", "Room access changed");
      else if (
        error.message.includes("room_identity") ||
        error.message.includes("UNIQUE constraint")
      ) {
        failure = new RoomError(
          409,
          "conflict",
          "Identity or idempotency conflict",
        );
      }
    }
    return new Response(
      JSON.stringify({
        error: { code: failure.code, message: failure.message },
      }),
      {
        status: failure.status,
        headers: headers(),
      },
    );
  }
}
