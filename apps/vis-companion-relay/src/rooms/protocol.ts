import schema from "../../../../packages/vis-contract/resources/vis-contract/schema/rooms.json";
import council from "../../../../packages/vis-contract/resources/vis-contract/schema/council.json";
import * as validators from "./generated/validators.js";
import { sha256Hex } from "../jwt";

export const limits = schema["x-vis-limits"];
export const definitions = schema.$defs;
export const councilDefinitions = council.$defs;
export type Data = Record<string, any>;
export type Database = D1DatabaseSession;

export class RoomError extends Error {
  constructor(
    readonly status: number,
    readonly code: string,
    message: string,
  ) {
    super(message);
  }
}

export function fail(status: number, code: string, message: string): never {
  throw new RoomError(status, code, message);
}

export function validate(name: keyof typeof validators, value: unknown): Data {
  if (!validators[name](value))
    fail(400, "invalid_request", `Invalid rooms.${name} contract`);
  return value as Data;
}

export function response(
  name: keyof typeof validators,
  value: unknown,
): Response {
  if (!validators[name](value))
    fail(503, "rooms_unavailable", "Rooms response failed validation");
  const text = JSON.stringify(value);
  if (bytes(text) > limits.response_bytes)
    fail(503, "rooms_unavailable", "Rooms response is too large");
  return new Response(text, { headers: headers() });
}

export function headers(): Record<string, string> {
  return {
    "content-type": "application/json; charset=utf-8",
    "cache-control": "no-store",
    "x-content-type-options": "nosniff",
    "referrer-policy": "no-referrer",
    "access-control-allow-origin": "*",
  };
}

export const bytes = (value: string): number =>
  new TextEncoder().encode(value).length;
export const digest = (value: string): Promise<string> => sha256Hex(value);

export function text(value: string, maxBytes: number): string {
  if (
    !value.trim() ||
    /[\u0000-\u0008\u000b\u000c\u000e-\u001f\u007f]/u.test(value) ||
    bytes(value) > maxBytes ||
    value !== new TextDecoder().decode(new TextEncoder().encode(value))
  ) {
    fail(
      400,
      "invalid_request",
      "Text exceeds its UTF-8 budget or contains invalid characters",
    );
  }
  return value;
}

export function clip(value: string, maxBytes: number): string {
  let result = "";
  for (const character of value) {
    if (bytes(result + character) > maxBytes) break;
    result += character;
  }
  return result;
}

export function canonical(value: unknown): string {
  if (Array.isArray(value)) return `[${value.map(canonical).join(",")}]`;
  if (value !== null && typeof value === "object") {
    const object = value as Data;
    return `{${Object.keys(object)
      .sort()
      .map((key) => `${JSON.stringify(key)}:${canonical(object[key])}`)
      .join(",")}}`;
  }
  return JSON.stringify(value);
}

export async function rows(
  db: Database,
  sql: string,
  ...args: (string | number | null)[]
): Promise<Data[]> {
  return (
    await db
      .prepare(sql)
      .bind(...args)
      .all<Data>()
  ).results;
}

export function machine(row: Data): Data {
  return {
    machine_id: row.machine_id,
    name: row.name,
    can_create_rooms: Boolean(row.can_create_rooms),
    created_at: row.created_at,
  };
}

export function room(row: Data): Data {
  return {
    room_id: row.room_id,
    name: row.name,
    owner_machine_id: row.owner_machine_id,
    created_at: row.created_at,
  };
}

export async function member(
  db: Database,
  roomId: string,
  machineId: string,
): Promise<Data> {
  const found = await db
    .prepare(
      `SELECT m.*, r.owner_machine_id FROM room_memberships m
    JOIN council_rooms r ON r.room_id = m.room_id
    WHERE m.room_id = ? AND m.machine_id = ? AND m.revoked_at IS NULL AND r.deleted_at IS NULL`,
    )
    .bind(roomId, machineId)
    .first<Data>();
  if (!found) fail(403, "forbidden", "Room membership is not active");
  return found;
}

export async function ownedSession(
  db: Database,
  roomId: string,
  machineId: string,
  sessionId: string,
  now: number,
): Promise<Data> {
  const found = await db
    .prepare(
      `SELECT p.* FROM room_presence p JOIN room_sessions s USING(session_id)
    WHERE p.room_id = ? AND p.session_id = ? AND s.machine_id = ? AND p.expires_at > ?`,
    )
    .bind(roomId, sessionId, machineId, now)
    .first<Data>();
  if (!found) fail(403, "forbidden", "Session is not present for this machine");
  return found;
}
