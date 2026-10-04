import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { Miniflare } from "miniflare";
import { build } from "esbuild";
import { readFile } from "node:fs/promises";
import { URL as NodeURL } from "node:url";
import { handle } from "../src/index";
import type { Env } from "../src/types";
import { digest } from "../src/rooms/protocol";
import schema from "../../../packages/vis-contract/resources/vis-contract/schema/rooms.json";
import * as validators from "../src/rooms/generated/validators.js";

const uuid = () => crypto.randomUUID();
const secret = () =>
  "s" + uuid().replaceAll("-", "") + uuid().replaceAll("-", "");
const admin = secret();
const owner = {
  machine_id: uuid(),
  name: "Laptop",
  credential: secret(),
  can_create_rooms: true,
};
const guest = {
  machine_id: uuid(),
  name: "Test gateway",
  credential: secret(),
};
const ownerSession = uuid();
const guestSession = uuid();
const roomId = uuid();
let now = 1700000000000;
let mf: Miniflare;
let env: Env;
const limiter = { limit: async () => ({ success: true }) } as RateLimit;
const observed = new Set<string>();

async function call(
  method: string,
  path: string,
  token = owner.credential,
  body?: unknown,
) {
  const response = await mf.dispatchFetch(
    `https://gateway.example.com${path}`,
    {
      method,
      headers: {
        authorization: `Bearer ${token}`,
        "content-type": "application/json",
        "x-test-clock": String(now),
      },
      ...(body === undefined ? {} : { body: JSON.stringify(body) }),
    },
  );
  const data = (await response.json()) as Record<string, any>;
  const contract = schema["x-vis-http"].find(
    (route) =>
      route.method === method &&
      new RegExp(`^${route.path.replace(/\{[^}]+\}/g, "[^/]+")}$`).test(
        path.split("?")[0],
      ),
  );
  if (contract) {
    observed.add(`${method} ${contract.path}`);
    expect(
      validators[
        (response.ok ? contract.response : "error") as keyof typeof validators
      ](data),
      JSON.stringify(data),
    ).toBe(true);
  }
  return { status: response.status, data };
}

async function invite(maxUses = 1) {
  const token = secret();
  const inviteId = uuid();
  const result = await call(
    "POST",
    `/v1/rooms/${roomId}/invites`,
    owner.credential,
    { invite_id: inviteId, token, expires_at: now + 60000, max_uses: maxUses },
  );
  expect(result.status, JSON.stringify(result.data)).toBe(200);
  return { token, inviteId, result };
}

async function advertise(
  actor: typeof owner | typeof guest,
  sid: string,
  state = "running",
  wake = false,
) {
  return call("POST", `/v1/rooms/${roomId}/presence`, actor.credential, {
    sessions: [
      { session_id: sid, title: actor.name, state, wake_allowed: wake },
    ],
  });
}

beforeAll(async () => {
  const bundle = await build({
    stdin: {
      contents: `import { handle } from './src/index.ts';
      export default { fetch(request, env) {
        const limiter = { limit: async () => ({ success: true }) };
        return handle(request, { ...env, ROOMS_ADDRESS_LIMIT: limiter, ROOMS_MACHINE_LIMIT: limiter },
          { fetch: globalThis.fetch, now: () => Number(request.headers.get('x-test-clock')) });
      } };`,
      resolveDir: process.cwd(),
    },
    bundle: true,
    format: "esm",
    platform: "browser",
    target: "es2022",
    write: false,
  });
  mf = new Miniflare({
    workers: [
      {
        config: {
          name: "rooms",
          compatibilityDate: "2025-01-01",
          manifest: {
            mainModule: "rooms.mjs",
            modulesRoot: process.cwd(),
            modules: {
              "rooms.mjs": {
                type: "esm",
                contents: bundle.outputFiles[0].text,
              },
            },
          },
          env: {
            ROOMS_DB: { type: "d1", id: "ROOMS_DB" },
            ROOMS_ADMIN_TOKEN: { type: "text", value: admin },
          },
          exports: {},
        },
      },
    ],
  });
  const db = await mf.getD1Database("ROOMS_DB");
  for (const name of ["0001_rooms.sql", "0002_rooms_limits.sql"]) {
    const sql = await readFile(
      new NodeURL(`../migrations/${name}`, import.meta.url),
      "utf8",
    );
    const statements = sql
      .replace(/^--.*$/gm, "")
      .split(/\n\s*\n/)
      .map((part) => part.trim())
      .filter(Boolean);
    await db.exec(
      statements.map((statement) => statement.replaceAll("\n", " ")).join("\n"),
    );
  }
  env = {
    ROOMS_DB: db as unknown as D1Database,
    ROOMS_ADMIN_TOKEN: admin,
    ROOMS_ADDRESS_LIMIT: limiter,
    ROOMS_MACHINE_LIMIT: limiter,
  } as Env;
}, 30000);
afterAll(async () => {
  await mf?.dispose();
});

describe.sequential("Rooms through the real relay router and D1", () => {
  it("registers machines and authorizes room creation separately from Push", async () => {
    expect(
      (await call("POST", "/v1/rooms/machines", admin, owner)).status,
    ).toBe(200);
    expect(
      (await call("POST", "/v1/rooms/machines", admin, guest)).status,
    ).toBe(200);
    expect(
      (
        await call("POST", "/v1/rooms", guest.credential, {
          room_id: uuid(),
          name: "Denied",
          owner_machine_id: guest.machine_id,
        })
      ).status,
    ).toBe(403);
    expect(
      (
        await call("POST", "/v1/rooms", owner.credential, {
          room_id: roomId,
          name: "Council test",
          owner_machine_id: owner.machine_id,
        })
      ).status,
    ).toBe(200);
    expect((await call("GET", "/v1/rooms/machine")).data).toEqual(
      expect.objectContaining({ machine_id: owner.machine_id }),
    );
    expect((await call("GET", "/v1/rooms")).data).toHaveLength(1);
    expect(
      (await call("GET", `/v1/rooms/${roomId}/members`, secret())).status,
    ).toBe(401);
    const push = await call("POST", "/v1/push", owner.credential, {});
    expect(push.status).not.toBe(200);
  });

  it("keeps GET invites inert and stores only hashes", async () => {
    const created = await invite();
    expect(created.result.data.invite_url).toBe(
      `https://gateway.example.com/rooms/join#invite=${created.token}`,
    );
    const opened = await handle(
      new Request(created.result.data.invite_url),
      env,
    );
    expect(opened.status).toBe(200);
    const stored = await env
      .ROOMS_DB!.prepare("SELECT * FROM room_invites WHERE invite_id = ?")
      .bind(created.inviteId)
      .first();
    expect(stored).toEqual(
      expect.objectContaining({
        uses: 0,
        token_hash: await digest(created.token),
      }),
    );
    expect(JSON.stringify(stored)).not.toContain(created.token);
    const body = {
      request_id: uuid(),
      invite_token: created.token,
      machine_id: guest.machine_id,
      machine_name: guest.name,
    };
    const joined = await call("POST", "/v1/rooms/join", guest.credential, body);
    expect(joined.status, JSON.stringify(joined.data)).toBe(200);
    expect(
      (await call("POST", "/v1/rooms/join", guest.credential, body)).data,
    ).toEqual(joined.data);
    const count = await env
      .ROOMS_DB!.prepare("SELECT uses FROM room_invites WHERE invite_id = ?")
      .bind(created.inviteId)
      .first();
    expect(count?.uses).toBe(1);
    expect(
      (await call("GET", `/v1/rooms/${roomId}/members`)).data,
    ).toHaveLength(2);
  });

  it("atomically admits one concurrent redeemer and rolls back the losing machine", async () => {
    const created = await invite();
    const contenders = Array.from({ length: 4 }, () => ({
      machine_id: uuid(),
      credential: secret(),
    }));
    const results = await Promise.all(
      contenders.map((actor) =>
        call("POST", "/v1/rooms/join", actor.credential, {
          request_id: uuid(),
          invite_token: created.token,
          machine_id: actor.machine_id,
          machine_name: "Contender",
        }),
      ),
    );
    expect(results.map((result) => result.status).sort()).toEqual([
      200, 410, 410, 410,
    ]);
    for (let index = 0; index < contenders.length; index++) {
      const stored = await env
        .ROOMS_DB!.prepare(
          "SELECT machine_id FROM room_machines WHERE machine_id = ?",
        )
        .bind(contenders[index].machine_id)
        .first();
      expect(Boolean(stored)).toBe(results[index].status === 200);
    }
  });

  it("rejects expired and revoked invites without changing membership", async () => {
    const members = (await call("GET", `/v1/rooms/${roomId}/members`)).data;
    const expired = await invite();
    now += 60001;
    const body = {
      request_id: uuid(),
      invite_token: expired.token,
      machine_id: uuid(),
      machine_name: "Expired",
    };
    expect((await call("POST", "/v1/rooms/join", secret(), body)).status).toBe(
      410,
    );
    const revoked = await invite();
    expect(
      (await call("DELETE", `/v1/rooms/${roomId}/invites/${revoked.inviteId}`))
        .status,
    ).toBe(200);
    expect(
      (
        await call("POST", "/v1/rooms/join", secret(), {
          ...body,
          invite_token: revoked.token,
        })
      ).status,
    ).toBe(410);
    const stale = await env
      .ROOMS_DB!.prepare(
        "SELECT COUNT(*) AS count FROM room_invites WHERE expires_at <= ? OR revoked_at IS NOT NULL",
      )
      .bind(now)
      .first<{ count: number }>();
    expect(stale?.count).toBe(0);
    expect(
      await env
        .ROOMS_DB!.prepare(
          "SELECT invite_id FROM room_invites WHERE invite_id IN (?, ?)",
        )
        .bind(expired.inviteId, revoked.inviteId)
        .first(),
    ).toBeNull();
    expect((await call("GET", `/v1/rooms/${roomId}/members`)).data).toEqual(
      members,
    );
  });

  it("renames a machine and keeps its identity", async () => {
    const renamed = await call("PATCH", "/v1/rooms/machine", guest.credential, {
      name: "Renamed gateway",
    });
    expect(renamed.status, JSON.stringify(renamed.data)).toBe(200);
    expect(renamed.data).toEqual(
      expect.objectContaining({
        machine_id: guest.machine_id,
        name: "Renamed gateway",
      }),
    );
    expect(
      (await call("GET", `/v1/rooms/${roomId}/members`)).data,
    ).toContainEqual(
      expect.objectContaining({
        machine_id: guest.machine_id,
        name: "Renamed gateway",
      }),
    );
    expect(
      (
        await call("PATCH", "/v1/rooms/machine", guest.credential, {
          name: "Bad\nname",
        })
      ).status,
    ).toBe(400);
    expect(
      (
        await call("PATCH", "/v1/rooms/machine", secret(), {
          name: "Intruder",
        })
      ).status,
    ).toBe(401);
    expect(
      (
        await call("POST", "/v1/rooms/machines", admin, {
          ...guest,
          name: "Admin name",
        })
      ).data,
    ).toEqual(
      expect.objectContaining({
        machine_id: guest.machine_id,
        name: "Admin name",
        can_create_rooms: false,
      }),
    );
    expect(
      (
        await call("POST", "/v1/rooms/machines", admin, {
          ...guest,
          credential: secret(),
        })
      ).status,
    ).toBe(409);
    expect(
      (
        await call("PATCH", "/v1/rooms/machine", guest.credential, {
          name: guest.name,
        })
      ).status,
    ).toBe(200);
  });

  it("keeps machine ownership, room membership and presence separate", async () => {
    expect((await advertise(owner, ownerSession)).status).toBe(200);
    expect((await advertise(guest, guestSession)).status).toBe(200);
    expect((await advertise(guest, ownerSession)).status).toBe(409);
    expect(
      (await call("GET", `/v1/rooms/${roomId}/sessions`)).data,
    ).toHaveLength(2);
    now += 90001;
    expect(
      (await call("GET", `/v1/rooms/${roomId}/sessions`)).data,
    ).toHaveLength(0);
    expect(
      (await call("GET", "/v1/rooms", guest.credential)).data,
    ).toHaveLength(1);
    await advertise(owner, ownerSession);
    await advertise(guest, guestSession);
  });

  it("delivers ordinary notifications and permits only an owned, opted-in self wake", async () => {
    await advertise(owner, ownerSession);
    await advertise(guest, guestSession, "idle", true);
    const sent = await call(
      "POST",
      `/v1/rooms/${roomId}/entries`,
      owner.credential,
      {
        session_id: ownerSession,
        publication: {
          kind: "informational",
          content: "Ready",
          ping: [guestSession],
        },
      },
    );
    expect(sent.status).toBe(200);
    const inbox = await call(
      "GET",
      `/v1/rooms/${roomId}/inbox?session_id=${guestSession}`,
      guest.credential,
    );
    expect(inbox.data.entries.map((entry: any) => entry.entry_id)).toContain(
      sent.data.entry_id,
    );
    expect(
      (
        await call(
          "GET",
          `/v1/rooms/${roomId}/inbox?session_id=${ownerSession}`,
          guest.credential,
        )
      ).status,
    ).toBe(403);
    const wake = {
      session_id: guestSession,
      event: {
        kind: "informational",
        content: "Job complete",
        idempotency_key: uuid(),
      },
    };
    expect(
      (await call("POST", `/v1/rooms/${roomId}/wake`, owner.credential, wake))
        .status,
    ).toBe(403);
    const first = await call(
      "POST",
      `/v1/rooms/${roomId}/wake`,
      guest.credential,
      wake,
    );
    expect(first.status, JSON.stringify(first.data)).toBe(200);
    expect(
      (await call("POST", `/v1/rooms/${roomId}/wake`, guest.credential, wake))
        .data.entry_id,
    ).toBe(first.data.entry_id);
    await advertise(guest, guestSession, "idle", false);
    expect(
      (await call("POST", `/v1/rooms/${roomId}/wake`, guest.credential, wake))
        .status,
    ).toBe(409);
    await advertise(guest, guestSession);
    expect(
      (
        await call("POST", `/v1/rooms/${roomId}/entries`, guest.credential, {
          session_id: guestSession,
          publication: {
            kind: "informational",
            content: "No self ping",
            ping: [guestSession],
          },
        })
      ).status,
    ).toBe(400);
    expect(
      await env
        .ROOMS_DB!.prepare(
          "SELECT name FROM sqlite_master WHERE name = 'room_terminal_reply'",
        )
        .first(),
    ).not.toBeNull();
  });

  it("exchanges an addressed question and a terminal reply with stable IDs", async () => {
    const body = {
      session_id: ownerSession,
      publication: {
        kind: "coordination",
        content: "Check the build",
        ping: [guestSession],
        reply_required: true,
        idempotency_key: uuid(),
      },
    };
    const simultaneous = await Promise.all(
      [1, 2, 3].map(() =>
        call("POST", `/v1/rooms/${roomId}/entries`, owner.credential, body),
      ),
    );
    expect(simultaneous.map((result) => result.status)).toEqual([
      200, 200, 200,
    ]);
    expect(
      new Set(simultaneous.map((result) => result.data.entry_id)).size,
    ).toBe(1);
    const asked = simultaneous[0];
    expect(asked.status, JSON.stringify(asked.data)).toBe(200);
    expect(asked.data.source_ref).toBeUndefined();
    const replay = await call(
      "POST",
      `/v1/rooms/${roomId}/entries`,
      owner.credential,
      body,
    );
    expect(replay.data.entry_id).toBe(asked.data.entry_id);
    expect(
      (
        await call("POST", `/v1/rooms/${roomId}/entries`, owner.credential, {
          ...body,
          publication: { ...body.publication, content: "Different" },
        })
      ).status,
    ).toBe(409);
    const inbox = await call(
      "GET",
      `/v1/rooms/${roomId}/pending?session_id=${guestSession}`,
      guest.credential,
    );
    expect(inbox.data).toHaveLength(1);
    expect(
      (
        await call(
          "GET",
          `/v1/rooms/${roomId}/pending?session_id=${guestSession}`,
          owner.credential,
        )
      ).status,
    ).toBe(403);
    expect(
      (
        await call("POST", `/v1/rooms/${roomId}/receipts`, guest.credential, {
          receipts: [
            {
              session_id: guestSession,
              entry_id: asked.data.entry_id,
              state: "delivered",
            },
          ],
        })
      ).status,
    ).toBe(200);
    const answered = await call(
      "POST",
      `/v1/rooms/${roomId}/entries`,
      guest.credential,
      {
        session_id: guestSession,
        publication: {
          kind: "informational",
          content: "Build passed",
          reply_to: asked.data.entry_id,
          title: 99,
          idempotency_key: uuid(),
        },
      },
    );
    expect(answered.status, JSON.stringify(answered.data)).toBe(200);
    const completed = await call(
      "GET",
      `/v1/rooms/${roomId}/entries/${asked.data.entry_id}`,
    );
    expect(completed.data.replies).toEqual([
      {
        session_id: guestSession,
        state: "replied",
        reply_entry_id: answered.data.entry_id,
      },
    ]);
    expect(
      (
        await call(
          "GET",
          `/v1/rooms/${roomId}/pending?session_id=${guestSession}`,
          guest.credential,
        )
      ).data,
    ).toHaveLength(0);
    const page = await call(
      "GET",
      `/v1/rooms/${roomId}/entries?after=0&limit=1`,
    );
    expect(page.data.entries).toHaveLength(1);
    expect(page.data.has_more).toBe(true);
    const roots = (
      await call(
        "GET",
        `/v1/rooms/${roomId}/threads?after=${asked.data.entry_id - 1}`,
      )
    ).data.entries;
    expect(roots.map((entry: any) => entry.thread_id)).toEqual([
      asked.data.entry_id,
    ]);
    const duplicates = await Promise.all(
      [1, 2].map(() =>
        call("POST", `/v1/rooms/${roomId}/entries`, guest.credential, {
          session_id: guestSession,
          publication: {
            kind: "informational",
            content: "Duplicate reply",
            reply_to: asked.data.entry_id,
            idempotency_key: uuid(),
          },
        }),
      ),
    );
    expect(duplicates.map((reply) => reply.status)).toEqual([409, 409]);
  });

  it("does not broaden room access and does not grant waking by joining", async () => {
    const otherRoom = uuid();
    expect(
      (
        await call("POST", "/v1/rooms", owner.credential, {
          room_id: otherRoom,
          name: "Private",
          owner_machine_id: owner.machine_id,
        })
      ).status,
    ).toBe(200);
    expect(
      (await call("GET", `/v1/rooms/${otherRoom}/entries`, guest.credential))
        .status,
    ).toBe(403);
    await advertise(guest, guestSession, "idle", false);
    const asked = await call(
      "POST",
      `/v1/rooms/${roomId}/entries`,
      owner.credential,
      {
        session_id: ownerSession,
        publication: {
          kind: "coordination",
          content: "Idle question",
          ping: [guestSession],
          reply_required: true,
        },
      },
    );
    expect(asked.data.replies[0].state).toBe("unavailable");
    const all = await call(
      "POST",
      `/v1/rooms/${roomId}/entries`,
      owner.credential,
      {
        session_id: ownerSession,
        publication: {
          kind: "coordination",
          content: "Broadcast",
          ping: "all",
        },
      },
    );
    expect(all.data.ping).toEqual([]);
    expect((await call("DELETE", `/v1/rooms/${otherRoom}`)).status).toBe(200);
  });

  it("revokes immediately and never reopens membership through replay", async () => {
    const created = await invite();
    const body = {
      request_id: uuid(),
      invite_token: created.token,
      machine_id: guest.machine_id,
      machine_name: guest.name,
    };
    expect(
      (await call("POST", "/v1/rooms/join", guest.credential, body)).status,
    ).toBe(200);
    expect(
      (await call("DELETE", `/v1/rooms/${roomId}/members/${guest.machine_id}`))
        .status,
    ).toBe(200);
    expect(
      (await call("GET", `/v1/rooms/${roomId}/entries`, guest.credential))
        .status,
    ).toBe(403);
    expect(
      (await call("POST", "/v1/rooms/join", guest.credential, body)).status,
    ).toBe(403);
    expect(
      (
        await call("PATCH", `/v1/rooms/machines/${guest.machine_id}`, admin, {
          can_create_rooms: true,
        })
      ).status,
    ).toBe(200);
  });

  it("isolates failures, validates requests and bounds upload streams", async () => {
    expect(
      (
        await call("POST", `/v1/rooms/${roomId}/presence`, owner.credential, {
          sessions: [],
          unknown: true,
        })
      ).status,
    ).toBe(400);
    expect(
      (await call("GET", `/v1/rooms/${roomId}/entries?limit=5000`)).status,
    ).toBe(400);
    const oversize = await call("POST", "/v1/rooms/join", owner.credential, {
      content: "x".repeat(schema["x-vis-limits"].request_bytes),
    });
    expect(oversize.status).toBe(413);
    const unavailable = await handle(
      new Request("https://gateway.example.com/v1/rooms"),
      {} as Env,
    );
    expect(unavailable.status).toBe(503);
    const health = await handle(
      new Request("https://gateway.example.com/healthz"),
      {} as Env,
    );
    expect(health.status).toBe(200);
    const refused = {
      ...env,
      ROOMS_ADDRESS_LIMIT: {
        limit: async () => ({ success: false }),
      } as RateLimit,
    };
    const limited = await handle(
      new Request("https://gateway.example.com/v1/rooms"),
      refused,
    );
    expect(limited.status).toBe(429);
  });

  it("deletes rooms and machines without orphaned history", async () => {
    const db = env.ROOMS_DB!;
    const count = async (sql: string, ...values: string[]) =>
      (
        await db
          .prepare(sql)
          .bind(...values)
          .first<{ total: number }>()
      )?.total;
    const host = {
      machine_id: uuid(),
      name: "Host",
      credential: secret(),
      can_create_rooms: true,
    };
    const visitor = {
      machine_id: uuid(),
      name: "Visitor",
      credential: secret(),
    };
    const hostSession = uuid();
    const visitorSession = uuid();
    const hostRoom = uuid();
    const spareRoom = uuid();
    expect((await call("POST", "/v1/rooms/machines", admin, host)).status).toBe(
      200,
    );
    for (const [id, name] of [
      [hostRoom, "Host room"],
      [spareRoom, "Spare room"],
    ]) {
      expect(
        (
          await call("POST", "/v1/rooms", host.credential, {
            room_id: id,
            name,
            owner_machine_id: host.machine_id,
          })
        ).status,
      ).toBe(200);
    }
    const token = secret();
    expect(
      (
        await call("POST", `/v1/rooms/${hostRoom}/invites`, host.credential, {
          invite_id: uuid(),
          token,
          expires_at: now + 60000,
          max_uses: 1,
        })
      ).status,
    ).toBe(200);
    for (const inviteToken of [token, (await invite()).token]) {
      expect(
        (
          await call("POST", "/v1/rooms/join", visitor.credential, {
            request_id: uuid(),
            invite_token: inviteToken,
            machine_id: visitor.machine_id,
            machine_name: visitor.name,
          })
        ).status,
      ).toBe(200);
    }
    await advertise(owner, ownerSession);
    const speakers = [
      [hostRoom, host, hostSession],
      [hostRoom, visitor, visitorSession],
      [roomId, visitor, visitorSession],
    ] as const;
    const published: number[] = [];
    for (const [room, actor, sid] of speakers) {
      expect(
        (
          await call("POST", `/v1/rooms/${room}/presence`, actor.credential, {
            sessions: [
              {
                session_id: sid,
                title: actor.name,
                state: "running",
                wake_allowed: false,
              },
            ],
          })
        ).status,
      ).toBe(200);
      const sent = await call(
        "POST",
        `/v1/rooms/${room}/entries`,
        actor.credential,
        {
          session_id: sid,
          publication: { kind: "informational", content: `${actor.name} note` },
        },
      );
      expect(sent.status, JSON.stringify(sent.data)).toBe(200);
      published.push(sent.data.entry_id);
    }
    const question = await call(
      "POST",
      `/v1/rooms/${roomId}/entries`,
      owner.credential,
      {
        session_id: ownerSession,
        publication: {
          kind: "coordination",
          content: "Still there?",
          ping: [visitorSession],
          reply_required: true,
        },
      },
    );
    expect(question.data.replies[0].state).toBe("pending");

    expect(
      (await call("DELETE", `/v1/rooms/${hostRoom}`, host.credential)).data,
    ).toEqual({ ok: true });
    expect((await call("DELETE", `/v1/rooms/${hostRoom}`, admin)).status).toBe(
      404,
    );
    expect(
      await count(
        `SELECT (SELECT COUNT(*) FROM council_rooms WHERE room_id = ?)
          + (SELECT COUNT(*) FROM room_entries WHERE room_id = ?)
          + (SELECT COUNT(*) FROM room_invites WHERE room_id = ?)
          + (SELECT COUNT(*) FROM room_memberships WHERE room_id = ?)
          + (SELECT COUNT(*) FROM room_presence WHERE room_id = ?) AS total`,
        hostRoom,
        hostRoom,
        hostRoom,
        hostRoom,
        hostRoom,
      ),
    ).toBe(0);

    expect(
      (
        await call(
          "DELETE",
          `/v1/rooms/machines/${owner.machine_id}`,
          guest.credential,
        )
      ).status,
    ).toBe(403);
    expect(
      (
        await call(
          "DELETE",
          `/v1/rooms/machines/${visitor.machine_id}`,
          visitor.credential,
        )
      ).data,
    ).toEqual({
      machine_id: visitor.machine_id,
      deleted_rooms: 0,
      retained_history: true,
    });
    expect(
      (await call("GET", "/v1/rooms/machine", visitor.credential)).status,
    ).toBe(401);
    expect(
      (
        await call("PATCH", `/v1/rooms/machines/${visitor.machine_id}`, admin, {
          can_create_rooms: true,
        })
      ).status,
    ).toBe(404);
    expect(
      (await call("DELETE", `/v1/rooms/machines/${visitor.machine_id}`, admin))
        .status,
    ).toBe(404);
    expect(
      (await call("GET", `/v1/rooms/${roomId}/entries/${published[2]}`)).status,
    ).toBe(200);
    expect(
      await count(
        `SELECT COUNT(*) AS total FROM room_deliveries
          WHERE session_id = ? AND state != 'unavailable'`,
        visitorSession,
      ),
    ).toBe(0);
    expect(
      await count(
        `SELECT (SELECT COUNT(*) FROM room_memberships WHERE machine_id = ?)
          + (SELECT COUNT(*) FROM room_presence WHERE session_id = ?) AS total`,
        visitor.machine_id,
        visitorSession,
      ),
    ).toBe(0);

    expect(
      (await call("DELETE", `/v1/rooms/machines/${host.machine_id}`, admin))
        .data,
    ).toEqual({
      machine_id: host.machine_id,
      deleted_rooms: 1,
      retained_history: false,
    });
    expect(
      await count(
        `SELECT (SELECT COUNT(*) FROM room_machines WHERE machine_id = ?)
          + (SELECT COUNT(*) FROM room_sessions WHERE machine_id = ?)
          + (SELECT COUNT(*) FROM council_rooms WHERE room_id = ?) AS total`,
        host.machine_id,
        host.machine_id,
        spareRoom,
      ),
    ).toBe(0);

    expect((await call("DELETE", `/v1/rooms/${roomId}`)).status).toBe(200);
    expect(
      await count(
        `SELECT (SELECT COUNT(*) FROM room_machines WHERE machine_id = ?)
          + (SELECT COUNT(*) FROM room_sessions WHERE machine_id = ?) AS total`,
        visitor.machine_id,
        visitor.machine_id,
      ),
    ).toBe(0);
  });
  it("covers every declared Rooms HTTP operation with canonical response validation", () => {
    expect([...observed].sort()).toEqual(
      schema["x-vis-http"]
        .map((route) => `${route.method} ${route.path}`)
        .sort(),
    );
  });
});
