import { beforeEach, describe, expect, it } from "vitest";

import { resetProviderTokens } from "../src/apns";
import { resetAccessTokens } from "../src/fcm";
import { sha256Hex } from "../src/jwt";
import { handle, sweep } from "../src/index";
import type { Deps, Env } from "../src/types";
import { openTestDb, type TestDb } from "./d1";
import { decodeJwt, generateEs256, generateRs256, verifyJwt } from "./keys";

interface Call {
  url: string;
  method: string;
  headers: Record<string, string>;
  body: string;
}

function recorder(responder: (call: Call) => Response) {
  const calls: Call[] = [];
  const fetchImpl = (async (input: RequestInfo | URL, init?: RequestInit) => {
    const headers: Record<string, string> = {};
    for (const [key, value] of Object.entries((init?.headers ?? {}) as Record<string, string>)) {
      headers[key.toLowerCase()] = value;
    }
    const call: Call = {
      url: String(input),
      method: init?.method ?? "GET",
      headers,
      body: String(init?.body ?? ""),
    };
    calls.push(call);
    return responder(call);
  }) as unknown as typeof fetch;
  return { calls, fetchImpl };
}

function jsonResponse(status: number, payload: unknown): Response {
  return new Response(JSON.stringify(payload), { status });
}

const APPLE_KEY = await generateEs256();
const GOOGLE_KEY = await generateRs256();

const SERVICE_ACCOUNT = JSON.stringify({
  type: "service_account",
  project_id: "vis-relay-test",
  client_email: "relay@vis-relay-test.iam.gserviceaccount.com",
  private_key: GOOGLE_KEY.pem,
});

let db: TestDb;
let clock: number;
let grantSeed: number;

function makeEnv(overrides: Partial<Env> = {}): Env {
  return {
    DB: db,
    APNS_KEY_P8: APPLE_KEY.pem,
    APNS_KEY_ID: "KID1234567",
    APNS_TEAM_ID: "TEAM123456",
    APNS_TOPIC: "com.example.viscompanion",
    APNS_DEFAULT_ENV: "production",
    FCM_SERVICE_ACCOUNT: SERVICE_ACCOUNT,
    ...overrides,
  } as Env;
}

function makeDeps(fetchImpl: typeof fetch): Deps {
  return {
    fetch: fetchImpl,
    now: () => clock,
    randomGrant: () => `grant-${(grantSeed += 1)}`,
  };
}

function post(path: string, body: unknown, headers: Record<string, string> = {}): Request {
  return new Request(`https://relay.example.com${path}`, {
    method: "POST",
    headers: { "content-type": "application/json", "cf-connecting-ip": "203.0.113.7", ...headers },
    body: JSON.stringify(body),
  });
}

async function createGrant(env: Env, deps: Deps, body: Record<string, unknown> = {}): Promise<string> {
  const response = await handle(
    post("/v1/grants", { device_token: "a".repeat(64), platform: "ios", ...body }),
    env,
    deps,
  );
  expect(response.status).toBe(201);
  return ((await response.json()) as { grant: string }).grant;
}

const ALERT = { title: "Fix the gateway", body: "Done.", thread_id: "s-1", collapse_id: "s-1" };

beforeEach(() => {
  db = openTestDb() as TestDb;
  clock = 1_700_000_000_000;
  grantSeed = 0;
  resetProviderTokens();
  resetAccessTokens();
});

describe("healthz", () => {
  it("reports which providers are configured and leaks no key material", async () => {
    const { fetchImpl } = recorder(() => jsonResponse(200, {}));
    const response = await handle(
      new Request("https://relay.example.com/healthz"),
      makeEnv(),
      makeDeps(fetchImpl),
    );
    const payload = await response.json();
    expect(response.status).toBe(200);
    expect(payload).toMatchObject({
      is_ok: true,
      apns: { is_available: true, topic: "com.example.viscompanion", environment: "production" },
      fcm: { is_available: true, project_id: "vis-relay-test" },
    });
    expect(JSON.stringify(payload)).not.toContain("PRIVATE KEY");
  });

  it("says a provider is unavailable rather than half-configured", async () => {
    const { fetchImpl } = recorder(() => jsonResponse(200, {}));
    const env = makeEnv({ APNS_TOPIC: "", FCM_SERVICE_ACCOUNT: "" });
    const response = await handle(
      new Request("https://relay.example.com/healthz"),
      env,
      makeDeps(fetchImpl),
    );
    expect(await response.json()).toMatchObject({
      apns: { is_available: false },
      fcm: { is_available: false },
    });
  });
});

describe("grants", () => {
  it("returns an opaque grant and stores only its sha256, never the grant itself", async () => {
    const { fetchImpl } = recorder(() => jsonResponse(200, {}));
    const env = makeEnv();
    const grant = await createGrant(env, makeDeps(fetchImpl), { label: "iPhone" });

    const rows = db.query("SELECT * FROM grants");
    expect(rows).toHaveLength(1);
    expect(rows[0].id).toBe(await sha256Hex(grant));
    expect(rows[0].id).not.toBe(grant);
    expect(rows[0].device_token).toBe("a".repeat(64));
    expect(rows[0].label).toBe("iPhone");
  });

  it("refuses a blank device token and an unknown platform", async () => {
    const env = makeEnv();
    const { fetchImpl } = recorder(() => jsonResponse(200, {}));
    const deps = makeDeps(fetchImpl);
    expect((await handle(post("/v1/grants", { device_token: "   " }), env, deps)).status).toBe(400);
    expect(
      (await handle(post("/v1/grants", { device_token: "abc", platform: "toaster" }), env, deps))
        .status,
    ).toBe(400);
  });

  it("keeps only the newest grants for one device token", async () => {
    const env = makeEnv({ MAX_GRANTS_PER_DEVICE: "2" });
    const { fetchImpl } = recorder(() => jsonResponse(200, {}));
    const deps = makeDeps(fetchImpl);
    const first = await createGrant(env, deps);
    clock += 1000;
    await createGrant(env, deps);
    clock += 1000;
    await createGrant(env, deps);

    expect(db.query("SELECT id FROM grants")).toHaveLength(2);
    const response = await handle(
      post("/v1/push", ALERT, { authorization: `Bearer ${first}` }),
      env,
      deps,
    );
    expect(response.status).toBe(404);
  });

  it("revokes exactly one grant, and revocation is what stops the push", async () => {
    const env = makeEnv();
    const { fetchImpl } = recorder(() => new Response("", { status: 200 }));
    const deps = makeDeps(fetchImpl);
    const keep = await createGrant(env, deps);
    const drop = await createGrant(env, deps, { device_token: "b".repeat(64) });

    const revoked = await handle(
      new Request(`https://relay.example.com/v1/grants/${drop}`, { method: "DELETE" }),
      env,
      deps,
    );
    expect(await revoked.json()).toEqual({ is_revoked: true });
    expect(
      (await handle(post("/v1/push", ALERT, { authorization: `Bearer ${drop}` }), env, deps)).status,
    ).toBe(404);
    expect(
      (await handle(post("/v1/push", ALERT, { authorization: `Bearer ${keep}` }), env, deps)).status,
    ).toBe(200);
  });
});

describe("apns push", () => {
  it("signs a verifiable ES256 provider token and posts exactly what Apple requires", async () => {
    const { calls, fetchImpl } = recorder(() => new Response("", { status: 200 }));
    const env = makeEnv();
    const deps = makeDeps(fetchImpl);
    const grant = await createGrant(env, deps);

    const response = await handle(
      post("/v1/push", { ...ALERT, data: { session_id: "s-1" } }, { authorization: `Bearer ${grant}` }),
      env,
      deps,
    );

    expect(response.status).toBe(200);
    expect(await response.json()).toMatchObject({ is_delivered: true, status: 200 });
    expect(calls).toHaveLength(1);
    expect(calls[0].url).toBe(`https://api.push.apple.com/3/device/${"a".repeat(64)}`);
    expect(calls[0].headers["apns-topic"]).toBe("com.example.viscompanion");
    expect(calls[0].headers["apns-push-type"]).toBe("alert");
    expect(calls[0].headers["apns-priority"]).toBe("10");
    expect(calls[0].headers["apns-collapse-id"]).toBe("s-1");

    const jwt = calls[0].headers.authorization.replace(/^bearer /, "");
    const { header, claims } = decodeJwt(jwt);
    expect(header).toEqual({ alg: "ES256", kid: "KID1234567" });
    expect(claims.iss).toBe("TEAM123456");
    expect(await verifyJwt(jwt, APPLE_KEY.publicKey, "ES256")).toBe(true);

    const payload = JSON.parse(calls[0].body) as Record<string, any>;
    expect(payload.aps.alert).toEqual({ title: "Fix the gateway", body: "Done." });
    expect(payload.aps["thread-id"]).toBe("s-1");
    expect(payload.session_id).toBe("s-1");
  });

  it("reuses the cached provider token and re-mints it once it ages out", async () => {
    const { calls, fetchImpl } = recorder(() => new Response("", { status: 200 }));
    const env = makeEnv();
    const deps = makeDeps(fetchImpl);
    const grant = await createGrant(env, deps);
    const push = () =>
      handle(post("/v1/push", ALERT, { authorization: `Bearer ${grant}` }), env, deps);

    await push();
    await push();
    expect(calls[0].headers.authorization).toBe(calls[1].headers.authorization);

    clock += 46 * 60 * 1000;
    await push();
    expect(calls[2].headers.authorization).not.toBe(calls[0].headers.authorization);
  });

  it("retries the other environment once and remembers the one that worked", async () => {
    const { calls, fetchImpl } = recorder((call) =>
      call.url.startsWith("https://api.push.apple.com")
        ? jsonResponse(400, { reason: "BadDeviceToken" })
        : new Response("", { status: 200 }),
    );
    const env = makeEnv();
    const deps = makeDeps(fetchImpl);
    const grant = await createGrant(env, deps);

    const response = await handle(
      post("/v1/push", ALERT, { authorization: `Bearer ${grant}` }),
      env,
      deps,
    );

    expect(response.status).toBe(200);
    expect(calls.map((c) => new URL(c.url).host)).toEqual([
      "api.push.apple.com",
      "api.sandbox.push.apple.com",
    ]);
    expect(db.query("SELECT environment FROM grants")[0].environment).toBe("sandbox");
  });

  it("deletes the grant when Apple says the device is gone", async () => {
    const { fetchImpl } = recorder(() => jsonResponse(410, { reason: "Unregistered" }));
    const env = makeEnv();
    const deps = makeDeps(fetchImpl);
    const grant = await createGrant(env, deps);

    const response = await handle(
      post("/v1/push", ALERT, { authorization: `Bearer ${grant}` }),
      env,
      deps,
    );

    expect(response.status).toBe(410);
    expect(await response.json()).toMatchObject({ is_delivered: false, is_revoked: true });
    expect(db.query("SELECT id FROM grants")).toHaveLength(0);
  });

  it("reports a provider failure as 502 and keeps the grant", async () => {
    const { fetchImpl } = recorder(() => jsonResponse(500, { reason: "InternalServerError" }));
    const env = makeEnv();
    const deps = makeDeps(fetchImpl);
    const grant = await createGrant(env, deps);

    const response = await handle(
      post("/v1/push", ALERT, { authorization: `Bearer ${grant}` }),
      env,
      deps,
    );

    expect(response.status).toBe(502);
    expect(await response.json()).toMatchObject({ status: 500, reason: "InternalServerError" });
    expect(db.query("SELECT id FROM grants")).toHaveLength(1);
  });

  it("refuses to push without a grant, and 503s when the relay has no Apple key", async () => {
    const { fetchImpl } = recorder(() => new Response("", { status: 200 }));
    const env = makeEnv();
    const deps = makeDeps(fetchImpl);
    expect((await handle(post("/v1/push", ALERT), env, deps)).status).toBe(401);

    const grant = await createGrant(env, deps);
    const bare = makeEnv({ APNS_KEY_P8: "" });
    const response = await handle(
      post("/v1/push", ALERT, { authorization: `Bearer ${grant}` }),
      bare,
      deps,
    );
    expect(response.status).toBe(503);
  });

  it("rate-limits one grant without touching another", async () => {
    const { fetchImpl } = recorder(() => new Response("", { status: 200 }));
    const env = makeEnv({ PUSH_RATE_LIMIT: "2" });
    const deps = makeDeps(fetchImpl);
    const noisy = await createGrant(env, deps);
    const quiet = await createGrant(env, deps, { device_token: "c".repeat(64) });
    const push = (grant: string) =>
      handle(post("/v1/push", ALERT, { authorization: `Bearer ${grant}` }), env, deps);

    expect((await push(noisy)).status).toBe(200);
    expect((await push(noisy)).status).toBe(200);
    expect((await push(noisy)).status).toBe(429);
    expect((await push(quiet)).status).toBe(200);

    clock += 3_600_000;
    expect((await push(noisy)).status).toBe(200);
  });
});

describe("fcm push", () => {
  it("mints a verifiable RS256 assertion, exchanges it once, and sends FCM v1", async () => {
    const { calls, fetchImpl } = recorder((call) =>
      call.url.includes("oauth2.googleapis.com")
        ? jsonResponse(200, { access_token: "ya29.test", expires_in: 3599 })
        : jsonResponse(200, { name: "projects/vis-relay-test/messages/1" }),
    );
    const env = makeEnv();
    const deps = makeDeps(fetchImpl);
    const grant = await createGrant(env, deps, { device_token: "fcm-token", platform: "android" });
    const push = () =>
      handle(post("/v1/push", ALERT, { authorization: `Bearer ${grant}` }), env, deps);

    expect((await push()).status).toBe(200);
    expect((await push()).status).toBe(200);

    const oauth = calls.filter((c) => c.url.includes("oauth2.googleapis.com"));
    expect(oauth).toHaveLength(1);
    const assertion = new URLSearchParams(oauth[0].body).get("assertion") ?? "";
    const { header, claims } = decodeJwt(assertion);
    expect(header).toEqual({ alg: "RS256", typ: "JWT" });
    expect(claims.iss).toBe("relay@vis-relay-test.iam.gserviceaccount.com");
    expect(await verifyJwt(assertion, GOOGLE_KEY.publicKey, "RS256")).toBe(true);

    const send = calls.filter((c) => c.url.includes("fcm.googleapis.com"));
    expect(send[0].url).toBe("https://fcm.googleapis.com/v1/projects/vis-relay-test/messages:send");
    expect(send[0].headers.authorization).toBe("Bearer ya29.test");
    const message = JSON.parse(send[0].body) as Record<string, any>;
    expect(message.message.token).toBe("fcm-token");
    expect(message.message.notification).toEqual({ title: "Fix the gateway", body: "Done." });
    expect(message.message.android.collapse_key).toBe("s-1");
  });

  it("deletes the grant when FCM says the registration is gone", async () => {
    const { fetchImpl } = recorder((call) =>
      call.url.includes("oauth2.googleapis.com")
        ? jsonResponse(200, { access_token: "ya29.test" })
        : jsonResponse(404, { error: { status: "NOT_FOUND", details: [{ errorCode: "UNREGISTERED" }] } }),
    );
    const env = makeEnv();
    const deps = makeDeps(fetchImpl);
    const grant = await createGrant(env, deps, { device_token: "fcm-token", platform: "android" });

    const response = await handle(
      post("/v1/push", ALERT, { authorization: `Bearer ${grant}` }),
      env,
      deps,
    );
    expect(response.status).toBe(410);
    expect(db.query("SELECT id FROM grants")).toHaveLength(0);
  });
});

describe("routing", () => {
  it("404s an unknown route", async () => {
    const { fetchImpl } = recorder(() => new Response("", { status: 200 }));
    const response = await handle(
      new Request("https://relay.example.com/v1/nope"),
      makeEnv(),
      makeDeps(fetchImpl),
    );
    expect(response.status).toBe(404);
  });
});

// Every route is public: nobody authenticates to ASK for a grant, and a gateway
// only ever proves it holds one. So the question these pin is not "is the
// caller allowed" but "what does an unwelcome caller cost".
describe("abuse", () => {
  it("caps push attempts per address, so made-up grants cost nothing to refuse", async () => {
    const { fetchImpl } = recorder(() => new Response("", { status: 200 }));
    const env = makeEnv({ IP_PUSH_RATE_LIMIT: "2" });
    const deps = makeDeps(fetchImpl);
    const attempt = (ip: string) =>
      handle(
        post("/v1/push", ALERT, { authorization: "Bearer made-up", "cf-connecting-ip": ip }),
        env,
        deps,
      );

    expect((await attempt("198.51.100.9")).status).toBe(404);
    expect((await attempt("198.51.100.9")).status).toBe(404);
    const blocked = await attempt("198.51.100.9");
    expect(blocked.status).toBe(429);
    expect(await blocked.json()).toMatchObject({ error: { code: "rate_limited" } });

    // The flood is charged to the address that made it, and to nobody else.
    expect((await attempt("203.0.113.99")).status).toBe(404);

    // A refused request must not even write its own counter, or the flood pays
    // for itself in D1 writes.
    const counter = db.query("SELECT count FROM quota WHERE subject = 'push-ip:198.51.100.9'");
    expect(counter[0].count).toBe(2);
  });

  it("refuses an oversized body on its declaration, before parsing or storing", async () => {
    const { fetchImpl } = recorder(() => new Response("", { status: 200 }));
    const env = makeEnv();
    const deps = makeDeps(fetchImpl);
    const request = new Request("https://relay.example.com/v1/grants", {
      method: "POST",
      headers: {
        "content-type": "application/json",
        "cf-connecting-ip": "203.0.113.7",
        "content-length": "1048576",
      },
      body: JSON.stringify({ device_token: "a".repeat(64) }),
    });

    expect((await handle(request, env, deps)).status).toBe(413);
    expect(db.query("SELECT id FROM grants")).toHaveLength(0);
    expect(db.query("SELECT subject FROM quota")).toHaveLength(0);
  });

  it("refuses an oversized body that declared no length at all", async () => {
    const { fetchImpl } = recorder(() => new Response("", { status: 200 }));
    const env = makeEnv();
    const deps = makeDeps(fetchImpl);
    const request = new Request("https://relay.example.com/v1/grants", {
      method: "POST",
      headers: { "content-type": "application/json", "cf-connecting-ip": "203.0.113.7" },
      body: JSON.stringify({ device_token: "a".repeat(64), label: "x".repeat(20000) }),
    });
    expect(request.headers.get("content-length")).toBeNull();

    expect((await handle(request, env, deps)).status).toBe(413);
    expect(db.query("SELECT id FROM grants")).toHaveLength(0);
  });

  it("caps how many custom data keys reach the provider", async () => {
    const { calls, fetchImpl } = recorder(() => new Response("", { status: 200 }));
    const env = makeEnv();
    const deps = makeDeps(fetchImpl);
    const grant = await createGrant(env, deps);
    const data = Object.fromEntries(
      Array.from({ length: 100 }, (_, index) => [`k${index}`, String(index)]),
    );

    const response = await handle(
      post("/v1/push", { ...ALERT, data }, { authorization: `Bearer ${grant}` }),
      env,
      deps,
    );

    expect(response.status).toBe(200);
    const payload = JSON.parse(calls[0].body) as Record<string, unknown>;
    expect(Object.keys(payload).filter((key) => key.startsWith("k"))).toHaveLength(32);
  });

  it("sweeps spent quota windows and grants nobody ever used, keeping live ones", async () => {
    const { fetchImpl } = recorder(() => new Response("", { status: 200 }));
    const env = makeEnv({ UNUSED_GRANT_TTL_MS: "86400000" });
    const deps = makeDeps(fetchImpl);
    const used = await createGrant(env, deps);
    await createGrant(env, deps, { device_token: "b".repeat(64) });
    expect(
      (await handle(post("/v1/push", ALERT, { authorization: `Bearer ${used}` }), env, deps))
        .status,
    ).toBe(200);
    expect(db.query("SELECT id FROM grants")).toHaveLength(2);

    clock += 8 * 86_400_000;
    const swept = await sweep(env, deps);

    expect(swept.grants).toBe(1);
    expect(swept.quota).toBeGreaterThan(0);
    expect(db.query("SELECT id FROM grants")).toHaveLength(1);
    expect(db.query("SELECT id FROM grants")[0].id).toBe(await sha256Hex(used));
    expect(db.query("SELECT subject FROM quota")).toHaveLength(0);
  });
});
