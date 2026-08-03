/**
 * The relay's whole contract, driven through the real router.
 *
 * Two things are deliberately NOT faked: the crypto and the grant. Provider
 * JWTs are verified against real generated keys, and every test that pushes
 * first mints a grant through `POST /v1/grants` — so if sealing, opening,
 * expiry, or key rotation broke, these tests fail rather than pass against a
 * convenient fixture. What IS faked is `fetch` (Apple and Google are not
 * reachable from a test) and the rate limiting bindings the platform provides.
 */

import { beforeEach, describe, expect, it } from "vitest";
import { handle } from "../src/index";
import { APNS_PRODUCTION_HOST, APNS_SANDBOX_HOST, resetProviderTokens } from "../src/apns";
import { resetAccessTokens } from "../src/fcm";
import { seal } from "../src/seal";
import type { Deps, Env } from "../src/types";
import { decodeJwt, generateEs256, generateRs256, verifyJwt } from "./keys";

const APPLE_TOKEN = "a".repeat(64);
const ANDROID_TOKEN = `${"d".repeat(22)}:APA91b${"E".repeat(120)}`;
const SEAL_KEY = "test-seal-key-aaaaaaaaaaaaaaaaaaaaaaaaaaaa";

let apple: { pem: string; publicKey: CryptoKey };
let google: { pem: string; publicKey: CryptoKey };

/** A rate limiting binding that counts, so a test can watch a flood be refused. */
function limiter(limit = 1_000_000): RateLimit & { keys: string[] } {
  const counts = new Map<string, number>();
  const keys: string[] = [];
  return {
    keys,
    async limit(options?: { key?: string }) {
      const key = options?.key ?? "";
      keys.push(key);
      const next = (counts.get(key) ?? 0) + 1;
      counts.set(key, next);
      return { success: next <= limit };
    },
  } as unknown as RateLimit & { keys: string[] };
}

interface Recorded {
  url: string;
  headers: Record<string, string>;
  body: string;
}

function fakeFetch(responder: (url: string) => Response | Promise<Response>) {
  const calls: Recorded[] = [];
  const fn = async (input: RequestInfo | URL, init?: RequestInit): Promise<Response> => {
    const url = String(input);
    const headers: Record<string, string> = {};
    for (const [key, value] of Object.entries((init?.headers ?? {}) as Record<string, string>)) {
      headers[key.toLowerCase()] = value;
    }
    calls.push({ url, headers, body: String(init?.body ?? "") });
    return await responder(url);
  };
  return { calls, fn: fn as unknown as typeof fetch };
}

function makeEnv(overrides: Partial<Env> = {}): Env {
  return {
    RELAY_SEAL_KEY: SEAL_KEY,
    GRANT_TTL_DAYS: "90",
    APNS_KEY_P8: apple.pem,
    APNS_KEY_ID: "KEYID12345",
    APNS_TEAM_ID: "TEAM123456",
    APNS_TOPIC: "com.blockether.vis.companion",
    APNS_DEFAULT_ENV: "production",
    FCM_SERVICE_ACCOUNT: JSON.stringify({
      project_id: "vis-companion",
      client_email: "relay@vis-companion.iam.gserviceaccount.com",
      private_key: google.pem,
    }),
    MINT_LIMIT: limiter(),
    PUSH_ADDRESS_LIMIT: limiter(),
    PUSH_DEVICE_LIMIT: limiter(),
    ...overrides,
  };
}

function makeDeps(fetchFn: typeof fetch, now = 1_700_000_000_000): Deps {
  return { fetch: fetchFn, now: () => now };
}

function post(path: string, body: unknown, init: RequestInit = {}): Request {
  return new Request(`https://relay.example.com${path}`, {
    method: "POST",
    headers: { "content-type": "application/json", "cf-connecting-ip": "203.0.113.7", ...(init.headers ?? {}) },
    body: JSON.stringify(body),
    ...init,
  });
}

async function mint(env: Env, deps: Deps, body: Record<string, unknown> = {}): Promise<string> {
  const response = await handle(post("/v1/grants", { device_token: APPLE_TOKEN, platform: "ios", ...body }), env, deps);
  expect(response.status).toBe(201);
  return ((await response.json()) as { grant: string }).grant;
}

function pushRequest(grant: string, body: Record<string, unknown> = {}): Request {
  return post("/v1/push", { title: "Vis", body: "a session needs you", ...body }, {
    headers: { authorization: `Bearer ${grant}`, "content-type": "application/json", "cf-connecting-ip": "203.0.113.7" },
  });
}

const ok = () => new Response("", { status: 200 });

/**
 * A 1 MiB-per-chunk body that reports how much of itself the relay pulled.
 * `highWaterMark: 0` means nothing is produced until something reads it, so
 * `pulled` counts real demand rather than the stream's own read-ahead.
 */
function countingBody(chunks: number): { stream: ReadableStream; pulled: number } {
  const chunk = new TextEncoder().encode("A".repeat(1024 * 1024));
  const state = { pulled: 0 } as { pulled: number; stream: ReadableStream };
  state.stream = new ReadableStream(
    {
      pull(controller) {
        if (state.pulled >= chunks) {
          controller.close();
          return;
        }
        state.pulled += 1;
        controller.enqueue(chunk);
      },
    },
    { highWaterMark: 0 },
  );
  return state;
}

beforeEach(async () => {
  resetProviderTokens();
  resetAccessTokens();
  apple = apple ?? (await generateEs256());
  google = google ?? (await generateRs256());
});

describe("health", () => {
  it("reports which providers are configured without leaking any key", async () => {
    const response = await handle(new Request("https://relay.example.com/healthz"), makeEnv(), makeDeps(fakeFetch(ok).fn));
    expect(response.status).toBe(200);
    const payload = (await response.json()) as Record<string, any>;
    expect(payload.is_ok).toBe(true);
    expect(payload.is_accepting_grants).toBe(true);
    expect(payload.apns).toEqual({
      is_available: true,
      topic: "com.blockether.vis.companion",
      environment: "production",
    });
    expect(payload.fcm).toEqual({ is_available: true, project_id: "vis-companion" });
    expect(JSON.stringify(payload)).not.toContain("PRIVATE KEY");
    expect(JSON.stringify(payload)).not.toContain(SEAL_KEY);
  });

  it("says so when nothing is configured, instead of pretending", async () => {
    const env = makeEnv({ RELAY_SEAL_KEY: undefined, APNS_KEY_P8: undefined, FCM_SERVICE_ACCOUNT: undefined });
    const payload = (await (await handle(new Request("https://relay.example.com/healthz"), env, makeDeps(fakeFetch(ok).fn))).json()) as Record<string, any>;
    expect(payload.is_accepting_grants).toBe(false);
    expect(payload.apns.is_available).toBe(false);
    expect(payload.fcm.is_available).toBe(false);
  });
});

describe("minting a grant", () => {
  it("hands back a sealed grant that does not contain the device token", async () => {
    const env = makeEnv();
    const response = await handle(post("/v1/grants", { device_token: APPLE_TOKEN, platform: "ios" }), env, makeDeps(fakeFetch(ok).fn));
    expect(response.status).toBe(201);
    const payload = (await response.json()) as Record<string, any>;
    expect(payload.grant.startsWith("vg1.")).toBe(true);
    // The gateway that will hold this string must not be able to read a device
    // token out of it — that is the difference between a capability and an id.
    expect(payload.grant).not.toContain(APPLE_TOKEN);
    expect(payload.relay_url).toBe("https://relay.example.com");
    expect(payload.platform).toBe("ios");
    expect(payload.expires_at).toBe(1_700_000_000_000 + 90 * 86_400_000);
  });

  it("mints a different grant every time for the same device", async () => {
    const env = makeEnv();
    const deps = makeDeps(fakeFetch(ok).fn);
    expect(await mint(env, deps)).not.toBe(await mint(env, deps));
  });

  it("refuses a token that is not shaped like a registration token", async () => {
    const env = makeEnv();
    const deps = makeDeps(fakeFetch(ok).fn);
    for (const token of ["", "short", `${APPLE_TOKEN}/../../3/device/victim`, "zz".repeat(32)]) {
      const response = await handle(post("/v1/grants", { device_token: token, platform: "ios" }), env, deps);
      expect(response.status).toBe(400);
    }
  });

  it("refuses an unknown platform", async () => {
    const response = await handle(
      post("/v1/grants", { device_token: APPLE_TOKEN, platform: "windows" }),
      makeEnv(),
      makeDeps(fakeFetch(ok).fn),
    );
    expect(response.status).toBe(400);
  });

  it("refuses to mint at all when the relay has no seal key", async () => {
    const response = await handle(
      post("/v1/grants", { device_token: APPLE_TOKEN, platform: "ios" }),
      makeEnv({ RELAY_SEAL_KEY: undefined }),
      makeDeps(fakeFetch(ok).fn),
    );
    expect(response.status).toBe(503);
  });
});

describe("pushing", () => {
  it("signs an ES256 provider token and posts the alert to the device path", async () => {
    const apns = fakeFetch(ok);
    const env = makeEnv();
    const deps = makeDeps(apns.fn);
    const grant = await mint(env, deps);

    const response = await handle(pushRequest(grant, { badge: 3, thread_id: "s-1", is_mutable: true }), env, deps);
    expect(response.status).toBe(200);
    expect(await response.json()).toEqual({ is_delivered: true, status: 200, reason: "", environment: "production" });

    expect(apns.calls).toHaveLength(1);
    const call = apns.calls[0];
    expect(call.url).toBe(`${APNS_PRODUCTION_HOST}/3/device/${APPLE_TOKEN}`);
    expect(call.headers["apns-topic"]).toBe("com.blockether.vis.companion");
    expect(call.headers["apns-push-type"]).toBe("alert");

    const jwt = call.headers.authorization.replace("bearer ", "").replace("Bearer ", "");
    const { header, claims } = decodeJwt(jwt);
    expect(header).toEqual({ alg: "ES256", kid: "KEYID12345" });
    expect(claims.iss).toBe("TEAM123456");
    expect(await verifyJwt(jwt, apple.publicKey, "ES256")).toBe(true);

    const payload = JSON.parse(call.body) as any;
    expect(payload.aps.alert).toEqual({ title: "Vis", body: "a session needs you" });
    expect(payload.aps.badge).toBe(3);
    expect(payload.aps["thread-id"]).toBe("s-1");
    expect(payload.aps["mutable-content"]).toBe(1);
  });

  it("reuses one provider token across pushes", async () => {
    const apns = fakeFetch(ok);
    const env = makeEnv();
    const deps = makeDeps(apns.fn);
    const grant = await mint(env, deps);
    await handle(pushRequest(grant), env, deps);
    await handle(pushRequest(grant), env, deps);
    expect(apns.calls[0].headers.authorization).toBe(apns.calls[1].headers.authorization);
  });

  it("retries the other APNs environment and reports the one that worked", async () => {
    const apns = fakeFetch((url) =>
      url.startsWith(APNS_PRODUCTION_HOST)
        ? new Response(JSON.stringify({ reason: "BadDeviceToken" }), { status: 400 })
        : ok(),
    );
    const env = makeEnv();
    const deps = makeDeps(apns.fn);
    const grant = await mint(env, deps);

    const response = await handle(pushRequest(grant), env, deps);
    expect(response.status).toBe(200);
    expect(((await response.json()) as any).environment).toBe("sandbox");
    expect(apns.calls.map((c) => c.url.startsWith(APNS_SANDBOX_HOST))).toEqual([false, true]);
  });

  it("answers 410 when Apple says the device is gone, so the gateway forgets it", async () => {
    const apns = fakeFetch(
      () => new Response(JSON.stringify({ reason: "Unregistered" }), { status: 410 }),
    );
    const env = makeEnv();
    const deps = makeDeps(apns.fn);
    const grant = await mint(env, deps);
    const response = await handle(pushRequest(grant), env, deps);
    expect(response.status).toBe(410);
    expect((await response.json()) as any).toMatchObject({ is_delivered: false, is_dead: true });
  });

  it("answers 502 when the provider merely fails", async () => {
    const apns = fakeFetch(() => new Response("boom", { status: 500 }));
    const env = makeEnv();
    const deps = makeDeps(apns.fn);
    const grant = await mint(env, deps);
    expect((await handle(pushRequest(grant), env, deps)).status).toBe(502);
  });

  it("requires a grant", async () => {
    const response = await handle(post("/v1/push", { title: "hi" }), makeEnv(), makeDeps(fakeFetch(ok).fn));
    expect(response.status).toBe(401);
  });

  it("refuses a forged grant, an expired one, and one sealed by another relay", async () => {
    const env = makeEnv();
    const deps = makeDeps(fakeFetch(ok).fn);
    const foreign = await seal("some-other-relays-key", {
      deviceToken: APPLE_TOKEN,
      platform: "ios",
      environment: "production",
      expiresAt: 9_999_999_999_999,
    });
    const expired = await seal(SEAL_KEY, {
      deviceToken: APPLE_TOKEN,
      platform: "ios",
      environment: "production",
      expiresAt: 1_600_000_000_000,
    });
    for (const grant of ["vg1.not-even-base64!!", "vg1.AAAAAAAAAAAAAAAAAAAAAAAA", "nonsense", foreign, expired]) {
      const response = await handle(pushRequest(grant), env, deps);
      expect(response.status, grant.slice(0, 16)).toBe(404);
    }
  });

  it("still opens a grant sealed with the key being rotated out", async () => {
    const apns = fakeFetch(ok);
    const deps = makeDeps(apns.fn);
    const grant = await mint(makeEnv(), deps);
    const rotated = makeEnv({ RELAY_SEAL_KEY: "the-new-key-bbbbbbbbbbbbbbbbbbbbbbbb", RELAY_SEAL_KEY_PREVIOUS: SEAL_KEY });
    expect((await handle(pushRequest(grant), rotated, deps)).status).toBe(200);
    // ...and once the previous key is dropped, that grant is dead everywhere.
    const finished = makeEnv({ RELAY_SEAL_KEY: "the-new-key-bbbbbbbbbbbbbbbbbbbbbbbb" });
    expect((await handle(pushRequest(grant), finished, deps)).status).toBe(404);
  });

  it("says so when it holds no key for the grant's platform", async () => {
    const env = makeEnv({ APNS_KEY_P8: undefined });
    const deps = makeDeps(fakeFetch(ok).fn);
    const grant = await mint(env, deps);
    expect((await handle(pushRequest(grant), env, deps)).status).toBe(503);
  });

  it("sends an Android grant through FCM with an RS256 token", async () => {
    const fcm = fakeFetch((url) =>
      url.includes("oauth2")
        ? new Response(JSON.stringify({ access_token: "ya29.fake", expires_in: 3600 }), { status: 200 })
        : ok(),
    );
    const env = makeEnv();
    const deps = makeDeps(fcm.fn);
    const grant = await mint(env, deps, { device_token: ANDROID_TOKEN, platform: "android" });

    expect((await handle(pushRequest(grant), env, deps)).status).toBe(200);
    expect(fcm.calls[1].url).toBe("https://fcm.googleapis.com/v1/projects/vis-companion/messages:send");
    expect(fcm.calls[1].headers.authorization).toBe("Bearer ya29.fake");
    expect(JSON.parse(fcm.calls[1].body).message.token).toBe(ANDROID_TOKEN);

    const assertion = new URLSearchParams(fcm.calls[0].body).get("assertion") ?? "";
    expect(decodeJwt(assertion).header.alg).toBe("RS256");
    expect(await verifyJwt(assertion, google.publicKey, "RS256")).toBe(true);
  });

  it("answers 410 when Google says the token is unregistered", async () => {
    const fcm = fakeFetch((url) =>
      url.includes("oauth2")
        ? new Response(JSON.stringify({ access_token: "ya29.fake", expires_in: 3600 }), { status: 200 })
        : new Response(JSON.stringify({ error: { status: "NOT_FOUND", details: [{ errorCode: "UNREGISTERED" }] } }), { status: 404 }),
    );
    const env = makeEnv();
    const deps = makeDeps(fcm.fn);
    const grant = await mint(env, deps, { device_token: ANDROID_TOKEN, platform: "android" });
    expect((await handle(pushRequest(grant), env, deps)).status).toBe(410);
  });
});

describe("what an unwelcome caller costs", () => {
  it("refuses a flood of forged grants before opening a single one", async () => {
    const apns = fakeFetch(ok);
    const address = limiter(2);
    const env = makeEnv({ PUSH_ADDRESS_LIMIT: address, PUSH_DEVICE_LIMIT: limiter() });
    const deps = makeDeps(apns.fn);

    const statuses: number[] = [];
    for (let i = 0; i < 4; i += 1) {
      statuses.push((await handle(pushRequest("vg1.forged"), env, deps)).status);
    }
    // The first two pay for an AES open and answer "unknown"; the rest are
    // refused at the door, and NOTHING ever reached a provider.
    expect(statuses).toEqual([404, 404, 429, 429]);
    expect(apns.calls).toHaveLength(0);
    // The flood is charged to its own address only.
    expect(new Set(address.keys)).toEqual(new Set(["push:203.0.113.7"]));
  });

  it("meters pushes per device, so a second grant for one phone buys nothing", async () => {
    const apns = fakeFetch(ok);
    const device = limiter(1);
    const env = makeEnv({ PUSH_DEVICE_LIMIT: device });
    const deps = makeDeps(apns.fn);
    const first = await mint(env, deps);
    const second = await mint(env, deps);
    expect(first).not.toBe(second);
    expect((await handle(pushRequest(first), env, deps)).status).toBe(200);
    expect((await handle(pushRequest(second), env, deps)).status).toBe(429);
    // Keyed by a hash of the token: the same phone, one bucket, no token here.
    expect(new Set(device.keys).size).toBe(1);
    expect(device.keys[0]).not.toContain(APPLE_TOKEN);
  });

  it("meters minting per address", async () => {
    const env = makeEnv({ MINT_LIMIT: limiter(1) });
    const deps = makeDeps(fakeFetch(ok).fn);
    const body = { device_token: APPLE_TOKEN, platform: "ios" };
    expect((await handle(post("/v1/grants", body), env, deps)).status).toBe(201);
    expect((await handle(post("/v1/grants", body), env, deps)).status).toBe(429);
  });

  it("refuses an oversized body by its declaration, before pulling a byte of it", async () => {
    const body = countingBody(64);
    const request = new Request("https://relay.example.com/v1/push", {
      method: "POST",
      headers: { "content-length": String(64 * 1024 * 1024), "content-type": "application/json" },
      body: body.stream,
      duplex: "half",
    } as RequestInit);
    expect((await handle(request, makeEnv(), makeDeps(fakeFetch(ok).fn))).status).toBe(413);
    expect(body.pulled).toBe(0);
  });

  /**
   * A chunked body declares no length, so the cap has to be enforced as the
   * bytes arrive: buffering 64 MiB and measuring afterwards is how one
   * unauthenticated POST takes an isolate down with it.
   */
  it("refuses an undeclared oversized body without buffering it", async () => {
    const body = countingBody(64);
    const request = new Request("https://relay.example.com/v1/push", {
      method: "POST",
      headers: { "content-type": "application/json", "cf-connecting-ip": "203.0.113.7" },
      body: body.stream,
      duplex: "half",
    } as RequestInit);
    expect((await handle(request, makeEnv(), makeDeps(fakeFetch(ok).fn))).status).toBe(413);
    expect(body.pulled).toBe(1);
  });

  it("caps the custom data keys that reach a provider", async () => {
    const apns = fakeFetch(ok);
    const env = makeEnv();
    const deps = makeDeps(apns.fn);
    const grant = await mint(env, deps);
    const data = Object.fromEntries(Array.from({ length: 200 }, (_, i) => [`k${i}`, "v"]));
    expect((await handle(pushRequest(grant, { data }), env, deps)).status).toBe(200);
    const payload = JSON.parse(apns.calls[0].body) as Record<string, unknown>;
    expect(Object.keys(payload).filter((key) => key !== "aps").length).toBeLessThanOrEqual(32);
  });
});

describe("routing", () => {
  it("answers a CORS preflight, because the companion is a WebView", async () => {
    const response = await handle(
      new Request("https://relay.example.com/v1/push", { method: "OPTIONS" }),
      makeEnv(),
      makeDeps(fakeFetch(ok).fn),
    );
    expect(response.status).toBe(204);
    expect(response.headers.get("access-control-allow-origin")).toBe("*");
    expect(response.headers.get("access-control-allow-headers")).toContain("authorization");
  });

  it("404s an unknown route and never echoes a query string back", async () => {
    const response = await handle(
      new Request("https://relay.example.com/v1/grants/<script>?x=1"),
      makeEnv(),
      makeDeps(fakeFetch(ok).fn),
    );
    expect(response.status).toBe(404);
    expect(await response.text()).not.toContain("x=1");
    expect(response.headers.get("x-content-type-options")).toBe("nosniff");
  });
});

describe("when the relay itself is misconfigured", () => {
  /**
   * A `.p8` pasted with its newlines mangled used to throw out of `atob` and
   * reach the caller as a bare 500 carrying a stack trace, on every push, with
   * `/healthz` still cheerfully reporting the provider as available.
   */
  it("treats key material it cannot decode as no key at all", async () => {
    const env = makeEnv({ APNS_KEY_P8: '"-----BEGIN PRIVATE KEY-----\nnot base64!!\n' });
    const deps = makeDeps(fakeFetch(ok).fn);

    const health = (await (
      await handle(new Request("https://relay.example.com/healthz"), env, deps)
    ).json()) as { apns: { is_available: boolean } };
    expect(health.apns.is_available).toBe(false);

    const response = await handle(pushRequest(await mint(env, deps)), env, deps);
    expect(response.status).toBe(503);
    expect((await response.json()) as { error: { code: string } }).toMatchObject({
      error: { code: "provider_unconfigured" },
    });
  });

  it("answers an unexpected throw in JSON, without a stack trace", async () => {
    const env = makeEnv({
      MINT_LIMIT: {
        limit: () => {
          throw new Error("binding is missing at /Users/someone/secret/path.ts:12");
        },
      } as unknown as RateLimit,
    });
    const response = await handle(
      post("/v1/grants", { device_token: APPLE_TOKEN }),
      env,
      makeDeps(fakeFetch(ok).fn),
    );
    expect(response.status).toBe(500);
    const text = await response.text();
    expect(JSON.parse(text)).toMatchObject({ error: { code: "internal_error" } });
    expect(text).not.toContain("/Users/");
  });
});
