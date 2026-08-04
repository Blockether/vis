// Fleet membership from the app. Until these three calls existed the companion
// could only ever OPERATE providers the machine already had — there was no way
// to list what could be added, add one, or remove one — so the URLs, the shape
// they unwrap, and the router cache they must drop are all worth pinning.
import { afterEach, beforeEach, expect, it, describe } from "vitest";
import type { GatewayConn } from "./types";

// `gateway.ts` hydrates its snapshot cache and arms request timeouts the moment
// it loads, so a browser-shaped `localStorage` and `window` have to exist
// BEFORE the import — which is why the module is pulled in dynamically here.
const memory = new Map<string, string>();
Object.defineProperty(globalThis, "localStorage", {
  configurable: true,
  value: {
    getItem: (key: string) => memory.get(key) ?? null,
    setItem: (key: string, value: string) => void memory.set(key, String(value)),
    removeItem: (key: string) => void memory.delete(key),
    clear: () => memory.clear(),
    key: () => null,
    length: 0,
  },
});
(globalThis as { window?: unknown }).window ??= globalThis;

const { GatewayClient } = await import("./gateway");

const CONN = {
  id: "gw",
  name: "workstation",
  url: "http://gateway.example.com:7777",
  token: "secret",
} as unknown as GatewayConn;

interface Call {
  url: string;
  method: string;
  body: unknown;
}

let calls: Call[];
let realFetch: typeof globalThis.fetch;

/** One canned JSON answer per request, in order. */
function answerWith(...payloads: unknown[]): void {
  let next = 0;
  globalThis.fetch = (async (input: RequestInfo | URL, init?: RequestInit) => {
    calls.push({
      url: String(input),
      method: init?.method ?? "GET",
      body: init?.body ? JSON.parse(String(init.body)) : undefined,
    });
    const payload = payloads[Math.min(next++, payloads.length - 1)];
    return new Response(JSON.stringify(payload), {
      status: 200,
      headers: { "Content-Type": "application/json" },
    });
  }) as typeof globalThis.fetch;
}

beforeEach(() => {
  calls = [];
  realFetch = globalThis.fetch;
});

afterEach(() => {
  globalThis.fetch = realFetch;
});

describe("provider presets", () => {
  it("reads the addable presets off their own route", async () => {
    answerWith({
      presets: [{ id: "lmstudio", label: "LM Studio", auth_kind: "none", is_local: true, models: [] }],
    });
    const presets = await new GatewayClient(CONN).providerPresets();

    expect(presets.map((preset) => preset.id)).toEqual(["lmstudio"]);
    expect(calls[0].method).toBe("GET");
    // NOT `/v1/providers/presets`: that path collides with `/v1/providers/:id`.
    expect(calls[0].url).toBe("http://gateway.example.com:7777/v1/provider-presets");
  });

  it("treats a daemon that reports no presets as an empty picker, not a crash", async () => {
    answerWith({});
    expect(await new GatewayClient(CONN).providerPresets()).toEqual([]);
  });
});

describe("fleet membership", () => {
  it("adds a preset by id and carries the base url the user owns", async () => {
    answerWith({ providers: [{ id: "lmstudio", label: "LM Studio" }] });
    const rows = await new GatewayClient(CONN).addProvider("lmstudio", "http://10.0.0.5:1234/v1");

    expect(rows.map((row) => row.id)).toEqual(["lmstudio"]);
    expect(calls[0].method).toBe("POST");
    expect(calls[0].url).toBe("http://gateway.example.com:7777/v1/providers");
    expect(calls[0].body).toEqual({ id: "lmstudio", base_url: "http://10.0.0.5:1234/v1" });
  });

  it("omits the base url for a provider that does not own one", async () => {
    answerWith({ providers: [] });
    await new GatewayClient(CONN).addProvider("anthropic-coding-plan");
    expect(calls[0].body).toEqual({ id: "anthropic-coding-plan" });
  });

  it("removes a provider by id", async () => {
    answerWith({ providers: [], is_removed: true });
    await new GatewayClient(CONN).removeProvider("lm studio/1");

    expect(calls[0].method).toBe("DELETE");
    expect(calls[0].url).toBe("http://gateway.example.com:7777/v1/providers/lm%20studio%2F1");
  });

  it("drops the shared router cache, so the next read is not the fleet from before the add", async () => {
    const client = new GatewayClient(CONN);
    answerWith(
      { providers: [{ id: "zai-coding-plan" }] },
      { providers: [{ id: "zai-coding-plan" }, { id: "lmstudio" }] },
      { providers: [{ id: "zai-coding-plan" }, { id: "lmstudio" }] },
    );

    await client.router();
    await client.router(); // cached: still one request
    expect(calls).toHaveLength(1);

    await client.addProvider("lmstudio");
    await client.router();
    expect(calls).toHaveLength(3);
    expect(calls[2].url).toBe("http://gateway.example.com:7777/v1/router");
  });
});
