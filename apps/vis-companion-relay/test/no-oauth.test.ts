import { expect, it, vi } from "vitest";
import { handle } from "../src/index";
import type { Env } from "../src/types";

it("has no OAuth endpoints and never forwards authorization material", async () => {
  const fetch = vi.fn();
  for (const [method, path] of [
    ["POST", "/v1/oauth/flows"],
    ["GET", "/v1/oauth/flows/test-flow"],
    ["DELETE", "/v1/oauth/flows/test-flow"],
    ["GET", "/oauth/callback/test-flow?state=test-state&code=test-code"],
  ]) {
    const response = await handle(new Request(`https://gateway.example.com${path}`, {
      method, ...(method === "POST" ? { body: JSON.stringify({ state: "test-state" }) } : {}),
    }), {} as Env, { fetch, now: () => 0 });
    expect(response.status).toBe(404);
    expect(await response.text()).not.toContain("test-code");
  }
  expect(fetch).not.toHaveBeenCalled();
});
