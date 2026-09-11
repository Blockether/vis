// @vitest-environment jsdom
// Regression: a domain deeplink selected an advertised IP, and reopening it
// replaced saved settings or created another row after an address switch.
import { act, waitFor } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import { renderApp } from "./app-harness";
import type { GatewayConn } from "./lib/types";

const native = vi.hoisted(() => ({
  launch: "",
  open: (_event: { url: string }) => {},
}));
vi.mock("@capacitor/app", () => ({
  App: {
    addListener: vi.fn(async (event, handler) => {
      if (event === "appUrlOpen") native.open = handler;
      return { remove: vi.fn() };
    }),
    getLaunchUrl: vi.fn(async () => ({ url: native.launch })),
  },
}));

const DOMAIN = "https://gateway.example.com";
const TAILNET = "http://100.64.0.10:7890";
const LAN = "http://10.0.0.5:7890";
const saved = (): GatewayConn[] =>
  JSON.parse(localStorage.getItem("vis.connections") ?? "[]");
const link = (url: string, token?: string) =>
  `vis://gateway?url=${encodeURIComponent(url)}${token ? `&token=${token}` : ""}`;

let view: ReturnType<typeof renderApp> | undefined;
afterEach(() => {
  view?.unmount();
  view?.restore();
  view = undefined;
  native.launch = "";
  native.open = () => {};
  vi.clearAllMocks();
});

describe("gateway pairing deeplinks", () => {
  it("keeps a newly imported domain when the gateway advertises physical addresses", async () => {
    native.launch = link(DOMAIN, "pairing-token");
    await act(async () => {
      view = renderApp({
        initiallyPaired: false,
        machines: [{ url: DOMAIN, alts: [TAILNET, LAN] }],
      });
    });
    await waitFor(() => {
      expect(saved()).toEqual([
        expect.objectContaining({
          url: DOMAIN,
          token: "pairing-token",
        }),
      ]);
    });
    expect(view!.requests).toContain(`${DOMAIN}/v1/capabilities`);
    expect(view!.requests.some((url) => url.startsWith(TAILNET))).toBe(false);
    expect(localStorage.getItem("vis.primaryConnection")).toBe(DOMAIN);
  });

  it.each([
    ["the same URL", DOMAIN, "stale-token"],
    [
      "a normalized URL without a token",
      "https://GATEWAY.example.com:443/",
      undefined,
    ],
    ["a known alternative", LAN, "stale-token"],
  ])("reuses saved settings when reopening %s", async (_name, url, token) => {
    await act(async () => {
      view = renderApp({
        machines: [
          { url: DOMAIN, label: "My server", pinned: true, alts: [LAN] },
        ],
      });
    });
    const before = saved();
    expect(before).toHaveLength(1);
    await act(async () => {
      native.open({ url: link(url, token) });
    });
    expect(saved()).toEqual(before);
    expect(localStorage.getItem("vis.primaryConnection")).toBe(DOMAIN);
  });

  it("reuses the durable pairing on a cold start", async () => {
    native.launch = link(DOMAIN);
    await act(async () => {
      view = renderApp({
        machines: [
          { url: DOMAIN, label: "My server", pinned: true, alts: [LAN] },
        ],
      });
    });
    expect(saved()).toEqual([
      expect.objectContaining({
        ...view!.conns[0],
        alts: expect.arrayContaining([DOMAIN, LAN]),
      }),
    ]);
  });

  it("recognizes the original link after the saved address has changed", async () => {
    await act(async () => {
      view = renderApp({
        machines: [
          { url: TAILNET, label: "My server", pinned: true, alts: [DOMAIN] },
        ],
      });
    });
    const before = saved();
    await act(async () => {
      native.open({ url: link(DOMAIN) });
    });
    expect(saved()).toEqual(before);
    expect(localStorage.getItem("vis.primaryConnection")).toBe(TAILNET);
  });

  it("still fails over when the domain stops answering", async () => {
    await act(async () => {
      view = renderApp({
        machines: [{ url: DOMAIN, alts: [TAILNET] }],
        unreachable: [DOMAIN],
      });
    });
    await waitFor(() => {
      expect(saved()).toEqual([
        expect.objectContaining({ url: TAILNET, token: "t" }),
      ]);
    });
  });
});
