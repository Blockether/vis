// @vitest-environment jsdom
import { afterEach, expect, it, vi } from "vitest";
import { GatewayClient } from "./gateway";
import { loadConnections, loadConnectionsSync, saveConnections, saveMachineOrder, setPrimaryUrl, switchConnectionUrl } from "./storage";

afterEach(() => { vi.unstubAllGlobals(); localStorage.clear(); });

it("sends identities only and respects the server response", async () => {
  const fetching = vi.fn(async () => Response.json({ machine_ids: ["primary", "z", "a"] }));
  vi.stubGlobal("fetch", fetching);
  const client = new GatewayClient({ url: "http://gateway.example.com", token: "test" });
  expect(await client.machineOrder(["a", "primary", "z"])).toEqual(["primary", "z", "a"]);
  const [url, init] = fetching.mock.calls[0] as unknown as [string, RequestInit];
  expect(url).toBe("http://gateway.example.com/v1/machines/order");
  expect(init.method).toBe("POST");
  expect(JSON.parse(init.body as string)).toEqual({ machine_ids: ["a", "primary", "z"] });
});

it("retains cached order offline, across address changes, and isolates primaries", async () => {
  const conns = ["primary", "a", "z", "new"].map(id => ({ id, url: `http://${id}.example.com` }));
  await saveConnections(conns);
  await setPrimaryUrl(conns[0]!.url);
  await saveMachineOrder("primary", ["primary", "z", "a"]);
  vi.stubGlobal("fetch", vi.fn(async () => { throw new TypeError("offline"); }));
  const ids = () => loadConnectionsSync().map(conn => conn.id);
  expect(ids()).toEqual(["primary", "z", "a", "new"]);
  await expect(new GatewayClient(conns[0]!).machineOrder(["primary"])).rejects.toThrow();
  expect((await loadConnections()).map(conn => conn.id)).toEqual(ids());
  await switchConnectionUrl(conns[2]!.url, "http://alternate.example.com");
  expect(ids()).toEqual(["primary", "z", "a", "new"]);
  await setPrimaryUrl(conns[1]!.url);
  await saveMachineOrder("a", ["a", "new", "z", "primary"]);
  expect(ids()).toEqual(["a", "new", "z", "primary"]);
  await setPrimaryUrl(conns[0]!.url);
  expect(ids()).toEqual(["primary", "z", "a", "new"]);
});
