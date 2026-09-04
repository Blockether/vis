import { afterEach, describe, expect, it, vi } from "vitest";

const native = vi.hoisted(() => ({
  platform: "ios",
  available: true,
  isMac: true,
  fails: false,
}));

vi.mock("@capacitor/core", () => ({
  Capacitor: {
    getPlatform: () => native.platform,
    isPluginAvailable: (name: string) => native.available && name === "VisHost",
  },
  registerPlugin: () => ({
    info: async () => {
      if (native.fails) throw new Error("not implemented");
      return { isMac: native.isMac };
    },
  }),
}));

describe("the iOS host", () => {
  afterEach(() => {
    vi.resetModules();
    native.platform = "ios";
    native.available = true;
    native.isMac = true;
    native.fails = false;
  });

  // Regression, user report (paraphrased: installed on a MacBook, tapping the input
  // shows a grey field where a keyboard would be): every web-side signal on a Mac
  // window said "iPad", so only the native host can say it is not one.
  it("learns it is a Mac window from the native host", async () => {
    const host = await import("./host");
    expect(host.isIosAppOnMac()).toBe(false);
    await host.loadHost();
    expect(host.isIosAppOnMac()).toBe(true);
  });

  it("stays an iPad when the host says so, or cannot say", async () => {
    native.isMac = false;
    let host = await import("./host");
    await host.loadHost();
    expect(host.isIosAppOnMac()).toBe(false);

    vi.resetModules();
    native.isMac = true;
    native.fails = true;
    host = await import("./host");
    await host.loadHost();
    expect(host.isIosAppOnMac()).toBe(false);

    vi.resetModules();
    native.fails = false;
    native.available = false;
    host = await import("./host");
    await host.loadHost();
    expect(host.isIosAppOnMac()).toBe(false);
  });

  it("never asks a browser or Android", async () => {
    native.platform = "web";
    const host = await import("./host");
    await host.loadHost();
    expect(host.isIosAppOnMac()).toBe(false);
  });
});
