// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

const native = vi.hoisted(
  () => new Map<string, (info?: { isActive: boolean }) => void>(),
);
vi.mock("@capacitor/app", () => ({
  App: {
    addListener: async (
      name: string,
      listener: (info?: { isActive: boolean }) => void,
    ) => {
      native.set(name, listener);
      return { remove: () => native.delete(name) };
    },
  },
}));

let visibility: DocumentVisibilityState;
const stops: Array<() => void> = [];
beforeEach(() => {
  vi.resetModules();
  vi.useFakeTimers();
  vi.setSystemTime(new Date("2026-09-08T12:00:00Z"));
  visibility = "visible";
  vi.spyOn(document, "visibilityState", "get").mockImplementation(() => visibility);
});
afterEach(async () => {
  for (const stop of stops.splice(0)) stop();
  await Promise.resolve();
  native.clear();
  vi.useRealTimers();
  vi.restoreAllMocks();
});

async function subscribe() {
  const { onWake, onAway } = await import("./wake");
  const wake = vi.fn();
  const away = vi.fn();
  stops.push(onWake(wake), onAway(away));
  return { wake, away };
}

// Input-lag report: foreground focus must not restart streams and reload session data.
// The supplied diagnostics recorded repeated wakes with no intervening background event.
describe("foreground wake signals", () => {
  it("does not wake an already-visible app when the window receives focus", async () => {
    const { wake, away } = await subscribe();
    for (let index = 0; index < 3; index += 1) {
      window.dispatchEvent(new Event("focus"));
      vi.advanceTimersByTime(800);
    }
    expect(wake).not.toHaveBeenCalled();
    expect(away).not.toHaveBeenCalled();
  });

  it("does not treat a duplicate visible notification as another resume", async () => {
    const { wake } = await subscribe();
    document.dispatchEvent(new Event("visibilitychange"));
    vi.advanceTimersByTime(800);
    expect(wake).not.toHaveBeenCalled();
  });

  it("keeps focus as a fallback after a real absence", async () => {
    const { wake, away } = await subscribe();
    window.dispatchEvent(new Event("pagehide"));
    vi.advanceTimersByTime(1_000);
    window.dispatchEvent(new Event("focus"));
    vi.advanceTimersByTime(250);
    expect(away).toHaveBeenCalledOnce();
    expect(wake).toHaveBeenCalledExactlyOnceWith({ awayMs: 1_250 });
  });

  it("resumes through stale native visibility without reloading again on later focus", async () => {
    const { wake } = await subscribe();
    visibility = "hidden";
    native.get("pause")!();
    vi.advanceTimersByTime(1_000);
    native.get("appStateChange")!({ isActive: true });
    native.get("resume")!();
    vi.advanceTimersByTime(250);
    expect(wake).toHaveBeenCalledExactlyOnceWith({ awayMs: 1_250 });

    vi.advanceTimersByTime(800);
    visibility = "visible";
    document.dispatchEvent(new Event("visibilitychange"));
    window.dispatchEvent(new Event("focus"));
    vi.advanceTimersByTime(250);
    expect(wake).toHaveBeenCalledOnce();
  });

  it("still accepts a native resume when the earlier pause was not observed", async () => {
    const { wake } = await subscribe();
    visibility = "hidden";
    native.get("resume")!();
    vi.advanceTimersByTime(250);
    expect(wake).toHaveBeenCalledExactlyOnceWith({ awayMs: 0 });
  });

  it("still refreshes after network recovery while visible", async () => {
    const { wake } = await subscribe();
    window.dispatchEvent(new Event("online"));
    vi.advanceTimersByTime(250);
    expect(wake).toHaveBeenCalledExactlyOnceWith({ awayMs: 0 });
  });

  it("ignores network recovery while hidden and coalesces the real return", async () => {
    const { wake, away } = await subscribe();
    visibility = "hidden";
    document.dispatchEvent(new Event("visibilitychange"));
    window.dispatchEvent(new Event("pagehide"));
    window.dispatchEvent(new Event("online"));
    vi.advanceTimersByTime(1_000);
    expect(wake).not.toHaveBeenCalled();
    expect(away).toHaveBeenCalledOnce();

    visibility = "visible";
    document.dispatchEvent(new Event("visibilitychange"));
    window.dispatchEvent(new Event("focus"));
    window.dispatchEvent(new Event("pageshow"));
    vi.advanceTimersByTime(250);
    expect(wake).toHaveBeenCalledExactlyOnceWith({ awayMs: 1_250 });
  });
});
