// @vitest-environment jsdom
// Regression, Android companion CI run 31789278484: the workflow went red on a
// suite where all 1167 tests passed. `App`'s idle warm-up threw its promise
// away (`void import("./screens/SessionScreen")`), so when that graph was still
// resolving as vitest tore the environment down under a test file that had
// already finished, the load rejected with nobody holding it —
// "Cannot load '/node_modules/@capacitor/clipboard/dist/plugin.cjs.js'
// imported from src/lib/image-share.ts after the environment was torn down",
// charged to src/lib/push-intent.test.tsx, which only ever rendered the app.
import { screen } from "@testing-library/react";
import { describe, expect, it, vi } from "vitest";

import { renderApp } from "./app-harness";
import {
  settleRejections,
  watchUnhandledRejections,
} from "./lib/unhandled.fixture";

// The first chunk the warm-up asks for, made unloadable — a stand-in for the
// deploy that replaced the asset, the WebView that lost the network, and the
// environment that went away mid-load.
vi.mock("./screens/SessionScreen", () => {
  throw new Error("chunk gone");
});

describe("warming the split screens", () => {
  it("owns the failure of a load nobody is waiting for", async () => {
    const watch = watchUnhandledRejections();
    const view = renderApp({ machines: [{ label: "laptop" }] });
    try {
      await screen.findByRole("button", { name: "Projects on laptop" });
      // Every iOS WebView is a browser without requestIdleCallback, and so is
      // jsdom: the warm-up here is the 300 ms fallback timer.
      await new Promise((resolve) => setTimeout(resolve, 400));
      await settleRejections();
      expect(watch.escaped).toEqual([]);
      // And the app is still the app: a chunk that will not warm is asked for
      // again at the tap, so nothing about the shell changed.
      expect(
        screen.getByRole("button", { name: "Open preferences" }),
      ).toBeTruthy();
    } finally {
      watch.stop();
      view.unmount();
      view.restore();
    }
  });
});
