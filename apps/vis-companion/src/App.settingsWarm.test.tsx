// @vitest-environment jsdom
// Regression, user report (paraphrased: "going to settings, when it opens the
// primary machine, flickers the MCP servers and the providers and the
// notifications"): every one of those panels asked its own machine at mount,
// so the dialog opened empty and filled itself a round trip later — on answers
// that change about once a week. They are read for the WHOLE fleet up front
// now, so whichever machine the reader opens has already answered.
import { screen, waitFor } from "@testing-library/react";
import { describe, expect, it } from "vitest";

import { renderApp } from "./app-harness";

/** How many times ONE machine was asked for exactly this route. */
const asked = (requests: string[], base: string, path: string) =>
  requests.filter((href) => {
    const url = new URL(href);
    return url.origin === new URL(base).origin && url.pathname === path;
  }).length;

describe("warming what the settings dialog will ask", () => {
  it("reads every panel of every paired machine, once, before one is opened", async () => {
    const view = renderApp({
      machines: [{ label: "laptop" }, { label: "buildbox" }],
    });
    try {
      // Nothing has to be OPENED for the sweep to run: it belongs to the app,
      // not to a panel.

      const panels = [
        "/v1/settings",
        "/v1/mcp/servers",
        "/v1/router",
        "/v1/devices",
      ];
      await waitFor(() => {
        for (const conn of view.conns)
          for (const path of panels)
            expect([conn.url, path, asked(view.requests, conn.url, path)]).toEqual([
              conn.url,
              path,
              1,
            ]);
      });

      // Nothing was OPENED: the sweep is the only reader so far, and it charges
      // each machine one request per panel and no more.
      expect(screen.queryByText("MCP servers")).toBeNull();
    } finally {
      view.unmount();
      view.restore();
    }
  });
});
