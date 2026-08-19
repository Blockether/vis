// @vitest-environment jsdom
import { fireEvent, waitFor } from "@testing-library/react";
import { afterEach, describe, expect, it } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";
import type { GatewayConn } from "../lib/types";

afterEach(() => {
  globalThis.localStorage?.clear();
});

// Regression, user report from the simulator: reaching Settings THROUGH a session --
// list, session, cog -- painted the machine's own bands (Notifications, MCP servers,
// providers) missing, while opening the cog straight off the list painted them. The
// row handed the app the connection this screen was built with, saved before the
// machine's durable id was backfilled, so Settings opened on an identity no paired
// machine carried.
describe("a machine whose id is backfilled after the fleet was built", () => {
  it("opens a session on the connection the app now holds", async () => {
    const opened: GatewayConn[] = [];
    const view = renderSessionsScreen({
      machines: [{ sessions: [listSession({ id: "s1", title: "A session" })] }],
      onOpen: (conn) => {
        opened.push(conn);
      },
    });
    try {
      await waitFor(() => expect(view.getByText("A session")).toBeTruthy());

      // What the app saves once `/identify` answers: the same machine at the same
      // address, now carrying the identity that survives an address change.
      view.setConns(
        view.conns.map((conn) => ({
          ...conn,
          id: "be2c15686eaef0f4",
          alts: [conn.url],
        })),
      );
      fireEvent.click(view.getByText("A session"));

      await waitFor(() => expect(opened).toHaveLength(1));
      expect(opened[0].id).toBe("be2c15686eaef0f4");
      expect(opened[0].alts).toEqual([view.conns[0].url]);
    } finally {
      view.restore();
    }
  });

  it("keeps the rows it already painted while it takes the new facts", async () => {
    const view = renderSessionsScreen({
      machines: [{ sessions: [listSession({ id: "s1", title: "A session" })] }],
    });
    try {
      await waitFor(() => expect(view.getByText("A session")).toBeTruthy());
      view.setConns(view.conns.map((conn) => ({ ...conn, id: "m1" })));
      // Learning an id is not news about a machine's sessions: nothing reloads, so
      // the list never blinks back to its empty frame.
      expect(view.getByText("A session")).toBeTruthy();
    } finally {
      view.restore();
    }
  });
});
