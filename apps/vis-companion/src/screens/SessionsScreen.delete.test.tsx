// @vitest-environment jsdom
import { fireEvent, screen, waitFor } from "@testing-library/react";
import { afterEach, describe, expect, it } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

let restore = () => {};
afterEach(() => restore());

const machines = () => [
  {
    label: "alpha",
    sessions: [
      listSession({ id: "a1", title: "First" }),
      listSession({ id: "a2", title: "Second" }),
    ],
  },
];

// Deleting ONE session is a row-level answer to a row-level question, so it is
// asked IN the row: two full-width answers standing the row's own height. Only
// the wider blast radius (rename, group purge) still opens a dialog.
describe("deleting one session confirms inside its own row", () => {
  it("asks in the row, not in a dialog, and only that row is asked", async () => {
    const view = renderSessionsScreen({ machines: machines() });
    restore = view.restore;
    await screen.findByText("First");

    fireEvent.click(
      screen.getByRole("group", { name: "First actions" }).querySelector(
        "button[aria-label='Delete']",
      )!,
    );

    const strip = await screen.findByRole("group", { name: "Delete First?" });
    expect(strip.querySelectorAll("button")).toHaveLength(2);
    expect(screen.queryByRole("dialog")).toBeNull();
    // The neighbour keeps its own row: one question, one row.
    expect(screen.queryByRole("group", { name: "Delete Second?" })).toBeNull();
    expect(screen.getByText("Second")).toBeTruthy();
  });

  it("deletes on yes and asks the gateway for exactly that session", async () => {
    const view = renderSessionsScreen({ machines: machines() });
    restore = view.restore;
    await screen.findByText("First");
    view.requests.length = 0;

    fireEvent.click(
      screen.getByRole("group", { name: "First actions" }).querySelector(
        "button[aria-label='Delete']",
      )!,
    );
    fireEvent.click(await screen.findByText("Yes, delete"));

    await waitFor(() =>
      expect(
        view.requests.some(
          (request) =>
            request.method === "DELETE" && request.path === "/v1/sessions/a1",
        ),
      ).toBe(true),
    );
  });

  it("no keeps the session: the row comes back and nothing is sent", async () => {
    const view = renderSessionsScreen({ machines: machines() });
    restore = view.restore;
    await screen.findByText("First");
    view.requests.length = 0;

    fireEvent.click(
      screen.getByRole("group", { name: "First actions" }).querySelector(
        "button[aria-label='Delete']",
      )!,
    );
    fireEvent.click(await screen.findByText("No, keep it"));

    await waitFor(() =>
      expect(screen.queryByRole("group", { name: "Delete First?" })).toBeNull(),
    );
    expect(screen.getByText("First")).toBeTruthy();
    expect(view.requests.some((request) => request.method === "DELETE")).toBe(false);
  });
});
