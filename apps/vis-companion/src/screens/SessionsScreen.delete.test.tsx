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
// asked IN the row: two full-width answers standing the row's own height. Renaming
// still needs a field; project deletion asks in its own inventory row too.
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
    // The frame must not add border pixels around the row-height answer strip.
    expect(strip.classList.contains("border")).toBe(false);
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
    fireEvent.click(await screen.findByText("No, keep"));

    await waitFor(() =>
      expect(screen.queryByRole("group", { name: "Delete First?" })).toBeNull(),
    );
    expect(screen.getByText("First")).toBeTruthy();
    expect(view.requests.some((request) => request.method === "DELETE")).toBe(false);
  });
});

// Regression, user report: project trash left its inventory row and opened a second
// dialog. The project manager must ask and commit in that exact row, like a session does.
describe("deleting a project confirms inside its inventory row", () => {
  it("keeps the projects sheet and replaces only the selected row", async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          label: "alpha",
          sessions: [listSession({ id: "a1", title: "First" })],
        },
      ],
    });
    restore = view.restore;
    await screen.findByText("First");
    fireEvent.click(screen.getByRole("button", { name: "Projects on alpha" }));
    const sheet = await screen.findByRole("dialog", {
      name: "Manage projects on alpha",
    });
    view.requests.length = 0;
    fireEvent.click(
      screen.getByRole("button", {
        name: "Remove every transcript in project",
      }),
    );

    await screen.findByRole("group", { name: "Delete project?" });
    expect(screen.getAllByRole("dialog")).toEqual([sheet]);
    expect(view.requests.some((request) => request.method === "DELETE")).toBe(false);

    fireEvent.click(screen.getByRole("button", { name: "No, keep" }));
    expect(await screen.findByRole("button", { name: /^project/ })).toBeTruthy();

    fireEvent.click(
      screen.getByRole("button", {
        name: "Remove every transcript in project",
      }),
    );
    fireEvent.click(screen.getByRole("button", { name: "Yes, delete" }));

    await waitFor(() =>
      expect(
        view.requests.some(
          (request) =>
            request.method === "DELETE" && request.path === "/v1/sessions/a1",
        ),
      ).toBe(true),
    );
    await waitFor(() =>
      expect(screen.queryByRole("group", { name: "Delete project?" })).toBeNull(),
    );
    expect(screen.getAllByRole("dialog")).toEqual([sheet]);
    expect(screen.getByText("This machine has no projects yet.")).toBeTruthy();
  });
});

// Regression, issue #2216: deleting ONE session re-read the WHOLE fleet's session
// list — every paired machine, every window of it — although the app already knew
// exactly which row had gone. On a few-hundred-session store that was ~315 KB
// re-downloaded, on every machine, to remove one row the app had just removed.
describe("deleting a session does not re-download the fleet", () => {
  const fleet = () => [
    {
      label: "alpha",
      sessions: [
        listSession({ id: "a1", title: "First" }),
        listSession({ id: "a2", title: "Second" }),
      ],
    },
    { label: "beta", sessions: [listSession({ id: "b1", title: "Elsewhere" })] },
  ];

  const listReads = (view: { requests: { method: string; path: string }[] }) =>
    view.requests.filter(
      (request) =>
        request.method === "GET" &&
        request.path.startsWith("/v1/sessions?") &&
        // A project's page is a read of ITS own (`GatewayClient.listProjectPage`).
        !request.path.includes("root="),
    );

  it("drops the active row locally and re-lists nothing", async () => {
    const view = renderSessionsScreen({ machines: fleet() });
    restore = view.restore;
    await screen.findByText("First");
    expect(screen.queryByText("Elsewhere")).toBeNull();
    view.requests.length = 0;

    fireEvent.click(
      screen.getByRole("group", { name: "First actions" }).querySelector(
        "button[aria-label='Delete']",
      )!,
    );
    fireEvent.click(await screen.findByText("Yes, delete"));

    // The row goes because the delete succeeded, not because a fresh list said so.
    await waitFor(() => expect(screen.queryByText("First")).toBeNull());
    expect(screen.getByText("Second")).toBeTruthy();
    expect(screen.queryByText("Elsewhere")).toBeNull();
    expect(listReads(view)).toEqual([]);
  });

  it("renames from the gateway's own answer, re-listing nothing", async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          label: "alpha",
          sessions: [listSession({ id: "a1", title: "First" })],
          routes: { "/v1/sessions/a1": listSession({ id: "a1", title: "Renamed" }) },
        },
        { label: "beta", sessions: [listSession({ id: "b1", title: "Elsewhere" })] },
      ],
    });
    restore = view.restore;
    await screen.findByText("First");
    expect(screen.queryByText("Elsewhere")).toBeNull();
    view.requests.length = 0;

    fireEvent.click(
      screen.getByRole("group", { name: "First actions" }).querySelector(
        "button[aria-label='Rename']",
      )!,
    );
    fireEvent.change(await screen.findByPlaceholderText("Session name"), {
      target: { value: "Renamed" },
    });
    fireEvent.click(screen.getByText("Save"));

    await waitFor(() => expect(screen.getByText("Renamed")).toBeTruthy());
    expect(screen.queryByText("Elsewhere")).toBeNull();
    expect(listReads(view)).toEqual([]);
  });
});
