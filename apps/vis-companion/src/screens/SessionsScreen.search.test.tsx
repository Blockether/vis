// @vitest-environment jsdom
import { screen, waitFor } from "@testing-library/react";
import { afterEach, describe, expect, it } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

let restore = () => {};
afterEach(() => restore());

const hit = { session_id: "a1", snippet: "the needle", role: "user" };

const machines = (matches: unknown[]) => [
  {
    label: "alpha",
    sessions: [listSession({ id: "a1", title: "First" })],
    routes: { "/v1/sessions/actions/search": { matches } },
  },
];

const searches = (requests: { path: string }[]) =>
  requests
    .filter((request) => request.path.startsWith("/v1/sessions/actions/search"))
    .map((request) => new URLSearchParams(request.path.split("?")[1]).get("q"));

// The fleet search is one ranked FTS query per paired machine and the gateway spends real
// time in SQLite before it answers, so the ONE thing this screen owes the network is
// restraint: ask once when typing rests, and never let a query the user has already
// replaced land on top of the one they are looking at.
describe("fleet search asks the gateway once per pause", () => {
  it("asks once for the query the typing rested on, not once per keystroke", async () => {
    const view = renderSessionsScreen({ machines: machines([hit]) });
    restore = view.restore;
    await screen.findByText("First");
    view.requests.length = 0;

    view.setQuery("n");
    view.setQuery("ne");
    view.setQuery("needle");
    await waitFor(() => expect(searches(view.requests)).toEqual(["needle"]));
    // Nothing else lands after the pause either.
    await new Promise((resolve) => setTimeout(resolve, 250));
    expect(searches(view.requests)).toEqual(["needle"]);
  });

  it("spends nothing at all on an empty query", async () => {
    const view = renderSessionsScreen({ machines: machines([hit]) });
    restore = view.restore;
    await screen.findByText("First");
    view.requests.length = 0;

    view.setQuery("   ");
    await new Promise((resolve) => setTimeout(resolve, 300));
    expect(searches(view.requests)).toEqual([]);
  });

  it("cancels a superseded query: the flight the user replaced is aborted", async () => {
    const view = renderSessionsScreen({ machines: machines([hit]) });
    restore = view.restore;
    await screen.findByText("First");
    view.requests.length = 0;

    view.setQuery("first");
    await waitFor(() => expect(searches(view.requests)).toEqual(["first"]));
    const superseded = view.requests.at(-1)!;
    view.setQuery("second");
    await waitFor(() => expect(searches(view.requests)).toEqual(["first", "second"]));
    // A response that outran its own cancellation must not be written on top of the
    // query the user is now looking at.
    expect(superseded.signal?.aborted).toBe(true);
  });

  // While a query is live the header reports the SEARCH instead of the scope's totals —
  // that tally is the only proof the query left this gateway.
  it("reports the search in the header, and says so when a machine has no hit", async () => {
    const view = renderSessionsScreen({ machines: machines([hit]) });
    restore = view.restore;
    await screen.findByText("First");
    expect(screen.getByText("1 session")).toBeTruthy();

    view.setQuery("needle");
    expect(await screen.findByText("1 match")).toBeTruthy();

  });

  it("says a machine has no hit, never \"No sessions yet\"", async () => {
    const view = renderSessionsScreen({ machines: machines([]) });
    restore = view.restore;
    await screen.findByText("First");

    view.setQuery("needle");
    await waitFor(() => expect(searches(view.requests)).toEqual(["needle"]));
    await waitFor(() => expect(screen.queryByText("First")).toBeNull());
    expect(screen.getByText("0 matches")).toBeTruthy();
    expect(screen.getByText("No matching sessions")).toBeTruthy();
    expect(document.body.textContent).not.toContain("No sessions yet");
  });
});
