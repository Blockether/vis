// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { fireEvent, screen, waitFor } from "@testing-library/react";

import { renderApp } from "../app-harness";
import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

const settle = (ms = 0) => new Promise((done) => setTimeout(done, ms));

/** A read of the fleet's own list — not a search, not one session. */
const isListRead = (request: { method: string; path: string }) =>
  request.method === "GET" &&
  (request.path === "/v1/sessions" || request.path.startsWith("/v1/sessions?"));

const fleet = () => [
  {
    label: "laptop",
    sessions: [
      listSession({ id: "s1", title: "A session" }),
      listSession({ id: "s2", title: "Another session" }),
    ],
  },
];

// Regression, reported from a phone ("writing in the input of the companion app
// hangs for a second or two, and it happens many times"): the sessions list stays
// MOUNTED behind an open transcript so its rows, scope and scroll position survive
// the trip — and it went on polling the whole fleet every ten seconds while nobody
// could see it. Every answer re-ran the fleet-wide filter and sort and re-rendered
// every project group, on the same main thread as the composer being typed in.
// It still may not POLL there — repetition is what the report was about — but a list
// with no rows at all takes ONE read wherever the reader is, so a relaunch straight into
// a session is not a skeleton the moment they leave it (`SessionsScreen.poll.test.tsx`).
describe("a sessions list that is not on the glass", () => {
  it("reads the fleet once, and not again until it is shown", async () => {
    const view = renderSessionsScreen({ isVisible: false, machines: fleet() });
    await settle(60);

    // ONE read: the warm-up that gives this list rows to arrive on. The poll is what
    // may not run off the glass, and it is pinned in `SessionsScreen.poll.test.tsx`.
    expect(view.requests.filter(isListRead)).toHaveLength(1);

    view.setVisible(true);
    await waitFor(() => expect(view.requests.filter(isListRead).length).toBeGreaterThan(1));
    // Shown means CURRENT: the load is the first thing coming back does.
    expect(await screen.findByText("A session")).toBeTruthy();
    view.unmount();
    view.restore();
  });

  it("is reloaded by the shell on the way back out of a session", async () => {
    const view = renderApp({ machines: fleet() });
    const inner = globalThis.fetch;
    let listReads = 0;
    globalThis.fetch = ((input: RequestInfo | URL, init?: RequestInit) => {
      const href =
        typeof input === "string"
          ? input
          : input instanceof URL
            ? input.href
            : input.url;
      if (
        (init?.method ?? "GET") === "GET" &&
        new URL(href).pathname === "/v1/sessions"
      )
        listReads += 1;
      return inner(input, init);
    }) as typeof fetch;

    fireEvent.click(await screen.findByText("A session"));
    await screen.findByLabelText("Message Vis");
    await settle(60);
    const whileReading = listReads;

    fireEvent.click(screen.getByRole("button", { name: "Back to sessions" }));

    await waitFor(() => expect(listReads).toBeGreaterThan(whileReading));
    view.unmount();
    view.restore();
  });
});
