// @vitest-environment jsdom
import { act, screen, waitFor } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

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

  // Regression, issue: a fleet search sat completely silent for as long as it took —
  // the row above the list said "0 matches" and the empty list said "No matching
  // sessions", both of them answers this screen did not have yet, and the real rows
  // appeared much later with no word in between about where the search was.
  it("says a search is IN FLIGHT before it can say what it found", async () => {
    const view = renderSessionsScreen({ machines: machines([hit]) });
    restore = view.restore;
    await screen.findByText("First");

    view.setQuery("needle");
    expect(await screen.findByText("searching...")).toBeTruthy();
    // Not a result, so not a dead end either.
    expect(document.body.textContent).not.toContain("No matching sessions");
    await waitFor(() => expect(screen.getByText("1 match")).toBeTruthy());
    expect(screen.queryByText("searching...")).toBeNull();
  });

  // A fleet search is one round trip PER MACHINE, so "how far along" is a real
  // number the reader can be given — and the machine that answered must not be held
  // behind the one that has not.
  it("reports how much of the fleet has answered, and paints the machine that did", async () => {
    const view = renderSessionsScreen({
      machines: [
        ...machines([hit]),
        {
          label: "beta",
          sessions: [listSession({ id: "b1", title: "Second" })],
          hangs: true,
        },
      ],
    });
    restore = view.restore;
    await screen.findByText("First");
    await screen.findByText("Second");

    view.setQuery("needle");
    expect(await screen.findByText("searching 1 of 2 machines...")).toBeTruthy();
    // alpha's hit is on screen while beta is still out, and beta's own section says
    // it is still reading rather than that it found nothing.
    expect(screen.getByText("First")).toBeTruthy();
    expect(screen.getByText("Searching this machine...")).toBeTruthy();
  });
});

// Regression, user report (paraphrased: "if we have many gateways this can be even
// worse, and the search might not take into account that some gateway is dead or not
// available"): every paired machine was asked — including one the fleet had already
// dropped out of `All` for failing its list read — and a machine that never answered
// held the progress line for the transport's whole 30s budget before being filed as an
// answer of "no matches". Both halves told the reader a machine had looked and found
// nothing when it had not looked at all.
describe("a dead machine is reported, not counted as an answer", () => {
  it("says a machine could not be reached instead of that it found nothing", async () => {
    const view = renderSessionsScreen({
      machines: [
        ...machines([hit]),
        { label: "beta", down: true, sessions: [listSession({ id: "b1", title: "Second" })] },
      ],
    });
    restore = view.restore;
    await screen.findByText("First");
    view.requests.length = 0;

    view.setQuery("needle");
    expect(await screen.findByText("1 machine did not answer")).toBeTruthy();
    // It found alpha's hit, and it does not pretend beta looked.
    expect(screen.getByText("1 match")).toBeTruthy();
    expect(document.body.textContent).not.toContain("No matches on this machine");
    // A machine that is already known to be dark is asked for nothing at all.
    expect(searches(view.requests)).toEqual(["needle"]);
  });
});

describe("a search gives up on a silent machine on its own clock", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    vi.useRealTimers();
  });

  /** Let every request, timer and repaint that fits inside `ms` happen. */
  const settle = async (ms = 0) => {
    await act(async () => {
      await vi.advanceTimersByTimeAsync(ms);
    });
  };

  it("stops waiting on a blackholed machine long before the transport would", async () => {
    const view = renderSessionsScreen({
      machines: [
        ...machines([hit]),
        {
          label: "beta",
          sessions: [listSession({ id: "b1", title: "Second" })],
          hangs: true,
        },
      ],
    });
    restore = view.restore;
    await settle(50);

    view.setQuery("needle");
    await settle(300);
    expect(screen.getByText("searching 1 of 2 machines...")).toBeTruthy();

    // Seven seconds of silence is still a machine reading its transcripts.
    await settle(7_000);
    expect(screen.getByText("searching 1 of 2 machines...")).toBeTruthy();

    // Past the search's own deadline it is absence, and it is reported as absence —
    // not as a thirty-second wait ending in "no matches".
    await settle(2_000);
    expect(screen.queryByText("searching 1 of 2 machines...")).toBeNull();
    expect(screen.getByText("1 machine did not answer")).toBeTruthy();
    expect(screen.getByText("1 match")).toBeTruthy();
  });

  // A machine that stopped answering is not a machine that read its transcripts and
  // found nothing — the dead end the empty list offers has to say which one happened.
  it("says the machine never answered rather than that nothing matched", async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          label: "alpha",
          sessions: [listSession({ id: "a1", title: "First" })],
          hangs: true,
        },
      ],
    });
    restore = view.restore;
    await settle(50);

    view.setQuery("needle");
    await settle(9_000);
    expect(screen.getByText("This machine did not answer.")).toBeTruthy();
    expect(document.body.textContent).not.toContain("Nothing on any paired machine");
  });
});

// Regression, user report (paraphrased: "make sure we are not putting search requests to
// machines that are genuinely dead"): a machine that had just spent a whole search
// deadline in silence was asked again by the very next query, so every further keystroke
// bought another eight seconds of waiting on a gateway already known to be dark — and the
// more gateways are paired, the more of them there are to wait on.
describe("a machine already known to be dark is not asked again", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    vi.useRealTimers();
  });

  /** Let every request, timer and repaint that fits inside `ms` happen. */
  const settle = async (ms = 0) => {
    await act(async () => {
      await vi.advanceTimersByTimeAsync(ms);
    });
  };

  /** Alive to the list, silent to every search — dark in the only way a search can tell. */
  const fleet = () => [
    ...machines([hit]),
    {
      label: "beta",
      sessions: [listSession({ id: "b1", title: "Second" })],
      searchHangs: true,
    },
  ];

  it("answers for a silent machine without spending a second request on it", async () => {
    const view = renderSessionsScreen({ machines: fleet() });
    restore = view.restore;
    await settle(50);
    view.requests.length = 0;

    view.setQuery("needle");
    await settle(9_000);
    // Both were asked the first time — nothing yet said beta was dark.
    expect(searches(view.requests)).toEqual(["needle", "needle"]);
    expect(screen.getByText("1 machine did not answer")).toBeTruthy();

    view.requests.length = 0;
    view.setQuery("other");
    await settle(300);
    // The second query goes ONLY to the machine that answers. Beta is reported as
    // unreachable at once, instead of holding the fleet for another deadline.
    expect(searches(view.requests)).toEqual(["other"]);
    expect(screen.getByText("1 machine did not answer")).toBeTruthy();
    expect(screen.getByText("1 match")).toBeTruthy();
  });

  it("asks it again as soon as a list read proves the machine alive", async () => {
    const view = renderSessionsScreen({ machines: fleet() });
    restore = view.restore;
    await settle(50);

    view.setQuery("needle");
    await settle(9_000);
    expect(screen.getByText("1 machine did not answer")).toBeTruthy();

    // The blackout is a memory of one failure, not a verdict on the machine: the 10s
    // poll's list read lands and the next search puts its question to beta again.
    view.requests.length = 0;
    await settle(2_000);
    view.setQuery("third");
    await settle(300);
    expect(searches(view.requests)).toEqual(["third", "third"]);
  });
});
