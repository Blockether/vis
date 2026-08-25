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

// Search is one ranked FTS query on the active machine and the gateway spends real
// time in SQLite before it answers, so the ONE thing this screen owes the network is
// restraint: ask once when typing rests, and never let a query the user has already
// replaced land on top of the one they are looking at.
describe("search asks the active gateway once per pause", () => {
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

  // Search follows the selected machine. An inactive destination must not add a request
  // or make the active machine's complete answer look unfinished.
  it("does not wait for an inactive machine", async () => {
    const view = renderSessionsScreen({
      machines: [
        ...machines([hit]),
        {
          label: "beta",
          sessions: [listSession({ id: "b1", title: "Second" })],
          searchHangs: true,
        },
      ],
    });
    restore = view.restore;
    await screen.findByText("First");
    expect(screen.queryByText("Second")).toBeNull();
    view.requests.length = 0;

    view.setQuery("needle");
    expect(await screen.findByText("1 match")).toBeTruthy();
    expect(searches(view.requests)).toEqual(["needle"]);
    expect(document.body.textContent).not.toContain("machines...");
  });
});

// A machine outside the selected scope is not part of the question. In particular, a
// paired machine already known to be unreachable must not turn a complete local answer
// into a partial-fleet warning.
describe("an inactive dead machine is outside the search", () => {
  it("asks only the active machine", async () => {
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
    expect(await screen.findByText("1 match")).toBeTruthy();
    expect(document.body.textContent).not.toContain("did not answer");
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

  it("stops waiting on the selected machine long before the transport would", async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          label: "alpha",
          sessions: [listSession({ id: "a1", title: "First" })],
          searchHangs: true,
        },
      ],
    });
    restore = view.restore;
    await settle(50);

    view.setQuery("needle");
    await settle(300);
    expect(screen.getByText("searching...")).toBeTruthy();

    // Seven seconds of silence is still a machine reading its transcripts.
    await settle(7_000);
    expect(screen.getByText("searching...")).toBeTruthy();

    // Past the search's own deadline it is absence, not a transport-length wait.
    await settle(2_000);
    expect(screen.queryByText("searching...")).toBeNull();
    expect(screen.getByText("This machine did not answer.")).toBeTruthy();
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
    // The query lands one PAUSE after the last keystroke (`SEARCH_DEBOUNCE_MS`), and the
    // search's own deadline starts from there.
    await settle(300);
    await settle(8_700);
    expect(screen.getByText("This machine did not answer.")).toBeTruthy();
    expect(document.body.textContent).not.toContain("Nothing on any paired machine");
  });
});

// Regression, user report: an active machine that had just spent a whole search
// deadline in silence was asked again by the very next query, so every further pause
// bought another wait on a gateway already known to be dark.
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
  const machine = () => [
    {
      label: "alpha",
      sessions: [listSession({ id: "a1", title: "First" })],
      searchHangs: true,
    },
  ];

  it("answers for a silent machine without spending a second request on it", async () => {
    const view = renderSessionsScreen({ machines: machine() });
    restore = view.restore;
    await settle(50);
    view.requests.length = 0;

    view.setQuery("needle");
    await settle(300);
    await settle(8_700);
    expect(searches(view.requests)).toEqual(["needle"]);
    expect(screen.getByText("This machine did not answer.")).toBeTruthy();

    view.requests.length = 0;
    view.setQuery("other");
    await settle(300);
    expect(searches(view.requests)).toEqual([]);
    expect(screen.getByText("This machine did not answer.")).toBeTruthy();
  });

  it("asks it again as soon as a list read proves the machine alive", async () => {
    const view = renderSessionsScreen({ machines: machine() });
    restore = view.restore;
    await settle(50);

    view.setQuery("needle");
    await settle(300);
    await settle(8_700);
    expect(screen.getByText("This machine did not answer.")).toBeTruthy();

    // The blackout is a memory of one failure, not a verdict on the machine: the 10s
    // poll's list read lands and the next search asks this machine again.
    view.requests.length = 0;
    await settle(2_000);
    view.setQuery("third");
    await settle(300);
    expect(searches(view.requests)).toEqual(["third"]);
  });
});

// Regression, user report (paraphrased: "now it makes everything jump on every character —
// it is not natural and most likely not debounced"): the pause held back only the NETWORK.
// Every keystroke still re-filed the answers under the half-typed needle, so the transcript
// hits on screen were discarded and the rows they had put in the list vanished and came
// back a pause later — the list rearranging itself under the thumb one letter at a time.
describe("typing does not redraw the list under the thumb", () => {
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

  /** One machine, two sessions, and a transcript hit in exactly one of them. */
  const machine = () => [
    {
      label: "alpha",
      sessions: [
        listSession({ id: "a1", title: "First" }),
        listSession({ id: "a2", title: "Later" }),
      ],
      routes: { "/v1/sessions/actions/search": { matches: [hit] } },
    },
  ];

  it("holds the rows and the tally of the settled needle through the whole pause", async () => {
    const view = renderSessionsScreen({ machines: machine() });
    restore = view.restore;
    await settle(50);
    view.requests.length = 0;

    view.setQuery("need");
    await settle(300);
    // A transcript hit, not a title match: this row is on screen only because the answer
    // to "need" put it there, which is exactly what a keystroke used to throw away.
    expect(screen.getByText("First")).toBeTruthy();
    expect(screen.getByText("1 match")).toBeTruthy();
    expect(searches(view.requests)).toEqual(["need"]);

    view.setQuery("needl");
    await settle(50);
    expect(screen.getByText("First")).toBeTruthy();
    expect(screen.getByText("1 match")).toBeTruthy();

    view.setQuery("needle");
    await settle(150);
    expect(screen.getByText("First")).toBeTruthy();
    expect(screen.getByText("1 match")).toBeTruthy();

    // One question for the word the typing rested on, not one per letter.
    await settle(300);
    expect(searches(view.requests)).toEqual(["need", "needle"]);
  });

  // A count is an ANSWER. Before a needle has been asked there is nothing filtered to
  // count, and printing the unfiltered list's size would be the screen answering a
  // question nobody has finished typing.
  it("publishes no tally for a needle nobody has rested on", async () => {
    const view = renderSessionsScreen({ machines: machine() });
    restore = view.restore;
    await settle(50);

    view.setQuery("n");
    await settle(50);
    expect(screen.getByText("searching...")).toBeTruthy();
    expect(screen.queryByText("2 matches")).toBeNull();
    expect(screen.queryByText("1 match")).toBeNull();

    await settle(300);
    expect(screen.getByText("1 match")).toBeTruthy();
  });

  // Regression, user report (paraphrased: "this looks awful on iPhone", with a screenshot
  // of the switch strip cut mid-address and "271 matches / 1 machine did not answer"
  // running to the very edge of the glass): the report stood in the trailing cluster of a
  // row that could not shrink, so on a 390px screen it ate the switch and then overran the
  // row's own 12px inset.
  it("gives the search report a line of its own instead of the switch's row", async () => {
    const view = renderSessionsScreen({ machines: machine() });
    restore = view.restore;
    await settle(50);

    view.setQuery("needle");
    await settle(300);
    const report = screen.getByText("1 match").closest("div");
    expect(report).toBeTruthy();
    const row = report!.parentElement!;
    // The row wraps on a phone and stops wrapping where there is room for both.
    expect(row.className).toContain("flex-wrap");
    expect(row.className).toContain("sm:flex-nowrap");
    // A whole line of its own below the switch, inline again from `sm` up.
    expect(report!.className).toContain("w-full");
    expect(report!.className).toContain("order-last");
    expect(report!.className).toContain("sm:w-auto");
    expect(report!.className).toContain("sm:order-none");
    // And it is not sharing a box with the machine's own verb any more.
    expect(report!.querySelector('[aria-label^="Projects on"]')).toBeNull();
  });
});

const rowOrder = () =>
  [...document.querySelectorAll("[data-session-id]")].map((row) =>
    row.getAttribute("data-session-id"),
  );

// Regression, user report (paraphrased: "the search results are not sorted by
// freshness — I care far more about freshness than about which band the hit
// landed in"). The screen sorted what a query matched by `SessionMatch.rank`,
// the gateway's relevance band, so every year-old session whose TITLE held the
// word sat above the one touched this morning and the dates jumped up and down
// the list. The gateway now answers freshest-first and the screen paints THAT
// order.
describe("search results are ordered by freshness", () => {
  const machines = [
    {
      label: "alpha",
      sessions: [
        listSession({
          id: "ancient",
          title: "star charts",
          modified_at: "2024-01-02T10:00:00Z",
        }),
        listSession({ id: "today", title: "Deploy", modified_at: "2024-06-01T10:00:00Z" }),
      ],
      routes: {
        "/v1/sessions/actions/search": {
          // The gateway's own order: today's session first, matched in a REPLY
          // (band 2); the ancient one second, matched in its very TITLE (band 0).
          matches: [
            {
              session_id: "today",
              rank: 2,
              is_in_reply: true,
              reply_snippet: "a star to steer by",
            },
            { session_id: "ancient", rank: 0, is_in_title: true },
          ],
        },
      },
    },
  ];

  it("paints the freshest match first, whatever band it matched in", async () => {
    const view = renderSessionsScreen({ machines });
    restore = view.restore;
    await screen.findByText("Deploy");

    view.setQuery("star");
    await waitFor(() => expect(rowOrder()).toEqual(["today", "ancient"]));
  });

  it("keeps that order while the answer is the one being painted", async () => {
    const view = renderSessionsScreen({ machines });
    restore = view.restore;
    await screen.findByText("Deploy");

    view.setQuery("star");
    await waitFor(() => expect(rowOrder()).toEqual(["today", "ancient"]));
    await new Promise((resolve) => setTimeout(resolve, 250));
    expect(rowOrder()).toEqual(["today", "ancient"]);
  });
});
