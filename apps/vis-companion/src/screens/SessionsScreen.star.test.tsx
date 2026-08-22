// @vitest-environment jsdom
import { fireEvent, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { afterEach, describe, expect, it } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

let restore = () => {};
afterEach(() => restore());

const rowOrder = () =>
  [...document.querySelectorAll("[data-session-id]")].map((row) =>
    row.getAttribute("data-session-id"),
  );

// Regression, user report ("the colour is the same as rename, and after I star it I
// don't see the star until I click on the session"): the swipe strip painted Star in
// the same neutral ink as Rename, and starring PINS the row to the top of its project
// — measured on a 390px viewport, the tapped row travelled from y=619 to y=325 — so
// the row left the spot it was tapped in and an unstarred neighbour slid under the
// thumb. Nothing changed where the user was looking until the list was rebuilt.
describe("starring a session", () => {
  const machines = [
    {
      sessions: [
        listSession({
          id: "older",
          title: "Older session",
          modified_at: "2024-05-01T09:00:00Z",
        }),
        listSession({
          id: "newer",
          title: "Newer session",
          modified_at: "2024-05-01T11:00:00Z",
        }),
      ],
    },
  ];

  it("paints the star's own cell in the brand amber, never the neutral verb ink", async () => {
    const view = renderSessionsScreen({ machines });
    restore = view.restore;
    await screen.findByText("Older session");

    const cell = () => screen.getByRole("group", { name: "Older session actions" });
    const slab = (label: string) =>
      cell().querySelector(`button[aria-label="${label}"]`)!.className;
    // The strip's meaning is in the CELL: the amber tint for the mark the human
    // leaves, nothing at all for the neutral verb beside it.
    expect(slab("Star")).toContain("bg-accent/15");
    expect(slab("Rename")).not.toContain("bg-accent/15");

    await userEvent.click(cell().querySelector('button[aria-label="Star"]')!);

    // The cell keeps the amber while the verb becomes its own undo, and the caption
    // keeps the READABLE amber — #ffc420 measured 1.37:1 as 9px text on this tint.
    expect(slab("Unstar")).toContain("bg-accent/15");
    expect(slab("Unstar")).toContain("text-accent-ink");
  });

  it("pins the starred row to the top and brings it back into view", async () => {
    const view = renderSessionsScreen({ machines });
    restore = view.restore;
    await screen.findByText("Older session");
    const before = rowOrder();
    expect(before).toHaveLength(2);
    const last = before[1]!;
    const title = last === "older" ? "Older session" : "Newer session";

    const seen: Element[] = [];
    const scrollIntoView = Element.prototype.scrollIntoView;
    Element.prototype.scrollIntoView = function record(this: Element) {
      seen.push(this);
    };
    try {
      const star = screen
        .getByRole("group", { name: `${title} actions` })
        .querySelector('button[aria-label="Star"]')!;
      await userEvent.click(star);

      // The pin moved the row the thumb was on — and the LIST is what moves it. The
      // star is the gateway's own fact, so the tap is answered by a read of the list
      // it owns, never by this device re-sorting rows behind the reader.
      await waitFor(() => expect(rowOrder()).toEqual([last, before[0]]));
    } finally {
      Element.prototype.scrollIntoView = scrollIntoView;
    }

    // The row itself is what scrolls back, so the user keeps looking at the session
    // they just starred.
    expect(seen).not.toHaveLength(0);
    expect(
      seen.some((element) =>
        element.contains(document.querySelector(`[data-session-id="${last}"]`)),
      ),
    ).toBe(true);
  });

  // Regression, user report ("the star is not showing on the session row as long
  // as I don't drag to open the session or come back"): the row's own mark was in
  // the DOM the moment the strip was tapped — this is what proves it — and it was
  // painted #ffc420 on #faf3eb paper at 1.45:1, so it could not be SEEN until the
  // list was left and re-entered and the eye went looking for it. The state was
  // never the bug; see `icons.tsx` for the outline that gives the mark a shape.
  it("wears its star on the row the moment the mark is tapped", async () => {
    const view = renderSessionsScreen({ machines });
    restore = view.restore;
    await screen.findByText("Older session");
    const row = () =>
      (document.querySelector('[data-session-id="older"]') as HTMLElement)
        .parentElement!;
    expect(row().querySelector("svg.fill-accent")).toBeNull();

    await userEvent.click(
      screen
        .getByRole("group", { name: "Older session actions" })
        .querySelector('button[aria-label="Star"]')!,
    );

    // No remount, no reopened list: the same row, in the same commit. One star,
    // not a mark and a control — the row's state IS the way to take it back.
    expect(row().querySelector("svg.fill-accent")).not.toBeNull();
    expect(row().querySelectorAll("svg.fill-accent")).toHaveLength(1);
    expect(
      screen
        .getByRole("group", { name: "Older session actions" })
        .querySelector('button[aria-label="Unstar"]'),
    ).not.toBeNull();
  });

  // Regression, user report (paraphrased: the star is in two states at once): the
  // mark was kept in THIS DEVICE's storage, so the machine never heard about it —
  // one screen showed a session starred, another showed it plain, and no answer the
  // gateway could give would settle which was true. The star is its fact now.
  it("tells the gateway, and wears the rank the gateway answers with", async () => {
    const view = renderSessionsScreen({ machines });
    restore = view.restore;
    await screen.findByText("Older session");
    const cell = () => screen.getByRole("group", { name: "Older session actions" });
    const patched = () => view.requests.filter((request) => request.method === "PATCH");

    await userEvent.click(cell().querySelector('button[aria-label="Star"]')!);

    expect(patched().map((request) => [request.path, request.body])).toEqual([
      ["/v1/sessions/older", { is_favorite: true }],
    ]);
    // The mark the row wears is the rank that came BACK — there is no local copy of
    // the tap left over to disagree with it.
    expect(await screen.findByRole("button", { name: "Unstar" })).toBeTruthy();

    await userEvent.click(cell().querySelector('button[aria-label="Unstar"]')!);

    expect(patched().map((request) => request.body)).toEqual([
      { is_favorite: true },
      { is_favorite: false },
    ]);
    expect(cell().querySelector('button[aria-label="Star"]')).not.toBeNull();
  });
  // Regression, user report on iOS ("when I click the star on some other row, first I
  // don't see the star automatically, only after I do slide once again ... there is
  // some mismatch with the state", with the cell painted over its own old caption):
  // the row starred at the TOP of the list showed its mark at once and every other row
  // did not. A verb closed the drawer by ASKING for an animated slide home while
  // `open` flipped on the spot, and the pin then fired a second animated scroll at
  // that same scroller in the same commit; when the platform ran neither, the strip
  // stood open over a row whose state said shut — and the mark the tap had just left
  // sits at the row's LEADING edge, which is the half a slid-open row hides.
  it("sends the row it pins home in the same tap, not on an animation", async () => {
    const view = renderSessionsScreen({ machines });
    restore = view.restore;
    await screen.findByText("Older session");
    // The row the pin MOVES: the one not already at the top of its project.
    const moved = rowOrder()[1]!;
    const title = moved === "older" ? "Older session" : "Newer session";
    const row = document.querySelector(
      `[data-session-id="${moved}"]`,
    ) as HTMLElement;
    const track = row.closest("[data-swipe-track]") as HTMLElement;

    // A thumb slid it open: the platform scrolls the track, the component reads it.
    Object.defineProperty(track, "scrollLeft", { value: 216, configurable: true });
    fireEvent.scroll(track);

    const home: ScrollToOptions[] = [];
    const scrollTo = Element.prototype.scrollTo;
    Element.prototype.scrollTo = function record(
      this: Element,
      options?: ScrollToOptions,
    ) {
      if (this === track && options) home.push(options);
    } as typeof Element.prototype.scrollTo;
    try {
      await userEvent.click(
        screen
          .getByRole("group", { name: `${title} actions` })
          .querySelector('button[aria-label="Star"]')!,
      );
    } finally {
      Element.prototype.scrollTo = scrollTo;
    }

    // Home in the same frame the star was tapped — there is no animation left to be
    // dropped by the re-order that same tap starts.
    expect(home).toEqual([{ left: 0, behavior: "auto" }]);
    expect(row.parentElement!.querySelector("svg.fill-accent")).not.toBeNull();
  });

  // Regression, user report ("the star is not showing on the session row ... as long
  // as I don't drag to open the session or come back"): a project is PAGED at ten
  // rows, and a star pins its row to the top of the project — which is page one. A
  // row starred on page two therefore left the page the user was looking at, so the
  // mark they had just asked for was two pages away and only turned up when the
  // screen was left and re-entered on page one.
  it("follows the starred row to the page its own pin moved it to", async () => {
    const many = Array.from({ length: 12 }, (_, index) =>
      listSession({
        id: `s${String(index + 1).padStart(2, "0")}`,
        title: `Session ${index + 1}`,
        // Descending, so the list order is s01 … s12 before anything is starred.
        modified_at: new Date(
          Date.UTC(2024, 4, 1, 23 - index, 0, 0),
        ).toISOString(),
      }),
    );
    const view = renderSessionsScreen({ machines: [{ sessions: many }] });
    restore = view.restore;
    await screen.findByText("Session 1");
    expect(rowOrder()).toHaveLength(10);

    await userEvent.click(screen.getByRole("button", { name: "Next page" }));
    expect(rowOrder()).toEqual(["s11", "s12"]);

    await userEvent.click(
      screen
        .getByRole("group", { name: "Session 12 actions" })
        .querySelector('button[aria-label="Star"]')!,
    );

    // The row the thumb was on is still on screen, wearing its mark — on page one,
    // where the pin put it, and at the top of its project. The gateway is what put
    // it there: the tap is answered by a read of the list it owns.
    await waitFor(() => expect(rowOrder()[0]).toBe("s12"));
    const row =
      document.querySelector('[data-session-id="s12"]')?.parentElement ?? null;
    expect(row).not.toBeNull();
    expect(row!.querySelector("svg.fill-accent")).not.toBeNull();
  });
  // Regression, user report on iOS (paraphrased: slide the LAST row open, tap the
  // star, the row moves up wearing no mark, and only the next slide shows it — with
  // the mark and the strip then saying two different things): the pin brought the row
  // back with an ANIMATED `scrollIntoView`, and that call walks EVERY scrollable
  // ancestor, so the FIRST scroller it moves is the row's own mandatory snap track —
  // the drawer a verb has just sent home, in the very commit that moves its node.
  // Measured in WebKit at 390px on this screen, same track, same call: an open track
  // (216px) was still at 163px 150ms after `behavior: "smooth"` was asked for and only
  // reached home ~900ms later, against home in the SAME FRAME for `behavior: "auto"`.
  // A drawer left standing hides the row's LEADING edge, which is where the mark sits.
  it("places the pinned row in the same frame, never on an animation", async () => {
    const view = renderSessionsScreen({ machines });
    restore = view.restore;
    await screen.findByText("Older session");
    // The row the pin MOVES: the one not already at the top of its project.
    const moved = rowOrder()[1]!;
    const title = moved === "older" ? "Older session" : "Newer session";

    const asked: (ScrollIntoViewOptions | undefined)[] = [];
    const scrollIntoView = Element.prototype.scrollIntoView;
    Element.prototype.scrollIntoView = function record(
      this: Element,
      options?: boolean | ScrollIntoViewOptions,
    ) {
      asked.push(typeof options === "object" ? options : undefined);
    } as typeof Element.prototype.scrollIntoView;
    try {
      await userEvent.click(
        screen
          .getByRole("group", { name: `${title} actions` })
          .querySelector('button[aria-label="Star"]')!,
      );
    } finally {
      Element.prototype.scrollIntoView = scrollIntoView;
    }

    expect(asked).not.toHaveLength(0);
    expect(asked[0]).toEqual({
      block: "nearest",
      inline: "nearest",
      behavior: "auto",
    });
    // Nothing in this tap may hand that track an animation the platform can drop.
    expect(asked.some((options) => options?.behavior === "smooth")).toBe(false);
  });
});
