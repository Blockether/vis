// @vitest-environment jsdom
import { screen } from "@testing-library/react";
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
    } finally {
      Element.prototype.scrollIntoView = scrollIntoView;
    }

    // The pin moved the row the thumb was on; the row itself is what scrolls back,
    // so the user keeps looking at the session they just starred.
    expect(rowOrder()).toEqual([last, before[0]]);
    expect(seen).not.toHaveLength(0);
    expect(
      seen[0].contains(document.querySelector(`[data-session-id="${last}"]`)),
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
    // where the pin put it, and at the top of its project.
    const row =
      document.querySelector('[data-session-id="s12"]')?.parentElement ?? null;
    expect(row).not.toBeNull();
    expect(row!.querySelector("svg.fill-accent")).not.toBeNull();
    expect(rowOrder()[0]).toBe("s12");
  });
});
