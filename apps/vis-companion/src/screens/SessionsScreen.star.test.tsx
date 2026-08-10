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

  it("paints the star action in the brand accent, not the neutral verb ink", async () => {
    const view = renderSessionsScreen({ machines });
    restore = view.restore;
    await screen.findByText("Older session");

    const star = screen.getAllByRole("button", { name: "Star" })[0];
    const rename = screen.getAllByRole("button", { name: "Rename" })[0];
    expect(star.className).toContain("bg-accent/15");
    expect(star.className).toContain("text-accent");
    // Rename stays neutral: the strip has exactly one coloured verb beside Delete.
    expect(rename.className).toContain("bg-panel-2");
    expect(rename.className).not.toContain("bg-accent/15");
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
});
