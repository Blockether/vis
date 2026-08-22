// @vitest-environment jsdom
import { fireEvent, waitFor } from "@testing-library/react";
import { afterEach, describe, expect, it } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

const at = (rank: number) =>
  new Date(Date.UTC(2024, 4, 1, 10, 0, rank)).toISOString();

// One project deeper than any screen: forty sessions, and an 844px phone holds twelve
// of them, so the project is four pages long.
const rows = Array.from({ length: 40 }, (_, index) =>
  listSession({
    id: `s${index}`,
    title: `alpha ${String(index).padStart(2, "0")}`,
    workspace: { root: "/Users/dev/alpha" },
    modified_at: at(40 - index),
  }),
);

type View = ReturnType<typeof renderSessionsScreen>;
const shown = (view: View) =>
  view.queryAllByText(/^alpha \d\d$/).map((node) => node.textContent);
const pageReads = (view: View) =>
  view.requests
    .filter(({ path }) => path.includes("root="))
    .map(({ path }) => decodeURIComponent(path));
const settle = () => new Promise((resolve) => setTimeout(resolve, 60));

afterEach(() => {
  globalThis.localStorage?.clear();
  window.innerHeight = 768;
});

// A pager over a list the client re-filters and re-orders is arithmetic on a lie: the
// gateway counted 1034 sessions in a project this list painted 763 of, so its last page
// sat 27 pages beyond the pager's and the reader watched three rows swap for ten. Every
// page is the gateway's own window now (`GatewayClient.listProjectPage`), asked for at
// the size this screen measured, so the header's count and the pager's arithmetic are
// one number and no page needs the fleet downloaded first.
describe("a project is paged by the gateway that counts it", () => {
  it("asks once for the page on screen, and reads the pages after it ahead", async () => {
    window.innerHeight = 844;
    const view = renderSessionsScreen({ machines: [{ sessions: rows }] });
    try {
      await waitFor(() => expect(shown(view)).toHaveLength(12));
      await settle();

      // The page on screen is ONE read, cut where the screen was measured — not a
      // slice of a fleet this device had to download first. Behind it, and only
      // behind it, the next two pages are warmed: what that buys is their cursors
      // and a validator each, so a page turn is a paint instead of a wait.
      const first = pageReads(view);
      expect(first).toHaveLength(3);
      expect(first[0]).toBe("/v1/sessions?root=/Users/dev/alpha&limit=12");
      expect(first.slice(1).every((read) => read.includes("&after="))).toBe(true);
      expect(first.every((read) => read.includes("limit=12"))).toBe(true);
      expect(shown(view)[0]).toBe("alpha 00");
      expect(view.getAllByText("40 sessions").length).toBeGreaterThan(0);

      // THE TURN COSTS NO ROUND TRIP: the page the reader steps onto is already
      // held, so it paints in the frame of the tap. It used to stand on the page
      // before it until the gateway answered.
      fireEvent.click(view.getByLabelText("Next page"));
      expect(shown(view)[0]).toBe("alpha 12");
      await settle();

      // The last page, tapped from page two, paints the four rows the header's
      // forty leaves, with no second paint under the thumb.
      fireEvent.click(view.getByLabelText("Page 4"));
      await waitFor(() => expect(shown(view)).toHaveLength(4));
      await settle();
      expect(shown(view)).toEqual([
        "alpha 36",
        "alpha 37",
        "alpha 38",
        "alpha 39",
      ]);
      // Every read is still one page of one project: no walk of the machine.
      expect(
        pageReads(view).every((read) => read.includes("limit=12")),
      ).toBe(true);
    } finally {
      view.unmount();
      view.restore();
    }
  });
});
