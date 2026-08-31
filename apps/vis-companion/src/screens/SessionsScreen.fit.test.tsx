// @vitest-environment jsdom
import { fireEvent, waitFor } from "@testing-library/react";
import { afterEach, describe, expect, it } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

const at = (rank: number) =>
  new Date(Date.UTC(2024, 4, 1, 10, 0, rank)).toISOString();

// One project, deeper than any screen: the list has to cut it into pages, and the
// row's number IS its place in the project.
const rows = Array.from({ length: 40 }, (_, index) =>
  listSession({
    id: `s${index}`,
    title: `alpha ${String(index).padStart(2, "0")}`,
    workspace: { root: "/Users/dev/alpha" },
    modified_at: at(40 - index),
  }),
);

type View = ReturnType<typeof renderSessionsScreen>;
const shown = (view: View) => view.queryAllByText(/^alpha \d\d$/);

const onScreen = (height: number) => {
  window.innerHeight = height;
  return renderSessionsScreen({ machines: [{ sessions: rows }] });
};

const expectRows = async (height: number, count: number) => {
  const view = onScreen(height);
  try {
    await waitFor(() => expect(view.getByText("alpha 00")).toBeTruthy());
    expect(shown(view)).toHaveLength(count);
  } finally {
    view.unmount();
    view.restore();
  }
};
// `mouse:` as `index.css` spells it — a wide window under a fine pointer — which is
// the density the desk's own row height and page are cut for.
const onADesk = () => {
  const previous = window.matchMedia;
  window.matchMedia = ((query: string) => ({
    matches: query.includes("pointer: fine"),
    media: query,
    onchange: null,
    addListener: () => {},
    removeListener: () => {},
    addEventListener: () => {},
    removeEventListener: () => {},
    dispatchEvent: () => false,
  })) as never;
  return () => {
    window.matchMedia = previous;
  };
};

afterEach(() => {
  globalThis.localStorage?.clear();
  window.innerHeight = 768;
});

// A page still grows with the screen; the device owns its useful upper size.
describe("a project's page is cut by the device", () => {
  // Regression, user report, Vis session 482fd0f2-1bee-4203-a959-9f3cd2ae80a5:
  // a phone in its short orientation asked each project for only three sessions.
  it("keeps at least fifteen rows in every mobile project", async () => {
    await expectRows(390, 15);
    await expectRows(568, 15);
    await expectRows(844, 15);
  });

  it("uses the extra room when more than fifteen rows fit", async () => {
    await expectRows(1200, 19);
  });
  it("keeps the row the reader is on when the screen changes shape", async () => {
    const view = onScreen(844);
    try {
      await waitFor(() => expect(view.getByText("alpha 00")).toBeTruthy());
      fireEvent.click(view.getByLabelText("Next page"));
      fireEvent.click(view.getByLabelText("Next page"));
      // Page 3 of 3 at fifteen rows a page: the reader is holding `alpha 30`.
      await waitFor(() => expect(view.getByText("alpha 30")).toBeTruthy());

      // The device is turned, and the step under the pager grows. A page NUMBER
      // kept across that would name a different stretch, so the row index is kept.
      window.innerHeight = 1200;
      fireEvent(window, new Event("resize"));
      await waitFor(() => expect(shown(view)).toHaveLength(19));
      expect(view.getByText("alpha 30")).toBeTruthy();
    } finally {
      view.unmount();
      view.restore();
    }
  });
});

// Regression, user report (paraphrased: on the desktop this does not look good
// either, it should sit further up): the panel spelled its top inset TWICE — 24px
// on the section and 32px again on the machine strip standing inside it — so on a
// 1440x900 desk the machine chips started 56px under the app bar, the first row at
// y=182, and the page was cut for a screen a hand's width shorter than the one it
// was painted on.
describe("a desk cuts a page for the room it really has", () => {
  it("fills the window the single top inset leaves", async () => {
    const restoreDensity = onADesk();
    const view = onScreen(900);
    try {
      await waitFor(() => expect(view.getByText("alpha 00")).toBeTruthy());
      // 900px, minus the 137px of bands above the first row, the 24px the detached
      // panel keeps under itself and the peek under the last row, over a 33px row.
      expect(shown(view)).toHaveLength(21);
    } finally {
      view.unmount();
      view.restore();
      restoreDensity();
    }
  });
});
