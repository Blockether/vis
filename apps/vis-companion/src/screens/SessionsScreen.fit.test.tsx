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

// Reported over a screenshot of a 390x844 phone: the expanded project stopped
// after five rows with the bottom half of the screen empty and a pager reading
// `1 / 102`, because the page had been sized by a settings panel ("Sessions per
// project": 5, 10 or 15) that never knew how tall the device was.
describe("a project's page is cut by the device", () => {
  it("fills the screen it is painted on", async () => {
    const tall = onScreen(844);
    try {
      await waitFor(() => expect(tall.getByText("alpha 00")).toBeTruthy());
      // 844px, minus the 211px of bands above the first row and the peek under
      // the last, over a 49px row.
      expect(shown(tall)).toHaveLength(12);
    } finally {
      tall.unmount();
      tall.restore();
    }

    const short = onScreen(568);
    try {
      await waitFor(() => expect(short.getByText("alpha 00")).toBeTruthy());
      // The shortest phone gets a shorter page instead of three rows below its fold.
      expect(shown(short)).toHaveLength(6);
    } finally {
      short.unmount();
      short.restore();
    }
  });

  it("keeps the row the reader is on when the screen changes shape", async () => {
    const view = onScreen(844);
    try {
      await waitFor(() => expect(view.getByText("alpha 00")).toBeTruthy());
      fireEvent.click(view.getByLabelText("Next page"));
      fireEvent.click(view.getByLabelText("Next page"));
      // Page 3 of 4 at twelve rows a page: the reader is holding `alpha 24`.
      await waitFor(() => expect(view.getByText("alpha 24")).toBeTruthy());

      // The device is turned, and the step under the pager grows. A page NUMBER
      // kept across that names a different stretch of the project — page 3 of a
      // nineteen-row page starts at `alpha 38` — so the INDEX is kept instead.
      window.innerHeight = 1200;
      fireEvent(window, new Event("resize"));
      await waitFor(() => expect(shown(view)).toHaveLength(19));
      expect(view.getByText("alpha 24")).toBeTruthy();
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
