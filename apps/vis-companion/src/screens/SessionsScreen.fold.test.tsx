// @vitest-environment jsdom
import { fireEvent, waitFor } from "@testing-library/react";
import { afterEach, describe, expect, it } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

// One machine, two projects: `alpha` moved last, so the list's own order puts it on
// top; `beta` is the history under it.
const at = (day: number) => new Date(`2024-05-0${day}T10:00:00Z`).toISOString();

const rows = [
  ...Array.from({ length: 3 }, (_, index) =>
    listSession({
      id: `a${index}`,
      title: `alpha ${index}`,
      workspace: { root: "/Users/dev/alpha" },
      modified_at: at(4 - index),
    }),
  ),
  ...Array.from({ length: 2 }, (_, index) =>
    listSession({
      id: `b${index}`,
      title: `beta ${index}`,
      workspace: { root: "/Users/dev/beta" },
      modified_at: at(2 - index),
    }),
  ),
];

afterEach(() => {
  globalThis.localStorage?.clear();
});

// Regression, user report: a project folded on the sessions list came back OPEN
// after a session was read and left, and every project was open to begin with — so
// a machine with several checkouts painted all of their history at once.
describe("folding a project", () => {
  it("opens the top project and no other", async () => {
    const view = renderSessionsScreen({ machines: [{ sessions: rows }] });
    try {
      await waitFor(() => expect(view.getByText("alpha 0")).toBeTruthy());
      // The one project that opens by itself is the one the order put on top.
      expect(view.getByLabelText("Collapse alpha")).toBeTruthy();
      expect(view.getByLabelText("Expand beta")).toBeTruthy();
      expect(view.queryAllByText(/beta \d/)).toHaveLength(0);
    } finally {
      view.restore();
    }
  });

  it("remembers a fold across a relaunch of the screen", async () => {
    const first = renderSessionsScreen({ machines: [{ sessions: rows }] });
    let conns;
    try {
      await waitFor(() => expect(first.getByText("alpha 0")).toBeTruthy());
      conns = first.conns;
      // The reader disagrees with both defaults: the top project is one they are done
      // with, the one under it is the one they are in.
      fireEvent.click(first.getByLabelText("Collapse alpha"));
      fireEvent.click(first.getByLabelText("Expand beta"));
      expect(first.queryAllByText(/alpha \d/)).toHaveLength(0);
      expect(first.queryAllByText(/beta \d/)).toHaveLength(2);
      first.unmount();
    } finally {
      first.restore();
    }

    // The app comes back: the same machine, a screen built from nothing.
    const again = renderSessionsScreen({ machines: [{ sessions: rows }], at: conns });
    try {
      await waitFor(() => expect(again.getByText("beta 0")).toBeTruthy());
      expect(again.getByLabelText("Expand alpha")).toBeTruthy();
      expect(again.getByLabelText("Collapse beta")).toBeTruthy();
      expect(again.queryAllByText(/alpha \d/)).toHaveLength(0);
    } finally {
      again.restore();
    }
  });

  it("shows what a query matched, fold or no fold", async () => {
    const view = renderSessionsScreen({ machines: [{ sessions: rows }] });
    try {
      await waitFor(() => expect(view.getByText("alpha 0")).toBeTruthy());
      expect(view.queryAllByText(/beta \d/)).toHaveLength(0);
      // A filter is a fleet-wide question. Answering it with a folded project would
      // be the screen saying it found nothing while holding the row.
      view.setQuery("beta 1");
      await waitFor(() => expect(view.getByText("beta 1")).toBeTruthy());
    } finally {
      view.restore();
    }
  });

  // Regression, user report, Vis session 78b0c0b5-f5ba-453f-97ee-af0a85f72d25:
  // a persisted project with zero sessions still wore a disclosure chevron, even though
  // there was no session list for that control to reveal.
  it("does not make an empty project a disclosure", async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [],
          projects: [
            {
              root: "/Users/dev/vis",
              project_id: "p-vis",
              name: "vis",
              session_count: 0,
              live_count: 0,
              awaiting_count: 0,
              last_activity_ms: 0,
            },
          ],
        },
      ],
    });
    try {
      await waitFor(() => expect(view.getByText("0 sessions")).toBeTruthy());
      expect(view.getByText("vis").closest("button")).toBeNull();
      expect(view.queryByLabelText(/^(Expand|Collapse) vis$/)).toBeNull();
    } finally {
      view.restore();
    }
  });
});
