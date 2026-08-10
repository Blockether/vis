// @vitest-environment jsdom
import { act, screen } from "@testing-library/react";
import { afterEach, describe, expect, it } from "vitest";

import { markSessionRead } from "../lib/unread";
import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

let restore = () => {};
afterEach(() => restore());

// Regression, user report: a session still wore its `NEW` badge after it had just
// been opened and read. The list is not unmounted while the transcript is on screen
// (`App.tsx` hides it), and `SessionRow` is `memo`ised over row objects a poll returns
// unchanged — so the read mark moved, the store announced, and the one component that
// paints the badge never re-rendered.
describe("the NEW badge clears without a reload", () => {
  it("drops the badge the moment the mark moves, with the list still mounted", async () => {
    markSessionRead("s1", 1);
    const view = renderSessionsScreen({
      machines: [{ sessions: [listSession({ id: "s1", turn_count: 3 })] }],
    });
    restore = view.restore;

    expect(await screen.findByText("2 new")).toBeInTheDocument();

    await act(async () => {
      markSessionRead("s1", 3);
    });

    expect(screen.queryByText("2 new")).not.toBeInTheDocument();
  });
});
