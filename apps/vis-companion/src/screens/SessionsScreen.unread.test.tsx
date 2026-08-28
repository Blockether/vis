// @vitest-environment jsdom
import { act, fireEvent, screen } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  forgetReadingPosition,
  parkedReadingPosition,
  rememberReadingPosition,
} from "../lib/reading-position";
import { markSessionRead } from "../lib/unread";
import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

const LATEST_SID = "latest-session";
let restore = () => {};
afterEach(() => {
  restore();
  forgetReadingPosition("s1");
  forgetReadingPosition(LATEST_SID);
});

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

// Regression, Vis session 448b3266-8836-4115-9cf5-6ed0679aa2f9: pressing a NEW
// session restored an old reading position instead of opening on the answer that raised
// the badge.
describe("opening an unread session", () => {
  it("forgets the parked place and enters at the latest answer", async () => {
    markSessionRead(LATEST_SID, 1);
    rememberReadingPosition(LATEST_SID, 2_000);
    const onOpen = vi.fn();
    const view = renderSessionsScreen({
      machines: [{ sessions: [listSession({ id: LATEST_SID, turn_count: 2 })] }],
      onOpen,
    });
    restore = view.restore;

    await screen.findByText("1 new");
    const row = document.querySelector<HTMLButtonElement>(
      `[data-session-id="${LATEST_SID}"]`,
    );
    expect(row).not.toBeNull();
    fireEvent.click(row!);

    expect(parkedReadingPosition(LATEST_SID)).toBeNull();
    expect(onOpen).toHaveBeenCalledWith(expect.anything(), LATEST_SID);
  });
});
