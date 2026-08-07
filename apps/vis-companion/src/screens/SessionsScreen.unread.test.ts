import { describe, expect, it } from "vitest";
import source from "./SessionsScreen.tsx?raw";

// Regression, user report: a session still wore its `NEW` badge after it had just
// been opened and read. The list is not unmounted while the transcript is on screen
// (`App.tsx` hides it), and `SessionRow` is `memo`ised over row objects a poll returns
// unchanged — so the read mark moved, the store announced, and the one component that
// paints the badge never re-rendered. The row must SUBSCRIBE to the marks it reads,
// exactly as it already does for favorites.
describe("the NEW badge clears without a reload", () => {
  it("subscribes the memoised row to read marks before it counts unread", () => {
    const row = source.slice(source.indexOf("const SessionRow = memo("));
    expect(row).toContain("useReadMarks()");
    expect(row.indexOf("useReadMarks()")).toBeLessThan(
      row.indexOf("unreadTurnCount(session)"),
    );
  });
});
