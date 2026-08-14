import { describe, expect, it } from "vitest";

import { SESSION_ID_MARKER_PREFIX, markSessionId } from "./session-id";

// The marker is the whole point of the copy: whoever the id is pasted to — a
// person, an issue, another agent — must be able to tell it names a Vis
// session. It has to be exactly the string the Clojure side stamps
// (`session-id-marker-prefix` in src/com/blockether/vis/internal/header.clj).
describe("the copied session id", () => {
  it("leads with the vis_session_id marker", () => {
    expect(SESSION_ID_MARKER_PREFIX).toBe("vis_session_id#");
    expect(markSessionId("123e4567-e89b-12d3-a456-426614174000")).toBe(
      "vis_session_id#123e4567-e89b-12d3-a456-426614174000",
    );
  });

  it("copies nothing at all when there is no id yet", () => {
    expect(markSessionId("")).toBe("");
    expect(markSessionId("   ")).toBe("");
    expect(markSessionId(null)).toBe("");
    expect(markSessionId(undefined)).toBe("");
  });
});
