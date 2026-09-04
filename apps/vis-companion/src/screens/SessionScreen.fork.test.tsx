// @vitest-environment jsdom
import { screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { describe, expect, it } from "vitest";

import { renderSessionScreen } from "./session-screen-harness";

const turn = (turn_id: string, request: string) => ({
  turn_id,
  request,
  status: "completed",
  created_at: Date.now(),
  content: [],
  iterations: [],
});

// Cutting a fork AT a turn is asked on that turn, where the reader can see what
// they are forking — not from a popover of truncated first lines on the session
// list. The verb sits on the turn's own role line and takes the reader into the copy.
describe("forking from a turn in the transcript", () => {
  it("forks THROUGH the turn whose verb was pressed and opens the copy", async () => {
    const rows = [turn("t1", "make the header amber"), turn("t2", "now undo the second half")];
    const forks: Array<[string, string | undefined]> = [];
    const opened: string[] = [];
    renderSessionScreen({
      client: {
        cachedTranscript: () => rows,
        transcript: () => Promise.resolve(rows),
        forkSession: (sid: string, through?: string) => {
          forks.push([sid, through]);
          return Promise.resolve({ id: "forked", title: "A session (fork)" });
        },
      },
      onOpenSession: (sid) => opened.push(sid),
    });

    const verbs = await screen.findAllByRole("button", { name: "Fork from here" });
    expect(verbs).toHaveLength(2);
    await userEvent.click(verbs[0]);

    await waitFor(() => expect(opened).toEqual(["forked"]));
    expect(forks).toEqual([["s1", "t1"]]);
  });
});
