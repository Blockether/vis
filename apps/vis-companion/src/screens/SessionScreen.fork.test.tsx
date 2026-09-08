// @vitest-environment jsdom
import { act, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { describe, expect, it, vi } from "vitest";

import { renderSessionScreen } from "./session-screen-harness";

const turn = (turn_id: string, request: string) => ({
  turn_id,
  request,
  status: "completed",
  created_at: Date.now(),
  content: [
    {
      id: `${turn_id}-answer`,
      type: "prose",
      markdown: `Answer to ${request}.`,
    },
  ],
  iterations: [],
});

// Forking belongs to the assistant answer and includes that entire turn.
describe("forking from a turn in the transcript", () => {
  it("forks THROUGH the turn whose verb was pressed and opens the copy", async () => {
    const rows = [
      turn("t1", "make the header amber"),
      turn("t2", "now undo the second half"),
    ];
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

    const verbs = await screen.findAllByRole("button", {
      name: "Fork from here",
    });
    expect(verbs).toHaveLength(2);
    for (const [index, row] of rows.entries()) {
      const answer = screen
        .getByText(`Answer to ${row.request}.`)
        .closest("article");
      expect(verbs[index].closest("article")).toBe(answer);
      const request = screen.getByText(row.request).closest("article")!;
      expect(
        within(request).queryByRole("button", { name: "Fork from here" }),
      ).toBeNull();
    }
    await userEvent.click(verbs[0]);

    await waitFor(() => expect(opened).toEqual(["forked"]));
    expect(forks).toEqual([["s1", "t1"]]);
  });

  it("offers a fork on an answer without a user message", async () => {
    const rows = [turn("t1", "")];
    renderSessionScreen({
      client: {
        cachedTranscript: () => rows,
        transcript: () => Promise.resolve(rows),
      },
    });

    const fork = await screen.findByRole("button", { name: "Fork from here" });
    expect(fork.closest("article")).toBe(
      screen.getByText("Answer to .").closest("article"),
    );
    expect(screen.queryByText("You", { exact: true })).toBeNull();
  });

  it("keeps the answer action disabled while forking and restores it on failure", async () => {
    const rows = [
      turn("t1", "make the header amber"),
      turn("t2", "now undo the second half"),
    ];
    let rejectFork!: (reason: Error) => void;
    const forkSession = vi.fn(
      () =>
        new Promise<never>((_resolve, reject) => {
          rejectFork = reject;
        }),
    );
    const opened = vi.fn();
    renderSessionScreen({
      client: {
        cachedTranscript: () => rows,
        transcript: () => Promise.resolve(rows),
        forkSession,
      },
      onOpenSession: opened,
    });

    const [fork, otherFork] = await screen.findAllByRole("button", {
      name: "Fork from here",
    });
    await userEvent.click(fork);
    expect(fork).toBeDisabled();
    expect(fork).toHaveTextContent("Forking...");
    expect(otherFork).toBeEnabled();
    await userEvent.click(fork);
    expect(forkSession).toHaveBeenCalledTimes(1);

    await act(async () => rejectFork(new Error("Could not fork this turn")));
    expect(await screen.findByText("Could not fork this turn")).toBeVisible();
    expect(fork).toBeEnabled();
    expect(fork).toHaveTextContent("Fork from here");
    expect(opened).not.toHaveBeenCalled();
  });
});
