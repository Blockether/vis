// @vitest-environment jsdom
import { act, screen } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

let restore = () => {};
afterEach(() => restore());

// A share arrives with a payload and NO destination. The app used to guess — it
// opened the most recent session and dropped the memo there — which is the one
// thing it must not do: the recording the human sent from Messages belongs to a
// conversation only they can name. So the list is the chooser, and it says what
// it is holding.
describe("the share the list is holding", () => {
  it("names the shared file and asks for a destination", async () => {
    const view = renderSessionsScreen({
      machines: [{ sessions: [listSession({ id: "s1" })] }],
      share: { files: [{ path: "/tmp/0/memo.m4a", name: "memo.m4a", type: "audio/mp4" }] },
    });
    restore = view.restore;

    expect(await screen.findByText("Sharing")).toBeInTheDocument();
    expect(
      screen.getByText(/memo\.m4a — pick a session, or start a new one/),
    ).toBeInTheDocument();
  });

  it("counts a multi-file share rather than listing it", async () => {
    const view = renderSessionsScreen({
      machines: [{ sessions: [listSession({ id: "s1" })] }],
      share: {
        files: [
          { path: "/tmp/0/a.png", name: "a.png" },
          { path: "/tmp/1/b.png", name: "b.png" },
        ],
      },
    });
    restore = view.restore;

    expect(await screen.findByText(/2 files — pick a session/)).toBeInTheDocument();
  });

  // Nothing is holding the bytes but this band: the ✕ is the only way to say "not
  // this one after all", and it has to reach the caller that deletes the copies.
  it("hands the discard back to the shell", async () => {
    const onDiscardShare = vi.fn();
    const view = renderSessionsScreen({
      machines: [{ sessions: [listSession({ id: "s1" })] }],
      share: { text: "look at this" },
      onDiscardShare,
    });
    restore = view.restore;

    const discard = await screen.findByLabelText("Discard the share");
    await act(async () => {
      discard.click();
    });

    expect(onDiscardShare).toHaveBeenCalledTimes(1);
  });

  it("says nothing at all when no share is parked", async () => {
    const view = renderSessionsScreen({ machines: [{ sessions: [listSession({ id: "s1" })] }] });
    restore = view.restore;

    await act(async () => {});
    expect(screen.queryByText("Sharing")).not.toBeInTheDocument();
  });
});
