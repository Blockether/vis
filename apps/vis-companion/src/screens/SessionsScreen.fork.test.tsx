// @vitest-environment jsdom
import { screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { afterEach, describe, expect, it } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

let restore = () => {};
afterEach(() => restore());

// A fork is a COPY of a conversation, so the row it starts from must come back
// untouched, and the row's verb asks nothing: one press forks the whole session and
// opens the copy. Cutting at a turn is that turn's own verb, in the transcript.
describe("forking a session from its slide", () => {
  const machines = [
    {
      sessions: [listSession({ id: "s1", title: "A session" })],
      routes: { "/v1/sessions/s1/forks": { session: { id: "forked", title: "A session (fork)" } } },
    },
  ];

  const strip = () => screen.getByRole("group", { name: "A session actions" });

  it("forks the whole session on one press and opens the copy", async () => {
    const opened: string[] = [];
    const view = renderSessionsScreen({
      machines,
      onOpen: (_conn, sid) => opened.push(sid),
    });
    restore = view.restore;
    await screen.findByText("A session");

    // The verb sits on the slide beside Rename, and it is NOT the red one: forking
    // takes nothing away from the row it starts on.
    const fork = strip().querySelector('button[aria-label="Fork A session"]')!;
    expect(fork.className).not.toContain("bg-err");
    await userEvent.click(fork);

    await screen.findByText("A session", {}, { timeout: 2000 });
    const posted = view.requests.filter(
      (request) => request.method === "POST" && request.path === "/v1/sessions/s1/forks",
    );
    expect(posted).toHaveLength(1);
    expect(posted[0].body).toEqual({});
    expect(opened).toEqual(["forked"]);
    // No question was asked on the way.
    expect(screen.queryByRole("dialog", { name: "Fork this session" })).toBeNull();
  });
});
