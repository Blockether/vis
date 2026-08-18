// @vitest-environment jsdom
import { screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { afterEach, describe, expect, it } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

let restore = () => {};
afterEach(() => restore());

// A fork is a COPY of a conversation, so the row it starts from must come back
// untouched: these tests hold the slide's verb, the panel's two answers (the whole
// session, or one turn), and the request each one actually puts on the wire.
describe("forking a session from its slide", () => {
  const forks = {
    turns: [
      { turn_id: "t1", request: "make the header amber" },
      { turn_id: "t2", request: "now undo the second half" },
    ],
    session: { id: "forked", title: "A session (fork)" },
  };
  const machines = [
    {
      sessions: [listSession({ id: "s1", title: "A session" })],
      routes: { "/v1/sessions/s1/forks": forks },
    },
  ];

  const strip = () => screen.getByRole("group", { name: "A session actions" });

  it("offers the whole session first and forks it without naming a turn", async () => {
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

    await screen.findByText("The whole session");
    await userEvent.click(screen.getByText("The whole session"));

    await screen.findByText("A session", {}, { timeout: 2000 });
    const posted = view.requests.filter(
      (request) => request.method === "POST" && request.path === "/v1/sessions/s1/forks",
    );
    expect(posted).toHaveLength(1);
    expect(posted[0].body).toEqual({});
    expect(opened).toEqual(["forked"]);
  });

  it("lists the session's turns and forks THROUGH the one that was picked", async () => {
    const opened: string[] = [];
    const view = renderSessionsScreen({
      machines,
      onOpen: (_conn, sid) => opened.push(sid),
    });
    restore = view.restore;
    await screen.findByText("A session");

    await userEvent.click(strip().querySelector('button[aria-label="Fork A session"]')!);

    // The turns are the picker, numbered as the reader counts them and carrying the
    // words that opened each one.
    const second = await screen.findByText("Turn 2 · now undo the second half");
    expect(screen.getByText("Turn 1 · make the header amber")).toBeTruthy();
    await userEvent.click(second);

    await screen.findByText("A session", {}, { timeout: 2000 });
    const posted = view.requests.filter(
      (request) => request.method === "POST" && request.path === "/v1/sessions/s1/forks",
    );
    expect(posted).toHaveLength(1);
    expect(posted[0].body).toEqual({ through_turn_id: "t2" });
    expect(opened).toEqual(["forked"]);
  });
});
