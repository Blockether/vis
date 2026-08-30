// @vitest-environment jsdom
import {
  cleanup,
  render,
  screen,
  waitFor,
  within,
} from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { afterEach, describe, expect, it, vi } from "vitest";

import { STORY_GATEWAYS, STORY_SESSION_ROW } from "../dev/story-data";
import { EMPTY_DRAFT_MESSAGE } from "../lib/draft-messages";
import { SessionRow } from "./SessionList";

const conn = STORY_GATEWAYS[0];

afterEach(cleanup);

function deferred() {
  let resolve!: () => void;
  let reject!: (cause: Error) => void;
  const promise = new Promise<void>((accept, refuse) => {
    resolve = accept;
    reject = refuse;
  });
  return { promise, resolve, reject };
}

function row(rename = vi.fn(async () => {})) {
  const commands = {
    open: vi.fn(),
    rename,
    fork: vi.fn(),
    requestDelete: vi.fn(),
    toggleStar: vi.fn(),
  };
  render(
    <SessionRow
      session={STORY_SESSION_ROW}
      draft={EMPTY_DRAFT_MESSAGE}
      conn={conn}
      match={null}
      needle=""
      commands={commands}
      deletion={null}
    />,
  );
  return commands;
}

function startRename() {
  const actions = screen.getByRole("group", {
    name: `${STORY_SESSION_ROW.title} actions`,
  });
  return userEvent.click(
    within(actions).getByRole("button", { name: "Rename" }),
  );
}

describe("session row inline rename", () => {
  // Regression, user report: entering rename replaced the session row with a field,
  // so metadata, status, and disclosure disappeared instead of only the title changing.
  it("edits only the title and saves the trimmed name", async () => {
    const request = deferred();
    const rename = vi.fn(() => request.promise);
    row(rename);

    await startRename();

    expect(screen.queryByRole("dialog")).toBeNull();
    const field = screen.getByRole("textbox", {
      name: `Rename ${STORY_SESSION_ROW.title}`,
    });
    expect(field).toHaveValue(STORY_SESSION_ROW.title);
    expect(field).toHaveFocus();
    expect((field as HTMLInputElement).selectionStart).toBe(0);
    expect((field as HTMLInputElement).selectionEnd).toBe(
      (field as HTMLInputElement).value.length,
    );
    expect((field as HTMLInputElement).selectionDirection).toBe("backward");
    expect(
      document.querySelector(`[data-session-id="${STORY_SESSION_ROW.id}"]`),
    ).not.toBeNull();
    expect(screen.getByText(STORY_SESSION_ROW.id)).toBeTruthy();
    expect(
      screen.getByText(`${STORY_SESSION_ROW.turn_count} turns`),
    ).toBeTruthy();
    expect(screen.getByText("INPUT NEEDED")).toBeTruthy();
    expect(
      screen.getByRole("button", {
        name: `Show details for ${STORY_SESSION_ROW.title}`,
      }),
    ).toBeTruthy();
    expect(
      screen.queryByRole("group", {
        name: `${STORY_SESSION_ROW.title} actions`,
      }),
    ).toBeNull();

    const user = userEvent.setup();
    await user.clear(field);
    await user.type(field, "  Release notes  {Enter}");

    expect(rename).toHaveBeenCalledWith(
      STORY_SESSION_ROW,
      conn,
      "Release notes",
    );
    expect(screen.getByText("Saving")).toBeTruthy();
    request.resolve();
    await waitFor(() => expect(screen.queryByRole("textbox")).toBeNull());
  });

  it("cancels with Escape and keeps a refused name in the row", async () => {
    const rename = vi.fn(async () => {
      throw new Error("Name already exists");
    });
    row(rename);
    const user = userEvent.setup();

    await startRename();
    let field = screen.getByRole("textbox");
    await user.clear(field);
    await user.type(field, "Discard me{Escape}");
    expect(screen.queryByRole("textbox")).toBeNull();
    expect(rename).not.toHaveBeenCalled();

    await startRename();
    field = screen.getByRole("textbox");
    await user.clear(field);
    await user.type(field, "Existing{Enter}");
    expect(await screen.findByRole("status")).toHaveTextContent(
      "Name already exists",
    );
    expect(screen.getByRole("textbox")).toHaveValue("Existing");
  });
});
