// @vitest-environment jsdom
import {
  cleanup,
  fireEvent,
  render,
  screen,
  waitFor,
} from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import { RenameSessionDialog } from "./RenameSessionDialog";

afterEach(cleanup);

const session = { id: "abc12345-rest", title: "Previous name" };

describe("rename session dialog", () => {
  it("owns its draft and closes only after the rename succeeds", async () => {
    const onDismiss = vi.fn();
    const onRename = vi.fn(async () => {});
    render(
      <RenameSessionDialog
        session={session}
        onDismiss={onDismiss}
        onRename={onRename}
      />,
    );

    fireEvent.change(screen.getByPlaceholderText("Session name"), {
      target: { value: "  Release notes  " },
    });
    fireEvent.click(screen.getByRole("button", { name: "Save" }));

    await waitFor(() => expect(onRename).toHaveBeenCalledWith("Release notes"));
    expect(onDismiss).toHaveBeenCalledOnce();
  });

  it("keeps invalid and refused names in the dialog", async () => {
    const onDismiss = vi.fn();
    const onRename = vi.fn(async () => {
      throw new Error("Name already exists");
    });
    render(
      <RenameSessionDialog
        session={session}
        onDismiss={onDismiss}
        onRename={onRename}
      />,
    );

    fireEvent.change(screen.getByPlaceholderText("Session name"), {
      target: { value: "   " },
    });
    fireEvent.click(screen.getByRole("button", { name: "Save" }));
    expect(
      await screen.findByText("A session name cannot be empty."),
    ).toBeTruthy();
    expect(onRename).not.toHaveBeenCalled();

    fireEvent.change(screen.getByPlaceholderText("Session name"), {
      target: { value: "Existing" },
    });
    fireEvent.keyDown(screen.getByPlaceholderText("Session name"), {
      key: "Enter",
    });
    expect(await screen.findByText("Name already exists")).toBeTruthy();
    expect(onDismiss).not.toHaveBeenCalled();
  });

  it("owns cancel and close dismissal", () => {
    const onDismiss = vi.fn();
    render(
      <RenameSessionDialog
        session={session}
        onDismiss={onDismiss}
        onRename={async () => {}}
      />,
    );

    fireEvent.click(screen.getByRole("button", { name: "Cancel" }));
    fireEvent.click(
      screen.getByRole("button", { name: "Close Rename session" }),
    );
    expect(onDismiss).toHaveBeenCalledTimes(2);
  });
});
