// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import { createComposerPaste } from "../lib/paste";
import { PasteEditor } from "./PasteEditor";

afterEach(cleanup);

const paste = createComposerPaste(4, "alpha\nbeta");

describe("paste editor", () => {
  it("owns the unsaved draft and returns only committed content", () => {
    const onSave = vi.fn();
    render(<PasteEditor paste={paste} onDismiss={() => {}} onSave={onSave} />);

    const editor = screen.getByRole("textbox", {
      name: "Content of pasted block 4",
    });
    fireEvent.change(editor, { target: { value: "alpha\nbeta\nrelease" } });

    expect(editor).toHaveProperty("value", "alpha\nbeta\nrelease");
    expect(onSave).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole("button", { name: "Save" }));
    expect(onSave).toHaveBeenCalledWith("alpha\nbeta\nrelease");
  });

  it("owns every dialog exit and saves from the keyboard", () => {
    const onDismiss = vi.fn();
    const onSave = vi.fn();
    render(<PasteEditor paste={paste} onDismiss={onDismiss} onSave={onSave} />);

    const dialog = screen.getByRole("dialog");
    const editor = screen.getByRole("textbox", {
      name: "Content of pasted block 4",
    });
    fireEvent.change(editor, { target: { value: "edited" } });
    fireEvent.keyDown(dialog, { key: "Enter", ctrlKey: true });
    expect(onSave).toHaveBeenCalledWith("edited");

    fireEvent.keyDown(dialog, { key: "Escape" });
    fireEvent.click(screen.getByRole("button", { name: "Cancel" }));
    fireEvent.click(screen.getByRole("button", { name: "Close paste editor" }));
    fireEvent.mouseDown(dialog.parentElement as HTMLElement);
    expect(onDismiss).toHaveBeenCalledTimes(4);
  });
});
