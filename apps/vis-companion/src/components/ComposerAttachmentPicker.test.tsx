// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import { ComposerAttachmentPicker } from "./ComposerAttachmentPicker";

afterEach(cleanup);

describe("composer attachment picker", () => {
  it("owns the browser file input and clears it after delivery", () => {
    const addBrowserFiles = vi.fn();
    const commands = { addBrowserFiles, pickNative: vi.fn() };
    const { container } = render(
      <ComposerAttachmentPicker
        accept="image/*,video/*"
        disabled={false}
        commands={commands}
        isNative={false}
      />,
    );
    const input = container.querySelector(
      'input[type="file"]',
    ) as HTMLInputElement;
    const file = new File(["bytes"], "release.txt", { type: "text/plain" });

    fireEvent.change(input, { target: { files: [file] } });

    expect(addBrowserFiles).toHaveBeenCalledWith([file]);
    expect(input.value).toBe("");
    expect(screen.queryByRole("dialog", { name: "Attach" })).toBeNull();
  });

  it("owns native source selection and dismissal", () => {
    const pickNative = vi.fn();
    const commands = { addBrowserFiles: vi.fn(), pickNative };
    render(
      <ComposerAttachmentPicker
        accept="image/*"
        disabled={false}
        commands={commands}
        isNative
      />,
    );
    const button = screen.getByRole("button", {
      name: "Attach a photo, clip, recording or file",
    });

    fireEvent.click(button);
    fireEvent.click(screen.getByRole("button", { name: "Photos or videos" }));
    expect(pickNative).toHaveBeenCalledWith("media");
    expect(screen.queryByRole("dialog", { name: "Attach" })).toBeNull();

    fireEvent.click(button);
    fireEvent.keyDown(button.parentElement as HTMLElement, { key: "Escape" });
    expect(screen.queryByRole("dialog", { name: "Attach" })).toBeNull();
  });

  it("disables the complete attachment door at its boundary", () => {
    render(
      <ComposerAttachmentPicker
        accept="image/*"
        disabled
        commands={{ addBrowserFiles: vi.fn(), pickNative: vi.fn() }}
        isNative
      />,
    );

    expect(
      screen.getByRole("button", {
        name: "Attach a photo, clip, recording or file",
      }),
    ).toBeDisabled();
  });
});
