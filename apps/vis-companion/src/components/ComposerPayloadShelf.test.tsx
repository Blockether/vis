// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { PendingAttachment } from "../lib/attachments";
import { createComposerPaste } from "../lib/paste";
import { ComposerPayloadShelf } from "./ComposerPayloadShelf";

const image: PendingAttachment = {
  id: "diagram",
  filename: "release-map.png",
  media_type: "image/png",
  base64: "data:image/png;base64,AA==",
  previewUrl: "data:image/png;base64,AA==",
  size: 1,
};

const recording: PendingAttachment = {
  id: "memo",
  filename: "release-note.m4a",
  media_type: "audio/mp4",
  base64: "data:audio/mp4;base64,AA==",
  previewUrl: "data:audio/mp4;base64,AA==",
  size: 1,
};

afterEach(cleanup);

describe("composer payload shelf", () => {
  it("owns staged-paste and attachment actions", () => {
    const commands = {
      editPaste: vi.fn(),
      removePaste: vi.fn(),
      editAttachment: vi.fn(),
      removeAttachment: vi.fn(),
    };
    render(
      <ComposerPayloadShelf
        pastes={[createComposerPaste(4, "alpha\nbeta")]}
        attachments={[image, recording]}
        commands={commands}
      />,
    );

    fireEvent.click(
      screen.getByRole("button", { name: "Edit pasted block 4" }),
    );
    fireEvent.click(
      screen.getByRole("button", { name: "Remove pasted block 4" }),
    );
    fireEvent.click(
      screen.getByRole("button", { name: "Remove release-map.png" }),
    );
    fireEvent.click(
      screen.getByRole("button", { name: "Remove release-note.m4a" }),
    );

    expect(
      screen.getByText("release-map.png").parentElement?.querySelector("img"),
    ).toHaveAttribute("alt", "");
    expect(commands.editPaste).toHaveBeenCalledWith(4);
    expect(commands.removePaste).toHaveBeenCalledWith(4);
    expect(commands.removeAttachment).toHaveBeenNthCalledWith(1, "diagram");
    expect(commands.removeAttachment).toHaveBeenNthCalledWith(2, "memo");
  });

  it("renders no shelf when the draft carries no payload", () => {
    const { container } = render(
      <ComposerPayloadShelf
        pastes={[]}
        attachments={[]}
        commands={{
          editPaste: vi.fn(),
          removePaste: vi.fn(),
          editAttachment: vi.fn(),
          removeAttachment: vi.fn(),
        }}
      />,
    );

    expect(container).toBeEmptyDOMElement();
  });
});
