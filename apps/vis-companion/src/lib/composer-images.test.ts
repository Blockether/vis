import { describe, expect, it } from "vitest";
import {
  detachLostReferences,
  insertImageReferences,
  referencedAttachments,
  restoreImageReferences,
} from "./composer-images";
import type { PendingAttachment } from "./attachments";

const image: PendingAttachment = {
  id: "image",
  filename: "image.png",
  media_type: "image/png",
  base64: "abc",
  previewUrl: "abc",
  size: 3,
};

describe("composer image references", () => {
  it("numbers only images in a mixed batch and preserves the insertion surroundings", () => {
    const audio = { ...image, id: "audio", media_type: "audio/wav" };
    const result = insertImageReferences(
      "  leftRIGHT  ",
      6,
      [audio, image, { ...image, id: "second" }],
      4,
    );
    expect(result.text).toBe("  left [IMAGE #5] [IMAGE #6] RIGHT  ");
    expect(result.attachments.map((item) => item.reference)).toEqual([
      undefined,
      "[IMAGE #5]",
      "[IMAGE #6]",
    ]);
    expect(result.counter).toBe(6);
  });

  it("keeps unowned literal tokens and unreferenced nonimage attachments independent", () => {
    const audio = { ...image, id: "audio", media_type: "audio/wav" };
    expect(
      referencedAttachments("[IMAGE #10]", [
        { ...image, reference: "[IMAGE #1]" },
        audio,
      ]),
    ).toEqual([audio]);
  });

  it("detaches only the lost owned tokens and keeps their bytes", () => {
    const audio = { ...image, id: "audio", media_type: "audio/wav" };
    const detached = detachLostReferences("[IMAGE #2] [IMAGE #10]", [
      { ...image, reference: "[IMAGE #1]" },
      { ...image, id: "kept", reference: "[IMAGE #2]" },
      audio,
    ]);
    expect(detached.map((item) => item.reference)).toEqual([
      undefined,
      "[IMAGE #2]",
      undefined,
    ]);
    expect(detached.map((item) => item.base64)).toEqual(["abc", "abc", "abc"]);
  });

  it("keeps the array identical while every owned token survives", () => {
    const attachments = [
      { ...image, reference: "[IMAGE #1]" },
      { ...image, id: "audio", media_type: "audio/wav" },
    ];
    expect(detachLostReferences("[IMAGE #1]", attachments)).toBe(attachments);
    const detached = detachLostReferences("", attachments);
    expect(detachLostReferences("", detached)).toBe(detached);
  });

  it("does not allocate references or alter text for a rejected batch", () => {
    expect(insertImageReferences("  untouched  ", 2, [], 3)).toEqual({
      text: "  untouched  ",
      caret: 2,
      attachments: [],
      counter: 3,
    });
  });

  it("rebases incoming collisions simultaneously without renumbering survivors", () => {
    const current = { ...image, id: "newer", reference: "[IMAGE #1]" };
    const first = { ...image, reference: "[IMAGE #1]" };
    const second = { ...image, id: "second", reference: "[IMAGE #2]" };
    const restored = restoreImageReferences(
      "[IMAGE #1] literal [IMAGE #2]",
      [current],
      "[IMAGE #1] [IMAGE #2] [IMAGE #1]",
      [first, second],
      2,
    );
    expect(restored.text).toBe("[IMAGE #3] [IMAGE #4] [IMAGE #3]");
    expect(restored.attachments).toEqual([
      current,
      { ...first, reference: "[IMAGE #3]" },
      { ...second, reference: "[IMAGE #4]" },
    ]);
    expect(restored.counter).toBe(4);
  });

  it("reuses an already restored attachment and ignores deleted incoming tokens", () => {
    const current = { ...image, reference: "[IMAGE #5]" };
    const restored = restoreImageReferences(
      "[IMAGE #5]",
      [current],
      "[IMAGE #1]",
      [
        { ...image, reference: "[IMAGE #1]" },
        { ...image, id: "deleted", reference: "[IMAGE #2]" },
      ],
      5,
    );
    expect(restored.text).toBe("[IMAGE #5]");
    expect(restored.attachments).toEqual([current]);
  });
});
