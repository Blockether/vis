import { describe, expect, it } from "vitest";
import {
  insertImageReferences,
  referencedAttachments,
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

  it("does not allocate references or alter text for a rejected batch", () => {
    expect(insertImageReferences("  untouched  ", 2, [], 3)).toEqual({
      text: "  untouched  ",
      caret: 2,
      attachments: [],
      counter: 3,
    });
  });
});
