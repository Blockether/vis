import { describe, expect, it } from "vitest";

import {
  mediaCaptionClass,
  mediaContentClass,
  mediaFrameClass,
  mediaGridClass,
  mediaGroupLayout,
  mediaTileContentClass,
  mediaTileFrameClass,
} from "./media-frame";

// Regression, issue: scrolling an iOS transcript full of screenshots jumped.
// A produced-image tile reserved a 96 px pulse while its object URL loaded and
// then became whatever the picture measured (up to 60svh), and a user bubble's
// pasted image reserved nothing at all until it decoded. Each of those swaps
// above the fold shoved the reader's line down — and the scroll corrector
// stands down while a finger is on the glass, so nobody put it back.
describe("transcript media frame", () => {
  it("sizes the box from the column and the viewport, never from the picture", () => {
    expect(mediaFrameClass).toContain("w-full");
    expect(mediaFrameClass).toMatch(/aspect-/u);
    // `w-auto`/`h-auto` are exactly the "ask the image how big it is" sizings
    // that made the box move once the bytes landed.
    expect(mediaFrameClass).not.toMatch(/\b[wh]-auto\b/u);
  });

  it("contains the media inside the reserved box instead of letting it push", () => {
    expect(mediaContentClass).toContain("h-full");
    expect(mediaContentClass).toContain("w-full");
    expect(mediaContentClass).toContain("object-contain");
  });
});

// Regression, user report ("the contain object makes it look awful — the image
// filename is ON the image instead of under it as a label"): the picture was
// shoved to `object-left` inside an unframed 4:3 box, so a tall screenshot left
// a wide empty half with the bare caption floating under it.
describe("the media plate", () => {
  it("centres the picture on the frame's own mat", () => {
    expect(mediaContentClass).toContain("object-center");
    expect(mediaContentClass).not.toContain("object-left");
  });

  it("frames the mat so the letterbox is paper, not a gap", () => {
    expect(mediaFrameClass).toContain("border");
    expect(mediaFrameClass).toContain("bg-code");
    expect(mediaFrameClass).toContain("overflow-hidden");
  });

  it("docks the name under the mat as a label sharing that frame", () => {
    expect(mediaCaptionClass).toContain("border");
    expect(mediaCaptionClass).toContain("border-t-0");
    expect(mediaCaptionClass).toContain("bg-thinking-surface");
  });
});

// ONE picture is a plate; several are a gallery. Four dropped screenshots used
// to be four 60svh plates stacked down the column, which is a wall to scroll
// past rather than something to look at.
describe("the media gallery", () => {
  it("plates a lone picture and grids the rest", () => {
    expect(mediaGroupLayout(0)).toBe("plate");
    expect(mediaGroupLayout(1)).toBe("plate");
    expect(mediaGroupLayout(2)).toBe("grid");
    expect(mediaGroupLayout(9)).toBe("grid");
  });

  it("reserves a tile exactly as it reserves a plate", () => {
    expect(mediaTileFrameClass).toContain("w-full");
    expect(mediaTileFrameClass).toMatch(/aspect-/u);
    expect(mediaTileFrameClass).toContain("overflow-hidden");
    expect(mediaTileFrameClass).not.toMatch(/\b[wh]-auto\b/u);
  });

  it("wears the plate's own paper and edge, at gallery size", () => {
    expect(mediaTileFrameClass).toContain("border-code-edge");
    expect(mediaTileFrameClass).toContain("bg-code");
  });

  it("fills a tile instead of matting it", () => {
    expect(mediaTileContentClass).toContain("object-cover");
    expect(mediaContentClass).toContain("object-contain");
  });

  // An iPad is a wide TOUCH device: width may add a column, and only a mouse
  // may take the tighter one. Nothing here pins a smaller box.
  it("lets width add columns and never shrink a hit box", () => {
    expect(mediaGridClass).toContain("grid-cols-2");
    expect(mediaGridClass).toContain("sm:grid-cols-3");
    expect(mediaGridClass).toContain("mouse:grid-cols-4");
    expect(mediaGridClass).not.toMatch(/\bsm:(?:min-)?[wh]-/u);
  });
});
