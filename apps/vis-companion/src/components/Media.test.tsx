import { renderToStaticMarkup } from "react-dom/server";
import { describe, expect, it } from "vitest";

import {
  mediaFrameClass,
  mediaGridClass,
  mediaPendingClass,
  mediaTileFrameClass,
} from "../lib/media-frame";
import { MediaGrid, MediaPlate, MediaTile, mediaMeta, mediaSummary } from "./Media";

/** The frame element of a rendered plate/tile, class list and all. */
const frame = (html: string) =>
  /<div class="([^"]*)"/u.exec(html)?.[1] ?? "";

// Regression, iOS scroll jump: the box a picture occupies is decided before
// anyone knows what is in it, so the pulse, the picture and the failure notice
// all have to reserve the SAME rectangle. It used to be spelled at the call
// site once per state; now it is spelled by the plate, once.
describe("MediaPlate", () => {
  const withChild = (child: string) =>
    frame(
      renderToStaticMarkup(
        <MediaPlate name="shot.png" meta="PNG · 8B">
          <div className={child} />
        </MediaPlate>,
      ),
    );

  it("reserves the same box whatever it is showing", () => {
    expect(withChild(mediaPendingClass)).toBe(mediaFrameClass);
    expect(withChild("some-picture")).toBe(mediaFrameClass);
    expect(withChild("some-failure-notice")).toBe(mediaFrameClass);
  });

  it("docks the name and the format under the mat", () => {
    const html = renderToStaticMarkup(
      <MediaPlate name="shot.png" meta="PNG · 8B">
        <div />
      </MediaPlate>,
    );

    expect(html).toContain("<figcaption");
    expect(html).toContain("shot.png");
    expect(html).toContain("PNG · 8B");
  });

  it("carries no caption strip at all when there is no name", () => {
    expect(
      renderToStaticMarkup(
        <MediaPlate>
          <div />
        </MediaPlate>,
      ),
    ).not.toContain("<figcaption");
  });
});

describe("MediaGrid", () => {
  it("lays its tiles out in the gallery and says what they are", () => {
    const html = renderToStaticMarkup(
      <MediaGrid summary="2 images · 16B">
        <MediaTile>
          <div />
        </MediaTile>
        <MediaTile>
          <div />
        </MediaTile>
      </MediaGrid>,
    );

    expect(html).toContain(mediaGridClass);
    expect(html.match(new RegExp(mediaTileFrameClass, "gu"))).toHaveLength(2);
    expect(html).toContain("2 images · 16B");
  });
});

// Regression, user report ("margin bottom from this component is not the same
// like margin top"): a media block sits in the transcript's own `gap-2.5` stack,
// and it used to spell a gap on BOTH edges — so the picture had 10px above it and
// 18-20px below, and the block read as if it belonged to the paragraph under it.
// The stack owns the trailing gap; the block only tops itself up to it, and never
// on the very first row.
describe("a media block's own edges", () => {
  const root = (html: string) => /^<\w+ class="([^"]*)"/u.exec(html)?.[1] ?? "";

  const edges = [
    ["MediaPlate", renderToStaticMarkup(<MediaPlate><div /></MediaPlate>)],
    [
      "MediaGrid",
      renderToStaticMarkup(
        <MediaGrid summary="1 image · 8B">
          <MediaTile>
            <div />
          </MediaTile>
        </MediaGrid>,
      ),
    ],
  ] as const;

  for (const [name, html] of edges) {
    it(`tops ${name} up to the stack's gap and never pads under it`, () => {
      const classes = root(html).split(/\s+/u);

      expect(classes).toContain("mt-2.5");
      expect(classes).toContain("first:mt-0");
      expect(classes.filter((one) => /^m[by]?-/u.test(one))).toEqual([]);
    });
  }
});

describe("the gallery's own line", () => {
  it("counts what it holds", () => {
    expect(mediaSummary([{ size: 1 }])).toBe("1 image · 1B");
    expect(mediaSummary([{ size: 1024 }, { size: 1024 }])).toBe(
      "2 images · 2.0KB",
    );
  });

  // A weight that only some of the pictures reported is a WRONG number, not a
  // smaller one, so the line simply does not claim one.
  it("claims a weight only when every picture reported one", () => {
    expect(mediaSummary([{ size: 1024 }, {}])).toBe("2 images");
  });
});

describe("a plate's caption", () => {
  it("names the format from the file, then its weight", () => {
    expect(mediaMeta({ filename: "shot.png", size: 8 })).toBe("PNG · 8B");
    expect(mediaMeta({ media_type: "image/webp" })).toBe("WEBP");
  });
});
