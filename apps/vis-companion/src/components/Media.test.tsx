// @vitest-environment jsdom
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { renderToStaticMarkup } from "react-dom/server";
import { describe, expect, it } from "vitest";

import {
  mediaFrameClass,
  mediaGridClass,
  mediaPendingClass,
  mediaTileFrameClass,
} from "../lib/media-frame";
import {
  MediaGrid,
  MediaPlate,
  MediaRecording,
  MediaTile,
  mediaMeta,
  mediaSummary,
} from "./Media";

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

// A recording is the one artifact with nothing to paint. Given the plate it
// would reserve a 4:3 box around silence and caption it like a picture that
// failed to decode.
describe("MediaRecording", () => {
  const html = renderToStaticMarkup(
    <MediaRecording name="memo.m4a" meta="M4A · 8B">
      <audio controls />
    </MediaRecording>,
  );

  it("reserves no picture box", () => {
    expect(html).not.toContain(mediaFrameClass);
    expect(html).not.toContain(mediaTileFrameClass);
  });

  it("names the recording under the player it hands over", () => {
    expect(html).toContain("<audio");
    expect(html).toContain("<figcaption");
    expect(html).toContain("memo.m4a");
    expect(html).toContain("M4A · 8B");
  });

  // The words a memo carries are the only thing a reader can SKIM, and the same
  // string the model was given — but a minute of speech is a paragraph, so the row
  // stays one line until it is asked.
  it("folds the transcription away behind its own band", () => {
    const withWords = renderToStaticMarkup(
      <MediaRecording
        name="memo.m4a"
        meta="M4A · 8B"
        transcription="buy milk and call back"
      >
        <audio controls />
      </MediaRecording>,
    );
    expect(withWords).toContain("TRANSCRIPTION");
    expect(withWords).toContain('aria-expanded="false"');
    expect(withWords).not.toContain("buy milk and call back");
  });

  it("shows no band at all when nobody has asked for a transcript", () => {
    expect(html).not.toContain("TRANSCRIPTION");
    expect(html).not.toContain("aria-expanded");
  });

  // Regression, issue: a recording whose transcription failed looked exactly like one
  // nobody had transcribed yet — an empty space under the player, and a model that was
  // handed the filename alone.
  it("says what the words are doing when there are none to show", () => {
    const band = (status: string) =>
      renderToStaticMarkup(
        <MediaRecording name="memo.m4a" meta="M4A · 8B" transcriptionStatus={status}>
          <audio controls />
        </MediaRecording>,
      );
    expect(band("pending")).toContain("TRANSCRIBING…");
    expect(band("unavailable")).toContain("NO TRANSCRIPTION");
    expect(band("silent")).toContain("NO SPEECH");
    // A status is a caption, never a control: there is nothing to open.
    expect(band("pending")).not.toContain("aria-expanded");
  });

  it("prefers the words themselves to whatever the status still says", () => {
    const settled = renderToStaticMarkup(
      <MediaRecording
        name="memo.m4a"
        transcription="buy milk and call back"
        transcriptionStatus="pending"
      >
        <audio controls />
      </MediaRecording>,
    );
    expect(settled).toContain("TRANSCRIPTION");
    expect(settled).not.toContain("TRANSCRIBING…");
  });

  // A mic glyph beside a player is the same claim twice: the control is already,
  // visibly, audio. It only ate column width from the scrubber.
  it("puts no icon beside the player", () => {
    expect(html).not.toContain("<svg");
  });

  // Speech is not code. Opened, the words read as an italic QUOTATION of the audio
  // above them without distorting the spacing between words.
  it("quotes the opened transcript in naturally spaced italic", async () => {
    render(
      <MediaRecording name="memo.m4a" transcription="buy milk and call back">
        <audio controls />
      </MediaRecording>,
    );
    await userEvent.click(screen.getByText("TRANSCRIPTION"));
    const words = screen.getByText(/buy milk and call back/);
    expect(words.textContent).toBe("“buy milk and call back”");
    expect(words.className).toContain("italic");
  });

  it("ignores a transcript that is only whitespace", () => {
    const blank = renderToStaticMarkup(
      <MediaRecording name="memo.m4a" transcription="   ">
        <audio controls />
      </MediaRecording>,
    );
    expect(blank).not.toContain("TRANSCRIPTION");
  });
});
