import artifactsSheetSource from "./ArtifactsSheet.tsx?raw";
import uiSource from "./ui.tsx?raw";
import { renderToStaticMarkup } from "react-dom/server";
import { describe, expect, it } from "vitest";
import {
  ArtifactsChip,
  ArtifactsSheet,
  previewLines,
} from "./ArtifactsSheet";
import type { GatewayClient } from "../lib/gateway";
import type { SessionArtifact } from "../lib/artifacts";

/** The height the OTHER chip in the session header wears, read from the
 *  component that owns it. The session id is a `CopyChip` (so is every `Copy`
 *  in the transcript), and it is the surviving half of the pair the deleted
 *  Share button belonged to — so "the same height as the button that used to be
 *  there" is checked against THAT control and not against a number typed into
 *  this file. */
const sessionIdChipHeight = () => {
  const chip =
    /export function CopyChip\([\s\S]*?\n}\n/.exec(uiSource)?.[0] ?? "";
  return /className=\{`[^`]*?\b(h-\d+)\b/.exec(chip)?.[1] ?? "";
};

/** Every class on a rendering's first <button>. */
const buttonClasses = (html: string) =>
  (/<button[^>]*class="([^"]*)"/.exec(html)?.[1] ?? "").split(" ");

/** The classes of the button whose tag carries `mark` — attribute order is React's. */
const classesOf = (html: string, mark: string) => {
  const tag =
    (html.match(/<button[^>]*>/g) ?? []).find((entry) => entry.includes(mark)) ??
    "";
  return /class="([^"]*)"/.exec(tag)?.[1] ?? "";
};

/** The bytes are never fetched in static markup: effects do not run. */
const client = {
  attachmentUrl: async () => "blob:none",
  retainAttachment: () => () => {},
} as unknown as GatewayClient;

const artifact = (over: Partial<SessionArtifact>): SessionArtifact => ({
  key: over.key ?? "i1:0",
  kind: "image",
  name: "revenue.png",
  media: "PNG",
  mediaType: "image/png",
  size: 2048,
  sizeLabel: "2.0KB",
  turn: 6,
  tool: "python_execution",
  iterationId: "i1",
  index: 0,
  version: 1,
  ...over,
});

const picture = artifact({});
const document = artifact({
  key: "i2:0",
  kind: "doc",
  name: "q3-report.pdf",
  media: "PDF",
  mediaType: "application/pdf",
  size: 1024,
  sizeLabel: "1.0KB",
  turn: 5,
  iterationId: "i2",
});
const recorded = artifact({
  key: "i3:0",
  kind: "file",
  name: "build.log",
  media: "LOG",
  mediaType: "text/plain",
  size: 1024,
  sizeLabel: "1.0KB",
  turn: 4,
  tool: "shell",
  iterationId: "i3",
});
const note = artifact({
  key: "i4:0",
  kind: "doc",
  name: "vis-issue-115-comment.md",
  media: "MD",
  mediaType: "text/markdown",
  size: 1900,
  sizeLabel: "1.9KB",
  turn: 1,
  tool: undefined,
  iterationId: "i4",
});

const sheet = (artifacts: SessionArtifact[]) =>
  renderToStaticMarkup(
    <ArtifactsSheet
      client={client}
      sid="s1"
      artifacts={artifacts}
      onClose={() => {}}
    />,
  );

/** Visible text of a rendered chunk: tags out, entities back. */
const text = (html: string) =>
  html
    .replace(/<[^>]+>/g, " ")
    .replace(/&quot;/g, '"')
    .replace(/&#x27;/g, "'")
    .replace(/&gt;/g, ">")
    .replace(/&lt;/g, "<")
    .replace(/&amp;/g, "&");

describe("the artifacts chip", () => {
  it("costs a session that produced nothing exactly nothing", () => {
    expect(
      renderToStaticMarkup(
        <ArtifactsChip count={0} open={false} onToggle={() => {}} />,
      ),
    ).toBe("");
  });

  it("says what it owns and whether that surface is open", () => {
    const html = renderToStaticMarkup(
      <ArtifactsChip count={12} open={false} onToggle={() => {}} />,
    );
    expect(html).toContain('aria-expanded="false"');
    expect(html).toContain('aria-controls="artifacts-surface"');
    expect(html).toContain('aria-label="12 artifacts produced by the model"');
    // The word does not fit a phone header, so the pixels carry a clip and `12`.
    expect(text(html)).toContain("12");
    expect(html).toContain("hidden sm:inline");
  });

  // Regression: the chip stood 44px, then 32px, tall beside a 24px session id,
  // so the header read as one big button with some text next to it. It occupies
  // the box the Share button used to — measured from the chip still shipping.
  it("is exactly the chip the session id beside it is", () => {
    const html = renderToStaticMarkup(
      <ArtifactsChip count={3} open onToggle={() => {}} />,
    );
    expect(sessionIdChipHeight()).toBe("h-6");
    expect(buttonClasses(html)).toContain(sessionIdChipHeight());
    // One height, in every state: no taller touch box to fall back out of.
    expect(buttonClasses(html).join(" ")).not.toMatch(/min-h-|sm:h-|mouse:h-/);
    expect(html).toContain('aria-expanded="true"');
  });

  // Regression: the attachment mark was `▣`, a geometric box that stood for
  // "some object" and read as a smudge at chip size.
  it("wears a paperclip drawn like every other icon in the app", () => {
    const html = renderToStaticMarkup(
      <ArtifactsChip count={3} open onToggle={() => {}} />,
    );
    expect(html).not.toContain("▣");
    // The composer's own icon grammar: 24-grid, currentColor, 1.8 stroke.
    expect(html).toContain('viewBox="0 0 24 24"');
    expect(html).toContain('stroke="currentColor"');
    expect(html).toContain('stroke-width="1.8"');
  });
});

describe("the artifacts sheet", () => {
  it("is the region the chip claims to control, over the transcript", () => {
    const html = sheet([picture, document, recorded]);
    expect(html).toContain('id="artifacts-surface"');
    expect(html).toContain('role="region"');
    expect(html).toContain("absolute inset-0");
  });

  // Regression, user report ("Why not black like all buttons"): the way out of the
  // sheet was ink on the strip's own paper, one row under a ‹ that leaves the session
  // as a black block — one gesture drawn two ways on one screen.
  it("opens on its filter strip and leaves by the app's own black block", () => {
    const html = sheet([picture]);
    expect(html).not.toMatch(/<header/);
    const close = classesOf(html, 'aria-label="Close artifacts"');
    expect(close).toContain("bg-dialog-title");
    expect(close).toContain("text-dialog-title-foreground");
    // The title paint is the MARK's own, never a band above the strip.
    expect(html.match(/bg-dialog-title/g)).toHaveLength(1);
  });

  // Regression, user report ("big on Ipad we don't need this"): a permanent band under
  // the grid spelled out "Tap to open · pinch to zoom …" — an instruction one tap
  // teaches, paid for on every screen for the life of the session.
  it("spends no band on telling you what a tap does", () => {
    const html = sheet([picture]);
    expect(html).not.toContain("Tap to open");
    expect(html).not.toMatch(/<footer/);
  });

  // Regression, user report ("why not sneakpeak! this should be visible too in
  // artifacts"): every document wore the same five grey bars, so two notes produced by
  // one session were told apart only by the filename under them.
  it("reads the head of a written note, blank lines dropped", () => {
    expect(previewLines("# Plan\n\n\nOne\nTwo\n")).toEqual(["# Plan", "One", "Two"]);
    // Eight lines is the whole box: a log does not get to scroll a thumbnail.
    expect(previewLines("a\nb\nc\nd\ne\nf\ng\nh\ni\nj")).toHaveLength(8);
    expect(previewLines("")).toEqual([]);
  });

  // The last line lands ON the box's edge. It fades out there rather than being cut
  // through the middle of its letters, which reads as a broken tile instead of a page
  // that continues.
  it("dissolves the peek at the bottom of the tile instead of slicing it", () => {
    expect(artifactsSheetSource).toContain(
      "[mask-image:linear-gradient(to_bottom,black_60%,transparent)]",
    );
  });

  // A PDF has no cheap raster and nothing this app can read without a renderer, so it
  // keeps the plate rather than faking a peek at itself.
  it("leaves the plate on a document it cannot read", () => {
    const html = sheet([document]);
    expect(text(html)).toContain("PDF");
    expect(html).toContain("bg-dialog-hint/50");
  });

  it("welds the close onto the filter strip instead of a band above it", () => {
    const html = sheet([picture]);
    const groupAt = html.indexOf('role="group"');
    const closeAt = html.indexOf('aria-label="Close artifacts"');
    const gridAt = html.indexOf('class="min-h-0 flex-1 overflow-y-auto');
    expect(groupAt).toBeGreaterThan(-1);
    expect(closeAt).toBeGreaterThan(groupAt);
    expect(closeAt).toBeLessThan(gridAt);
  });

  // Regression: a `.md` note was classified as an unreadable file, so the tile
  // was a <div> with a `≡` plate and tapping it did nothing at all.
  it("opens a written note and says what format it is", () => {
    const html = sheet([note]);
    expect(html).toContain(
      'aria-label="Open vis-issue-115-comment.md, MD, 1.9KB, produced in turn 1"',
    );
    expect(text(html)).toContain("MD");
    expect(html).not.toContain("≡");
  });

  it("gives a recorded file its format instead of a glyph for everything", () => {
    const html = sheet([recorded]);
    expect(html).not.toContain("≡");
    expect(text(html)).toContain("LOG");
  });

  it("draws a filter with nothing behind it disabled, never hidden", () => {
    const html = sheet([document]);
    expect(html).toContain('aria-label="Pictures, 0 artifacts"');
    expect(html).toContain('aria-label="Documents, 1 artifacts"');
    // The strip keeps its shape: a disabled chip is still four chips wide.
    expect(html.match(/aria-pressed=/g)).toHaveLength(4);
    expect(html).toContain('disabled=""');
  });

  // Regression: the filter chips were 32px tall, taller than a chip and taller
  // than the app's own phone-height button, so the strip read as a row of empty
  // boxes above the grid.
  it("keeps the filter strip at chip height, shrinking only under a cursor", () => {
    const strip =
      /<button[^>]*aria-pressed="[^"]*"[^>]*class="([^"]*)"/.exec(
        sheet([document]),
      )?.[1] ?? "";
    expect(strip.split(" ")).toContain("min-h-7");
    expect(strip.split(" ")).toContain("mouse:min-h-6");
    expect(strip).not.toMatch(/sm:min-h|sm:h-/);
  });

  it("opens what it can read and refuses to fake the rest", () => {
    const html = sheet([picture, document, recorded]);
    expect(html).toContain(
      'aria-label="Open revenue.png, PNG, 2.0KB, produced in turn 6 by python_execution"',
    );
    expect(html).toContain('aria-label="Open q3-report.pdf, PDF, 1.0KB');
    // A recorded file has no reader in the app, so it is not a control at all.
    expect(html).not.toContain("build.log, LOG");
    expect(text(html)).toContain("build.log");
    expect(html.match(/<button/g)).toHaveLength(
      // close + four filters + two openable tiles
      7,
    );
  });

  it("carries the provenance that makes an artifact citable", () => {
    const visible = text(sheet([picture]));
    expect(visible).toContain("revenue.png");
    expect(visible).toContain("PNG · 2.0KB · turn 6");
  });

  it("says so when a filter has nothing behind it", () => {
    expect(text(sheet([]))).toContain("Nothing of that kind in this session.");
  });
});

describe("the artifacts sheet, paged", () => {
  const many = (count: number, size: number) =>
    Array.from({ length: count }, (_, at) =>
      artifact({
        key: `i${at}:0`,
        name: `shot-${at}.png`,
        iterationId: `i${at}`,
        size,
        sizeLabel: "64.0KB",
      }),
    );
  const tiles = (html: string) => html.match(/aria-label="Open /g)?.length ?? 0;

  it("paints one page of thumbnails, not two hundred downloads", () => {
    // Every tile that mounts fetches its own bytes, so opening this sheet on a
    // long session used to fire a request per artifact in a single tick.
    const html = sheet(many(30, 64 * 1024));
    expect(tiles(html)).toBe(12);
    expect(text(html)).toContain("Load 18 more");
    expect(html).toContain('aria-label="Load 18 more · 1.1MB of artifacts"');
  });

  it("pages on WEIGHT too, when a handful of artifacts is already heavy", () => {
    const html = sheet(many(6, 8 * 1024 * 1024));
    expect(tiles(html)).toBe(2);
    expect(text(html)).toContain("4 more");
  });

  it("says nothing about more when the whole production fits", () => {
    const html = sheet(many(4, 64 * 1024));
    expect(tiles(html)).toBe(4);
    expect(text(html)).not.toContain("more");
  });

  // Regression: the reveal stood 36px tall, taller than the app's own button.
  it("keeps the reveal control tappable, at the app's button height", () => {
    const html = sheet(many(30, 64 * 1024));
    const reveal =
      /<button[^>]*aria-label="Load [^"]*"[^>]*class="([^"]*)"/.exec(
        html,
      )?.[1] ??
      /<button[^>]*class="(mt-3[^"]*)"/.exec(html)?.[1] ??
      "";
    expect(reveal.split(" ")).toContain("min-h-8");
    expect(reveal.split(" ")).toContain("mouse:min-h-7");
    // A width query may never shrink a hit box; only a pointer may.
    expect(reveal).not.toMatch(/sm:min-h|sm:h-/);
  });
});

describe("an artifact with a history", () => {
  const cut = (version: number) =>
    artifact({
      key: `i${version}:0`,
      name: "chart.png",
      iterationId: `i${version}`,
      turn: version,
      version,
    });
  const threaded = { ...cut(3), versions: [cut(3), cut(2), cut(1)] };

  it("is ONE tile — the latest cut — not one tile per rewrite", () => {
    const html = sheet([threaded]);
    expect(html.match(/aria-label="Open /g)).toHaveLength(1);
    expect(html).toContain("produced in turn 3");
    expect(text(html)).toContain("chart.png");
  });

  it("wears its version and offers the thread behind it", () => {
    const html = sheet([threaded]);
    expect(text(html)).toContain("v3 · PNG");
    expect(html).toContain('aria-label="Show 3 versions of chart.png"');
  });

  it("offers no history to an artifact that has none", () => {
    const html = sheet([{ ...cut(1), versions: [cut(1)] }]);
    expect(html).not.toContain("versions of");
    // A single cut is not labelled `v1`: the number only means something
    // once there is a second one.
    expect(text(html)).not.toContain("v1");
  });

  // The dot cannot live INSIDE the tile: a button inside a button is invalid
  // HTML and the browser hands the inner clicks to the outer control.
  it("keeps the history control a sibling of the tile it belongs to", () => {
    const html = sheet([threaded]);
    expect(html).not.toMatch(/<button(?:(?!<\/button>)[\s\S])*<button/);
  });

  // Regression, user report ("not visible and goes outside of card!"): the dot wore the
  // box a `⋯` that ENDS A ROW wears, and that box reclaims the row's trailing gutter
  // with a negative margin — placed `right-1` on a tile, it hung outside the card.
  it("keeps the history control inside the card it belongs to", () => {
    const dots = classesOf(sheet([threaded]), "versions of chart.png");
    expect(dots).toContain("bg-dialog-title");
    expect(dots).not.toContain("-mr-3");
    expect(dots).not.toContain("sm:-mr-4");
    expect(dots).not.toContain("justify-items-end");
  });
});

// Regression: an opened attachment was read in a letterbox. Every kind that owns
// its own scrolling — a clip, a note, a text file, a document — is handed the
// overlay's whole body (`fill`); only a list of rows keeps the padded scroller.
describe("an opened artifact", () => {
  const detail =
    /function ArtifactDetail\([\s\S]*?\n}\n/.exec(artifactsSheetSource)?.[0] ??
    "";

  it("is given the whole height of the overlay", () => {
    const overlays = detail.match(/<DetailOverlay[^>]*>/g) ?? [];
    // The first overlay is the loading/failed line, which is prose in a scroller.
    expect(overlays.length).toBeGreaterThan(1);
    expect(overlays.slice(1).every((tag) => tag.includes("fill"))).toBe(true);
  });

  it("hands the filled body to the frame rather than padding around it", () => {
    const overlay =
      /function DetailOverlay\([\s\S]*?\n}\n/.exec(artifactsSheetSource)?.[0] ??
      "";
    expect(overlay).toContain("flex min-h-0 min-w-0 flex-1 flex-col");
  });
});
