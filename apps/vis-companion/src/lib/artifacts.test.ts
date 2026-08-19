import { describe, expect, it } from "vitest";
import {
  ARTIFACT_FILTERS,
  artifactKind,
  artifactMedia,
  artifactTotalLabel,
  attachmentBytes,
  attachmentIsAudio,
  attachmentIsDoc,
  attachmentIsImage,
  attachmentIsLive,
  attachmentIsPlayable,
  attachmentIsVideo,
  collapseArtifactVersions,
  collapseAttachmentVersions,
  collectArtifacts,
  artifactsFromIndex,
  mergeArtifacts,
  withSavedAttachment,
  docKindLabel,
  isDocMedia,
  isMarkdownMedia,
  isTextMedia,
  pageBySize,
  LIVE_ARTIFACT_MEDIA,
  RAIL_PAGE,
  SHEET_PAGE,
  isPdfMedia,
} from "./artifacts";
import type { SessionArtifact } from "./artifacts";
import type { IterationAttachment, TranscriptTurn } from "./types";

describe("document media types", () => {
  it("recognises the three types the engine keeps off the wire", () => {
    expect(isDocMedia("application/pdf")).toBe(true);
    expect(isDocMedia("text/html")).toBe(true);
    expect(isDocMedia("application/xhtml+xml")).toBe(true);
    expect(isDocMedia("TEXT/HTML; charset=utf-8")).toBe(true);
    expect(isDocMedia("image/png")).toBe(false);
    expect(isDocMedia("text/csv")).toBe(false);
    expect(isDocMedia(undefined)).toBe(false);
  });

  it("labels the kind the way the fence summary does", () => {
    expect(docKindLabel("application/pdf")).toBe("PDF");
    expect(docKindLabel("text/html")).toBe("HTML");
    expect(isPdfMedia("application/pdf")).toBe(true);
    expect(isPdfMedia("text/html")).toBe(false);
  });
});

// Regression: a `.md` artifact was classified as an unreadable `file`, so the
// artifacts sheet drew it as a `≡` plate on a <div> and tapping the only thing
// the session had produced did nothing.
describe("written artifacts", () => {
  const written = (over: Partial<IterationAttachment>): IterationAttachment =>
    ({ kind: "file", ...over }) as IterationAttachment;

  it("reads markdown as a document, by media type or by name", () => {
    expect(isMarkdownMedia("text/markdown")).toBe(true);
    expect(isMarkdownMedia("text/x-markdown; charset=utf-8")).toBe(true);
    // The gateway's guess for a written note is routinely a generic type: the
    // name the human gave it is the better evidence.
    expect(isMarkdownMedia("application/octet-stream", "notes.md")).toBe(true);
    expect(isMarkdownMedia("text/plain", "README.markdown")).toBe(true);
    expect(isMarkdownMedia("text/plain", "build.log")).toBe(false);
    expect(isMarkdownMedia("image/png", "chart.md")).toBe(false);
  });

  it("reads plain text too, and nothing it cannot render", () => {
    expect(isTextMedia("text/plain")).toBe(true);
    expect(isTextMedia("application/octet-stream", "run.log")).toBe(true);
    expect(isTextMedia("application/pdf")).toBe(false);
    expect(isTextMedia("image/png")).toBe(false);
    expect(isTextMedia(undefined)).toBe(false);
  });

  it("makes a written note openable instead of a dead tile", () => {
    const note = written({
      media_type: "text/markdown",
      filename: "vis-issue-115-comment.md",
    });
    expect(attachmentIsDoc(note)).toBe(true);
    expect(artifactKind(note)).toBe("doc");
    expect(artifactKind(written({ filename: "notes.md" }))).toBe("doc");
    expect(artifactKind(written({ filename: "model.bin" }))).toBe("file");
  });

  it("says MD and TXT instead of calling every note a DOC", () => {
    expect(docKindLabel("text/markdown")).toBe("MD");
    expect(docKindLabel("application/octet-stream", "notes.md")).toBe("MD");
    expect(docKindLabel("text/plain", "build.log")).toBe("TXT");
    expect(docKindLabel("application/pdf")).toBe("PDF");
    expect(docKindLabel("text/html")).toBe("HTML");
  });
});

describe("attachment classification", () => {
  it("reads the media type first and the kind only as a fallback", () => {
    expect(attachmentIsImage({ index: 0, media_type: "image/png" })).toBe(true);
    expect(attachmentIsImage({ index: 0, kind: "image" })).toBe(true);
    expect(attachmentIsImage({ index: 0, media_type: "text/csv" })).toBe(false);
    expect(attachmentIsVideo({ index: 0, media_type: "video/mp4" })).toBe(true);
    expect(attachmentIsAudio({ index: 0, media_type: "audio/mp4" })).toBe(true);
    expect(attachmentIsAudio({ index: 0, media_type: "video/mp4" })).toBe(false);
    expect(attachmentIsDoc({ index: 0, media_type: "application/pdf" })).toBe(
      true,
    );
    expect(attachmentIsDoc({ index: 0, kind: "doc" })).toBe(true);
  });

  it("puts a still, a clip and a recording on the same rail and nothing else", () => {
    expect(attachmentIsPlayable({ index: 0, media_type: "image/png" })).toBe(
      true,
    );
    expect(attachmentIsPlayable({ index: 0, media_type: "video/mp4" })).toBe(
      true,
    );
    expect(attachmentIsPlayable({ index: 0, media_type: "audio/mpeg" })).toBe(
      true,
    );
    expect(
      attachmentIsPlayable({ index: 0, media_type: "application/pdf" }),
    ).toBe(false);
  });

  const run = {
    index: 0,
    kind: "file",
    media_type: LIVE_ARTIFACT_MEDIA,
    filename: "fleet-scan.live.json",
  };

  // A settled live view is filed as a FILE carrying the engine's own media type
  // (`hi-spec/live-artifact-media-type`), so the media type — never the `.json`
  // the record happens to be written in — is what sorts it.
  it("sorts a settled run by the media type the engine sealed it with", () => {
    expect(attachmentIsLive(run)).toBe(true);
    expect(attachmentIsLive({ ...run, media_type: `${LIVE_ARTIFACT_MEDIA}; charset=utf-8` })).toBe(true);
    expect(attachmentIsLive({ index: 0, filename: "notes.live.json" })).toBe(false);
    expect(artifactKind(run)).toBe("live");
    expect(artifactMedia(run)).toBe("RUN");
    expect(attachmentIsDoc(run)).toBe(false);
    expect(attachmentIsPlayable(run)).toBe(false);
  });

  it("sorts every attachment into exactly one kind", () => {
    expect(artifactKind({ index: 0, media_type: "image/png" })).toBe("image");
    expect(artifactKind({ index: 0, media_type: "video/mp4" })).toBe("video");
    expect(artifactKind({ index: 0, media_type: "audio/mp4" })).toBe("audio");
    expect(artifactKind({ index: 0, media_type: "audio/mpeg" })).toBe("audio");
    expect(artifactKind({ index: 0, media_type: "text/html" })).toBe("doc");
    expect(artifactKind({ index: 0, media_type: "text/csv" })).toBe("file");
    expect(artifactKind({ index: 0 })).toBe("file");
  });

  it("names the format from the filename, then the media type", () => {
    expect(artifactMedia({ index: 0, filename: "chart.png" })).toBe("PNG");
    expect(
      artifactMedia({
        index: 0,
        filename: "coverage.json",
        media_type: "text/plain",
      }),
    ).toBe("JSON");
    expect(
      artifactMedia({
        index: 0,
        filename: "report",
        media_type: "application/pdf",
      }),
    ).toBe("PDF");
    expect(
      artifactMedia({ index: 0, media_type: "application/xhtml+xml" }),
    ).toBe("XML");
    expect(artifactMedia({ index: 0 })).toBe("FILE");
  });

  it("renders a size only when the gateway declared one", () => {
    expect(attachmentBytes(512)).toBe("512B");
    expect(attachmentBytes(2048)).toBe("2.0KB");
    expect(attachmentBytes(3 * 1024 * 1024)).toBe("3.0MB");
    expect(attachmentBytes(undefined)).toBe("");
    expect(attachmentBytes(-1)).toBe("");
  });
});

const turns: TranscriptTurn[] = [
  {
    id: "t1",
    iterations: [
      {
        id: "i1",
        attachments: [
          {
            index: 0,
            iteration_id: "i1",
            filename: "revenue.png",
            media_type: "image/png",
            size: 2048,
          },
          {
            index: 1,
            iteration_id: "i1",
            filename: "notes.csv",
            media_type: "text/csv",
            size: 1024,
          },
        ],
      },
    ],
  },
  {
    id: "t2",
    iterations: [
      { id: "i2" },
      {
        id: "i3",
        attachments: [
          {
            index: 0,
            iteration_id: "i3",
            filename: "report.pdf",
            media_type: "application/pdf",
          },
        ],
      },
    ],
  },
];

describe("collecting what a session produced", () => {
  it("flattens turn → iteration → attachment, newest first", () => {
    const list = collectArtifacts(turns);
    expect(list.map((entry) => entry.name)).toEqual([
      "report.pdf",
      "notes.csv",
      "revenue.png",
    ]);
    expect(list.map((entry) => entry.kind)).toEqual(["doc", "file", "image"]);
  });

  it("counts the turn from the start of the session, not of the window", () => {
    expect(collectArtifacts(turns).map((entry) => entry.turn)).toEqual([
      2, 1, 1,
    ]);
    // 40 turns live before the page we hold: the newest one is turn 42.
    expect(collectArtifacts(turns, 40).map((entry) => entry.turn)).toEqual([
      42, 41, 41,
    ]);
  });

  it("carries the provenance a tile has to announce", () => {
    const [doc, , image] = collectArtifacts(turns);
    expect(doc.iterationId).toBe("i3");
    expect(doc.sizeLabel).toBe("");
    expect(image.key).toBe("i1:0");
    expect(image.iterationId).toBe("i1");
    expect(image.sizeLabel).toBe("2.0KB");
  });

  it("adds up only the sizes it was told, and says nothing otherwise", () => {
    expect(artifactTotalLabel(collectArtifacts(turns))).toBe("3.0KB");
    expect(artifactTotalLabel([])).toBe("");
    expect(artifactTotalLabel(collectArtifacts([turns[1]]))).toBe("");
  });

  it("has a filter for every kind an artifact can be", () => {
    const covered = new Set(
      ARTIFACT_FILTERS.filter((filter) => filter.label !== "All").flatMap(
        (filter) => filter.kinds,
      ),
    );
    expect([...covered].sort()).toEqual([
      "audio",
      "doc",
      "file",
      "image",
      "live",
      "video",
    ]);
    expect(ARTIFACT_FILTERS[0].kinds).toEqual([
      "image",
      "video",
      "audio",
      "live",
      "doc",
      "file",
    ]);
  });
});

describe("collapsing an artifact into its versions", () => {
  const cut = (name: string, version: number): SessionArtifact => ({
    key: `${name}:${version}`,
    kind: "image",
    name,
    media: "PNG",
    mediaType: "image/png",
    size: version * 1024,
    sizeLabel: `${version}.0KB`,
    turn: version,
    iterationId: `i${version}`,
    index: 0,
    version,
  });

  it("reads the version the engine stamped, defaulting to the first cut", () => {
    const list = collectArtifacts([
      {
        iterations: [
          {
            id: "i1",
            attachments: [
              { index: 0, filename: "chart.png", media_type: "image/png" },
              {
                index: 1,
                filename: "chart.png",
                media_type: "image/png",
                version: 4,
              },
            ],
          },
        ],
      } as unknown as TranscriptTurn,
    ]);
    expect(list.map((entry) => entry.version)).toEqual([4, 1]);
  });

  it("keeps ONE row per name, the newest cut, with the thread behind it", () => {
    const collapsed = collapseArtifactVersions([
      cut("chart.png", 2),
      cut("notes.md", 1),
      cut("chart.png", 3),
      cut("chart.png", 1),
    ]);
    expect(collapsed.map((entry) => entry.name)).toEqual([
      "chart.png",
      "notes.md",
    ]);
    const [chart] = collapsed;
    // The primary view is the LATEST cut: its size, its turn, its bytes.
    expect(chart.version).toBe(3);
    expect(chart.turn).toBe(3);
    expect(chart.key).toBe("chart.png:3");
    // Newest first, the head included, so the dropdown is the whole thread.
    expect(chart.versions?.map((entry) => entry.version)).toEqual([3, 2, 1]);
  });

  it("gives a lone artifact a one-element thread, so nothing branches", () => {
    const [only] = collapseArtifactVersions([cut("solo.png", 1)]);
    expect(only.versions).toHaveLength(1);
    expect(only.versions?.[0].name).toBe("solo.png");
  });

  it("never merges two different names into one artifact", () => {
    const collapsed = collapseArtifactVersions([
      cut("a.png", 1),
      cut("b.png", 1),
    ]);
    expect(collapsed).toHaveLength(2);
    expect(collapsed.every((entry) => entry.versions?.length === 1)).toBe(true);
  });
});

// Regression, user report: "I commented on the model's document, saved, and now
// instead of one there are two." The save files the SAME filename as the next
// version of the same artifact, so the rail's descriptors thread by name exactly
// as the gallery's rows do.
describe("collapseAttachmentVersions", () => {
  const cut = (
    filename: string,
    version: number,
    index: number,
    size = 100,
  ): IterationAttachment => ({
    filename,
    version,
    index,
    size,
    iteration_id: "i1",
  });

  it("keeps ONE thread per name, newest cut first", () => {
    const threads = collapseAttachmentVersions([
      cut("README.md", 1, 0, 13_000),
      cut("chart.png", 1, 1),
      cut("README.md", 2, 2, 12_700),
    ]);
    expect(threads).toHaveLength(2);
    const [note, chart] = threads;
    // The head is the row the transcript paints: the newest cut, its own weight.
    expect(note.map((entry) => entry.version)).toEqual([2, 1]);
    expect(note[0].size).toBe(12_700);
    expect(chart).toHaveLength(1);
  });

  it("gives a file written once a one-element thread, so nothing branches", () => {
    const [only] = collapseAttachmentVersions([cut("solo.pdf", 1, 0)]);
    expect(only).toHaveLength(1);
    expect(only[0].filename).toBe("solo.pdf");
  });

  it("threads a descriptor that carries no version at all", () => {
    const threads = collapseAttachmentVersions([
      { filename: "notes.md", index: 0 },
      cut("notes.md", 2, 1),
    ]);
    expect(threads).toHaveLength(1);
    expect(threads[0].map((entry) => entry.version ?? 1)).toEqual([2, 1]);
  });

  it("never merges two different names into one thread", () => {
    const threads = collapseAttachmentVersions([
      cut("a.png", 1, 0),
      cut("b.png", 1, 1),
    ]);
    expect(threads.map((thread) => thread.length)).toEqual([1, 1]);
  });
});

describe("pageBySize", () => {
  const MB = 1024 * 1024;
  const page = (sizes: (number | undefined)[], pages = 1, limits = RAIL_PAGE) =>
    pageBySize(
      sizes.map((size, at) => ({ name: `a${at}.png`, size })),
      (entry) => entry.size,
      pages,
      limits,
    );

  it("hides nothing when a gallery already fits", () => {
    const shown = page([1024, 1024, 1024]);
    expect(shown.shown).toHaveLength(3);
    expect(shown.rest).toEqual([]);
    expect(shown.restLabel).toBe("");
  });

  it("stops at the COUNT bound and says what is left, with its weight", () => {
    const shown = page(Array.from({ length: 20 }, () => 1024));
    expect(shown.shown).toHaveLength(RAIL_PAGE.items);
    expect(shown.rest).toHaveLength(14);
    expect(shown.restBytes).toBe(14 * 1024);
    expect(shown.restLabel).toBe("14 more · 14.0KB");
  });

  it("stops at the BYTE bound long before the count one", () => {
    // Six 3 MB screenshots are not six thumbnails: two fit an 8 MB page.
    const shown = page([3 * MB, 3 * MB, 3 * MB, 3 * MB]);
    expect(shown.shown).toHaveLength(2);
    expect(shown.rest).toHaveLength(2);
    expect(shown.restBytes).toBe(6 * MB);
  });

  it("always shows the first artifact, however heavy it is", () => {
    // A budget that can hide the ONLY picture there is would be a broken
    // screen, not a thrifty one.
    const shown = page([64 * MB, 1024]);
    expect(shown.shown).toHaveLength(1);
    expect(shown.rest).toHaveLength(1);
  });

  it("falls back to the count bound when no size is known", () => {
    const shown = page(Array.from({ length: 9 }, () => undefined));
    expect(shown.shown).toHaveLength(RAIL_PAGE.items);
    expect(shown.restBytes).toBe(0);
    // Nothing weighed: claiming "3 more · 0B" would be a lie.
    expect(shown.restLabel).toBe("3 more");
  });

  it("buys exactly one more page of BOTH bounds per reveal", () => {
    const sizes = Array.from({ length: 20 }, () => 1024);
    expect(page(sizes, 2).shown).toHaveLength(2 * RAIL_PAGE.items);
    expect(page([3 * MB, 3 * MB, 3 * MB, 3 * MB], 2).shown).toHaveLength(4);
  });

  it("gives the thumbnail grid a bigger page than the transcript rail", () => {
    expect(SHEET_PAGE.items).toBeGreaterThan(RAIL_PAGE.items);
    expect(SHEET_PAGE.bytes).toBeGreaterThan(RAIL_PAGE.bytes);
  });
});

// Regression, user report: saving a revision of an artifact answered "the version
// was updated", but the artifacts sheet went on listing the old cut and reopening
// the note showed none of the comments that had just been saved. A revision is
// appended to an ITERATION THAT ALREADY EXISTS, so it moves neither the turn count
// nor the transcript stamp: every refresh the app makes is a no-op and the held
// transcript — the only thing the gallery is derived from — never learns about it.
describe("folding a saved revision back into the transcript", () => {
  const turns = (): TranscriptTurn[] =>
    [
      {
        id: "t1",
        iterations: [
          {
            id: "i1",
            attachments: [
              { index: 0, filename: "notes.md", media_type: "text/markdown" },
            ],
          },
        ],
      },
      { id: "t2", iterations: [{ id: "i2", attachments: [] }] },
    ] as unknown as TranscriptTurn[];

  const saved: IterationAttachment = {
    index: 1,
    iteration_id: "i1",
    filename: "notes.md",
    media_type: "text/markdown",
    version: 2,
    size: 64,
  };

  it("appends the cut to the iteration that owns it", () => {
    const next = withSavedAttachment(turns(), saved);
    expect(next[0].iterations?.[0].attachments).toEqual([
      { index: 0, filename: "notes.md", media_type: "text/markdown" },
      saved,
    ]);
  });

  it("leaves every other turn identical, so only that row repaints", () => {
    const held = turns();
    const next = withSavedAttachment(held, saved);
    expect(next).not.toBe(held);
    expect(next[1]).toBe(held[1]);
  });

  it("makes the gallery show the new version with its thread behind it", () => {
    const [note] = collapseArtifactVersions(
      collectArtifacts(withSavedAttachment(turns(), saved)),
    );
    expect(note.version).toBe(2);
    expect(note.index).toBe(1);
    expect(note.versions?.map((cut) => cut.version)).toEqual([2, 1]);
  });

  it("re-reads a cut that lands on an index it already holds", () => {
    const next = withSavedAttachment(turns(), { ...saved, index: 0 });
    expect(next[0].iterations?.[0].attachments).toEqual([{ ...saved, index: 0 }]);
  });

  it("changes nothing when the iteration is not in the window we hold", () => {
    const held = turns();
    expect(withSavedAttachment(held, { ...saved, iteration_id: "i9" })).toBe(held);
    expect(withSavedAttachment(held, { ...saved, iteration_id: undefined })).toBe(held);
  });
});

// Regression: the artifacts sheet was built only from the transcript rows the
// screen held. The transcript is fetched newest-page-first, so a long session
// opened on an almost empty gallery and artifacts appeared one page at a time
// as the reader scrolled upward.
describe("the session-wide artifact index", () => {
  const rows = [
    {
      index: 0,
      iteration_id: "i1",
      filename: "revenue.png",
      media_type: "image/png",
      size: 2048,
      turn: 3,
    },
    {
      index: 0,
      iteration_id: "i9",
      filename: "report.pdf",
      media_type: "application/pdf",
      turn: 3,
    },
  ];

  it("reads the whole session, newest first, with the turn that made it", () => {
    const list = artifactsFromIndex(rows);
    expect(list.map((entry) => entry.name)).toEqual([
      "report.pdf",
      "revenue.png",
    ]);
    expect(list.map((entry) => entry.turn)).toEqual([3, 3]);
    expect(list[1].key).toBe("i1:0");
    expect(list[1].sizeLabel).toBe("2.0KB");
    expect(list[0].kind).toBe("doc");
  });

  it("lists an artifact from a turn the reader never scrolled back to", () => {
    const held = collectArtifacts(turns, 40);
    const merged = mergeArtifacts(held, artifactsFromIndex(rows));
    expect(merged.map((entry) => entry.name)).toEqual([
      "report.pdf",
      "notes.csv",
      "revenue.png",
      "report.pdf",
    ]);
    // The held rows keep the iteration they hang off; the turn nobody scrolled
    // back to is the one the index adds.
    expect(merged.map((entry) => entry.iterationId)).toEqual([
      "i3",
      "i1",
      "i1",
      "i9",
    ]);
    expect(merged.map((entry) => entry.turn)).toEqual([42, 41, 41, 3]);
  });
});
