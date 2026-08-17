// @vitest-environment jsdom
import { render, waitFor } from "@testing-library/react";
import { renderToStaticMarkup } from "react-dom/server";
import { describe, expect, it } from "vitest";
import {
  AssistantMessage,
  AttachmentRail,
  Markdown,
  ThinkingBand,
  UserMessage,
} from "./ChatContent";
import type { GatewayClient } from "../lib/gateway";
import {
  mediaFrameClass,
  mediaGridClass,
  mediaTileFrameClass,
} from "../lib/media-frame";
import type { IterationAttachment, TranscriptTurn } from "../lib/types";

/** Visible text of a rendered chunk: tags out, entities back. */
const text = (html: string) =>
  html
    .replace(/<[^>]+>/g, "")
    .replace(/&quot;/g, '"')
    .replace(/&#x27;/g, "'")
    .replace(/&gt;/g, ">")
    .replace(/&lt;/g, "<")
    .replace(/&amp;/g, "&");

/** One entry per PAINTED code row — the code block gives every line its own div. */
const codeRows = (html: string) =>
  (html.match(/<div class="flex w-fit[^"]*">.*?<\/div>/g) ?? []).map(text);

const count = (html: string, pattern: RegExp) =>
  (html.match(pattern) ?? []).length;

describe("Markdown thinking breaks", () => {
  // The engine's `reasoning->ast` turns a single authored newline into `[:br]`, and the
  // TUI paints it as its own row. `hardBreaks` is how the web card honours that contract.
  it("keeps every authored newline as its own line", () => {
    const html = renderToStaticMarkup(
      <Markdown compact hardBreaks>
        {"**Plan**\nfirst line\nsecond line\n\nnext para"}
      </Markdown>,
    );
    expect(count(html, /<br\s*\/?>/g)).toBe(2);
    // Still real paragraphs — a blank line is a break BETWEEN blocks, not a third `<br>`.
    expect(count(html, /<p class=/g)).toBe(2);
    expect(text(html)).toContain("Plan\nfirst line\nsecond line");
    expect(text(html)).not.toContain("Planfirst");
  });

  it("flows soft newlines when hard breaks are off", () => {
    const html = renderToStaticMarkup(
      <Markdown compact>{"first line\nsecond line"}</Markdown>,
    );
    expect(html).not.toContain("<br");
    expect(count(html, /<p class=/g)).toBe(1);
  });
});

describe("Markdown tool card body", () => {
  it("keeps blank lines and indentation inside a COMMAND block", () => {
    const html = renderToStaticMarkup(
      <Markdown compact>
        {
          "**COMMAND**\n\n```bash\nset -e\n\nif [ -f x ]; then\n  npm test\nfi\n```\n"
        }
      </Markdown>,
    );
    expect(codeRows(html)).toEqual([
      "set -e",
      " ",
      "if [ -f x ]; then",
      "  npm test",
      "fi",
    ]);
  });

  it("keeps a blank line between two phases of STDOUT", () => {
    const html = renderToStaticMarkup(
      <Markdown compact>
        {"**STDOUT**\n\n```\nphase one ok\n\nphase two ok\n```\n"}
      </Markdown>,
    );
    expect(codeRows(html)).toEqual(["phase one ok", " ", "phase two ok"]);
  });

  it("splits a quoted commit MESSAGE into subject and body", () => {
    const html = renderToStaticMarkup(
      <Markdown compact>
        {"**MESSAGE**\n\n> feat: thing\n>\n> body line\n"}
      </Markdown>,
    );
    const quote = html.slice(
      html.indexOf("<blockquote"),
      html.indexOf("</blockquote>"),
    );
    expect(quote).not.toBe("");
    expect(count(quote, /<p class=/g)).toBe(2);
    expect(text(quote).replace(/\n+/g, "\n").trim()).toBe(
      "feat: thing\nbody line",
    );
  });
});

// Regression, companion transcript report: PATCH and STRUCT_PATCH diffs used a two-column
// desktop layout, leaving each side unreadably narrow in the web and native apps.
describe("compact diff blocks", () => {
  it("renders one unified column with explicit removed and added lines", () => {
    const html = renderToStaticMarkup(
      <Markdown compact>
        {
          "```diff\n--- a/file.ts\n+++ b/file.ts\n@@ -1,2 +1,2 @@\n keep\n-old\n+new\n```"
        }
      </Markdown>,
    );

    expect(html).toContain('aria-label="Unified diff"');
    expect(html).not.toContain("grid-cols-2");
    expect(html).toContain('aria-label="Removed line 2"');
    expect(html).toContain('aria-label="Added line 2"');
  });
});

// Regression, iOS scroll jump: a pasted picture used to be laid out at whatever
// its own decoded pixels measured (`max-h-[min(28rem,60dvh)] w-auto`), so the
// bubble reserved NOTHING for it until the decode landed — which, with
// `loading="lazy"` on iOS, happens as the bubble nears the viewport, i.e. while
// the reader is scrolling. Everything below it then jumped down by the height
// of the picture, and this scroller (`overflow-anchor:none`, no WebKit
// anchoring, corrector standing down mid-gesture) never put it back.
describe("user bubble pictures", () => {
  const html = () =>
    renderToStaticMarkup(
      <UserMessage
        attachments={[
          {
            filename: "shot.png",
            media_type: "image/png",
            base64: "iVBORw0KGgo=",
            size: 8,
          },
        ]}
      >
        {"look at this"}
      </UserMessage>,
    );

  it("reserves the picture box before a single byte has decoded", () => {
    expect(html()).toContain(mediaFrameClass);
  });

  it("never lets the picture size its own slot", () => {
    expect(html()).not.toMatch(/<img[^>]*\bw-auto\b/u);
    expect(html()).not.toMatch(/<img[^>]*\bh-auto\b/u);
  });

  // Regression: the picture the HUMAN sent wore no frame, while the model's own
  // artifact two rows below it did. `ExpandableImage`'s trigger spells `border-0
  // bg-transparent` on itself, and this rail handed that very same element
  // `border border-code-edge bg-code` through `frameClassName` — two competing
  // utilities on one element are settled by Tailwind's emission order and never
  // by which one the call site typed. The frame is a WRAPPER, and the plate owns
  // it (`MediaPlate`), so both rails wear the same face.
  it("never spells the frame on the element that disowns its own border", () => {
    expect(html()).not.toMatch(
      /class="[^"]*\bborder-0\b[^"]*\bborder-code-edge\b/u,
    );
  });

  it("captions a lone picture with its name and format", () => {
    expect(text(html())).toContain("shot.png");
    expect(text(html())).toContain("PNG");
  });
});

// ONE picture is a plate; several are a GALLERY. A transcript where somebody
// dropped four screenshots used to be four 60svh plates stacked down the
// column — a wall to scroll past rather than something to look at.
describe("user bubble galleries", () => {
  const gallery = (count: number) =>
    renderToStaticMarkup(
      <UserMessage
        attachments={Array.from({ length: count }, (_, i) => ({
          filename: `shot-${i}.png`,
          media_type: "image/png",
          base64: "iVBORw0KGgo=",
          size: 8,
        }))}
      >
        {"look at these"}
      </UserMessage>,
    );

  it("keeps a lone picture on its plate", () => {
    expect(gallery(1)).toContain(mediaFrameClass);
    expect(gallery(1)).not.toContain(mediaGridClass);
  });

  it("lays several pictures out as a grid of tiles", () => {
    const html = gallery(3);

    expect(html).toContain(mediaGridClass);
    expect(html).toContain(mediaTileFrameClass);
    expect(html).not.toContain(mediaFrameClass);
  });

  it("reports what the gallery holds instead of captioning every tile", () => {
    expect(text(gallery(3))).toContain("3 images");
    expect(text(gallery(3))).not.toContain("shot-0.png");
  });
});

// Regression, TestFlight crash feedback: build 2875 rendered every collapsed tool result body,
// so a large transcript left WebKit with hundreds of thousands of DOM nodes until iOS killed it
// at the 2 GiB per-process limit.
describe("collapsed tool results", () => {
  it("does not mount result bodies before a card is opened", () => {
    const bodySentinel = "UNMOUNTED_TOOL_RESULT_BODY";
    const turn: TranscriptTurn = {
      id: "large-trace",
      status: "completed",
      iterations: [
        {
          id: "iteration-1",
          forms: Array.from({ length: 400 }, (_, index) => ({
            op: "shell",
            result: "ok",
            result_summary: `summary ${index}`,
            result_render: `**STDOUT**\n\n\`\`\`\n${bodySentinel} ${index}\n\`\`\``,
          })),
        },
      ],
    };

    const html = renderToStaticMarkup(<AssistantMessage turn={turn} />);

    expect(count(html, /<details/g)).toBe(400);
    expect(html).toContain("summary 0");
    expect(html).toContain("summary 399");
    expect(html).not.toContain(bodySentinel);
  });
});

// Regression, reported white artefacts while scrolling a long session upward:
// measured in headless WebKit on this very transcript (77 000 px, 401 code
// blocks), a fling up left as much as 376 px of the 900 px viewport painting as
// bare paper, card frames already there and their bodies still empty. That was
// `contain:layout style` on every turn — a paint island WebKit rasterizes only
// when it enters the viewport, which a fast scroll always outruns.
//
// A finished turn IS skipped again (`useMeasuredPaintSkip`), and this is the
// difference that made it safe: the skip is armed by a MEASUREMENT and never
// declared in the markup, so no turn can be skipped at a height nobody measured
// — the guessed size is what used to correct itself above the reader.
// `ChatContent.paintSkip.test.tsx` pins the armed half.
describe("a turn declares no size it has not measured", () => {
  const paintIsland = /\bcontain:|\bcontent-visibility|contain-intrinsic-size/u;

  it("gives a user bubble no paint-isolation boundary", () => {
    const html = renderToStaticMarkup(
      <UserMessage>{"look at this"}</UserMessage>,
    );

    expect(html).not.toMatch(paintIsland);
  });

  it("leaves an assistant turn's markup free of containment", () => {
    const html = renderToStaticMarkup(
      <AssistantMessage
        turn={{
          id: "paint",
          status: "completed",
          iterations: [
            {
              id: "iteration-1",
              forms: [{ op: "shell", result_summary: "ok" }],
            },
          ],
        }}
      />,
    );

    expect(html).not.toMatch(paintIsland);
  });
});

describe("a card wears no op badge, but its band has a name", () => {
  const card = (form: Record<string, unknown>) =>
    text(
      renderToStaticMarkup(
        <AssistantMessage
          turn={{
            id: "titles",
            status: "completed",
            iterations: [{ id: "iteration-1", forms: [form] }],
          }}
        />,
      ),
    );

  it("shows the tally the printed value carried, never its op", () => {
    const rendered = card({ op: "grep", result_summary: "12 results" });
    expect(rendered).toContain("12 results");
    expect(rendered).not.toContain("GREP");
    // The tally IS the headline; the band's name would only repeat it.
    expect(rendered).not.toContain("RESULT");
  });

  // The block's own output carries no tally, so this row was a chevron and a
  // duration with no word at all for what it held. The TUI names it RESULT
  // (`render/tool-card-entries`); the app said nothing.
  it("names the band when the value carried no tally", () => {
    expect(card({ result_render: "```\nprinted\n```" })).toContain("RESULT");
    expect(card({ duration_ms: 39 })).toContain("RESULT");
  });

  it("names nothing while the op is still running", () => {
    expect(card({})).not.toContain("RESULT");
    expect(card({ result_summary: "Running…" })).not.toContain("RESULT");
  });

  it("never paints a private transport op a handle method answered", () => {
    const rendered = card({ op: "_shell_wait", result_summary: "exit 0" });
    expect(rendered).toContain("exit 0");
    expect(rendered).not.toContain("_SHELL_WAIT");
  });
});

// Regression, reported from the companion: a `/reload` turn painted the command
// itself as a PYTHON program with a RESULT band under it. A slash (`user-slash`)
// and a bang (`user-shell`) are persisted as ONE synthetic form so history and
// resume keep them; the answer beside it already IS the whole result.
describe("a command turn shows its answer, never a program", () => {
  const trace = (form: Record<string, unknown>) =>
    renderToStaticMarkup(
      <AssistantMessage
        turn={{
          id: "command",
          status: "completed",
          user_request: "/reload",
          iterations: [{ id: "iteration-1", forms: [form] }],
          content: [
            { id: "block-1", type: "prose", markdown: "Reloaded — configuration" },
          ],
        }}
      />,
    );

  it("hides the slash envelope a slash turn persists", () => {
    const html = trace({
      scope: "t1/i1/f1",
      tag: "user-slash",
      src: "/reload",
      result: { "slash/status": "ok", "slash/title": "Reloaded" },
    });

    expect(html).not.toContain("PYTHON");
    expect(text(html)).not.toContain("/reload");
    expect(text(html)).not.toContain("slash/status");
    expect(text(html)).toContain("Reloaded — configuration");
  });

  it("hides the shell result a bang turn persists", () => {
    const html = trace({
      scope: "t1/i1/f1",
      tag: "user-shell",
      op: "shell",
      src: "!ls",
      result_summary: "exit 0",
      result_render: "```\nREADME.md\n```",
    });

    expect(html).not.toContain("PYTHON");
    expect(text(html)).not.toContain("exit 0");
    expect(text(html)).toContain("Reloaded — configuration");
  });
});
// Every tile in this rail fetches its own bytes on first paint, so an iteration
// that produced forty artifacts fired forty requests in one tick — on whatever
// connection the phone had. A page at a time now, by count AND by weight.
describe("the attachment rail", () => {
  const client = {
    attachmentUrl: async () => "blob:none",
    retainAttachment: () => () => {},
  } as unknown as GatewayClient;
  const rail = (count: number, size: number) =>
    renderToStaticMarkup(
      <AttachmentRail
        client={client}
        sid="s1"
        attachments={Array.from({ length: count }, (_, at) => ({
          filename: `report-${at}.pdf`,
          media_type: "application/pdf",
          size,
          iteration_id: "i1",
          index: at,
        }))}
      />,
    );

  it("paints one page of artifacts and offers the rest", () => {
    const html = rail(20, 64 * 1024);
    expect(html).toContain("report-5.pdf");
    expect(html).not.toContain("report-6.pdf");
    expect(text(html)).toContain("Load 14 more");
  });

  it("pages on WEIGHT before it ever reaches the count", () => {
    const html = rail(6, 3 * 1024 * 1024);
    expect(html).toContain("report-1.pdf");
    expect(html).not.toContain("report-2.pdf");
    expect(text(html)).toContain("4 more");
  });

  it("leaves a rail that already fits completely alone", () => {
    const html = rail(3, 64 * 1024);
    expect(html).toContain("report-2.pdf");
    expect(text(html)).not.toContain("more");
  });

  // Regression, user report: "the model attached a document, I commented on it and
  // saved, and now instead of one there are two". Saving files the same filename
  // as the NEXT VERSION of the same artifact; this rail painted a row per
  // descriptor, so the revision arrived as a second row under the same name and
  // the stack grew a header summing both cuts.
  const note = (size: number, index: number, version: number) => ({
    filename: "README.md",
    media_type: "text/markdown",
    size,
    iteration_id: "i1",
    index,
    version,
  });
  const revised = (extra: IterationAttachment[] = []) =>
    renderToStaticMarkup(
      <AttachmentRail
        client={client}
        sid="s1"
        attachments={[note(13_000, 0, 1), note(12_700, 1, 2), ...extra]}
      />,
    );

  it("answers a saved revision with the SAME one row", () => {
    const html = revised();
    expect(html.split('aria-label="Open README.md"').length - 1).toBe(1);
    // One artifact is one row and no header at all — nothing to report a group of.
    expect(text(html)).not.toContain("documents");
    expect(text(html)).not.toContain("25.1KB");
  });

  it("makes the row say the version moved, and weighs the newest cut", () => {
    const html = revised();
    expect(text(html)).toContain("v2");
    expect(text(html)).toContain("12.4KB");
    expect(text(html)).not.toContain("12.7KB");
  });

  it("still reports a GROUP, counting artifacts and not cuts", () => {
    const html = revised([
      {
        filename: "NOTES.md",
        media_type: "text/markdown",
        size: 1_000,
        iteration_id: "i1",
        index: 2,
        version: 1,
      },
    ]);
    expect(text(html)).toContain("2 documents");
    expect(text(html)).not.toContain("3 documents");
  });
});

// Regression, live reasoning scroll jump: the trace ramp used to pin its own
// scrollTop while SessionScreen's content observer pinned the same scroller again,
// making streamed thinking visibly jump.
describe("transcript scroll ownership", () => {
  it("leaves scroll correction to the session screen owner", async () => {
    const view = render(
      <div data-testid="scroller" className="overflow-y-auto">
        <ThinkingBand>{"one\ntwo\nthree\nfour\nfive\nsix"}</ThinkingBand>
      </div>,
    );
    const scroller = view.getByTestId("scroller");
    scroller.scrollTop = 120;
    // The band grows the way a live trace grows: same component, more rows.
    view.rerender(
      <div data-testid="scroller" className="overflow-y-auto">
        <ThinkingBand>
          {Array.from({ length: 40 }, (_, row) => `line ${row}`).join("\n")}
        </ThinkingBand>
      </div>,
    );
    await waitFor(() => expect(scroller.textContent).toContain("line 39"));
    expect(scroller.scrollTop).toBe(120);
    view.unmount();
  });
});

// Regression: a document artifact was on screen twice — the `vis-doc` fence
// painted its own card in the tool result while the attachment rail below it
// painted the openable tile for the very same file.
describe("a vis-doc fence", () => {
  const fence = [
    "````vis-doc",
    "[Document: report.pdf PDF, 1.2 MB]",
    "/tmp/vis-python/doc-1/report.pdf",
    "application/pdf",
    "report.pdf",
    "1.2 MB",
    "````",
  ].join("\n");

  it("paints nothing: the attachment tile is the document's one appearance", () => {
    const markup = renderToStaticMarkup(<Markdown compact>{fence}</Markdown>);
    expect(text(markup)).not.toContain("report.pdf");
    expect(text(markup)).not.toContain("/tmp/vis-python/doc-1/report.pdf");
    expect(markup).not.toContain("<iframe");
  });
});

// Reported from a phone: a table's first column was one atom wide — `manifest.edn`
// painted as six stacked fragments and `THIRD_PARTY_MODELS.md` as eight. Inline
// `code` carries `break-all` so the justifier has a stop inside an atom it cannot
// break, and in a table that makes the column's MIN-CONTENT one character: the auto
// layout handed the file column 58px of a 366px bubble.
describe("a markdown table", () => {
  const table = [
    "| Plik | Zmiana |",
    "| --- | --- |",
    "| `manifest.edn` | the size gate is gone, only the licence gates a download |",
    "| `THIRD_PARTY_MODELS.md` | regenerated |",
  ].join("\n");

  it("lets a file column ask for the width of its name, not of one character", () => {
    const markup = renderToStaticMarkup(<Markdown>{table}</Markdown>);
    const cellRules = (
      /<table class="([^"]*)"/.exec(markup)?.[1] ?? ""
    ).replace(/&amp;/g, "&");
    expect(cellRules).toContain("[&_code]:[word-break:normal]");
    expect(cellRules).toContain("[&_a]:[word-break:normal]");
    // The other half of the contract: a table too wide for the bubble reaches for
    // its own scroller instead of shredding a column to fit.
    expect(markup).toContain("overflow-x-auto");
    expect(text(markup)).toContain("manifest.edn");
  });

  it("leaves running prose breaking at every character, where the justifier needs it", () => {
    const markup = renderToStaticMarkup(
      <Markdown>{"A path `src/components/ChatContent.tsx` in a paragraph."}</Markdown>,
    );
    expect(/<p class="[^"]*text-justify/.test(markup)).toBe(true);
    expect(/<code class="[^"]*break-all/.test(markup)).toBe(true);
  });
});
// Reported from a phone: a message was sent, the session title updated, and the
// answer rail under it was a bare "Vis" — no phase, no clock, no trace — for the
// whole turn. A `running` row the screen had stopped following rendered nothing
// at all, so the reader had no word that work had even started.
describe("a turn that has not answered yet", () => {
  const running = {
    id: "t1",
    request: "check the logs",
    status: "running",
    iterations: [],
    content: [],
  } as unknown as TranscriptTurn;

  it("names its phase while the screen is following it", () => {
    const html = renderToStaticMarkup(<AssistantMessage turn={running} />);

    expect(text(html)).toContain("Vis is waiting for an update");
    expect(html).toContain("animate-spinner-frame");
  });

  it("keeps naming its phase once the screen stops following it", () => {
    const html = renderToStaticMarkup(
      <AssistantMessage turn={running} settled />,
    );

    expect(text(html)).toContain("Vis is waiting for an update");
    // What `settled` takes off is the TICKER — the spinner and the elapsed
    // clock that made a finished turn look alive — and nothing else.
    expect(html).not.toContain("animate-spinner-frame");
    expect(text(html)).not.toMatch(/\d/);
  });
});


// Regression, session 0ec1e9f3-23d5-4070-a17e-46f8e7f514e8: the moment a long
// streamed answer became the finished one, the transcript flickered — it jumped
// up, went blank, and dropped back to the bottom a few frames later. The live
// bubble's trace was unmounted and the persisted row mounted a BRAND NEW trace,
// which started the opening ramp over: only the last `SEGMENT_FIRST_PAINT`
// segments painted, and the rest came back a chunk per frame. Measured on the
// handover of a short turn, the scroller lost 102 nodes and 378 px and got them
// back 8 ms later; on a long answer that is most of the transcript.
describe("the trace a settled row inherits from the live bubble", () => {
  const iterations = Array.from({ length: 20 }, (_, index) => ({
    id: `i${index}`,
    assistant_prose: `step ${index}`,
  })) as unknown as TranscriptTurn["iterations"];
  const finished = {
    id: "t1",
    request: "do the long thing",
    status: "completed",
    iterations,
    content: [],
  } as unknown as TranscriptTurn;

  it("mounts every segment in the first paint", () => {
    const html = renderToStaticMarkup(<AssistantMessage turn={finished} whole />);

    for (let step = 0; step < 20; step += 1)
      expect(text(html)).toContain(`step ${step}`);
  });

  it("still ramps a trace nobody has seen yet", () => {
    const html = renderToStaticMarkup(<AssistantMessage turn={finished} />);

    // The tail is what the reader can see when a session OPENS pinned to the
    // bottom, and mounting only that is what keeps the opening frame short.
    expect(text(html)).toContain("step 19");
    expect(text(html)).not.toContain("step 0");
  });

  // Regression, the same flicker one tick later: the row can MOUNT before the
  // bubble is retired. Its settled transcript row lands while the registry still
  // reports the turn as running, so the handover verdict only comes on the NEXT
  // reconcile tick — and `whole`, read once as the ramp's initial state, arrived
  // after that mount and changed nothing. The transcript collapsed to
  // `SEGMENT_FIRST_PAINT` exactly as before for every handover that took more
  // than one tick.
  it("takes the whole trace even when it mounted before the handover", () => {
    const { container, rerender } = render(<AssistantMessage turn={finished} />);

    // Mounted cold: the ramp is holding everything but the tail.
    expect(container.textContent).not.toContain("step 0");

    rerender(<AssistantMessage turn={finished} whole />);

    for (let step = 0; step < 20; step += 1)
      expect(container.textContent).toContain(`step ${step}`);
  });

  // Regression, the same collapse deferred by one turn: `handedOverRowId` holds
  // ONE row, so the next turn's handover takes the flag away from this one. A
  // trace that showed everything only while the flag was up dropped back to the
  // tail the moment it moved on.
  it("keeps the whole trace after the flag moves to the next turn's row", () => {
    const { container, rerender } = render(<AssistantMessage turn={finished} />);
    rerender(<AssistantMessage turn={finished} whole />);
    rerender(<AssistantMessage turn={finished} />);

    expect(container.textContent).toContain("step 0");
    expect(container.textContent).toContain("step 19");
  });
});
