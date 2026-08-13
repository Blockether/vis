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
import type { TranscriptTurn } from "../lib/types";

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
