// @vitest-environment jsdom
import { fireEvent, render, waitFor } from "@testing-library/react";
import { renderToStaticMarkup } from "react-dom/server";
import { describe, expect, it, vi } from "vitest";
import {
  AssistantMessage,
  AttachmentRail,
  ContentBlockView,
  InlineMarkdown,
  Markdown,
  SpeechBlock,
  ThinkingBand,
  UserMessage,
} from "./ChatContent";
import type { GatewayClient } from "../lib/gateway";
import {
  mediaFrameClass,
  mediaGridClass,
  mediaTileFrameClass,
} from "../lib/media-frame";
import { speechOutput } from "../lib/speech";
import type { IterationAttachment, TranscriptTurn } from "../lib/types";
import type { LiveView as LiveViewModel } from "../lib/live-view";

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

describe("spoken transcript", () => {
  it("opens as naturally spaced transcript under a waveform you can seek", () => {
    const html = renderToStaticMarkup(
      <ContentBlockView
        block={{
          id: "speech-1",
          type: "speech",
          text: "A spoken answer that the reader can replay from any point.",
        }}
      />,
    );

    expect(html).toContain('aria-expanded="true"');
    expect(html).toContain('role="slider"');
    expect(html).toContain('aria-label="Speech position"');
    expect(html).toContain("text-left");
    expect(html).toContain('lang="en"');
  });

  // Reported as "za żółto ... dwa headery": the block painted an amber band for
  // its name and a SECOND amber band under it holding a worded Play/Pause button,
  // so one spoken reply carried two headers and two yellow surfaces.
  it("carries one header band, with the transport as an icon inside it", () => {
    const html = renderToStaticMarkup(
      <ContentBlockView
        block={{ id: "speech-1", type: "speech", text: "Listen again." }}
      />,
    );

    expect(count(html, /data-disclosure-toggle/g)).toBe(1);
    expect(html).not.toContain("bg-accent-surface");

    const view = render(<SpeechBlock text="Listen again." />);
    try {
      const play = view.getByRole("button", { name: "Play" });
      expect(play.textContent).toBe("");
      expect(play.querySelector("svg")).not.toBeNull();
      // The transport LEADS the header row, and the name is no longer in it.
      const header = play.parentElement;
      expect(header?.hasAttribute("data-speech-header")).toBe(true);
      expect(header?.firstElementChild).toBe(play);
      expect(header?.querySelector("[data-disclosure-toggle]")).toBeNull();
    } finally {
      view.unmount();
    }
  });

  // Reported as "waveform i play etc jest w headerze ... oczywiście można sobie to
  // rozwinąć i zobaczyć wersję tekstową": the seek control stood in the body, so the
  // reply could not be played without its text on screen, and the row that opens that
  // text never said what it opens.
  it("plays from the header and keeps only the named transcript in the body", () => {
    const view = render(<SpeechBlock text="Listen again." />);
    try {
      const wave = view.getByRole("slider", { name: "Speech position" });
      const header = view.container.querySelector("[data-speech-header]");
      const play = view.getByRole("button", { name: "Play" });

      expect(header?.contains(wave)).toBe(true);
      expect(header?.contains(play)).toBe(true);

      // The name stands OUTSIDE the frame, above its border.
      const frame = header?.parentElement;
      const name = view.getByRole("button", { name: /Transcript/ });
      expect(frame?.contains(name)).toBe(false);
      expect(name.nextElementSibling).toBe(frame);
      const body = header?.nextElementSibling;
      expect(body?.querySelector("[role='slider']")).toBeNull();
      expect(body?.querySelector("button")).toBeNull();
      expect(body?.querySelector("p")?.textContent).toBe("Listen again.");
    } finally {
      view.unmount();
    }
  });

  it("keeps the wave and the transport when the transcript is collapsed", () => {
    const view = render(<SpeechBlock text="Listen again." />);
    try {
      fireEvent.click(view.getByRole("button", { name: /Transcript/ }));

      expect(view.container.querySelector("p")).toBeNull();
      expect(view.getByRole("slider", { name: "Speech position" })).not.toBeNull();
      expect(view.getByRole("button", { name: "Play" })).not.toBeNull();
    } finally {
      view.unmount();
    }
  });

  // Reported as "ten play i pause powinien być mniejszy i może po lewej stronie a
  // transcript napis powinien być poza bloczkiem mały i full uppercased po lewej
  // stronie nad borderem": the name sat inside the amber frame, and the transport
  // ended the row on the far right in a 16px glyph.
  it("captions the frame from outside it and leads the row with a small transport", () => {
    const html = renderToStaticMarkup(
      <ContentBlockView
        block={{ id: "speech-1", type: "speech", text: "Listen again." }}
      />,
    );
    const caption = html.slice(
      html.indexOf("data-disclosure-toggle"),
      html.indexOf("</button>"),
    );

    // Above the border, not in it.
    expect(html.indexOf("data-disclosure-toggle")).toBeLessThan(
      html.indexOf("<section"),
    );
    expect(caption).toContain("uppercase");
    expect(caption).toContain("text-chip");

    const view = render(<SpeechBlock text="Listen again." />);
    try {
      const play = view.getByRole("button", { name: "Play" });
      // The icons' own 14px grammar, not the 16px one this row used to spell.
      expect(play.querySelector("svg")?.getAttribute("class")).toContain(
        "size-3.5",
      );
      expect(
        play.compareDocumentPosition(
          view.getByRole("slider", { name: "Speech position" }),
        ) & Node.DOCUMENT_POSITION_FOLLOWING,
      ).toBeTruthy();
    } finally {
      view.unmount();
    }
  });
  it("draws a flat rule until real samples exist, and never a made-up shape", () => {
    const view = render(<SpeechBlock text="Nothing has been synthesised yet." />);
    try {
      const bars = [
        ...view
          .getByRole("slider", { name: "Speech position" })
          .querySelectorAll("rect"),
      ].map((bar) => bar.getAttribute("height"));

      expect(bars.length).toBeGreaterThan(0);
      expect(new Set(bars).size).toBe(1);
    } finally {
      view.unmount();
    }
  });

  it("puts the disclosure chevron before its name and points it down while open", () => {
    const html = renderToStaticMarkup(
      <ContentBlockView
        block={{ id: "speech-1", type: "speech", text: "Listen again." }}
      />,
    );
    const disclosure = html.slice(
      html.indexOf("data-disclosure-toggle"),
      html.indexOf("</button>"),
    );

    expect(disclosure.indexOf("<svg")).toBeLessThan(disclosure.indexOf("Transcript"));
    expect(disclosure).toContain("rotate-90");
  });

  it("restarts speech at the position pressed on the wave", async () => {
    const calls: string[] = [];
    let finish: () => void = () => undefined;
    const pending = new Promise<void>((resolve) => {
      finish = resolve;
    });
    const speak = vi.spyOn(speechOutput, "speak").mockImplementation((value) => {
      calls.push(value);
      return pending;
    });
    const stop = vi.spyOn(speechOutput, "stop").mockImplementation(() => undefined);
    const view = render(<SpeechBlock text="one two three four five six" />);

    try {
      fireEvent.click(view.getByRole("button", { name: "Play" }));
      expect(calls).toEqual(["one two three four five six"]);

      const wave = view.getByRole("slider", { name: "Speech position" });
      wave.getBoundingClientRect = () =>
        ({ left: 0, width: 100 }) as unknown as DOMRect;
      fireEvent.pointerDown(wave, { clientX: 50 });

      await waitFor(() => expect(calls.at(-1)).toBe("four five six"));
      expect(stop).toHaveBeenCalled();
    } finally {
      finish();
      view.unmount();
      speak.mockRestore();
      stop.mockRestore();
    }
  });

  // Regression, user report: the live speech block unmounted during the transcript
  // handoff and its cleanup cancelled the separate voice-mode reading.
  it("does not stop voice-mode speech when a transcript block unmounts", () => {
    let voiceModeStopped = false;
    const stop = vi
      .spyOn(speechOutput, "stop")
      .mockImplementation((owner?: object) => {
        if (owner === undefined) voiceModeStopped = true;
      });
    const view = render(<SpeechBlock text="The same answer appears in the transcript." />);

    view.unmount();

    expect(stop).toHaveBeenCalledWith(expect.any(Object));
    expect(voiceModeStopped).toBe(false);
    stop.mockRestore();
  });
});

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

// User report: a Markdown attachment preview looked like a link but tapping it
// invoked the unsupported `attachment:` browser scheme and opened nothing.
describe("Markdown attachment links", () => {
  it("hands a safe attachment link to the artifact opener", () => {
    const opened = vi.fn();
    const view = render(
      <AssistantMessage
        turn={{
          id: "t1",
          status: "completed",
          content: [
            {
              id: "answer",
              type: "prose",
              markdown:
                "[Open preview](attachment://8e3a587d-232c-497d-a290-7d16cfcf0e02) and [open docs](https://example.com/docs).",
            },
          ],
        }}
        onOpenAttachment={opened}
      />,
    );

    const preview = view.getByRole("link", { name: "Open preview" });
    expect(preview.getAttribute("target")).toBeNull();
    fireEvent.click(preview);
    expect(opened).toHaveBeenCalledOnce();
    expect(opened).toHaveBeenCalledWith("8e3a587d-232c-497d-a290-7d16cfcf0e02");

    const external = view.getByRole("link", { name: "open docs" });
    expect(external.getAttribute("target")).toBe("_blank");
  });

  it("does not turn an unsafe attachment target into an internal action", () => {
    const opened = vi.fn();
    const view = render(
      <Markdown onOpenAttachment={opened}>
        {"[Not an artifact](attachment://../../settings)"}
      </Markdown>,
    );

    expect(view.queryByRole("link", { name: "Not an artifact" })).toBeNull();
    expect(opened).not.toHaveBeenCalled();
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

// Regression, companion transcript report: PATCH diffs used a two-column
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

// A voice memo the human sent used to reach this bubble and paint NOTHING: the
// rail admitted `image/*` and `video/*` only, so the recording the gateway had
// stored was invisible in the very message that carried it.
describe("user bubble recordings", () => {
  const html = () =>
    renderToStaticMarkup(
      <UserMessage
        attachments={[
          {
            filename: "memo.m4a",
            media_type: "audio/mp4",
            base64: "AAAAIGZ0eXBNNEEg",
            size: 12,
          },
        ]}
      >
        {"listen to this"}
      </UserMessage>,
    );

  it("plays what it cannot show", () => {
    expect(html()).toMatch(/<audio[^>]*controls/u);
    expect(text(html())).toContain("memo.m4a");
    expect(text(html())).toContain("M4A");
  });

  // Nothing ever decodes into it, so the 4:3 box a still reserves would be a
  // frame around silence.
  it("never stands a recording on a picture's plate", () => {
    expect(html()).not.toContain(mediaFrameClass);
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

// Regression, issue td-546817: Python evaluations without detected Activity
// retained the old always-expanded frame instead of the canonical execution receipt.
describe("a Python evaluation without detected Activity", () => {
  const turnWith = (form: Record<string, unknown>, status = "completed") => ({
    id: "python-only",
    status,
    iterations: [{ id: "iteration-1", position: 41, forms: [form] }],
    content: [],
  }) as unknown as TranscriptTurn;

  it("is the running execution before any semantic activity appears", () => {
    const painted = render(
      <AssistantMessage
        turn={turnWith({ source: "answer = 42" }, "running")}
        streaming
      />,
    );

    expect(painted.getByRole("button", { name: "Expand execution trace" })).toBeTruthy();
    expect(painted.container.textContent).toContain("RUNNING · PYTHON");
    expect(painted.container.textContent).not.toContain("answer = 42");
    expect(painted.container.textContent).not.toContain("0 activities");
  });

  it("restores one settled receipt with Python and Result evidence", () => {
    const painted = render(
      <AssistantMessage
        turn={turnWith({ source: "answer = 42", result: 42, duration_ms: 29 })}
      />,
    );

    expect(painted.container.textContent).toContain("DONE · PYTHON · 29ms");
    expect(painted.container.textContent).not.toContain("answer = 42");
    fireEvent.click(painted.getByRole("button", { name: "Expand execution trace" }));
    expect(painted.container.textContent).toContain("answer = 42");
    expect(painted.container.textContent).toContain("RESULT");
    expect(painted.container.textContent).not.toContain("ACTIVITY");
  });

  it("keeps a failed Python execution in the same receipt anatomy", () => {
    const painted = render(
      <AssistantMessage
        turn={turnWith({ source: "raise Error()", error: "failed", duration_ms: 29 })}
      />,
    );

    expect(painted.container.textContent).toContain("FAILED · PYTHON · 29ms");
    expect(painted.getByRole("button", { name: "Expand execution trace" })).toBeTruthy();
  });

  it("shows an interrupted Python execution as a stop, not a JVM failure", () => {
    const painted = render(
      <AssistantMessage
        turn={turnWith({
          source: "walk_the_tree()",
          error: {
            message: "java.lang.InterruptedException",
            trace: "java.lang.InterruptedException: FutureTask/awaitDone",
          },
          duration_ms: 29,
        })}
      />,
    );

    expect(painted.container.textContent).toContain("INTERRUPTED · PYTHON · 29ms");
    expect(painted.container.textContent).not.toContain("FAILED · PYTHON");
    fireEvent.click(painted.getByRole("button", { name: "Expand execution trace" }));
    expect(painted.container.textContent).toContain("Interrupted");
    expect(painted.container.textContent).not.toContain("FutureTask/awaitDone");
  });

  it("enhances the same receipt when semantic activity arrives", () => {
    const runningTurn = turnWith({ source: "answer = search()" }, "running");
    const painted = render(<AssistantMessage turn={runningTurn} streaming />);
    const receipt = painted.getByRole("button", { name: "Expand execution trace" });
    expect(painted.container.textContent).toContain("RUNNING · PYTHON");

    const detected = {
      id: "detected-activity",
      title: "Activity",
      classification: "activity",
      seq: 1,
      activity: {
        schema_version: 1,
        anchor: { iteration: 41, form_index: 0 },
        state: "running",
        counts: { running: 1, succeeded: 0, failed: 0, cancelled: 0 },
        rows: [{
          id: "grep-1",
          sequence: 1,
          operation: "grep",
          state: "running",
          summary: "searching",
          resources: [],
          evidence: [],
        }],
        omitted: { rows: 0, by_classification: {} },
      },
      nodes: [],
    } as unknown as LiveViewModel;
    painted.rerender(
      <AssistantMessage turn={runningTurn} streaming liveActivities={[detected]} />,
    );

    expect(painted.getByRole("button", { name: "Expand execution trace" })).toBe(receipt);
    expect(painted.container.textContent).toContain("RUNNING · GREP · searching");
  });

  it("does not invent Activity when an empty projection settles", () => {
    const emptyActivity = {
      id: "empty-activity",
      title: "Activity",
      classification: "activity",
      seq: 0,
      is_settled: true,
      created_at: 1_000,
      ended_at: 1_029,
      activity: {
        schema_version: 1,
        anchor: { iteration: 41, form_index: 0 },
        state: "succeeded",
        counts: { running: 0, succeeded: 0, failed: 0, cancelled: 0 },
        rows: [],
        omitted: { rows: 0, by_classification: {} },
      },
      nodes: [],
    } as unknown as LiveViewModel;
    const painted = render(
      <AssistantMessage
        turn={turnWith({ source: "answer = 42", result: 42 })}
        liveActivities={[emptyActivity]}
      />,
    );

    expect(painted.container.textContent).toContain("DONE · PYTHON · 29ms");
    expect(painted.container.textContent).not.toContain("0 activities");
    fireEvent.click(painted.getByRole("button", { name: "Expand execution trace" }));
    expect(painted.container.textContent).not.toContain("ACTIVITY");
  });
});

// Regression, issue td-cc41a1: Activity was appended after the entire assistant
// row, so a multi-form iteration could not show which complete Python form owned it.
describe("Activity owns the slot after its Python form and result", () => {
  const client = {
    attachmentUrl: async () => "blob:none",
    retainAttachment: () => () => {},
  } as unknown as GatewayClient;
  const attachment: IterationAttachment = {
    index: 0,
    iteration_id: "iteration-1",
    view_id: "activity-view",
    classification: "activity",
    activity_anchor: {
      evaluation_id: "evaluation-1",
      iteration: 41,
      form_index: 1,
    },
    kind: "file",
    media_type: "application/vnd.vis.live+ndjson",
    filename: "activity.live.ndjson",
  };
  const firstAttachment: IterationAttachment = {
    ...attachment,
    index: 1,
    view_id: "first-activity-view",
    activity_anchor: { ...attachment.activity_anchor!, form_index: 0 },
  };
  const runningActivity: LiveViewModel = {
    id: "activity-view",
    title: "Activity",
    classification: "activity",
    seq: 0,
    activity: {
      schema_version: 1,
      anchor: { iteration: 41, form_index: 1 },
      state: "running",
      counts: { running: 1, succeeded: 1, failed: 0, cancelled: 0 },
      rows: [
        { id: "call-1", sequence: 1, operation: "grep", presenter: "observation", signal: "observation", state: "succeeded", summary: "18 matches", resources: [], evidence: [] },
        { id: "call-2", sequence: 2, operation: "run_tests", presenter: "tests", signal: "verification", state: "running", summary: "companion suite", resources: [], evidence: [] },
      ],
      omitted: { rows: 0, by_classification: {} },
    },
    nodes: [],
  };
  const turn: TranscriptTurn = {
    id: "activity-turn",
    status: "completed",
    iterations: [
      {
        id: "iteration-1",
        position: 41,
        forms: [
          { source: "first_form()", result_summary: "first result" },
          {
            source: "second_form()",
            result_render: "```\nsecond result\n```",
          },
        ],
        attachments: [firstAttachment, attachment],
      },
    ],
  };

  it("puts a historical receipt under only its anchored form", () => {
    const painted = render(<AssistantMessage turn={turn} client={client} sid="s1" />);
    const receipts = painted.getAllByRole("button", { name: "Expand execution trace" });

    expect(receipts).toHaveLength(2);
    expect(painted.container.textContent).not.toContain("first_form()");
    fireEvent.click(receipts[0]);
    expect(painted.container.textContent).toContain("first_form()");
    expect(painted.container.textContent).toContain("first result");
    expect(painted.container.textContent).not.toContain("second_form()");
    fireEvent.click(receipts[1]);
    expect(painted.container.textContent).toContain("second_form()");
    expect(painted.container.textContent).toContain("RESULT");
    expect(painted.container.textContent).not.toContain("second result");
    expect(painted.container.textContent?.match(/Loading Activity/g)).toHaveLength(2);
  });

  // Regression, issue td-65cdf6: a production 1-based iteration anchor was
  // compared with the iteration's array index, so the running panel vanished.
  it("replaces the filed receipt with the same live view without duplicating it", () => {
    const rendered = text(
      renderToStaticMarkup(
        <AssistantMessage
          turn={turn}
          client={client}
          sid="s1"
          liveActivities={[runningActivity]}
        />,
      ),
    );
    expect(rendered.match(/ACTIVITY/g)).toHaveLength(1);
    expect(rendered).toContain("RUNNING");
    expect(rendered.match(/Loading Activity/g) ?? []).toHaveLength(0);
  });

  // Regression, issue td-f9035e: Python, Result, and Activity each painted an
  // independent receipt, while the live Activity headline claimed an unknowable total.
  it.each([320, 390, 768, 1440])(
    "collapses one honest execution receipt and opens its three evidence bands at %ipx",
    (width) => {
    const focused = { ...runningActivity, activity: { ...runningActivity.activity!, anchor: { iteration: 41, form_index: 0 } } };
    const focusedTurn: TranscriptTurn = {
      ...turn,
      iterations: [{
        ...turn.iterations![0],
        forms: [{ source: "line_1()\nline_2()\nline_3()\nline_4()\nline_5()\nline_6()", result_render: "```\nresult body\n```" }],
        attachments: [],
      }],
    };
    const painted = render(
      <div style={{ width }}>
        <AssistantMessage turn={focusedTurn} liveActivities={[focused]} />
      </div>,
    );
    expect(painted.container.firstElementChild).toHaveStyle({ width: `${width}px` });

    expect(painted.getByRole("button", { name: "Expand execution trace" })).toBeTruthy();
    expect(painted.container.textContent).toContain("RUNNING · RUN_TESTS · companion suite · and more");
    expect(painted.container.textContent).not.toContain("line_1()");
    expect(painted.container.textContent).not.toContain("RESULT");
    expect(painted.container.textContent).not.toContain("18 matches");
    fireEvent.click(painted.getByRole("button", { name: "Expand execution trace" }));
    expect(painted.container.textContent).toContain("PYTHON +1 more");
    expect(painted.container.textContent).toContain("line_5()");
    expect(painted.container.textContent).not.toContain("line_6()");
    expect(painted.container.textContent).toContain("RESULT");
    expect(painted.container.textContent).toContain("ACTIVITY");
    expect(painted.container.textContent).toContain("18 matches");
  });

  // Regression, issue td-5b6b08: the filed Companion receipt omitted its
  // primary operation and elapsed time and retained the old terminal copy.
  it("keeps terminal Activity copy and elapsed time at the transcript boundary", () => {
    const settled = {
      ...runningActivity,
      is_settled: true,
      created_at: 1_000,
      ended_at: 13_600,
      activity: {
        ...runningActivity.activity!,
        anchor: { iteration: 41, form_index: 0 },
        state: "succeeded" as const,
        counts: { running: 0, succeeded: 2, failed: 0, cancelled: 0 },
        rows: runningActivity.activity!.rows.map((row) => ({ ...row, state: "succeeded" as const })),
      },
    };
    const settledTurn: TranscriptTurn = {
      ...turn,
      iterations: [{
        ...turn.iterations![0],
        forms: [{ source: "work()", result_summary: "done" }],
        attachments: [],
      }],
    };

    const rendered = text(renderToStaticMarkup(
      <AssistantMessage turn={settledTurn} liveActivities={[settled]} />,
    ));

    expect(rendered).toContain("DONE · GREP and more · 2 activities · 12.6s");
  });

  it("uses only the actual terminal Activity count after settlement", () => {
    const settled = {
      ...runningActivity,
      is_settled: true,
      activity: {
        ...runningActivity.activity!,
        anchor: { iteration: 41, form_index: 0 },
        state: "failed" as const,
        counts: { running: 0, succeeded: 5, failed: 1, cancelled: 0 },
        rows: [],
        omitted: { rows: 6, by_classification: { observation: 6 } },
      },
    };
    const settledTurn: TranscriptTurn = {
      ...turn,
      iterations: [{
        ...turn.iterations![0],
        forms: [{ source: "work()", result_summary: "done" }],
        attachments: [],
      }],
    };
    const rendered = text(renderToStaticMarkup(
      <AssistantMessage turn={settledTurn} liveActivities={[settled]} />,
    ));

    expect(rendered).toContain("FAILED · 6 activities");
    expect(rendered).not.toContain("finished 6/");
  });

  it("does not attach Activity to a print-only form", () => {
    const printOnly: TranscriptTurn = {
      ...turn,
      iterations: [
        {
          ...turn.iterations![0],
          forms: [{ result_render: "```\nprinted\n```" }],
          attachments: [
            {
              ...attachment,
              activity_anchor: {
                ...attachment.activity_anchor!,
                form_index: 0,
              },
            },
          ],
        },
      ],
    };
    const rendered = text(
      renderToStaticMarkup(
        <AssistantMessage turn={printOnly} client={client} sid="s1" />,
      ),
    );
    expect(rendered).not.toContain("Activity");
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

  // Regression, user report with a screenshot: a `gh` watch that had FINISHED
  // read as "1 file · release.live.ndjson" — a nameless line under the recorded
  // files disclosure, while the record behind it holds the picture the run
  // ended on and its whole log. A settled run is an artifact this app opens,
  // so it stands in the step as one.
  it("collapses repeated cuts of one settled run into one row", () => {
    const run = {
      filename: "release.live.ndjson",
      media_type: "application/vnd.vis.live+ndjson",
      iteration_id: "i1",
    };
    const html = renderToStaticMarkup(
      <AttachmentRail
        client={client}
        sid="s1"
        attachments={Array.from({ length: 11 }, (_, index) => ({
          ...run,
          index,
          version: index + 1,
          size: 40_000 + index,
        }))}
      />,
    );

    // Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: every saved cut of
    // one CI run rendered as another identical RUN row in the same tool result.
    expect(html.match(/aria-label="Open run release"/g)).toHaveLength(1);
    expect(text(html).match(/RUN/g)).toHaveLength(1);
    expect(text(html)).not.toContain("release.live.ndjson");
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

  // A narrow phone transcript used full justification, stretching ordinary spaces
  // around inline code into conspicuous gaps.
  it("keeps Markdown prose and every code surface naturally spaced", () => {
    const markdown = renderToStaticMarkup(
      <Markdown>{"Release `update version files for v0.7.126, bump next dev version`.\n\n```shell\ngit status --short\n```"}</Markdown>,
    );
    const inline = renderToStaticMarkup(
      <InlineMarkdown>{"Run `update version files` now"}</InlineMarkdown>,
    );

    for (const markup of [markdown, inline]) {
      expect(/<code class="[^"]*inline-block/.test(markup)).toBe(true);
      expect(/<code class="[^"]*text-left/.test(markup)).toBe(true);
    }
    expect(/<p class="[^"]*text-left/.test(markdown)).toBe(true);
    expect(/<code class="[^"]*break-all/.test(markdown)).toBe(true);
    expect(/<pre class="[^"]*text-left/.test(markdown)).toBe(true);
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

  it("says when a finished live turn is waiting for its transcript row", () => {
    const html = renderToStaticMarkup(
      <AssistantMessage
        turn={{ ...running, status: "completed" }}
        pending="Loading latest changes"
      />,
    );

    expect(text(html)).toContain("Loading latest changes");
    expect(html).toContain("animate-spinner-frame");
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
