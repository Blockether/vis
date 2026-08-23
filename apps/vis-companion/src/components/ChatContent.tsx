import {
  Fragment,
  isValidElement,
  memo,
  useCallback,
  useEffect,
  useLayoutEffect,
  useMemo,
  useRef,
  useState,
  type ReactNode,
} from "react";
import Prism from "prismjs";
import { DataTable } from "./DataTable";
import { DocPreview, DocStack, docStackSummary } from "./DocArtifact";
import { LiveRunRow } from "./LiveArtifact";
import { LiveViewPanel } from "./LiveView";
import type { LiveView as LiveViewModel } from "../lib/live-view";
import { AlertIcon, ArrowOutIcon, ChevronIcon, PauseIcon, PlayIcon } from "./icons";
import {
  attachmentBytes,
  attachmentIsAudio,
  attachmentIsDoc,
  attachmentIsImage,
  attachmentIsLive,
  attachmentIsPlayable,
  attachmentIsVideo,
  collapseAttachmentVersions,
  pageBySize,
  RAIL_PAGE,
} from "../lib/artifacts";
import {
  BandLabel,
  BandTally,
  CopyChip,
  Disclosure,
  IconButton,
  LoadMore,
  PROSE,
  Spinner,
  Waveform,
} from "./ui";
import "prismjs/components/prism-bash";
import "prismjs/components/prism-clojure";
import "prismjs/components/prism-css";
import "prismjs/components/prism-diff";
import "prismjs/components/prism-java";
import "prismjs/components/prism-json";
import "prismjs/components/prism-markdown";
import "prismjs/components/prism-python";
import "prismjs/components/prism-rust";
import "prismjs/components/prism-typescript";
import "prismjs/components/prism-jsx";
import "prismjs/components/prism-tsx";
import "prismjs/components/prism-yaml";
import ReactMarkdown from "react-markdown";
import remarkBreaks from "remark-breaks";
import remarkGfm from "remark-gfm";
import { parseUserMessage } from "../lib/paste";
import { formatCost, formatTokens, turnUsage } from "../lib/usage";
import { isViewportRotating, onViewportRotation } from "../lib/viewport";
import type {
  ContentBlock,
  GatewayAttachment,
  IterationAttachment,
  JsonValue,
  TranscriptForm,
  TranscriptIteration,
  TranscriptTurn,
} from "../lib/types";
import type { GatewayClient } from "../lib/gateway";
import { speechOutput } from "../lib/speech";
import type { SpokenTrack } from "../lib/speech";
import { ExpandableImage } from "./ImageViewer";
import {
  mediaContentClass,
  mediaGroupLayout,
  mediaPendingClass,
  mediaTileContentClass,
  type MediaLayout,
} from "../lib/media-frame";
import {
  MediaGrid,
  MediaPlate,
  MediaRecording,
  MediaTile,
  mediaMeta,
  mediaSummary,
} from "./Media";

// An inline formatting context inherits its paragraph's justification. Code is an
// atomic value instead: its own box keeps authored spaces natural while still
// wrapping multi-word commands within a narrow transcript column.
const INLINE_CODE_CLASS =
  "mx-px inline-block max-w-full rounded-none bg-result-path px-0.5 py-px text-left font-mono font-medium text-result-path-foreground";
// Transcript nodes the stream appends rise + fade in instead of popping into
// place. A keyframe animation (see `--animate-transcript-*` in index.css) plays
// exactly once — on the element's first paint after insertion — so a re-render
// can never replay it. Only live subtrees pass `live`, so replaying history (or
// a finished turn re-keyed out of the live slot into the turn list) stays
// perfectly still.
export const transcriptEnterClass =
  "animate-transcript-enter motion-reduce:animate-none";

// For nodes that land INSIDE a bubble that is already on screen (a new tool
// form, a result card joining the grid). They only rise: a second opacity ramp
// nested in the first is what read as a wash-out, and content that is still
// streaming must never fade.
export const transcriptRiseClass =
  "animate-transcript-rise motion-reduce:animate-none";

// One assistant turn is thousands of nodes, and opening a session mounts many
// of them at once: measured on device, a single turn of this transcript is ~13k
// elements and the first paint cost 409 ms, the full window 1256 ms.
//
// What a long session costs AFTER that is paint, and paint scales with the
// whole mounted tree rather than with what is on screen. Measured on the iOS
// simulator against a 30-turn session (64 517 nodes, 265 635 px), one scroll
// frame: 82 ms for the live screen, 74 ms for an inert clone of the same DOM
// with no React and no listeners in it — so no amount of memoization moves that
// number, and 17 ms once each turn is its own skippable box.
//
// `content-visibility:auto` was reverted here once and the reason was never the
// containment: it was the SIZE. `contain-intrinsic-size` GUESSED (480 px, later
// 800 px), every first reveal corrected the guess, and on a scroller with no
// scroll anchoring (WebKit has none, and the transcript sets
// `overflow-anchor:none` on purpose) a correction ABOVE the viewport moves what
// you are reading — 39 corrections totalling 53 307 px on one 10 000 px scroll
// up, the worst single ones 21 002 / 12 940 / 7 752 px. The guess reproduces on
// the simulator: `auto 800px` over those 30 turns made 23 height changes and a
// transcript that measured 24 916 px instead of 265 635 px.
//
// So `useMeasuredPaintSkip` hands the engine the height a turn ALREADY has, and
// only once layout has measured it. Same session, same fling: 0 corrections, 0
// height changes, 0 px of drift, and mid-fling screenshots at 72 000 px/s whose
// longest ink-free run is the settled one's (28.0 pt).
//
// `contain:layout style` stays retired. It faked no size, but it split a
// 41 148 px transcript into 198 paint-isolated islands and a fast fling then
// exposed the paper background before WebKit rasterized them — the white bands.
// One box per TURN is the whole win (30 boxes: 17 ms a frame; the same CSS on
// all 5 008 trace segments: 33 ms — every containment box costs ~3 µs a frame),
// so nothing smaller than a turn declares containment.
//
// What keeps the OPEN cheap is still the bounded window: pagination mounts only
// INITIAL_VISIBLE_TURNS, `Load earlier` brings the rest in on demand, and the
// iteration ramp below stages a turn's trace. None of those guess a height.

type DiffLineKind = "add" | "del" | "ctx";
type DiffLine =
  | { kind: "meta"; text: string }
  | {
      kind: DiffLineKind;
      beforeLine: number | null;
      afterLine: number | null;
      marker: "+" | "-" | " ";
      text: string;
    };

function unifiedDiff(value: string): DiffLine[] {
  const rows: DiffLine[] = [];
  let beforeLine: number | null = null;
  let afterLine: number | null = null;

  for (const line of value.split("\n")) {
    const hunk = /^@@ -(\d+)(?:,\d+)? \+(\d+)(?:,\d+)? @@/.exec(line);
    if (hunk) {
      beforeLine = Number(hunk[1]);
      afterLine = Number(hunk[2]);
      rows.push({ kind: "meta", text: line });
    } else if (line.startsWith("---") || line.startsWith("+++")) {
      rows.push({ kind: "meta", text: line });
    } else if (line.startsWith("-")) {
      rows.push({
        kind: "del",
        beforeLine,
        afterLine: null,
        marker: "-",
        text: line.slice(1),
      });
      beforeLine = beforeLine === null ? null : beforeLine + 1;
    } else if (line.startsWith("+")) {
      rows.push({
        kind: "add",
        beforeLine: null,
        afterLine,
        marker: "+",
        text: line.slice(1),
      });
      afterLine = afterLine === null ? null : afterLine + 1;
    } else if (line.startsWith(" ")) {
      rows.push({
        kind: "ctx",
        beforeLine,
        afterLine,
        marker: " ",
        text: line.slice(1),
      });
      beforeLine = beforeLine === null ? null : beforeLine + 1;
      afterLine = afterLine === null ? null : afterLine + 1;
    } else {
      rows.push({ kind: "meta", text: line });
    }
  }

  return rows;
}

function diffLineClass(kind: DiffLineKind): string {
  if (kind === "add") return "bg-code-ok text-code-success";
  if (kind === "del") return "bg-code-err text-code-error";
  return "bg-code text-code-foreground";
}

// A diff cell NEVER clips its own text: the block's horizontal scroller (below) is
// what reveals a long line, and `overflow-hidden`/`text-ellipsis` here silently ate
// the tail of every wide hunk with nothing left to scroll to.
// Unified diffs spend the narrow companion viewport on ONE content column. The
// two fixed gutters retain both source coordinates without halving every line.
function DiffLineView({
  line,
  className = "",
}: {
  line: Exclude<DiffLine, { kind: "meta" }>;
  className?: string;
}) {
  const lineNumber = line.kind === "add" ? line.afterLine : line.beforeLine;
  const label =
    `${line.kind === "add" ? "Added" : line.kind === "del" ? "Removed" : "Unchanged"} line ${lineNumber ?? ""}`.trim();

  return (
    <span
      className={`flex w-max min-w-full whitespace-pre py-px ${diffLineClass(line.kind)} ${className}`}
      aria-label={label}
    >
      <span className="w-8 shrink-0 select-none pr-1 text-right text-code-duration">
        {line.beforeLine ?? ""}
      </span>
      <span className="w-8 shrink-0 select-none pr-1 text-right text-code-duration">
        {line.afterLine ?? ""}
      </span>
      <span className="w-4 shrink-0 select-none text-center" aria-hidden="true">
        {line.marker}
      </span>
      <span className="pr-3">{line.text || " "}</span>
    </span>
  );
}

const DiffBlock = memo(function DiffBlock({
  value,
  compact,
  frameless = false,
}: {
  value: string;
  compact: boolean;
  frameless?: boolean;
}) {
  const rows = unifiedDiff(value);
  return (
    <div
      className={`${compact ? "my-2" : "my-3"} relative overflow-hidden bg-code ${frameless ? "" : "border border-code-edge"}`}
      aria-label="Unified diff"
    >
      {!frameless && (
        <CopyChip
          value={value}
          label="Copy code"
          className="absolute right-2 top-2 z-10"
        >
          Copy
        </CopyChip>
      )}
      <div
        className={`${compact ? "text-meta" : "text-ui"} max-w-full overflow-x-auto overscroll-x-contain py-2 font-mono`}
      >
        <div className="w-max min-w-full">
          {rows.map((row, index) => {
            // The copy chip floats over the top-right corner; only the first row keeps
            // its tail clear. Long lines remain available through this block's scroller.
            const clearance = !frameless && index === 0 ? " pr-16" : "";
            return row.kind === "meta" ? (
              <span
                className={`flex w-max min-w-full whitespace-pre px-2 py-px text-code-syntax-keyword${clearance}`}
                key={`${index}-${row.text}`}
              >
                {row.text || " "}
              </span>
            ) : (
              <DiffLineView
                line={row}
                className={clearance}
                key={`${index}-${row.kind}-${row.text}`}
              />
            );
          })}
        </div>
      </div>
    </div>
  );
});

const languageAliases: Record<string, string> = {
  clj: "clojure",
  edn: "clojure",
  js: "javascript",
  jsx: "jsx",
  md: "markdown",
  py: "python",
  sh: "bash",
  shell: "bash",
  ts: "typescript",
  yml: "yaml",
};

const syntaxTokenClasses: Record<string, string> = {
  boolean: "text-code-syntax-number",
  builtin: "text-code-syntax-special",
  char: "text-code-syntax-string",
  className: "text-code-syntax-special",
  comment: "italic text-code-syntax-comment",
  constant: "text-code-syntax-number",
  decorator: "text-code-syntax-special",
  function: "text-code-syntax-special",
  important: "font-semibold text-code-syntax-keyword",
  keyword: "font-medium text-code-syntax-keyword",
  number: "text-code-syntax-number",
  operator: "text-code-syntax-special",
  regex: "text-code-syntax-string",
  string: "text-code-syntax-string",
  symbol: "text-code-syntax-number",
};

function syntaxClass(token: Prism.Token): string {
  const aliases = Array.isArray(token.alias)
    ? token.alias
    : token.alias
      ? [token.alias]
      : [];
  for (const candidate of [token.type, ...aliases]) {
    const normalized = candidate === "class-name" ? "className" : candidate;
    if (syntaxTokenClasses[normalized]) return syntaxTokenClasses[normalized];
  }
  return "text-code-foreground";
}

type SyntaxSegment = { text: string; className: string };

// The colour the `<pre>` already paints. A segment that only asks for it needs
// no element of its own — see `SyntaxCodeBlock`.
const CODE_DEFAULT_CLASS = "text-code-foreground";

function flattenSyntax(
  tokens: (string | Prism.Token)[],
  inherited: string,
  out: SyntaxSegment[],
): void {
  for (const token of tokens) {
    if (typeof token === "string") {
      if (token) out.push({ text: token, className: inherited });
      continue;
    }
    const className = syntaxClass(token);
    if (Array.isArray(token.content)) {
      flattenSyntax(token.content as (string | Prism.Token)[], className, out);
    } else if (typeof token.content === "string") {
      if (token.content) out.push({ text: token.content, className });
    } else {
      flattenSyntax([token.content as Prism.Token], className, out);
    }
  }
}

function highlightSegments(value: string, language: string): SyntaxSegment[] {
  const normalized = languageAliases[language] ?? language;
  const grammar = Prism.languages[normalized];
  if (!grammar) return [{ text: value, className: CODE_DEFAULT_CLASS }];
  const out: SyntaxSegment[] = [];
  flattenSyntax(Prism.tokenize(value, grammar), CODE_DEFAULT_CLASS, out);
  return out;
}

// Split a flat segment stream into per-line segment arrays, preserving the
// class of tokens (e.g. block comments) that span multiple newlines.
// Split a flat segment stream into per-line segment arrays, preserving the
// class of tokens (e.g. block comments) that span multiple newlines.
//
// Adjacent segments that ask for the SAME class are merged as they land.
// Prism hands back long runs of them — the plain text either side of every
// token, and every token whose children all fall back to the default — and one
// element per run instead of one per piece is worth tens of thousands of spans
// in a transcript of tool output: measured 68,450 spans in a six-turn session,
// 34,282 of them carrying nothing but the default colour.
function segmentsToLines(segments: SyntaxSegment[]): SyntaxSegment[][] {
  const lines: SyntaxSegment[][] = [[]];
  const push = (text: string, className: string): void => {
    if (!text) return;
    const line = lines[lines.length - 1];
    const last = line[line.length - 1];
    if (last && last.className === className) last.text += text;
    else line.push({ text, className });
  };
  for (const segment of segments) {
    const parts = segment.text.split("\n");
    parts.forEach((part, index) => {
      if (index > 0) lines.push([]);
      push(part, segment.className);
    });
  }
  return lines;
}

const GUTTER_LINE = /^(\s*\d+) {2}(.*)$/;
const GUTTER_DIVIDER = /^(\s*\u22ef)\s*$/;

// `cat` bodies arrive as a numbered gutter (` 12 <code>`) fenced with the
// file language. Feeding those line numbers to Prism poisons the grammar, so
// peel the gutter off, highlight the code, and restore the gutter uncolored —
// exactly what the TUI does.
function splitGutter(
  value: string,
): { gutters: string[]; code: string } | null {
  const rawLines = value.split("\n");
  const gutters: string[] = [];
  const codeLines: string[] = [];
  let numbered = 0;
  for (const line of rawLines) {
    const match = GUTTER_LINE.exec(line);
    if (match) {
      numbered += 1;
      gutters.push(match[1]);
      codeLines.push(match[2]);
      continue;
    }
    const divider = GUTTER_DIVIDER.exec(line);
    if (divider) {
      gutters.push(divider[1]);
      codeLines.push("");
      continue;
    }
    if (line === "") {
      gutters.push("");
      codeLines.push("");
      continue;
    }
    return null;
  }
  if (numbered < 2) return null;
  return { gutters, code: codeLines.join("\n") };
}

// memo: during a live stream the turn re-renders every ~150 ms flush; memoized
// leaves make every UNCHANGED block bail out instead of re-running Prism /
// ReactMarkdown over the whole accumulated body (that main-thread churn is
// what made typing during streaming lag).
const SyntaxCodeBlock = memo(function SyntaxCodeBlock({
  value,
  language,
  compact,
  copyValue,
  bare = false,
  frameless = false,
}: {
  value: string;
  language: string;
  compact: boolean;
  copyValue?: string;
  /** Drop this block's own frame + margin so a parent can own the chrome. */
  bare?: boolean;
  /** Keep the spacing but drop the frame: an enclosing card already draws one. */
  frameless?: boolean;
}) {
  const gutter = splitGutter(value);
  const marks = extractMarks(gutter ? gutter.code : value);
  const source = marks.text;
  const lines = segmentsToLines(
    applyMarks(highlightSegments(source, language), marks.ranges),
  );

  return (
    <div
      className={`relative overflow-hidden bg-code ${bare ? "" : `${compact ? "my-2" : "my-3"} ${frameless ? "" : "border border-code-edge"}`}`}
    >
      {/* An enclosing card (a tool result) owns ONE copy control for the whole
          body, so a frameless block does not add a second, third, … chip. */}
      {!frameless && (
        <CopyChip
          value={copyValue ?? source}
          label="Copy code"
          className="absolute right-2 top-2 z-10"
        >
          Copy
        </CopyChip>
      )}
      <pre
        className={`${compact ? "py-2 text-meta " : "py-2.5 text-ui "} m-0 max-w-full overflow-x-auto overscroll-x-contain text-left font-mono text-code-foreground`}
      >
        <code className="block min-w-max [tab-size:2]">
          {lines.map((segments, index) => (
            <div
              key={index}
              className={`flex w-fit min-w-full whitespace-pre px-3 ${frameless ? "" : "first:pr-16"}`}
            >
              {gutter && (
                <span
                  className="mr-3 shrink-0 select-none text-right text-code-duration"
                  aria-hidden="true"
                >
                  {gutter.gutters[index] ?? ""}
                </span>
              )}
              <span className="min-w-0">
                {segments.length === 0
                  ? " "
                  : segments.map((segment, segmentIndex) =>
                      // A default-coloured run inherits the `<pre>`'s own class:
                      // wrapping it in a span buys nothing and costs a box to
                      // style, lay out, and paint on every scroll frame.
                      segment.className === CODE_DEFAULT_CLASS ? (
                        <Fragment key={segmentIndex}>{segment.text}</Fragment>
                      ) : (
                        <span className={segment.className} key={segmentIndex}>
                          {segment.text}
                        </span>
                      ),
                    )}
              </span>
            </div>
          ))}
        </code>
      </pre>
    </div>
  );
});

export const Markdown = memo(function Markdown({
  children,
  compact = false,
  hardBreaks = false,
  nested = false,
}: {
  children: string;
  compact?: boolean;
  hardBreaks?: boolean;
  /** Rendered INSIDE an already-framed container (a tool result card): code and
      diff blocks drop their own border so the card shows ONE frame, not two. */
  nested?: boolean;
}) {
  // Transcript prose keeps natural word spacing on narrow screens. Inline code and
  // links remain breakable so a long atom cannot force the whole column to overflow.
  const runningText = PROSE;
  // A heading inside a tool result card is a STRUCTURAL divider — one file in a
  // multi-file `cat`, one occurrence in an index, one step in a batch — not a
  // document hierarchy. The card body is `text-meta` (10px), so `### path · L12-30`
  // rendered at `text-body`/`text-title` towered over the very rows it labels. Nested
  // headings therefore step DOWN to the card's own scale and separate by weight,
  // case and rule instead of by size.
  const heading = (
    nestedClass: string,
    compactClass: string,
    fullClass: string,
  ) => (nested ? nestedClass : compact ? compactClass : fullClass);
  return (
    <div className="min-w-0 break-words [&>:first-child]:mt-0 [&>:last-child]:mb-0">
      <ReactMarkdown
        // A newline the model authored is a HARD break in reasoning: the engine's
        // `reasoning->ast` emits `[:br]` for it and the TUI paints it as its own row.
        // CommonMark would otherwise flow those lines into one paragraph.
        remarkPlugins={hardBreaks ? [remarkGfm, remarkBreaks] : [remarkGfm]}
        components={{
          a: ({ children: label, ...props }) => (
            <a
              {...props}
              className="font-medium text-link underline underline-offset-3 break-all hover:text-link-hover"
              target="_blank"
              rel="noreferrer"
            >
              {label}
            </a>
          ),
          blockquote: ({ children: quote }) => (
            <blockquote
              className={`${compact ? "my-2 pl-3" : "my-3 pl-4"} border-l-2 border-answer-edge text-dialog-hint`}
            >
              {quote}
            </blockquote>
          ),
          code: ({ children: inline }) => (
            <code className={`${INLINE_CODE_CLASS} break-all`}>
              {inline}
            </code>
          ),
          h1: ({ children: heading1 }) => (
            <h1
              className={`${heading("mb-1 mt-3 text-ui", "mb-1.5 mt-4 text-subhead", "mb-2 mt-6 text-head")} border-b-2 border-answer-edge pb-1 font-semibold tracking-[-0.015em] text-heading-1`}
            >
              {heading1}
            </h1>
          ),
          h2: ({ children: heading2 }) => (
            <h2
              className={`${heading("mb-1 mt-3 text-ui", "mb-1 mt-3.5 text-title", "mb-1.5 mt-5 text-subhead")} border-b border-answer-edge pb-0.5 font-semibold tracking-[-0.01em] text-heading-2`}
            >
              {heading2}
            </h2>
          ),
          h3: ({ children: heading3 }) => (
            <h3
              className={`${heading("mb-0.5 mt-2.5 text-meta", "mb-1 mt-3 text-body", "mb-1 mt-4 text-title")} font-semibold text-heading-3`}
            >
              {heading3}
            </h3>
          ),
          h4: ({ children: heading4 }) => (
            <h4
              className={`${heading("mb-0.5 mt-2 text-meta", "mb-0.5 mt-2.5 text-body", "mb-1 mt-3.5 text-body")} font-semibold text-heading-3`}
            >
              {heading4}
            </h4>
          ),
          h5: ({ children: heading5 }) => (
            <h5
              className={`${heading("mb-0.5 mt-2 text-chip uppercase tracking-[0.06em]", "mb-0.5 mt-2.5 text-ui", "mb-1 mt-3 text-ui")} font-semibold text-heading-3`}
            >
              {heading5}
            </h5>
          ),
          h6: ({ children: heading6 }) => (
            <h6
              className={`${heading("mb-0.5 mt-2 text-chip", "mb-0.5 mt-2.5 text-meta", "mb-1 mt-3 text-meta")} font-semibold uppercase tracking-[0.08em] text-heading-3`}
            >
              {heading6}
            </h6>
          ),
          hr: () => (
            <hr className={`${compact ? "my-3" : "my-5"} border-answer-edge`} />
          ),
          li: ({ children: item }) => (
            <li
              className={`${compact ? "my-0.5 pl-0.5" : "my-0.5 pl-1"} ${runningText}`}
            >
              {item}
            </li>
          ),
          ol: ({ children: list }) => (
            <ol
              className={`${compact ? "my-2 pl-5" : "my-3 pl-6"} list-decimal space-y-0.5`}
            >
              {list}
            </ol>
          ),
          p: ({ children: paragraph }) => (
            <p className={`${compact ? "my-2" : "my-2.5"} ${runningText}`}>
              {paragraph}
            </p>
          ),
          pre: ({ children: codeNode }) => {
            const raw = extractText(codeNode).replace(/\n$/, "");
            const language = codeLanguage(codeNode);
            if (
              language === "diff" ||
              language === "patch" ||
              language === "udiff"
            ) {
              return (
                <DiffBlock
                  value={stripMarks(raw)}
                  compact={compact}
                  frameless={nested}
                />
              );
            }
            // A CSV/TSV artifact is DATA: `attach` fences it as `vis-table` and
            // both surfaces paint a real grid — the TUI's table dialog, this table.
            if (language === "vis-table") {
              return (
                <DataTable
                  body={stripMarks(raw)}
                  compact={compact}
                  frameless={nested}
                />
              );
            }
            // A PDF or a note is a DOCUMENT: it never reaches the model, and the
            // fence carries a descriptor only. The attachment rail below the block
            // already shows that artifact as an openable tile, so painting the
            // fence too put the same file on screen twice — the fence itself is
            // rendered as nothing.
            if (language === "vis-doc") return null;
            return (
              <SyntaxCodeBlock
                value={raw}
                language={language}
                compact={compact}
                frameless={nested}
              />
            );
          },
          strong: ({ children: strong }) => (
            <strong className="font-semibold">{strong}</strong>
          ),
          table: ({ children: table }) => (
            <div
              className={`${compact ? "my-2" : "my-3"} max-w-full overflow-x-auto overscroll-x-contain`}
            >
              {/* The table sits on the SAME step as the surrounding prose — hardcoding
                  `text-ui` made a tool result's table one step BIGGER than the compact
                  code blocks (`text-meta`) in the very same card.
                  A CELL IS NOT RUNNING PROSE. Inline `code` and links carry `break-all`,
                  which gives the auto layout a one-character column it is free to starve.
                  the auto layout is free to starve. Measured at 390px: the file column got
                  58px of a 366px bubble and painted `manifest.edn` down six lines, while
                  the scroller around this table had nothing left to scroll. Word-break goes
                  back to `normal` here — the inherited `overflow-wrap: break-word` still
                  breaks a token too long for a line of its own — so a column asks for the
                  width its content needs and a wide table reaches for the scroller. */}
              <table
                className={`w-full border-collapse ${compact ? "text-meta" : "text-ui"} [&_a]:[word-break:normal] [&_code]:[word-break:normal]`}
              >
                {table}
              </table>
            </div>
          ),
          // `align-top`: one wrapping cell makes the whole row tall, and middle-aligned
          // neighbours then float in the middle of that emptiness. Rows read as rows
          // when every cell starts on the first line.
          td: ({ children: cell }) => (
            <td
              className={`${compact ? "px-1.5 py-1" : "px-2 py-1.5"} border border-code-edge text-left align-top`}
            >
              {cell}
            </td>
          ),
          th: ({ children: cell }) => (
            <th
              className={`${compact ? "px-1.5 py-1" : "px-2 py-1.5"} border border-code-edge bg-code text-left align-top font-semibold`}
            >
              {cell}
            </th>
          ),
          ul: ({ children: list }) => (
            <ul
              className={`${compact ? "my-2 pl-5" : "my-3 pl-6"} list-disc space-y-0.5`}
            >
              {list}
            </ul>
          ),
        }}
      >
        {children}
      </ReactMarkdown>
    </div>
  );
});

function extractText(node: ReactNode): string {
  if (typeof node === "string" || typeof node === "number") return String(node);
  if (Array.isArray(node)) return node.map(extractText).join("");
  if (node && typeof node === "object" && "props" in node) {
    return extractText(
      (node as { props: { children?: ReactNode } }).props.children,
    );
  }
  return "";
}

function codeLanguage(node: ReactNode): string {
  if (!isValidElement<{ className?: string }>(node)) return "";
  return (
    /(?:^|\s)language-([^\s]+)/.exec(node.props.className ?? "")?.[1] ?? ""
  );
}

function jsonText(value: JsonValue | unknown): string {
  if (typeof value === "string") return value;
  if (value == null) return "";
  try {
    return JSON.stringify(value, null, 2);
  } catch {
    return String(value);
  }
}

function stripAnsi(value: string): string {
  return value.replace(/\u001b\[[0-?]*[ -/]*[@-~]/g, "");
}

// The engine paints grep needles with reverse video (`ESC[7m … ESC[27m`), exactly
// like the TUI. Markdown cannot carry SGR, so the pair is rewritten into private-use
// sentinels that survive fence parsing and are turned back into a highlighted span
// by the code renderer (and dropped everywhere else).
const MARK_OPEN = "\u0091";
const MARK_CLOSE = "\u0092";
const MARK_SENTINELS = /[\u0091\u0092]/g;
const MARK_CLASS = "bg-accent text-accent-foreground";

function markAnsiHighlights(value: string): string {
  const marked = value
    .replace(MARK_SENTINELS, "")
    .replace(/\u001b\[([0-9;]*)m/g, (match, params: string) => {
      const codes = params === "" ? ["0"] : params.split(";");
      if (codes.includes("7")) return MARK_OPEN;
      if (codes.includes("27") || codes.includes("0")) return MARK_CLOSE;
      return match;
    });
  return stripAnsi(marked);
}

function stripMarks(value: string): string {
  return value.replace(MARK_SENTINELS, "");
}

// Pull the sentinels back out, leaving clean text plus the [from, to) offsets that
// were highlighted — so Prism only ever sees real source.
function extractMarks(value: string): {
  text: string;
  ranges: [number, number][];
} {
  if (!value.includes(MARK_OPEN) && !value.includes(MARK_CLOSE))
    return { text: value, ranges: [] };
  const ranges: [number, number][] = [];
  let text = "";
  let open: number | null = null;
  for (const char of value) {
    if (char === MARK_OPEN) {
      if (open == null) open = text.length;
      continue;
    }
    if (char === MARK_CLOSE) {
      if (open != null && text.length > open) ranges.push([open, text.length]);
      open = null;
      continue;
    }
    text += char;
  }
  if (open != null && text.length > open) ranges.push([open, text.length]);
  return { text, ranges };
}

// Re-cut syntax segments so every highlighted range becomes its own segment; the
// match class replaces the token class outright (reverse video wins, like the TUI).
function applyMarks(
  segments: SyntaxSegment[],
  ranges: [number, number][],
): SyntaxSegment[] {
  if (ranges.length === 0) return segments;
  const out: SyntaxSegment[] = [];
  let offset = 0;
  for (const segment of segments) {
    const start = offset;
    const end = start + segment.text.length;
    offset = end;
    let cursor = start;
    for (const [from, to] of ranges) {
      if (to <= cursor || from >= end) continue;
      const hitFrom = Math.max(from, cursor);
      const hitTo = Math.min(to, end);
      if (hitFrom > cursor) {
        out.push({
          text: segment.text.slice(cursor - start, hitFrom - start),
          className: segment.className,
        });
      }
      out.push({
        text: segment.text.slice(hitFrom - start, hitTo - start),
        className: MARK_CLASS,
      });
      cursor = hitTo;
    }
    if (cursor < end)
      out.push({
        text: segment.text.slice(cursor - start),
        className: segment.className,
      });
  }
  return out;
}

function formatDuration(value?: number): string | null {
  if (value == null || !Number.isFinite(value) || value <= 0) return null;
  const milliseconds = Math.trunc(value);
  if (milliseconds < 1_000) return `${milliseconds}ms`;
  if (milliseconds < 60_000) return `${(milliseconds / 1_000).toFixed(1)}s`;
  const minutes = Math.floor(milliseconds / 60_000);
  return `${minutes}m ${Math.floor((milliseconds % 60_000) / 1_000)}s`;
}

const META_SEPARATOR = " · ";
// Wire statuses a transcript row carries while its turn has NOT finished (the
// engine persists the row at submit; the gateway overlay renames `running` to
// `streaming`). Mirrors `IN_FLIGHT_ROW_STATUSES` in `SessionScreen`.
const IN_FLIGHT_STATUSES = new Set([
  "running",
  "streaming",
  "queued",
  "pending",
]);

function modelPair(value?: {
  provider?: string;
  model?: string;
}): string | null {
  const provider = value?.provider?.replace(/^:/, "").trim();
  const model = value?.model?.trim().replaceAll("/", "-");
  if (provider && model) return `${provider}/${model}`;
  return model || provider || null;
}

function turnRouting(turn: TranscriptTurn) {
  const last = turn.iterations?.at(-1);
  return {
    selected: turn.llm_selected ?? last?.llm_selected,
    actual: turn.llm_actual ?? last?.llm_actual,
    fallback: turn.is_llm_fallback ?? last?.is_llm_fallback ?? false,
    trace: turn.llm_routing_trace ?? last?.llm_routing_trace ?? [],
  };
}

function turnMetaSummary(turn: TranscriptTurn): string | null {
  if (turn.meta_summary?.trim()) return turn.meta_summary.trim();

  const routing = turnRouting(turn);
  const cost =
    typeof turn.cost === "object" && turn.cost ? turn.cost : undefined;
  const model =
    modelPair(routing.actual) ??
    modelPair({
      provider: turn.provider ?? cost?.provider,
      model: turn.model ?? cost?.model,
    });
  const usage = turnUsage(turn);
  const tokens = formatTokens(usage);
  const price = formatCost(usage.cost);
  const duration = formatDuration(turn.duration_ms);
  const parts = [model, tokens, price, duration].filter(
    (part): part is string => Boolean(part),
  );
  return parts.length ? parts.join(META_SEPARATOR) : null;
}

function turnFallbackNote(turn: TranscriptTurn): string | null {
  if (turn.meta_fallback_note?.trim()) return turn.meta_fallback_note.trim();
  const routing = turnRouting(turn);
  const fallbackTypes = new Set([
    "llm.routing/provider-fallback",
    "llm.routing/model-fallback",
    "llm.routing/format-fallback",
  ]);
  const cacheBreakingTypes = new Set([
    "llm.routing/provider-fallback",
    "llm.routing/model-fallback",
  ]);
  // `wire/->wire` transforms Clojure's `:event/type` into `event_type`.
  // Accept `type` too so old persisted traces remain legible.
  const eventType = (item: Record<string, JsonValue>) =>
    String(item.event_type ?? item.type ?? "");
  const scopeOf = (item: Record<string, JsonValue>) =>
    String(item.scope ?? "").replace(/^:/, "");
  const retryEvents = routing.trace.filter(
    (item) => eventType(item) === "llm.routing/provider-retry",
  );
  if (!routing.fallback && !retryEvents.length) return null;

  // A `session-pick` event records that the SESSION was repointed off a dead
  // credential, not how this turn was routed: it is reported at the end of the line,
  // never as the reason the turn moved (issue #154).
  const pickMove = routing.trace.find((item) => scopeOf(item) === "session-pick");
  const fallbackEvent = routing.trace.find(
    (item) => fallbackTypes.has(eventType(item)) && scopeOf(item) !== "session-pick",
  );
  // A changed provider or model means the peer never saw the cache the previous route
  // built, so every following request re-sends the whole context.
  const cacheLost = routing.trace.some((item) =>
    cacheBreakingTypes.has(eventType(item)),
  );
  // A trace value is JSON: an unnamed half of the pair arrives as `null`, which reads
  // as "not named" to `modelPair` only once it is undefined.
  const nameOf = (value: JsonValue | undefined) =>
    typeof value === "string" && value.trim() ? value : undefined;
  const movedTo = pickMove
    ? modelPair({
        provider: nameOf(pickMove.to_provider),
        model: nameOf(pickMove.to_model),
      })
    : null;
  const event = fallbackEvent ?? retryEvents.at(-1);
  const retries = retryEvents.length;
  const from = modelPair(routing.selected) ?? "previous model";
  const status = event?.status;
  const reason = event?.reason;
  const error = event?.error;
  const why =
    status != null
      ? String(status)
      : reason != null
        ? String(reason).replace(/^:/, "")
        : error != null
          ? typeof error === "string"
            ? error
            : jsonText(error)
          : null;
  const tail = [
    why,
    retries > 0 ? `retried ${retries}×` : null,
    cacheLost ? "prompt cache lost" : null,
    movedTo ? `session now on ${movedTo}` : null,
  ].filter((part): part is string => Boolean(part));
  return `↳ from ${from}${tail.length ? ` — ${tail.join(", ")}` : ""}`;
}

function assistantUsage(turn: TranscriptTurn): boolean {
  const usage = turnUsage(turn);
  return usage.input > 0 || usage.output > 0 || usage.cost > 0;
}

function commandTurn(turn: TranscriptTurn): boolean {
  const request = (turn.user_request ?? turn.request ?? "").trimStart();
  return request.startsWith("/") || request.startsWith("!");
}

// A fenced block must not be closable by the content it wraps: file text, tool
// stdout, and pretty-printed JSON can all carry ``` runs of their own, and a
// fixed triple-backtick wrapper then closes EARLY — the rest of the payload
// renders as prose (headings, blockquotes) instead of code. CommonMark allows
// longer fences; pick the shortest safe one. Mirrors `strutil/fenced`.
function fenced(body: string, lang = ""): string {
  const longest = (body.match(/`+/g) ?? []).reduce(
    (max, run) => Math.max(max, run.length),
    0,
  );
  const delimiter = "`".repeat(Math.max(3, longest + 1));
  return `${delimiter}${lang}\n${body}\n${delimiter}`;
}

function resultBody(form: TranscriptForm): string {
  if (form.error != null) return jsonText(form.error);
  const rendered = form.result_render?.trimEnd();
  if (rendered) return markAnsiHighlights(rendered);
  if (form.result_summary?.trim()) return "";
  if (form.result == null || form.result === "") return "";
  const raw = jsonText(form.result);
  return typeof form.result === "string" ? raw : fenced(raw, "json");
}

// A synthetic COMMAND turn carries ONE form the model never wrote: `/reload` is
// persisted as the slash envelope (`run-slash-turn!`, tag `user-slash`) and `!ls`
// as the shell result (`run-bang-turn!`, tag `user-shell`), purely so history and
// resume keep them. The engine stamps those tags so a channel SUPPRESSES the
// trace — the answer band under the turn already is the whole result — and
// without that rule the app painted `/reload` as a PYTHON program with its own
// RESULT band under it.
const COMMAND_FORM_TAGS = new Set(["user-slash", "user-shell"]);

/** A form the trace paints NOTHING for: engine chrome, an answer, or a command. */
function hiddenForm(form: TranscriptForm): boolean {
  return (
    Boolean(form.silent) ||
    form.result === "vis_silent" ||
    form.result === "vis_answer" ||
    COMMAND_FORM_TAGS.has(String(form.tag ?? ""))
  );
}

// Every visible form is its OWN single card: a block's result is ONE result no
// matter how many values it printed, so there is no per-result fan-out here.
function toolCards(form: TranscriptForm): TranscriptForm[] {
  return hiddenForm(form) ? [] : [form];
}

const INLINE_MARK = /[*_`~[]/;

/**
 * ONE LINE A HUMAN WROTE, WITH ITS MARKS — `code`, **bold**, _italic_, ~~struck~~.
 *
 * It renders no element of its own, so the row that carries it keeps its layout,
 * its size and its colour and a mark only changes the WORDS. Block markdown has
 * no inline meaning — a heading, a fence, a list or a table would fight the row
 * it sits in — so the element list is CLOSED and anything else is unwrapped to
 * its text. A link keeps its label and drops its target for the reason a live
 * view keeps its targets in a `link` node: something a human can open is a
 * declaration, never a word inside a sentence that scrolls away.
 *
 * A string carrying no mark never reaches the parser at all — the same fast path
 * the terminal takes (`live_view/markdown-mark`), which is what makes this
 * affordable once per table cell.
 */
export const InlineMarkdown = memo(function InlineMarkdown({
  children,
}: {
  children: string;
}) {
  if (!INLINE_MARK.test(children)) return <>{children}</>;
  return (
    <ReactMarkdown
      remarkPlugins={[remarkGfm]}
      allowedElements={["p", "strong", "em", "del", "code"]}
      unwrapDisallowed
      components={{
        p: ({ children: content }) => <>{content}</>,
        strong: ({ children: content }) => (
          <strong className="font-bold">{content}</strong>
        ),
        em: ({ children: content }) => <em>{content}</em>,
        del: ({ children: content }) => <del>{content}</del>,
        // The code span inherits the line's font size while its own inline box
        // keeps paragraph justification out of the authored value.
        code: ({ children: code }) => (
          <code className={INLINE_CODE_CLASS}>{code}</code>
        ),
      }}
    >
      {children}
    </ReactMarkdown>
  );
});

function ToolSummary({
  children,
  className,
}: {
  children: string;
  className: string;
}) {
  return (
    <span
      className={`min-w-0 flex-1 truncate text-chip font-medium ${className}`}
      title={children}
    >
      <InlineMarkdown>{children}</InlineMarkdown>
    </span>
  );
}

/**
 * THE HEADER BAND OF A TRANSCRIPT CARD, and there is exactly one of it.
 *
 * A tool result with no body, a tool result that opens, and a program's own header
 * are the same row read three times in one column: a name, a summary, a duration and
 * — where there is something to take — the `Copy` chip. So the band owns the height
 * (`min-h-8`, the app's compact step) and the side inset, and it CENTRES what it
 * carries: a 24px `CopyChip` gets 4px of air above and below without the row spelling
 * padding, and a 32px `Disclosure` fills the band instead of adding its own height to
 * it. That padding is exactly what left one program header 41px tall beside a 33px
 * one, while a chip in a third header stood rule-to-rule with no air at all.
 */
const CARD_BAND = "flex min-h-8 items-center gap-1.5 px-2";

const ToolCard = memo(function ToolCard({ form }: { form: TranscriptForm }) {
  const resultText = resultBody(form);
  const failed = form.error != null;
  // Once any real outcome has arrived (body/result/render/duration) a stale
  // "Running…" placeholder from the gateway must not linger — the op is done.
  const hasOutcome =
    resultText !== "" ||
    form.result != null ||
    form.result_render != null ||
    form.duration_ms != null;
  const placeholderSummary = form.result_summary?.trim() === "Running…";
  const rawSummary =
    placeholderSummary && hasOutcome
      ? ""
      : form.result_summary?.trim() || (failed ? "Failed" : "");
  const running =
    !failed && !hasOutcome && (!form.result_summary || placeholderSummary);
  const body = resultText;
  const summary = rawSummary;
  const duration = formatDuration(form.duration_ms);
  // A COLLAPSED result body is not in the DOM at all. Measured on device on a
  // real transcript: those bodies were 52k of the screen's 72k elements, and
  // WebKit computes style for them even though a closed <details> paints
  // nothing — 307ms of style recalc and 653ms to first paint when the session
  // opens, against 100ms/145ms once they are unmounted. `content-visibility`
  // cannot help here: it defers layout and paint, never style. The flag is
  // one-way, so re-collapsing keeps the parsed body for the next open, and
  // "Copy result" copies `body` (the string), never the DOM.
  const [wasOpened, setWasOpened] = useState(false);
  const summaryClass = failed
    ? "text-err"
    : running
      ? "text-code-result"
      : "text-accent-ink";
  // A card wears no OP-NAME badge — GREP, a private transport's _SHELL_WAIT, the
  // op that produced it: a result is its own tally and its own body, and the TUI
  // card (`tool-card-entries`) paints the same way. What a card with NO tally
  // wears is the band's own NAME, because the whole content of that row was a
  // duration: it named nothing, in either channel.
  const headline = (
    <div className="flex min-w-0 flex-1 items-baseline gap-1.5">
      {summary ? (
        <ToolSummary className={summaryClass}>{summary}</ToolSummary>
      ) : (
        !running && <BandLabel className="min-w-0 flex-1">RESULT</BandLabel>
      )}
      {/* A finished call that produced NO summary and NO body still says so: a
          `RESULT 39ms` row reads as a rendering bug rather than as the empty
          result it is. `running` keeps the spinner-less placeholder quiet until
          the outcome actually lands. */}
      {!summary && !body && !running && !failed && (
        <span className="shrink-0 truncate font-mono text-chip font-medium text-code-duration">
          none
        </span>
      )}
      {duration && (
        <span className="shrink-0 font-mono text-chip tabular-nums text-code-duration">
          {duration}
        </span>
      )}
    </div>
  );

  if (!body) {
    return (
      <div
        className={`${CARD_BAND} border-l-2 ${failed ? "border-err" : "border-accent"} bg-result`}
      >
        {headline}
      </div>
    );
  }

  return (
    <details
      className={`group min-w-0 border-l-2 ${failed ? "border-err" : "border-accent"} bg-result`}
      onToggle={(event) => {
        if (event.currentTarget.open) setWasOpened(true);
      }}
    >
      <summary
        className={`${CARD_BAND} list-none cursor-pointer select-none text-code-result hover:bg-hover [&::-webkit-details-marker]:hidden`}
      >
        <ChevronIcon
          className={`size-3 shrink-0 group-open:rotate-90 ${failed ? "text-err" : "text-accent-ink"}`}
        />
        {headline}
        {/* ONE copy control per result card: the body's code blocks are frameless
            inside this card and render no chip of their own. */}
        <CopyChip value={body} label="Copy result" className="shrink-0">
          Copy
        </CopyChip>
      </summary>
      {/* A tool result is SUBORDINATE to the answer it feeds: its body is `text-meta`
          (10px), the step its compact code blocks and diffs already render at, so the
          card is internally uniform and steps down from the answer's `text-ui`. */}
      {wasOpened && (
        <div
          className={`min-w-0 overflow-hidden border-t border-code-edge bg-result px-2.5 py-1.5 text-meta text-code-result ${failed ? "text-code-error-result" : ""}`}
        >
          {failed ? (
            <pre className="m-0 overflow-x-auto whitespace-pre-wrap break-words font-mono text-meta ">
              {body}
            </pre>
          ) : (
            <Markdown compact nested>
              {body}
            </Markdown>
          )}
        </div>
      )}
    </details>
  );
});

function formCode(form: TranscriptForm): string {
  // The canonical formatted surface, falling back to whatever source the event
  // carried. A block is the program the model wrote; there is no second dialect.
  const source = form.display_code ?? form.code ?? form.source ?? form.src;
  return typeof source === "string" ? source.trim() : "";
}

/** Highlighting language for a form's code block; python is the default surface. */
function formCodeLanguage(form: TranscriptForm): string {
  const language = (form.display_language ?? "").trim();
  return language || "python";
}

// The program the model wrote is the evidence on screen in every state — while
// it runs, when it lands, and when it fails. The only sources a block hides are
// one it never had and one no trace paints at all: a command turn's `src` is the
// command itself, so painting it turned `/reload` into a program.
function showFormCode(form: TranscriptForm, code: string): boolean {
  return Boolean(code) && !hiddenForm(form);
}

const PYTHON_PREVIEW_LINES = 5;

const CollapsibleFormCode = memo(function CollapsibleFormCode({
  value,
  label,
  language = "python",
  bare = false,
}: {
  value: string;
  label: string;
  language?: string;
  bare?: boolean;
}) {
  const [expanded, setExpanded] = useState(false);
  const lines = value.split(/\r?\n/);
  const hiddenLines = Math.max(0, lines.length - PYTHON_PREVIEW_LINES);
  const collapsible = hiddenLines > 0;
  const visibleValue =
    collapsible && !expanded
      ? lines.slice(0, PYTHON_PREVIEW_LINES).join("\n")
      : value;
  // Same frame as the result cards this program produced (see `FormTrace`).
  // The disclosure row is a header for the source block beneath it.
  return (
    <div
      className={
        bare
          ? "min-w-0"
          : "mb-1 min-w-0 overflow-hidden border border-dialog-edge bg-dialog-edge shadow-[2px_2px_0_var(--dialog-shadow)]"
      }
    >
      <div className="min-w-0 border-l-2 border-accent bg-code">
        {/* The header row OWNS the copy control (right edge), exactly like the
            `ToolCard` result headline — never a chip floating over the source.
            It is rendered even when the program is too short to collapse, so a
            4-line snippet and a 40-line one carry the same chrome. */}
        {/* The rule belongs to the CARD, not to the band: spelled on the band it ate
            a pixel of the row's own 32px (boxes are border-box here), so the header
            with a `Disclosure` in it measured 33 beside a plain one at 32. */}
        <div className="border-b border-code-edge">
          <div className={CARD_BAND}>
          {collapsible ? (
            <Disclosure
              isOpen={expanded}
              tone="step"
              bleed
              className="min-w-0 flex-1"
              onClick={() => setExpanded((current) => !current)}
            >
              <span className="min-w-0 truncate">
                {label}
                {!expanded && <BandTally> +{hiddenLines} more</BandTally>}
              </span>
            </Disclosure>
          ) : (
            <BandLabel className="min-w-0 flex-1">{label}</BandLabel>
          )}
          <CopyChip value={value} label="Copy code" className="shrink-0">
            Copy
          </CopyChip>
          </div>
        </div>
        <SyntaxCodeBlock
          value={visibleValue}
          copyValue={value}
          language={language}
          compact
          bare
          frameless
        />
      </div>
    </div>
  );
});

const CardGrid = memo(function CardGrid({
  cards,
  live = false,
  bare = false,
}: {
  cards: TranscriptForm[];
  live?: boolean;
  bare?: boolean;
}) {
  if (!cards.length) return null;

  // ONE framed stack per RUN of op-cards, whatever produced them: a python block
  // that printed several results, several native calls in one iteration, or a run
  // of consecutive tool-only iterations (see `IterationTrace`). A frame per call is
  // what read as "some cards are joined, some are not" - one run of work, one frame.
  return (
    <div
      className={`grid grid-cols-[minmax(0,1fr)] gap-px${bare ? "" : " overflow-hidden border border-dialog-edge bg-dialog-edge shadow-[2px_2px_0_var(--dialog-shadow)]"}${live ? ` ${transcriptRiseClass}` : ""}`}
      aria-label={`${cards.length} ${cards.length === 1 ? "result" : "results"}`}
    >
      {cards.map((card, cardIndex) => (
        <ToolCard
          key={`${card.scope ?? card.op ?? "result"}-${cardIndex}`}
          form={card}
        />
      ))}
    </div>
  );
});

const FormTrace = memo(function FormTrace({
  form,
  live = false,
  activity,
}: {
  form: TranscriptForm;
  live?: boolean;
  activity?: ReactNode;
}) {
  if (hiddenForm(form)) return null;
  const code = formCode(form);
  const showCode = showFormCode(form, code);
  const cards = toolCards(form);
  if (!showCode && !cards.length) return null;
  const codeLabel = "PYTHON";

  return (
    <div className={live ? `min-w-0 ${transcriptRiseClass}` : "min-w-0"}>
      {showCode && form.comment?.trim() && (
        <div className="mb-1 bg-thinking-surface px-3 py-1.5 text-ui not-italic text-vis-message">
          <Markdown compact>{form.comment.trim()}</Markdown>
        </div>
      )}
      {/* A program and the results IT produced are ONE frame, joined by the same
          hairline `gap-px` rule that separates sibling cards inside `CardGrid`.
          Whitespace only ever separates one CALL from the next (see the chunk
          gap in `IterationTrace`); a gap here made a result read as if it
          belonged to the program printed BELOW it. */}
      {showCode && cards.length > 0 ? (
        <div className="grid min-w-0 grid-cols-[minmax(0,1fr)] gap-px overflow-hidden border border-dialog-edge bg-dialog-edge shadow-[2px_2px_0_var(--dialog-shadow)]">
          <CollapsibleFormCode
            value={code}
            label={codeLabel}
            language={formCodeLanguage(form)}
            bare
          />
          {activity}
          <CardGrid cards={cards} bare />
        </div>
      ) : (
        <>
          {showCode && (
            <CollapsibleFormCode
              value={code}
              label={codeLabel}
              language={formCodeLanguage(form)}
            />
          )}
          {activity}
          <CardGrid cards={cards} />
        </>
      )}
    </div>
  );
});

const ENCRYPTED_REASONING_PLACEHOLDER =
  "[provider returned encrypted reasoning; plaintext reasoning is unavailable]";

/** Mirrors com.blockether.vis.internal.render/normalize-reasoning. */
function normalizeReasoning(value: string): string {
  return value
    .replace(/[ \t\r\f\v]+\r?\n/g, "\n")
    .replace(/(?:\r?\n){2,}/g, "\n")
    .replace(/([.!?…]["')\]]?)\r?\n(?=\S)/g, "$1\n\n")
    .trim();
}

const REASONING_PREVIEW_LINES = 3;
// Mirrors com.blockether.vis.internal.render/reasoning-collapse-min-hidden (3).
// A disclosure that buys back one or two clipped rows is pure friction — you
// uncollapse just to read one more line — so a barely-overflowing trace renders
// inline, in full, with no toggle at all. Same rule as the TUI band and the
// Clojure transcript split; keep the three in step.
const REASONING_COLLAPSE_MIN_HIDDEN = 3;

// Height measurement batched across the whole transcript: THINKING bands
// deciding what they hide, and finished turns sizing their own paint skip. One
// shared observer and one animation-frame flush prevent a long session from
// doing N independent layout reads on rotation.
//
// A measure READS, and returns the write it wants performed (or nothing). Every
// read in a flush therefore happens before every write: a callback that
// measured and then restyled its own box would invalidate layout for the next
// box in the same flush, and a transcript would lay itself out once per turn.
const boxMeasures = new WeakMap<Element, () => (() => void) | void>();
const observedBoxes = new Set<Element>();
const pendingBoxes = new Set<Element>();
let boxFrame: number | null = null;
let boxObserver: ResizeObserver | null = null;

/**
 * True while WebKit is SKIPPING this subtree for `content-visibility:auto`.
 *
 * Nothing inside a skipped box is laid out, so every metric it reports is the
 * placeholder that was written for it — believing one is how a measurement
 * decays back into a guess.
 */
function isPaintSkipped(box: Element): boolean {
  return typeof box.checkVisibility === "function"
    ? !box.checkVisibility({ contentVisibilityAuto: true })
    : false;
}

function flushBoxes() {
  boxFrame = null;
  if (isViewportRotating()) return;
  const targets = [...pendingBoxes];
  pendingBoxes.clear();
  const writes: Array<() => void> = [];
  for (const box of targets) {
    const write = boxMeasures.get(box)?.();
    if (write) writes.push(write);
  }
  for (const write of writes) write();
}

function scheduleBoxes(boxes: Iterable<Element>) {
  for (const box of boxes) pendingBoxes.add(box);
  if (boxFrame !== null || typeof window === "undefined") return;
  boxFrame = window.requestAnimationFrame(flushBoxes);
}

function observeBox(box: Element, measure: () => (() => void) | void): () => void {
  boxMeasures.set(box, measure);
  observedBoxes.add(box);
  if (typeof ResizeObserver !== "undefined") {
    if (!boxObserver) {
      boxObserver = new ResizeObserver((entries) =>
        scheduleBoxes(entries.map((entry) => entry.target)),
      );
      onViewportRotation((phase) => {
        if (phase === "end") scheduleBoxes(observedBoxes);
      });
    }
    boxObserver.observe(box);
  }
  return () => {
    boxObserver?.unobserve(box);
    observedBoxes.delete(box);
    pendingBoxes.delete(box);
    boxMeasures.delete(box);
  };
}

export const ThinkingBand = memo(function ThinkingBand({
  children,
}: {
  children: string;
}) {
  const normalized = normalizeReasoning(children);
  const bodyRef = useRef<HTMLDivElement>(null);
  const [isExpandRequested, setExpandRequested] = useState(false);
  const [hiddenRows, setHiddenRows] = useState(0);

  useLayoutEffect(() => {
    const body = bodyRef.current;
    if (!body) return;

    const measure = () => {
      // A band inside a turn WebKit is skipping has no laid-out content:
      // `scrollHeight` reads 0, and the disclosure would retire itself — on
      // rotation, for every collapsed band in the transcript at once. A skipped
      // band is measured when it is rendered again, never before.
      if (isPaintSkipped(body)) return;
      const lineHeight =
        Number.parseFloat(window.getComputedStyle(body).lineHeight) || 20;
      const previewHeight = lineHeight * REASONING_PREVIEW_LINES;
      const hiddenHeight = Math.max(0, body.scrollHeight - previewHeight);
      const nextHiddenRows = Math.ceil(hiddenHeight / lineHeight);
      setHiddenRows(
        nextHiddenRows >= REASONING_COLLAPSE_MIN_HIDDEN ? nextHiddenRows : 0,
      );
    };

    measure();
    return observeBox(body, measure);
  }, [normalized]);

  // Collapsing is derived, not stored: a block with nothing hidden is never expanded.
  const expanded = isExpandRequested && hiddenRows > 0;
  if (!normalized || normalized === ENCRYPTED_REASONING_PLACEHOLDER)
    return null;
  const collapsible = hiddenRows >= REASONING_COLLAPSE_MIN_HIDDEN;

  return (
    // A band never pushes ITSELF down: when it opens a step it is the first
    // block of that section and the transcript stack has already spaced it.
    // Spelling the gap twice is what made the whitespace under a picture wider
    // than the whitespace over it.
    <section className="my-2 min-w-0 bg-thinking-surface px-3 py-2 text-ui text-thinking first:mt-0">
      {collapsible && (
        <Disclosure
          isOpen={expanded}
          tone="thinking"
          className="mb-1"
          onClick={() => setExpandRequested((value) => !value)}
        >
          <span className="min-w-0 truncate">
            THINKING
            {!expanded && <BandTally> +{hiddenRows} more</BandTally>}
          </span>
        </Disclosure>
      )}
      <div
        ref={bodyRef}
        className={`${collapsible && !expanded ? "max-h-[3.75rem] overflow-hidden" : ""} min-w-0 italic`}
      >
        <Markdown compact hardBreaks>
          {normalized}
        </Markdown>
      </div>
    </section>
  );
});

// ONE artifact a tool call produced (a matplotlib figure, an `attach`ed
// image). The gateway ships descriptors only, never bytes, so the picture is
// pulled from the attachment endpoint on first paint — with the auth headers an
// `<img src>` cannot carry, hence the object URL. This is the app's twin of the
// TUI's inline image: the SAME produced artifact, painted where it was made.
const AttachmentTile = memo(function AttachmentTile({
  client,
  sid,
  attachment,
  layout,
  galleryAt,
}: {
  client: GatewayClient;
  sid: string;
  attachment: IterationAttachment;
  // A lone artifact is a PLATE with its own caption; from the second one the
  // rail is a gallery and this is one square tile in it (`mediaGroupLayout`).
  layout: MediaLayout;
  // Where this picture sits in the rail's gallery, so the viewer it opens can
  // step to the ones beside it.
  galleryAt?: number;
}) {
  const [url, setUrl] = useState<string | null>(null);
  // Bumped when the browser refuses the URL we handed it — the client's object
  // URL cache is bounded, so a picture parked off-screen long enough can have
  // been revoked under it. Re-asking repopulates the cache; it is not a retry
  // loop, because a genuinely broken artifact gives up after the second try.
  const [attempt, setAttempt] = useState(0);
  const [failed, setFailed] = useState(false);
  const iterationId = attachment.iteration_id ?? "";
  const index = attachment.index ?? 0;
  const isVideo = attachmentIsVideo(attachment);
  const isAudio = attachmentIsAudio(attachment);
  const isPlayable = isVideo || isAudio || attachmentIsImage(attachment);
  const name = attachment.filename || "attachment";

  useEffect(() => {
    if (!isPlayable || !iterationId || !sid) return;
    let alive = true;
    // Hold this artifact's object URL for as long as the tile is mounted. The
    // client's cache is bounded and REVOKES what it evicts, and re-entering a
    // session re-mounts every tile in one tick — without the hold the newest
    // fetches revoke the pictures still decoding beside them and the transcript
    // comes back as `✗ name` placeholders. Released on unmount, so the bound
    // still applies to everything off screen.
    const release = client.retainAttachment(sid, iterationId, index);
    client
      .attachmentUrl(sid, iterationId, index)
      .then((next) => {
        if (alive) setUrl(next);
      })
      .catch(() => {
        if (alive) setFailed(true);
      });
    return () => {
      alive = false;
      release();
    };
  }, [client, sid, iterationId, index, isPlayable, attempt]);

  // A non-visual artifact reaching a tile is the failure path only — the rail
  // below routes files into the collapsed recorded-files row. Decided at mount
  // and never revisited, so this branch cannot resize anything.
  if (!isPlayable || !iterationId) {
    return (
      <div className="mt-2 flex min-w-0 items-center gap-1.5 font-mono text-chip text-footer-muted">
        <ArrowOutIcon className="size-3" />
        <span className="min-w-0 truncate">{name}</span>
      </div>
    );
  }

  // A RECORDING is a ROW, not a plate: there is no picture to reserve a box for,
  // and the reader's whole question about it is answered by its name and the one
  // control that starts it.
  if (isAudio) {
    return (
      <MediaRecording
        name={name}
        meta={mediaMeta(attachment)}
        transcription={attachment.transcription}
      >
        {failed ? (
          <span className="flex min-w-0 items-center gap-1.5 font-mono text-chip text-footer-muted">
            <AlertIcon className="size-3" />
            <span className="min-w-0 truncate">{name}</span>
          </span>
        ) : !url ? (
          <div
            className="h-11 w-full animate-pulse bg-thinking-surface"
            aria-hidden="true"
          />
        ) : (
          <audio
            src={url}
            controls
            preload="metadata"
            onError={() => setFailed(true)}
            className="h-11 w-full"
          />
        )}
      </MediaRecording>
    );
  }

  // ONE reserved box for the whole life of the slot — see `lib/media-frame`. A
  // failure that arrives after the bytes were requested says so INSIDE the box
  // it already holds: collapsing to a text line here would shove the reader
  // just as hard as growing did. The box belongs to `MediaPlate`/`MediaTile`,
  // which is why nothing below can change it.
  const isTile = layout === "grid";
  const body = failed ? (
    <div
      className={`flex h-full w-full items-center gap-1.5 bg-thinking-surface font-mono text-chip text-footer-muted ${
        isTile ? "justify-center" : "px-2"
      }`}
    >
      <AlertIcon className="size-3" />
      {isTile ? null : <span className="min-w-0 truncate">{name}</span>}
    </div>
  ) : !url ? (
    <div className={mediaPendingClass} aria-hidden="true" />
  ) : isVideo ? (
    // A clip PLAYS in place, with the platform's own controls. It streams from
    // the same attachment endpoint as the pictures, and `preload="metadata"`
    // means a transcript full of clips costs a poster frame, not the bytes.
    <video
      src={url}
      controls
      playsInline
      preload="metadata"
      onError={() => setFailed(true)}
      className={mediaContentClass}
    />
  ) : (
    <ExpandableImage
      src={url}
      alt={name}
      galleryAt={galleryAt}
      loading="lazy"
      decoding="async"
      frameClassName="h-full w-full"
      onError={() => {
        if (attempt >= 2) {
          setFailed(true);
          return;
        }
        setUrl(null);
        setAttempt((current) => current + 1);
      }}
      className={isTile ? mediaTileContentClass : mediaContentClass}
    />
  );

  if (isTile) return <MediaTile>{body}</MediaTile>;
  return (
    <MediaPlate
      name={name}
      meta={mediaMeta(attachment)}
    >
      {body}
    </MediaPlate>
  );
});

// Non-image artifacts (a csv, a zip, a report a tool attached) are RECORDED,
// not painted: `attach` writes them to the session DB and they NEVER enter the
// provider conversation — only images replay, see `loop.clj`'s
// `attachment->image-block`. One naked line per file buried the transcript, so
// they collapse to a single disclosure row ("↗ name +N more") that opens into
// the full recorded list with media type and size.
type RecordedFile = {
  key: string;
  name: string;
  media: string;
  size?: number;
  count: number;
};

// Same file written by three attempts of the same block is ONE recorded thing
// with a count, not three identical rows.
function recordedFiles(attachments: IterationAttachment[]): RecordedFile[] {
  const byIdentity = new Map<string, RecordedFile>();
  attachments.forEach((attachment) => {
    const name = attachment.filename || "attachment";
    const key = `${name}:${attachment.size ?? 0}`;
    const seen = byIdentity.get(key);
    if (seen) seen.count += 1;
    else {
      byIdentity.set(key, {
        key,
        name,
        media: attachment.media_type ?? "",
        size: attachment.size,
        count: 1,
      });
    }
  });
  return [...byIdentity.values()];
}

// The reader for a document artifact. The bytes are fetched once the tile is on
// screen and land inside `DocPreview`'s sandboxed frame, which is a separate
// document with its own CSS scope and an opaque origin — untrusted markup can
// neither restyle the app nor read its storage. See `DocArtifact`.
const AttachmentDocTile = memo(function AttachmentDocTile({
  client,
  sid,
  attachment,
  versions,
}: {
  client: GatewayClient;
  sid: string;
  attachment: IterationAttachment;
  /** Every cut of this name in the step, newest first and this one included. */
  versions: IterationAttachment[];
}) {
  const [url, setUrl] = useState<string | null>(null);
  const [failed, setFailed] = useState(false);
  const [wanted, setWanted] = useState(false);
  // The ROW is always the newest cut. The reader may go back through the band's
  // own version cell, and then it is that cut's bytes the tile fetches.
  const [shownAt, setShownAt] = useState(0);
  const cut = versions[shownAt] ?? attachment;
  const iterationId = cut.iteration_id ?? "";
  const index = cut.index ?? 0;
  const name = attachment.filename || "document";
  const needed = useCallback(() => setWanted(true), []);

  useEffect(() => {
    if (!wanted || !iterationId || !sid) return;
    let alive = true;
    // Another cut is another set of bytes: the ones on screen are dropped rather
    // than left painting the version that was just left behind.
    setUrl(null);
    setFailed(false);
    const release = client.retainAttachment(sid, iterationId, index);
    client
      .attachmentUrl(sid, iterationId, index)
      .then((next) => {
        if (alive) setUrl(next);
      })
      .catch(() => {
        if (alive) setFailed(true);
      });
    return () => {
      alive = false;
      release();
    };
  }, [client, sid, iterationId, index, wanted]);

  if (!iterationId) return null;

  return (
    <DocPreview
      name={name}
      mime={attachment.media_type ?? ""}
      sizeLabel={attachmentBytes(attachment.size)}
      url={url}
      failed={failed}
      // Opened from the transcript, the artifact is markable up exactly as it is
      // from the artifacts sheet: a note takes comments, a PDF takes ink, and
      // saving either one is the next version of the same filename.
      annotate={{ client, sid, iterationId }}
      onNeeded={needed}
      versions={versions}
      shownAt={shownAt}
      onPick={setShownAt}
    />
  );
});

export const AttachmentRail = memo(function AttachmentRail({
  client,
  sid,
  attachments,
}: {
  client: GatewayClient;
  sid: string;
  attachments: IterationAttachment[];
}) {
  const [open, setOpen] = useState(false);
  // ONE page of media at a time — see `pageBySize`. Every tile that mounts asks
  // the gateway for its bytes, so an iteration that produced forty figures used
  // to start forty downloads in the same tick, on whatever connection a phone
  // happens to have. Revealed a page at a time, by count AND by weight.
  const [pages, setPages] = useState(1);
  // ONE ROW PER ARTIFACT, NOT PER CUT.
  //
  // Regression, user report: commenting on a note and saving it turned one
  // attached document into two rows. Re-attaching a filename is the NEXT VERSION
  // of that artifact — the gallery has collapsed those threads ever since
  // `collapseArtifactVersions` — but this rail painted a row per descriptor, so
  // the human's own revision arrived as a second row under the same name and the
  // group header summed both cuts ("2 documents · 25.6KB" over one 12.7KB note).
  // The newest cut is the row; the rest is the history the reader opens.
  const threads = collapseAttachmentVersions(
    attachments.filter(
      (entry) => attachmentIsPlayable(entry) || attachmentIsDoc(entry),
    ),
  );
  const media = threads.map((thread) => thread[0]);
  const threadOf = new Map(media.map((head, at) => [head, threads[at]]));
  const page = pageBySize(media, (entry) => entry.size, pages, RAIL_PAGE);
  const playable = page.shown.filter(attachmentIsPlayable);
  // A clip is never a gallery tile: at ~183px the platform's own controls do not
  // fit, and a still frame with no way to start it is a picture that lies. Clips
  // keep the plate; the pictures beside them still become the gallery. A recording
  // has no frame at all, so it leaves the picture rail entirely.
  const recordings = playable.filter(attachmentIsAudio);
  const clips = playable.filter(attachmentIsVideo);
  const pictures = playable.filter(
    (entry) => !attachmentIsVideo(entry) && !attachmentIsAudio(entry),
  );
  const layout = mediaGroupLayout(pictures.length);
  const gallery = pictures.map((attachment, at) => (
    <AttachmentTile
      key={`${attachment.iteration_id ?? "iter"}-${attachment.index}`}
      client={client}
      sid={sid}
      attachment={attachment}
      layout={layout}
      galleryAt={at}
    />
  ));
  // A tile with no iteration to fetch from paints nothing, so it must not count
  // towards the stack's own report either.
  const docs = page.shown.filter(
    (entry) => attachmentIsDoc(entry) && entry.iteration_id,
  );
  // A SETTLED RUN IS A ROW OF THE TRACE, NOT A RECORDED FILE.
  //
  // Reported from the app: a finished `gh` watch read as "1 file ·
  // release.live.ndjson" under the recorded-files disclosure — an unnamed line,
  // while the record behind it holds the picture the run ended on and its whole
  // log. It is an artifact this app can open, so it gets an artifact's row.
  const runs = collapseAttachmentVersions(
    attachments.filter(
      (entry) =>
        attachmentIsLive(entry) &&
        entry.classification !== "activity" &&
        entry.iteration_id,
    ),
  ).map((thread) => thread[0]);
  const files = recordedFiles(
    attachments.filter(
      (entry) =>
        !attachmentIsPlayable(entry) &&
        !attachmentIsDoc(entry) &&
        !attachmentIsLive(entry),
    ),
  );
  const total = files.reduce((sum, file) => sum + file.count, 0);
  const head = files[0];
  const rest = head ? total - head.count : 0;

  return (
    <>
      {recordings.map((attachment) => (
        <AttachmentTile
          key={`rec-${attachment.iteration_id ?? "iter"}-${attachment.index}`}
          client={client}
          sid={sid}
          attachment={attachment}
          layout="plate"
        />
      ))}
      {clips.map((attachment) => (
        <AttachmentTile
          key={`${attachment.iteration_id ?? "iter"}-${attachment.index}`}
          client={client}
          sid={sid}
          attachment={attachment}
          layout="plate"
        />
      ))}
      {layout === "grid" ? (
        <MediaGrid summary={mediaSummary(pictures)}>{gallery}</MediaGrid>
      ) : (
        gallery
      )}
      {docs.length > 0 && (
        // One frame for the step's documents, and the header only when there is a
        // GROUP to report: a single document is one row and no header at all.
        // `docs` are ARTIFACTS, one per name, so a revised note never becomes a
        // group of its own cuts.
        <DocStack summary={docs.length > 1 ? docStackSummary(docs) : undefined}>
          {docs.map((attachment) => (
            <AttachmentDocTile
              key={`doc-${attachment.iteration_id ?? "iter"}-${attachment.index}`}
              client={client}
              sid={sid}
              attachment={attachment}
              versions={threadOf.get(attachment) ?? [attachment]}
            />
          ))}
        </DocStack>
      )}
      {runs.length > 0 && (
        // The same frame the documents stand in, and the same rule: a run is a
        // ROW that opens, never a panel painted in place — a run that logged
        // thousands of lines would stand taller than the turn that made it.
        <DocStack>
          {runs.map((attachment) => (
            <LiveRunRow
              key={`run-${attachment.iteration_id ?? "iter"}-${attachment.index}`}
              client={client}
              sid={sid}
              attachment={attachment}
            />
          ))}
        </DocStack>
      )}
      {page.rest.length > 0 && (
        <LoadMore
          label={`Load ${page.restLabel} of attachments`}
          onClick={() => setPages((current) => current + 1)}
        >
          Load {page.restLabel}
        </LoadMore>
      )}
      {head && (
        <div className="mt-2 min-w-0">
          <Disclosure
            isOpen={open}
            onClick={() => setOpen((current) => !current)}
          >
            <ArrowOutIcon className="size-3 shrink-0 opacity-70" />
            <span className="min-w-0 truncate">
              {head.name}
              {head.count > 1 ? ` ×${head.count}` : ""}
            </span>
            {rest > 0 && (
              <span className="shrink-0 opacity-70">+{rest} more</span>
            )}
          </Disclosure>
          {open && (
            <ul className="grid min-w-0 gap-0.5 pl-4">
              {files.map((file) => (
                <li
                  key={file.key}
                  className="min-w-0 truncate font-mono text-chip text-footer-muted"
                >
                  {[
                    file.count > 1 ? `${file.name} ×${file.count}` : file.name,
                    file.media,
                    attachmentBytes(file.size),
                  ]
                    .filter(Boolean)
                    .join(" · ")}
                </li>
              ))}
            </ul>
          )}
        </div>
      )}
    </>
  );
});

// One turn can be 400+ tool calls, and mounting every segment of it in the frame
// that opens the session IS the open: measured on device, the click-to-paint gap
// was one 509 ms blocked frame, ~250 ms of it remark parsing prose and code that
// lands hundreds of screens above the fold. The transcript opens PINNED TO THE
// BOTTOM, so the tail is the only part anyone can see. Mount that tail, then ramp
// the rest a chunk per frame, holding the reader's pixel while the page grows
// above them (`overflow-anchor` is off on this scroller, so nobody else will).
const SEGMENT_FIRST_PAINT = 8;
// A step costs the same whatever its size — one reconcile of the whole trace,
// one style pass, one paint — and only the nodes it mounts scale with it.
// Measured on device (iPhone 17 Pro, a 30-turn session, "Load earlier"):
// 140 nodes in a step cost 81 ms, 2 105 nodes in ONE commit cost 160 ms, so the
// fixed part is ~70 ms and the marginal part ~0.05 ms per node. The step size
// is therefore not a work budget at all: it counts how many times that 70 ms is
// paid. The controller before this one aimed each step at 6 ms of "work" it
// never measured (the field was written nowhere) and halved on any frame over
// 32 ms — which EVERY step overruns — so it sat on its floor and paid the fixed
// cost once per two segments: 20 000 nodes took 6.7 s of 30-200 ms frames, and
// a reader scrolling up chased bare paper the whole way. Triple while a step
// stays inside its target, halve only when one really hurt.
const SEGMENT_RAMP_START = 16;
const SEGMENT_RAMP_MIN = 8;
const SEGMENT_RAMP_MAX = 128;
/** Each whole step multiplies by this, so a 400-call turn lands in ~10 frames. */
const RAMP_GROWTH = 3;
/** A step is allowed to cost this much end to end; under it, grow. */
const RAMP_STEP_TARGET_MS = 100;
/** Over this the step really did hurt the scroll: halve and re-learn. */
const RAMP_STEP_LONG_MS = 200;

// A screen holds several traces (one per turn), and if they all ramp at once
// every frame pays for several mounts and several forced layouts while each
// trace's stopwatch is really timing its neighbours — so all of them read "too
// expensive" and shrink to the minimum, which is the stutter. Exactly one trace
// backfills at a time, and it is the LAST one mounted: the turn at the bottom,
// where the reader is.
const rampQueue: symbol[] = [];

/** True when this trace is the bottom-most one still ramping. */
function claimRamp(id: symbol): boolean {
  if (!rampQueue.includes(id)) rampQueue.push(id);
  return rampQueue[rampQueue.length - 1] === id;
}

function releaseRamp(id: symbol): void {
  const at = rampQueue.indexOf(id);
  if (at >= 0) rampQueue.splice(at, 1);
}

/**
 * The element the reader's eye is on: whatever sits at the top edge of the
 * scroller, plus where that edge is right now. Putting THIS element back after
 * a mutation is the only correction that survives content which shrinks as well
 * as grows, and it self-corrects against any other corrector that moved the
 * scroller in the same layout pass — the drift it measures is already zero.
 */

/**
 * Trimmed Markdown of every PROSE block in a settled answer, plus the answer
 * `fallbackAnswer` PROMOTES out of the last iteration when the row carries no
 * content blocks — exactly the strings the answer band under the trace paints.
 */
export function answeredProse(
  blocks: readonly ContentBlock[] | undefined,
  promoted = "",
): ReadonlySet<string> {
  const answered = new Set<string>();
  for (const block of blocks ?? []) {
    if (block.type !== "prose") continue;
    const markdown = block.markdown?.trim();
    if (markdown) answered.add(markdown);
  }
  const promotedAnswer = promoted.trim();
  if (promotedAnswer) answered.add(promotedAnswer);
  return answered;
}

/** Stable empty answer, for a trace painted with no answer beside it. */
const NOTHING_ANSWERED: ReadonlySet<string> = new Set<string>();

/**
 * The commentary this iteration paints INSIDE the trace, minus any copy of the
 * settled answer.
 *
 * Regression, issue #145: the model narrates its answer and then hands the SAME
 * text to `done(...)`, so the row carries it both as the iteration's
 * `assistant_prose` (`prose-beyond-code` only strips prose restating the CODE)
 * and as the turn's answer block — and the reader saw one answer twice, at the
 * trace's width and again at the answer band's. The match is exact on the
 * trimmed text, so commentary that merely resembles the answer survives.
 */
function traceProse(
  iteration: TranscriptIteration,
  answered: ReadonlySet<string>,
): string {
  const prose = iteration.assistant_prose?.trim() ?? "";
  return answered.has(prose) ? "" : prose;
}

function traceEntry(
  iteration: TranscriptIteration,
  index: number,
  answered: ReadonlySet<string>,
) {
  return {
    iteration,
    index,
    thinking: iteration.thinking?.trim() ?? "",
    prose: traceProse(iteration, answered),
    forms: iteration.forms ?? [],
    attachments: iteration.attachments ?? [],
  };
}

type TraceEntry = ReturnType<typeof traceEntry>;
type TraceSegmentData = {
  key: string;
  head: TraceEntry;
  items: TraceEntry[];
  closed: boolean;
};
type Chunk =
  | {
      kind: "code";
      key: string;
      form: TranscriptForm;
      iterationPosition?: number;
      formIndex: number;
      attachment?: IterationAttachment;
    }
  | { kind: "cards"; key: string; cards: TranscriptForm[] };

function activityAnchor(
  value: unknown,
): { iteration: number; formIndex: number } | null {
  if (!value || typeof value !== "object") return null;
  const anchor = value as Record<string, unknown>;
  const iteration = anchor.iteration;
  const formIndex = anchor.form_index;
  return typeof iteration === "number" && typeof formIndex === "number"
    ? { iteration, formIndex }
    : null;
}

function activityAt(
  views: LiveViewModel[],
  iterationPosition: number | undefined,
  formIndex: number,
): LiveViewModel | undefined {
  return views.find((view) => {
    const anchor = activityAnchor(view.activity?.anchor);
    return (
      anchor !== null &&
      anchor.iteration === iterationPosition &&
      anchor.formIndex === formIndex
    );
  });
}

const NO_LIVE_ACTIVITIES: LiveViewModel[] = [];

// Consecutive TOOL-ONLY iterations are one run of work, not N bubbles: the model
// kept calling tools without saying anything in between. Mirrors the TUI
// (`render/merge-iteration-entries`): a narrated iteration may OPEN a run (its
// thinking / prose renders above the cards), an interior narrated call closes it,
// and so does an iteration that produced attachments (those render last).
function buildSegments(
  iterations: TranscriptIteration[],
  answered: ReadonlySet<string> = NOTHING_ANSWERED,
): TraceSegmentData[] {
  const visible = iterations
    .map((iteration, index) => traceEntry(iteration, index, answered))
    .filter(
      ({ thinking, prose, forms, attachments }) =>
        thinking ||
        prose ||
        attachments.length ||
        forms.some(
          (form) =>
            showFormCode(form, formCode(form)) || toolCards(form).length,
        ),
    );

  const segments: TraceSegmentData[] = [];
  visible.forEach((entry) => {
    const open = segments.at(-1);
    if (open && !open.closed && !entry.thinking && !entry.prose)
      open.items.push(entry);
    else {
      segments.push({
        key: String(
          entry.iteration.id ?? entry.iteration.position ?? entry.index,
        ),
        head: entry,
        items: [entry],
        closed: false,
      });
    }
    if (entry.attachments.length) segments[segments.length - 1].closed = true;
  });
  return segments;
}

// A segment renders ONCE and then holds still. The ramp below bumps a counter on
// this component's parent every frame, so without a memo boundary here every
// frame would re-render every segment already on screen -- quadratic over a
// transcript, and measurably so: a 400-call session spent 1.8 s of main thread
// re-rendering settled cards.
//
// Identity alone is not that boundary while a turn STREAMS. One delta hands the
// screen a new `iterations` array (`reduceLiveEvent` rebuilds it to grow the
// tail), `buildSegments` runs again, and every segment it returns is a fresh
// object — so a `memo` comparing identity re-rendered the whole trace on every
// flush, ~7 times a second, for a turn whose settled iterations cannot change.
// The entries are pure derivations of the iteration objects and THOSE keep
// their identity across a flush, so the entries are what to compare.
type TraceSegmentProps = {
  segment: TraceSegmentData;
  live: boolean;
  client?: GatewayClient;
  sid?: string;
  liveActivities: LiveViewModel[];
};

/**
 * Same iteration, same trace: `thinking`/`prose` carry the `answered` set's
 * effect, and `forms`/`attachments` come straight off the iteration this entry
 * was built from.
 */
function sameTraceEntry(a: TraceEntry, b: TraceEntry): boolean {
  return (
    a.iteration === b.iteration &&
    a.index === b.index &&
    a.thinking === b.thinking &&
    a.prose === b.prose
  );
}

function sameTraceSegment(
  a: TraceSegmentProps,
  b: TraceSegmentProps,
): boolean {
  if (
    a.live !== b.live ||
    a.client !== b.client ||
    a.sid !== b.sid ||
    a.liveActivities !== b.liveActivities
  )
    return false;
  const before = a.segment;
  const after = b.segment;
  if (before === after) return true;
  return (
    before.key === after.key &&
    before.closed === after.closed &&
    before.items.length === after.items.length &&
    sameTraceEntry(before.head, after.head) &&
    before.items.every((entry, index) =>
      sameTraceEntry(entry, after.items[index]),
    )
  );
}

const TraceSegment = memo(function TraceSegment({
  segment,
  live,
  client,
  sid,
  liveActivities,
}: TraceSegmentProps) {
  // Inside a segment, adjacent code-less forms pool into ONE grid; a python
  // block keeps its own frame under its source and starts a new pool after it.
  const chunks = useMemo(() => {
    const built: Chunk[] = [];
    segment.items.forEach((entry) => {
      entry.forms.forEach((form, formIndex) => {
        if (hiddenForm(form)) return;
        const key = `${entry.index}-${formIndex}-${form.scope ?? "form"}`;
        if (showFormCode(form, formCode(form))) {
          built.push({
            kind: "code",
            key,
            form,
            iterationPosition: entry.iteration.position,
            formIndex,
            attachment: entry.attachments.find(
              (candidate) =>
                candidate.classification === "activity" &&
                candidate.activity_anchor?.iteration ===
                  entry.iteration.position &&
                candidate.activity_anchor?.form_index === formIndex,
            ),
          });
          return;
        }
        const cards = toolCards(form);
        if (!cards.length) return;
        const pool = built.at(-1);
        if (pool?.kind === "cards") pool.cards.push(...cards);
        else built.push({ kind: "cards", key, cards: [...cards] });
      });
    });
    return built;
  }, [segment]);
  const attachments = useMemo(
    () => segment.items.flatMap((entry) => entry.attachments),
    [segment],
  );

  return (
    <section className={live ? `min-w-0 ${transcriptEnterClass}` : "min-w-0"}>
      {segment.head.thinking && (
        <ThinkingBand>{segment.head.thinking}</ThinkingBand>
      )}
      {segment.head.prose && (
        // Same rhythm as every other block in the stack: the gap above this
        // prose is the stack's, so neither the block nor its first paragraph
        // adds one of its own on top of it.
        <div className="mb-2.5 text-ui text-vis-message [&>:first-child]:mt-0">
          <Markdown>{segment.head.prose}</Markdown>
        </div>
      )}
      {/* Chunk-to-chunk breathing room: each chunk is one call (its program
          glued to its own results), so the ONLY whitespace in the stack
          falls BETWEEN calls. */}
      {chunks.length > 0 && (
        <div className="grid min-w-0 gap-2.5">
          {chunks.map((chunk) => {
            if (chunk.kind === "code") {
              const liveActivity = activityAt(
                liveActivities,
                chunk.iterationPosition,
                chunk.formIndex,
              );
              const activity = liveActivity ? (
                <LiveViewPanel
                  view={liveActivity}
                  isSettled={liveActivity.is_settled}
                  endedAt={liveActivity.ended_at}
                />
              ) : client && sid && chunk.attachment ? (
                <LiveRunRow
                  client={client}
                  sid={sid}
                  attachment={chunk.attachment}
                />
              ) : undefined;
              return (
                <FormTrace
                  key={chunk.key}
                  form={chunk.form}
                  live={live}
                  activity={activity}
                />
              );
            }
            return (
              <CardGrid key={chunk.key} cards={chunk.cards} live={live} />
            );
          })}
        </div>
      )}
      {client && sid && (
        <AttachmentRail client={client} sid={sid} attachments={attachments} />
      )}
    </section>
  );
}, sameTraceSegment);

export const IterationTrace = memo(function IterationTrace({
  iterations,
  answered = NOTHING_ANSWERED,
  live = false,
  whole = false,
  client,
  sid,
  liveActivities = NO_LIVE_ACTIVITIES,
}: {
  iterations: TranscriptIteration[];
  /** Prose the ANSWER band already paints — see `answeredProse`. */
  answered?: ReadonlySet<string>;
  live?: boolean;
  /**
   * Mount every segment in the FIRST paint and ramp nothing.
   *
   * The ramp below buys a short frame when a trace arrives on a screen that has
   * none of it yet. A trace that REPLACES one already painted has no such frame
   * to protect — the live bubble drew these very segments a moment ago — and
   * ramping again is pure subtraction: the transcript drops back to
   * `SEGMENT_FIRST_PAINT` segments for a frame and grows the rest back over the
   * next few. Measured on the live-to-settled handover of a short turn, 102 DOM
   * nodes and 378 px left the scroller and returned 8 ms later, which the
   * reader sees as the whole conversation jerking down and back; on a long
   * answer the collapse is most of the transcript, so the screen empties, the
   * answer leaves the fold, and the corrector chases it back to the bottom.
   */
  whole?: boolean;
  client?: GatewayClient;
  sid?: string;
  /** Host Activity views, placed by 1-based iteration and 0-based form anchors. */
  liveActivities?: LiveViewModel[];
}) {
  const rootRef = useRef<HTMLDivElement>(null);
  // Identity in the ramp queue, so only the bottom-most trace backfills at once.
  const [rampId] = useState(() => Symbol("trace-ramp"));
  useEffect(() => () => releaseRamp(rampId), [rampId]);
  // Adaptive ramp step: how many segments the next frame mounts, and when the
  // current one started (0 = none in flight).
  const stepRef = useRef({ size: SEGMENT_RAMP_START, startedAt: 0 });

  const segments = useMemo(
    () => buildSegments(iterations, answered),
    [iterations, answered],
  );

  // How many segments at the START of the trace are still held back. The ramp
  // only ever SHRINKS it, which is what makes it safe on a turn that is still
  // being written: counted from the END instead, every segment the agent
  // streams would slide the mounted window down by one and DROP the oldest
  // segment on screen — content above the reader leaving the scroller for a
  // frame and coming back, with the screen's anchor corrector chasing it both
  // ways. Measured on the simulator as a -294 px write followed by +294 px
  // 39 ms later, on a transcript nobody was touching.
  const [hiddenSegments, setHiddenSegments] = useState(() =>
    whole ? 0 : Math.max(0, segments.length - SEGMENT_FIRST_PAINT),
  );

  // `whole` also arrives LATE, and it has to count then too. WHICH reconcile
  // tick retires the live bubble is not this trace's business: the settled row
  // can mount a tick BEFORE the bubble is dropped — the registry still calls the
  // turn running when its finished row lands — and read only as the initial
  // state, `whole` changed nothing for exactly those handovers. The collapse
  // documented above came back for them. Derived per render, it costs no commit
  // and no frame.
  const hidden = whole ? 0 : hiddenSegments;

  const rampDone = hidden <= 0;

  // A chunk per frame, so the work the first paint skipped never lands as one
  // long frame either.
  useEffect(() => {
    // A late `whole` also has to STICK. It arrives as a prop and can leave the
    // same way — the NEXT turn's handover moves the flag to its own row — so a
    // trace that showed everything only because the flag was up would collapse
    // the moment it left. Bank it in the ramp's own state, which never hands
    // a segment back.
    if (whole) {
      setHiddenSegments(0);
      releaseRamp(rampId);
      return;
    }
    if (rampDone) {
      releaseRamp(rampId);
      return;
    }
    let frame = 0;
    let waited = false;
    const tick = () => {
      // Wait our turn: a trace below is still backfilling, and two ramps in one
      // frame is what makes both of them look expensive.
      if (!claimRamp(rampId)) {
        waited = true;
        frame = window.requestAnimationFrame(tick);
        return;
      }
      // One rAF to the next spans the previous step end to end, paint included.
      // After waiting, that span is somebody else's work: do not price on it.
      const step = stepRef.current;
      if (step.startedAt > 0 && !waited) {
        const frameCost = performance.now() - step.startedAt;
        const next =
          frameCost > RAMP_STEP_LONG_MS
            ? Math.floor(step.size / 2)
            : frameCost < RAMP_STEP_TARGET_MS
              ? step.size * RAMP_GROWTH
              : step.size;
        step.size = Math.min(
          SEGMENT_RAMP_MAX,
          Math.max(SEGMENT_RAMP_MIN, next),
        );
      }

      step.startedAt = performance.now();
      setHiddenSegments((count) => Math.max(0, count - step.size));
    };
    frame = window.requestAnimationFrame(tick);
    return () => window.cancelAnimationFrame(frame);
  }, [hiddenSegments, rampDone, rampId, whole]);

  if (!segments.length) return null;
  const shown = rampDone ? segments : segments.slice(hidden);

  return (
    <div ref={rootRef} className="mb-2.5 grid gap-2.5">
      {shown.map((segment) => (
        <TraceSegment
          key={segment.key}
          segment={segment}
          live={live}
          client={client}
          sid={sid}
          liveActivities={liveActivities}
        />
      ))}
    </div>
  );
});

const speechDuration = (text: string) =>
  Math.max(1, text.trim().split(/\s+/).filter(Boolean).length / 2.5);

const speechTime = (seconds: number) => {
  const whole = Math.max(0, Math.round(seconds));
  return `${Math.floor(whole / 60)}:${String(whole % 60).padStart(2, "0")}`;
};

const speechFrom = (text: string, position: number) => {
  if (position <= 0) return text;
  const approximate = Math.min(text.length - 1, Math.floor(text.length * position));
  const boundary = text.indexOf(" ", approximate);
  return text.slice(boundary < 0 ? approximate : boundary + 1).trimStart();
};

export function SpeechBlock({ text }: { text: string }) {
  const [open, setOpen] = useState(true);
  const [speaking, setSpeaking] = useState(false);
  const [position, setPosition] = useState(0);
  const [track, setTrack] = useState<SpokenTrack | null>(null);
  const [error, setError] = useState<string | null>(null);
  const positionRef = useRef(0);
  const runRef = useRef(0);
  const startedAtRef = useRef(0);
  const fromRef = useRef(0);
  const measuredRef = useRef(false);
  // A measured reply says how long it is; a reply nobody has synthesised yet is
  // counted from its own words, which is a guess and is replaced the moment the
  // machine hands over audio.
  const duration = track?.duration ?? speechDuration(text);
  const durationRef = useRef(duration);
  durationRef.current = duration;

  const rememberPosition = (next: number) => {
    const clamped = Math.max(0, Math.min(1, next));
    positionRef.current = clamped;
    setPosition(clamped);
  };

  const stop = useCallback(() => {
    runRef.current += 1;
    speechOutput.stop();
    setSpeaking(false);
  }, []);

  const play = useCallback(
    (requested = positionRef.current) => {
      const from = requested >= 0.995 ? 0 : requested;
      const spoken = speechFrom(text, from);
      const run = runRef.current + 1;
      runRef.current = run;
      speechOutput.stop();
      fromRef.current = from;
      measuredRef.current = false;
      rememberPosition(from);
      startedAtRef.current = performance.now() - from * durationRef.current * 1000;
      setError(null);
      setSpeaking(true);
      void speechOutput
        .speak(spoken, {
          // Only a run that speaks the WHOLE reply may draw the whole reply: audio for
          // a tail is the shape of that tail, and the bars are a claim about samples.
          onTrack: (measured) => {
            if (runRef.current !== run || from > 0) return;
            setTrack(measured);
          },
          onProgress: (seconds) => {
            if (runRef.current !== run) return;
            measuredRef.current = true;
            rememberPosition(fromRef.current + seconds / durationRef.current);
          },
        })
        .then(() => {
          if (runRef.current === run) rememberPosition(1);
        })
        .catch((cause: unknown) => {
          if (runRef.current === run) setError((cause as Error).message);
        })
        .finally(() => {
          if (runRef.current === run) setSpeaking(false);
        });
    },
    [text],
  );

  useEffect(() => {
    if (!speaking) return;
    const timer = window.setInterval(() => {
      // The device engine reports nothing, so the position is estimated until real
      // audio starts reporting its own clock - and then never again.
      if (measuredRef.current) return;
      rememberPosition(
        Math.min(0.995, (performance.now() - startedAtRef.current) / (duration * 1000)),
      );
    }, 200);
    return () => window.clearInterval(timer);
  }, [duration, speaking]);

  useEffect(() => () => stop(), [stop]);

  const seek = (next: number) => {
    rememberPosition(next);
    if (speaking) play(next);
  };
  const language = /[ąćęłńóśźż]/i.test(text) ? "pl" : "en";

  return (
    <div className="my-2">
      {/* The name stands OUTSIDE the frame, over its top-left corner. Inside it, a
          word competed with the very thing it names: the block IS the transport —
          one row of wave and clock — and the reader opens it for the text, so the
          name is the app's field caption, caps at chip size, and keeps its chevron
          because it still opens something. */}
      <Disclosure
        isOpen={open}
        tone="caption"
        onClick={() => setOpen((was) => !was)}
      >
        Transcript
      </Disclosure>
      <section className="border border-accent bg-panel">
        {/* ONE band, and everything the reader can PRESS rides in it. The transport
            LEADS the row: the gesture comes before the shape it moves, and the clock
            it produces ends the row. */}
        <div
          data-speech-header
          className="flex min-h-12 items-center gap-2 pl-1 pr-2.5 mouse:min-h-9"
        >
          <IconButton
            label={speaking ? "Pause" : "Play"}
            variant="quiet"
            onClick={() => (speaking ? stop() : play())}
          >
            {speaking ? <PauseIcon /> : <PlayIcon />}
          </IconButton>
          <Waveform
            className="flex-1"
            peaks={track?.peaks ?? []}
            value={position}
            label="Speech position"
            onSeek={seek}
          />
          <span className="shrink-0 font-mono text-chip tabular-nums text-dialog-hint">
            {speechTime(position * duration)}
            <span className="hidden sm:inline"> / {speechTime(duration)}</span>
          </span>
        </div>
        {open && (
          <div className="border-t border-edge">
            <p
              lang={language}
              className={`${PROSE} px-2.5 py-2.5 text-body text-dialog-foreground`}
            >
              {text}
            </p>
            {error && (
              <p className="px-2.5 pb-2 font-mono text-meta text-err">{error}</p>
            )}
          </div>
        )}
      </section>
    </div>
  );
}

export const ContentBlockView = memo(function ContentBlockView({
  block,
}: {
  block: ContentBlock;
}) {
  switch (block.type) {
    case "prose":
      return block.markdown ? <Markdown>{block.markdown}</Markdown> : null;
    case "speech":
      return block.text ? <SpeechBlock text={block.text} /> : null;
    case "code":
      return (
        <Markdown>{fenced(block.text ?? "", block.language ?? "")}</Markdown>
      );
    case "reasoning":
      return block.text ? <ThinkingBand>{block.text}</ThinkingBand> : null;
    case "tool": {
      const form: TranscriptForm = {
        op: block.tool ?? undefined,
        result_summary: block.status,
        result_render:
          block.output == null ? undefined : jsonText(block.output),
        error: block.error,
      };
      return <ToolCard form={form} />;
    }
    case "error":
      return (
        <div className="my-2 flex gap-2 border border-warn-edge bg-warn-surface px-2.5 py-2 font-mono text-meta text-err">
          <strong>{block.code}</strong>
          <span>{block.message}</span>
        </div>
      );
    case "attachment":
      return (
        <div className="my-2 flex w-fit items-center gap-1.5 border border-dialog-edge bg-panel px-2.5 py-1.5 font-mono text-meta text-dialog-foreground">
          <ArrowOutIcon />
          <span className="min-w-0 truncate">{block.name ?? "Attachment"}</span>
          <small className="text-dialog-hint">{block.media_type}</small>
        </div>
      );
    case "notice":
      return (
        <div className="my-2 border border-dialog-edge bg-panel px-2.5 py-2 font-mono text-meta text-dialog-hint">
          {block.message}
        </div>
      );
    default:
      return null;
  }
});

function fallbackAnswer(turn: TranscriptTurn): string {
  const iterations = turn.iterations ?? [];
  for (let index = iterations.length - 1; index >= 0; index -= 1) {
    const answer = iterations[index].answer?.trim();
    if (answer) return answer;
  }
  return "";
}

function runningTurnPhase(turn: TranscriptTurn): string {
  const iterations = turn.iterations ?? [];
  const iteration = iterations.length;
  const request = (turn.user_request ?? turn.request ?? "").trim();
  if (iteration === 0) {
    if (request.startsWith("!&")) return "Vis is starting a command";
    if (request.startsWith("!")) return "Vis is running a command";
    if (request.startsWith("/"))
      return `Vis is running: ${request.split(/\s+/, 1)[0]}`;
    return "Vis is waiting for an update";
  }
  const last = iterations.at(-1);
  const suffix = `(iter ${iteration})`;
  if (last?.error != null) return `Vis is retrying ${suffix}`;
  if (last?.forms?.length) return `Vis is running code ${suffix}`;
  if (last?.thinking?.trim()) return `Vis is thinking ${suffix}`;
  return `Vis is working ${suffix}`;
}

/**
 * The phase line under a turn that has not answered yet.
 *
 * `still` is the same line with the spinner and the clock taken off: the screen
 * has stopped FOLLOWING this turn (see `settled`), which is never a reason to
 * stop SAYING what it is. Silence there is what put a bare "Vis" — no phase, no
 * clock, no trace — under a message that had just been sent.
 */
function LiveProgress({
  phase,
  startedAt,
  still = false,
}: {
  phase: string;
  startedAt?: number;
  still?: boolean;
}) {
  const [now, setNow] = useState(() => Date.now());

  useEffect(() => {
    if (still) return;
    const timer = window.setInterval(() => setNow(Date.now()), 100);
    return () => window.clearInterval(timer);
  }, [still]);

  const elapsed =
    formatDuration(Math.max(0, now - (startedAt ?? now))) ?? "0ms";

  return (
    <>
      <div
        className="mt-5 truncate whitespace-nowrap font-mono text-ui text-vis-message"
        aria-hidden="true"
      >
        {still ? null : <Spinner />}
        <span>
          {still ? null : <>&nbsp;&nbsp;</>}
          {phase}...
          {still ? null : <>&nbsp;&nbsp;{elapsed}</>}
        </span>
      </div>
      <span className="sr-only" role="status">
        {phase}
      </span>
    </>
  );
}

// A turn is armed only once its size has HELD STILL for this long. A freshly
// mounted turn paints its prose before its code blocks, pictures and tables
// land, and a skip armed on that halfway height freezes the turn there: the
// content keeps arriving into a subtree WebKit is no longer laying out, so
// nothing reports the change. Measured on the simulator, arming on first sight:
// a 24-turn transcript that really stands 443 315 px measured 100 701 px.
const PAINT_SKIP_QUIET_MS = 400;

// How far outside the scroller a turn still counts as the reader's own, and on
// top of it the turn on either SIDE of anything inside that band is kept warm
// too (see `Neighbourhood`).
//
// A skipped subtree is not laid out at all, so the frame that reveals it pays
// for its layout AND its rasterization at once — reported as the request and
// response before the current one appearing to load themselves in, with a
// flash on the way up. `IntersectionObserver` answers proximity off the scroll
// path, since a rect read per turn per frame is the forced layout this file
// exists to avoid. Beyond the neighbourhood the transcript is skipped exactly
// as it was.
const PAINT_SKIP_NEAR_MARGIN = "100%";

/**
 * The scroller a turn hangs in, and it has to be the OBSERVER'S ROOT: a root
 * margin only ever expands the ROOT's own rect, so a band measured against the
 * window is still clipped by this scroller and would buy nothing at all. Every
 * turn hangs in its own wrapper, so the walk — `getComputedStyle` up four or
 * five ancestors — is cached against that wrapper, which is what a turn that
 * stops streaming and re-runs this effect asks against.
 */
const boxScrollers = new WeakMap<Element, Element | null>();

function scrollerOf(box: Element): Element | null {
  const wrapper = box.parentElement;
  if (!wrapper) return null;
  if (boxScrollers.has(wrapper)) return boxScrollers.get(wrapper) ?? null;
  let scroller: Element | null = null;
  if (typeof window !== "undefined") {
    for (
      let parent: Element | null = wrapper;
      parent;
      parent = parent.parentElement
    ) {
      const overflow = window.getComputedStyle(parent).overflowY;
      if (overflow === "auto" || overflow === "scroll") {
        scroller = parent;
        break;
      }
    }
  }
  boxScrollers.set(wrapper, scroller);
  return scroller;
}

/**
 * The neighbourhood of one scroller: its turns, whether the band can see each
 * one, and what each was last told.
 *
 * Warmth is not the band alone. Measured in the browser on a 30-turn session,
 * a turn of this transcript stands 16 000 to 22 000 px in a 708 px viewport —
 * so the turn BEFORE the one being read has its near edge a whole screen away
 * while the reader is one flick from its body, which is exactly the report
 * this answers. The turn on either SIDE of a turn the band can see is
 * therefore warm as well, however tall it is: the reader always holds one
 * whole turn in each direction. Dropping a skip and laying that turn out
 * measured 2-54 ms per turn, and it is spent where nobody is looking.
 */
type Neighbourhood = {
  observer: IntersectionObserver;
  /** Every turn of this scroller, and whether the band can see it. */
  seen: Map<Element, boolean>;
  /** What each turn was last told, so nothing is told it twice. */
  told: Map<Element, boolean>;
  report: Map<Element, (near: boolean) => void>;
};

const scrollerNeighbourhoods = new WeakMap<Element, Neighbourhood>();
let windowNeighbourhood: Neighbourhood | null = null;

/** The turns of one scroller, top to bottom. */
function inDocumentOrder(boxes: Element[]): Element[] {
  return boxes.sort((a, b) =>
    a.compareDocumentPosition(b) & Node.DOCUMENT_POSITION_FOLLOWING ? -1 : 1,
  );
}

function settle(hood: Neighbourhood): void {
  const order = inDocumentOrder([...hood.seen.keys()]);
  const warm = new Set<Element>();
  order.forEach((box, at) => {
    if (!hood.seen.get(box)) return;
    const before = order[at - 1];
    const after = order[at + 1];
    warm.add(box);
    if (before) warm.add(before);
    if (after) warm.add(after);
  });
  for (const box of order) {
    const near = warm.has(box);
    if (hood.told.get(box) === near) continue;
    hood.told.set(box, near);
    hood.report.get(box)?.(near);
  }
}

function neighbourhoodFor(root: Element | null): Neighbourhood | null {
  if (typeof IntersectionObserver === "undefined") return null;
  const known = root ? scrollerNeighbourhoods.get(root) : windowNeighbourhood;
  if (known) return known;
  const seen = new Map<Element, boolean>();
  const told = new Map<Element, boolean>();
  const report = new Map<Element, (near: boolean) => void>();
  const observer = new IntersectionObserver(
    (entries) => {
      for (const entry of entries) {
        if (seen.has(entry.target)) seen.set(entry.target, entry.isIntersecting);
      }
      settle(hood);
    },
    { root, rootMargin: PAINT_SKIP_NEAR_MARGIN },
  );
  const hood: Neighbourhood = { observer, seen, told, report };
  if (root) scrollerNeighbourhoods.set(root, hood);
  else windowNeighbourhood = hood;
  return hood;
}

/**
 * Report whether the reader has this turn in hand: inside the band, or beside
 * a turn that is. Nothing is reported until the observer's first answer, so a
 * turn counts as far away until proven otherwise — that answer lands frames
 * before the quiet period the skip waits out, and warmth gates nothing but
 * arming.
 */
function observeNear(
  box: Element,
  onNear: (near: boolean) => void,
): () => void {
  const hood = neighbourhoodFor(scrollerOf(box));
  if (!hood) return () => {};
  hood.seen.set(box, false);
  hood.report.set(box, onNear);
  hood.observer.observe(box);
  return () => {
    hood.observer.unobserve(box);
    hood.seen.delete(box);
    hood.told.delete(box);
    hood.report.delete(box);
    settle(hood);
  };
}
/**
 * Lets WebKit skip a finished turn it is not painting, at that turn's OWN size.
 *
 * The skip is only ever armed from a MEASUREMENT: never during the commit that
 * created the turn (a read there would force one full transcript layout per
 * turn), never while the turn is streaming (its height is still moving, and it
 * is the row being read), and never before the size has been the same for
 * `PAINT_SKIP_QUIET_MS`.
 *
 * Armed, the turn goes blind — a skipped subtree is not laid out, so no resize
 * can report what changed inside it. So anything that CAN change it drops the
 * skip first and measures again from scratch: the width (a rotation or a split
 * view), a DOM mutation anywhere under it, a picture that finishes loading, and
 * a size that no longer matches when the turn is rendered again. That is the
 * whole difference from the `contain-intrinsic-size` guess this replaced: the
 * engine is never handed a height that nobody measured.
 *
 * The box has to be its own formatting context BEFORE it is armed — `flow-root`
 * on the turn's `<article>`. `content-visibility:auto` implies `contain:layout`,
 * which makes it one, and a box that was not one already GROWS by its last
 * child's bottom margin the moment it is armed: a turn ending in the notice card
 * that a failed, interrupted or cancelled turn carries measured 7 620.06 px
 * armed against 7 612.06 px unarmed. Those 8 px read as content landing, so the
 * skip drops, the box shrinks back, and one quiet period later it arms again —
 * measured in WebKit, everything below that turn stepped 8 px every ~533 ms for
 * as long as the turn stayed on screen.
 *
 * The turn beside the viewport is never armed, and one that WAS armed drops the
 * skip a screenful before the reader reaches it (`PAINT_SKIP_NEAR_MARGIN`): a
 * reveal must never be the frame that lays a turn out.
 *
 * See the note at the top of this file for what the guess did instead.
 */
function useMeasuredPaintSkip(live: boolean) {
  const ref = useRef<HTMLElement | null>(null);

  useLayoutEffect(() => {
    const box = ref.current;
    if (!box) return;

    type Size = { width: number; height: number };
    /** The size the skip is armed with, or null while the turn is still open. */
    let armed: Size | null = null;
    /** The last size seen unarmed, and when this size was first seen. */
    let seen: (Size & { at: number }) | null = null;
    let recheck: number | null = null;
    let content: MutationObserver | null = null;

    /** True while this turn is within a screenful of what the reader sees. */
    let near = false;

    const same = (a: Size, b: Size) =>
      Math.abs(a.width - b.width) < 0.5 && Math.abs(a.height - b.height) < 0.5;

    /** Look again after the quiet period: nothing else will report stillness. */
    const soon = () => {
      if (recheck !== null || typeof window === "undefined") return;
      recheck = window.setTimeout(() => {
        recheck = null;
        scheduleBoxes([box]);
      }, PAINT_SKIP_QUIET_MS);
    };

    const unwatch = () => {
      content?.disconnect();
      content = null;
      box.removeEventListener("load", unarm, true);
    };

    const drop = () => {
      unwatch();
      armed = null;
      seen = null;
      box.style.contentVisibility = "";
      box.style.containIntrinsicSize = "";
    };

    /** Something moved under the skip: measure the turn again, from scratch. */
    const unarm = () => {
      if (!armed) return;
      drop();
      scheduleBoxes([box]);
    };

    const watchContent = () => {
      if (content || typeof MutationObserver === "undefined") return;
      // Attributes are left out on purpose: arming writes two of this box's
      // own, and a turn nobody is painting is a turn nobody can toggle. A
      // picture that finishes loading mutates no DOM at all, so its `load`
      // is caught on the way through instead.
      content = new MutationObserver(unarm);
      content.observe(box, {
        subtree: true,
        childList: true,
        characterData: true,
      });
      box.addEventListener("load", unarm, true);
    };

    if (live) {
      drop();
      return;
    }

    const measure = () => {
      const width = box.offsetWidth;

      // The reader's own neighbourhood is never skipped, and a turn that walked
      // into it gives its skip back: from here it is one reveal away, and a
      // reveal is not a moment to be laying a turn out in.
      if (near) {
        if (!armed && !seen) return;
        return drop;
      }

      if (isPaintSkipped(box)) {
        // Everything a skipped turn reports is the placeholder it was given —
        // except the WIDTH, which is still the layout's own answer.
        if (!armed || Math.abs(width - armed.width) < 0.5) return;
        return () => {
          drop();
          scheduleBoxes([box]);
        };
      }

      const height = box.getBoundingClientRect().height;
      // 0 is "not laid out" — a detached or hidden subtree — never "empty".
      if (height <= 0) return;
      const size = { width, height };

      if (armed) {
        if (same(armed, size)) return;
        return () => {
          drop();
          scheduleBoxes([box]);
        };
      }

      if (!seen || !same(seen, size)) {
        seen = { ...size, at: Date.now() };
        soon();
        return;
      }
      if (Date.now() - seen.at < PAINT_SKIP_QUIET_MS) {
        soon();
        return;
      }
      return () => {
        armed = size;
        // The one-size shorthand applies the tall turn's HEIGHT to both axes. An
        // off-screen 20 000 px turn then advertises a 20 000 px intrinsic WIDTH,
        // widening the transcript until the whole app is rendered as a thin strip.
        box.style.containIntrinsicSize = `auto ${width}px auto ${height}px`;
        box.style.contentVisibility = "auto";
        watchContent();
      };
    };

    const unwatchNear = observeNear(box, (isNear) => {
      if (isNear === near) return;
      near = isNear;
      scheduleBoxes([box]);
    });

    const stop = observeBox(box, measure);
    scheduleBoxes([box]);
    return () => {
      if (recheck !== null && typeof window !== "undefined")
        window.clearTimeout(recheck);
      stop();
      unwatchNear();
      drop();
    };
  }, [live]);

  return ref;
}

export const AssistantMessage = memo(function AssistantMessage({
  turn,
  streaming = false,
  activity,
  startedAt,
  settled = false,
  whole = false,
  client,
  sid,
  livePanel,
  liveActivities,
}: {
  turn: TranscriptTurn;
  streaming?: boolean;
  activity?: string;
  startedAt?: number;
  /** This row's trace is already on screen — see `IterationTrace`'s `whole`. */
  whole?: boolean;
  /**
   * The caller KNOWS this row's turn is over (the gateway is not running this
   * session, or its terminal frame already landed) even though the persisted
   * row still reads `running` — persistence lags the terminal event. Without
   * it the placeholder row paints the live ticker, and "Vis is thinking..."
   * kept spinning at the bottom for a turn that had long finished. It stops the
   * TICKER, never the words: the row still names its phase (see `still`).
   */
  settled?: boolean;
  client?: GatewayClient;
  sid?: string;
  /** Live work belongs after its tool/prose trace and before the phase ticker. */
  livePanel?: ReactNode;
  /** Host Activity belongs to its Python form, not to the detached live rail. */
  liveActivities?: LiveViewModel[];
}) {
  const blocks = turn.content ?? [];
  const fallback = blocks.length ? "" : fallbackAnswer(turn);
  // One answer, one copy: the trace never repeats prose the answer band under it
  // already paints — including the answer PROMOTED out of the last iteration when
  // the row carries no content blocks (issue #145).
  const answered = useMemo(
    () => answeredProse(turn.content, fallback),
    [turn.content, fallback],
  );
  const cancelled =
    turn.status === "cancelled" || turn.prior_outcome === "cancelled";
  // A row still IN FLIGHT has no footer to show. Usage, cost and duration only
  // exist once the turn ends, so mid-turn the summary degrades to a bare
  // `provider/model` (the gateway stamps it from the last completed iteration's
  // routing) — a finished-looking meta line under a turn that is still working.
  const inFlight =
    streaming || IN_FLIGHT_STATUSES.has(String(turn.status ?? ""));
  const meta =
    !inFlight && !commandTurn(turn) && (!cancelled || assistantUsage(turn))
      ? turnMetaSummary(turn)
      : null;
  const fallbackNote = meta && !cancelled ? turnFallbackNote(turn) : null;

  // A finished turn is one skippable paint box, sized by `useMeasuredPaintSkip`
  // from its own measured height. The live turn is never skipped: it is the row
  // being read, and its height is still moving. The box is a `flow-root` so that
  // arming it cannot change the height it was armed with.
  const paintSkip = useMeasuredPaintSkip(streaming);

  return (
    <article className="flow-root mt-4 w-full" aria-busy={streaming} ref={paintSkip}>
      <div
        className={`mb-1 font-mono text-meta font-bold ${cancelled ? "text-dialog-hint" : "text-vis-role"}`}
      >
        Vis
      </div>
      <div className="min-w-0">
        <IterationTrace
          iterations={turn.iterations ?? []}
          answered={answered}
          live={streaming}
          whole={whole}
          client={client}
          sid={sid}
          liveActivities={liveActivities}
        />
        {/* Message prose sits on the SAME canonical step as the trace it grows out of:
            tool results, thinking bands and code cards are all `text-ui` (11px), so an
            answer at `text-body` (12px) was one px of drift, not a hierarchy. The role
            label (`text-meta`) and the meta footer (`text-chip`) still step down from it. */}
        <div
          className={`bg-answer text-ui ${cancelled ? "italic text-cancelled-foreground" : "text-answer-foreground"}`}
        >
          {blocks.map((block) => (
            <ContentBlockView key={block.id} block={block} />
          ))}
          {fallback && <Markdown>{fallback}</Markdown>}
          {!streaming &&
            !blocks.length &&
            !fallback &&
            turn.status !== "completed" &&
            turn.status !== "running" && (
              <span>
                {cancelled
                  ? "Cancelled by user."
                  : (turn.status ?? "No response")}
              </span>
            )}
        </div>
        {livePanel}
        {streaming ? (
          <LiveProgress
            phase={activity ?? "Vis is working"}
            startedAt={startedAt}
          />
        ) : turn.status === "running" ? (
          // A row this screen has stopped following still reads `running`, so it
          // still says so: the spinner and the elapsed clock go (they are what
          // made a finished turn look alive), the words stay. Rendering nothing
          // here left the reader a bare "Vis" for the whole turn.
          <LiveProgress
            phase={runningTurnPhase(turn)}
            startedAt={turn.created_at}
            still={settled}
          />
        ) : null}
        {meta && (
          <footer className="mt-5 min-w-0 text-right font-mono text-chip text-footer-muted">
            <div
              className="overflow-hidden text-ellipsis whitespace-nowrap"
              title={meta}
            >
              {meta}
            </div>
            {fallbackNote && (
              <div
                className="overflow-hidden text-ellipsis whitespace-nowrap italic text-footer-muted"
                title={fallbackNote}
              >
                {fallbackNote}
              </div>
            )}
          </footer>
        )}
      </div>
    </article>
  );
});

// A user attachment carries its own bytes, already base64 — persisted, so the
// picture survives a restart after the clipboard or temp file it came from is
// gone. Some rows store the data URL whole and some only its payload.
function attachmentSrc(att: GatewayAttachment): string {
  return att.base64.startsWith("data:")
    ? att.base64
    : `data:${att.media_type};base64,${att.base64}`;
}

export const UserMessage = memo(function UserMessage({
  children,
  attachments,
}: {
  children: string;
  attachments?: GatewayAttachment[];
}) {
  const parts = parseUserMessage(children);
  // Persisted user images re-render from DB-owned base64 (survives a restart even
  // after the original clipboard/temp source file is gone). Tool artifacts render
  // in the assistant trace, so only the `user` rail belongs in the user bubble.
  const mediaAttachments = (attachments ?? []).filter(
    (a) =>
      (a.source ?? "user") === "user" &&
      !!a.base64 &&
      (!!a.media_type?.startsWith("image/") ||
        !!a.media_type?.startsWith("video/") ||
        !!a.media_type?.startsWith("audio/")),
  );
  // The very rule the assistant rail follows (`mediaGroupLayout`): ONE picture
  // is a plate with its own caption, several are a gallery. A clip always keeps
  // the plate, since the platform's controls do not fit a gallery tile, and a
  // recording is a row of its own because it has nothing to paint.
  const recordings = mediaAttachments.filter((a) =>
    Boolean(a.media_type?.startsWith("audio/")),
  );
  const clips = mediaAttachments.filter((a) =>
    Boolean(a.media_type?.startsWith("video/")),
  );
  const pictures = mediaAttachments.filter(
    (a) =>
      !a.media_type?.startsWith("video/") &&
      !a.media_type?.startsWith("audio/"),
  );
  const layout = mediaGroupLayout(pictures.length);
  // These bytes are inline, so there is no pulse to swap out — but a picture
  // whose box is its own decoded size still reserves NOTHING until it decodes,
  // and on iOS that decode happens as the bubble nears the viewport, i.e.
  // mid-scroll. Same reserved frame as a produced artifact, for the same
  // reason — and the frame is the PLATE's, never a class list on the zoom
  // trigger, which spells `border-0 bg-transparent` on itself.
  const picture = (att: GatewayAttachment, index: number, fill: boolean) => (
    <ExpandableImage
      key={att.id ?? `pic-${index}`}
      src={attachmentSrc(att)}
      alt={att.filename ?? "attachment"}
      galleryAt={index}
      frameClassName="h-full w-full"
      className={fill ? mediaTileContentClass : mediaContentClass}
    />
  );
  // The bubble uses the same ragged prose rule as answers. Its raw text can contain
  // paths and URLs that the renderer cannot scope separately, so `break-words` remains
  // the last-resort overflow guard without changing ordinary word spacing.
  return (
    <article className="mt-4 w-full">
      <div className="mb-1 font-mono text-meta font-bold text-you-role">
        You
      </div>
      <div
        className={`block w-full whitespace-pre-wrap break-words border-l-2 border-you-role bg-code px-3 py-2 text-ui text-you-message-foreground ${PROSE}`}
      >
        {parts.map((part) =>
          part.type === "text" ? (
            <span key={part.key}>{part.text}</span>
          ) : part.type === "image" ? (
            <span
              key={part.key}
              className="my-1 mr-1 inline-flex items-center gap-1 border border-code-edge bg-code px-2 py-1 align-middle font-mono text-meta text-dialog-hint first:mt-0"
            >
              {part.summary}
            </span>
          ) : (
            // Full-bleed to the bubble's own `px-3`: the rules then read as the
            // bubble's dividers instead of a stray box hairline-close to the
            // sentence above and below it. Vertical margin is the paste's ONLY
            // separation from that prose, so it stays wider than the block's own
            // padding.
            <details
              key={part.key}
              className="group -mx-3 my-3 block max-w-none border-y border-code-edge bg-code text-code-foreground first:mt-0 last:mb-0"
            >
              <summary className="cursor-pointer list-none select-none px-3 py-2 font-mono text-meta font-semibold text-accent-ink marker:hidden [&::-webkit-details-marker]:hidden">
                <ChevronIcon className="mr-1.5 inline-block text-dialog-hint group-open:rotate-90" />
                {part.summary}
              </summary>
              <pre className="max-h-[min(28rem,60dvh)] overflow-auto overscroll-contain border-t border-code-edge px-3 py-2 font-mono text-meta [tab-size:2]">
                <code>{part.content}</code>
              </pre>
            </details>
          ),
        )}
      </div>
      {mediaAttachments.length > 0 && (
        <div className="mt-2.5 min-w-0">
          {/* The clip the user sent replays from the SAME DB-owned bytes as a
              picture, so it survives a restart after the source file is gone. */}
          {/* A recording the user sent replays from those same bytes — it just
              has no picture, so it stands as a row rather than on a plate. */}
          {recordings.map((att, index) => (
            <MediaRecording
              key={att.id ?? `rec-${index}`}
              name={att.filename}
              meta={mediaMeta(att)}
              transcription={att.transcription}
            >
              <audio
                src={attachmentSrc(att)}
                controls
                preload="metadata"
                className="h-11 w-full"
              />
            </MediaRecording>
          ))}
          {clips.map((att, index) => (
            <MediaPlate
              key={att.id ?? `clip-${index}`}
              name={att.filename}
              meta={mediaMeta(att)}
            >
              <video
                src={attachmentSrc(att)}
                controls
                playsInline
                preload="metadata"
                className={mediaContentClass}
              />
            </MediaPlate>
          ))}
          {layout === "grid" ? (
            <MediaGrid summary={mediaSummary(pictures)}>
              {pictures.map((att, index) => (
                <MediaTile key={att.id ?? `tile-${index}`}>
                  {picture(att, index, true)}
                </MediaTile>
              ))}
            </MediaGrid>
          ) : (
            pictures.map((att, index) => (
              <MediaPlate
                key={att.id ?? `plate-${index}`}
                name={att.filename}
                meta={mediaMeta(att)}
              >
                {picture(att, index, false)}
              </MediaPlate>
            ))
          )}
        </div>
      )}
    </article>
  );
});
