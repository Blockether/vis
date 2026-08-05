import {
  Fragment,
  isValidElement,
  memo,
  useEffect,
  useLayoutEffect,
  useMemo,
  useRef,
  useState,
  type MouseEvent,
  type ReactNode,
} from 'react';
import Prism from 'prismjs';
import { DataTable } from './DataTable';
import { DocCard, DocPreview } from './DocArtifact';
import {
  attachmentBytes,
  attachmentIsDoc,
  attachmentIsImage,
  attachmentIsPlayable,
  attachmentIsVideo,
  pageBySize,
  RAIL_PAGE,
} from '../lib/artifacts';
import { Spinner } from './ui';
import 'prismjs/components/prism-bash';
import 'prismjs/components/prism-clojure';
import 'prismjs/components/prism-css';
import 'prismjs/components/prism-diff';
import 'prismjs/components/prism-java';
import 'prismjs/components/prism-json';
import 'prismjs/components/prism-markdown';
import 'prismjs/components/prism-python';
import 'prismjs/components/prism-rust';
import 'prismjs/components/prism-typescript';
import 'prismjs/components/prism-jsx';
import 'prismjs/components/prism-tsx';
import 'prismjs/components/prism-yaml';
import ReactMarkdown from 'react-markdown';
import remarkBreaks from 'remark-breaks';
import remarkGfm from 'remark-gfm';
import { parseUserMessage } from '../lib/paste';
import {
  readerOwnsScroll,
} from '../lib/reader-gesture';
import { formatCost, formatTokens, turnUsage } from '../lib/usage';
import { isViewportRotating, onViewportRotation } from '../lib/viewport';
import type {
  ContentBlock,
  GatewayAttachment,
  IterationAttachment,
  JsonValue,
  TranscriptForm,
  TranscriptIteration,
  TranscriptTurn,
} from '../lib/types';
import type { GatewayClient } from '../lib/gateway';
import { ExpandableImage } from './ImageViewer';
import {
  mediaContentClass,
  mediaFrameClass,
  mediaPendingClass,
  mediaSlotFrame,
  type MediaSlotState,
} from '../lib/media-frame';

const disclosureClass =
  'inline-block shrink-0 text-ui transition-transform duration-150 group-open:rotate-90';

// Transcript nodes the stream appends rise + fade in instead of popping into
// place. A keyframe animation (see `--animate-transcript-*` in index.css) plays
// exactly once — on the element's first paint after insertion — so a re-render
// can never replay it. Only live subtrees pass `live`, so replaying history (or
// a finished turn re-keyed out of the live slot into the turn list) stays
// perfectly still.
export const transcriptEnterClass = 'animate-transcript-enter motion-reduce:animate-none';

// For nodes that land INSIDE a bubble that is already on screen (a new tool
// form, a result card joining the grid). They only rise: a second opacity ramp
// nested in the first is what read as a wash-out, and content that is still
// streaming must never fade.
export const transcriptRiseClass = 'animate-transcript-rise motion-reduce:animate-none';

// One assistant turn is thousands of nodes, and opening a session used to mount
// them all in one shot: measured on device, a single turn of this transcript is
// ~13k elements and the first paint cost 409 ms, the full window 1256 ms — the
// stall between tapping a row and seeing the conversation.
//
// `content-visibility:auto` was that fix and had to be REVERTED: it buys first
// paint by guessing the height of everything it skips, and every first reveal
// then corrects that guess. On a scroller with no scroll anchoring (WebKit has
// none, and the transcript sets `overflow-anchor:none` on purpose) a correction
// ABOVE the viewport moves what you are reading. Measured in WebKit on a 28-turn
// session, one 10 000 px scroll up: 39 height corrections totalling 53 307 px,
// the worst single ones 21 002 / 12 940 / 7 752 px — the flicker while scrolling
// a freshly opened session. Without it: zero height corrections.
//
// `contain:layout style` had to go with it. It faked no size, but it split a
// 41 148 px transcript into 198 paint-isolated islands, and a fast fling then
// exposed the paper background before WebKit rasterized them — the white bands.
// A continuous paint tree has no such catch-up boundary.
//
// What keeps the open cheap instead is the bounded window: pagination mounts
// only INITIAL_VISIBLE_TURNS, `Load earlier` brings the rest in on demand, and
// the iteration ramp below stages a turn's trace. None of those guess a height.

const toolRoleClasses: Record<string, { border: string; text: string }> = {
  'tool-color/read': { border: 'border-tool-read', text: 'text-tool-read' },
  'tool-color/search': { border: 'border-tool-search', text: 'text-tool-search' },
  'tool-color/preview': { border: 'border-tool-preview', text: 'text-tool-preview' },
  'tool-color/edit': { border: 'border-tool-edit', text: 'text-tool-edit' },
  'tool-color/create': { border: 'border-tool-create', text: 'text-tool-create' },
  'tool-color/delete': { border: 'border-tool-delete', text: 'text-tool-delete' },
  'tool-color/move': { border: 'border-tool-move', text: 'text-tool-move' },
  'tool-color/shell': { border: 'border-tool-shell', text: 'text-tool-shell' },
  'tool-color/meta': { border: 'border-tool-meta', text: 'text-tool-meta' },
  'tool-color/test': { border: 'border-tool-test', text: 'text-tool-test' },
};

const toolLabelOverrides: Record<string, string> = {
  python_execution: 'RESULT',
  repl_eval: 'REPL',
  // `shell` needs no override: ONE tool, whose card names the op that ran.
};

function CopyButton({
  value,
  className = 'absolute right-2 top-2 z-10',
  label = 'Copy code',
}: {
  value: string;
  /** Placement only — the chip's own look is fixed. */
  className?: string;
  label?: string;
}) {
  const [copied, setCopied] = useState(false);

  async function copy(event: MouseEvent<HTMLButtonElement>) {
    // This chip also lives inside a <summary>, where a bare click would toggle
    // the disclosure as well as copy.
    event.preventDefault();
    event.stopPropagation();
    try {
      await navigator.clipboard.writeText(value);
      setCopied(true);
      window.setTimeout(() => setCopied(false), 1_500);
    } catch {
      // Clipboard access can be unavailable in an untrusted mobile webview.
    }
  }

  return (
    <button
      type="button"
      // min-w keeps 'Copy' and 'Copied' the same width so the chip never jumps.
      className={`${className} min-w-[6ch] border border-dialog-edge bg-button px-1.5 py-0.5 text-center font-mono text-chip text-button-foreground transition-colors hover:bg-hover`}
      onClick={copy}
      aria-label={label}
    >
      {copied ? 'Copied' : 'Copy'}
    </button>
  );
}

type DiffCellKind = 'add' | 'del' | 'ctx';
type DiffCell = { line: number | null; text: string; kind: DiffCellKind };
type DiffRow =
  | { kind: 'meta'; text: string }
  | { kind: 'code'; before: DiffCell | null; after: DiffCell | null };

function sideBySideDiff(value: string): DiffRow[] {
  const rows: DiffRow[] = [];
  let beforeLine: number | null = null;
  let afterLine: number | null = null;
  let removed: DiffCell[] = [];
  let added: DiffCell[] = [];

  const flushChanges = () => {
    for (let index = 0; index < Math.max(removed.length, added.length); index += 1) {
      rows.push({ kind: 'code', before: removed[index] ?? null, after: added[index] ?? null });
    }
    removed = [];
    added = [];
  };

  for (const line of value.split('\n')) {
    const hunk = /^@@ -(\d+)(?:,\d+)? \+(\d+)(?:,\d+)? @@/.exec(line);
    if (hunk) {
      flushChanges();
      beforeLine = Number(hunk[1]);
      afterLine = Number(hunk[2]);
      rows.push({ kind: 'meta', text: line });
    } else if (line.startsWith('-') && !line.startsWith('---')) {
      removed.push({ line: beforeLine, text: line.slice(1), kind: 'del' });
      beforeLine = beforeLine === null ? null : beforeLine + 1;
    } else if (line.startsWith('+') && !line.startsWith('+++')) {
      added.push({ line: afterLine, text: line.slice(1), kind: 'add' });
      afterLine = afterLine === null ? null : afterLine + 1;
    } else if (line.startsWith(' ')) {
      flushChanges();
      rows.push({
        kind: 'code',
        before: { line: beforeLine, text: line.slice(1), kind: 'ctx' },
        after: { line: afterLine, text: line.slice(1), kind: 'ctx' },
      });
      beforeLine = beforeLine === null ? null : beforeLine + 1;
      afterLine = afterLine === null ? null : afterLine + 1;
    } else {
      flushChanges();
      rows.push({ kind: 'meta', text: line });
    }
  }
  flushChanges();
  return rows;
}

function diffCellClass(cell: DiffCell | null): string {
  if (!cell) return 'bg-code';
  if (cell.kind === 'add') return 'bg-code-ok text-code-success';
  if (cell.kind === 'del') return 'bg-code-err text-code-error';
  return 'bg-code text-code-foreground';
}

// A diff cell NEVER clips its own text: the block's horizontal scroller (below) is
// what reveals a long line, and `overflow-hidden`/`text-ellipsis` here silently ate
// the tail of every wide hunk with nothing left to scroll to.
function DiffCellView({ cell, className = '' }: { cell: DiffCell | null; className?: string }) {
  return (
    <span className={`flex whitespace-pre px-3 py-px ${diffCellClass(cell)} ${className}`}>
      <span className="mr-2 w-8 shrink-0 select-none text-right text-code-duration">{cell?.line ?? ''}</span>
      <span>{cell?.text || ' '}</span>
    </span>
  );
}

const DiffBlock = memo(function DiffBlock({ value, compact, frameless = false }: { value: string; compact: boolean; frameless?: boolean }) {
  const rows = sideBySideDiff(value);
  return (
    <div
      className={`${compact ? 'my-2' : 'my-3'} relative overflow-hidden bg-code ${frameless ? '' : 'border border-code-edge'}`}
      aria-label="Side-by-side diff"
    >
      {!frameless && <CopyButton value={value} />}
      <div className={`${compact ? 'text-meta' : 'text-ui'} max-w-full overflow-x-auto overscroll-x-contain py-2 font-mono`}>
        {/* ONE grid for the whole hunk (rows are fragments, not nested grids) so both
            columns share a track sizing pass: `w-max` lets those tracks grow to the
            widest line and hands the real content width to the scroller, `min-w-full`
            keeps the row backgrounds spanning the viewport when the diff is narrow. */}
        <div className="grid w-max min-w-full grid-cols-2">
          {rows.map((row, index) => {
            // The copy chip floats over the top-right corner; only the FIRST row needs
            // to keep its tail out from under it.
            const clearance = !frameless && index === 0 ? ' pr-16' : '';
            return row.kind === 'meta' ? (
              <span className={`col-span-2 whitespace-pre px-3 py-px text-code-syntax-keyword${clearance}`} key={`${index}-${row.text}`}>
                {row.text || ' '}
              </span>
            ) : (
              <Fragment key={`${index}-${row.before?.text}-${row.after?.text}`}>
                <DiffCellView cell={row.before} />
                <DiffCellView cell={row.after} className={`border-l border-code-edge${clearance}`} />
              </Fragment>
            );
          })}
        </div>
      </div>
    </div>
  );
});

const languageAliases: Record<string, string> = {
  clj: 'clojure',
  edn: 'clojure',
  js: 'javascript',
  jsx: 'jsx',
  md: 'markdown',
  py: 'python',
  sh: 'bash',
  shell: 'bash',
  ts: 'typescript',
  yml: 'yaml',
};

const syntaxTokenClasses: Record<string, string> = {
  boolean: 'text-code-syntax-number',
  builtin: 'text-code-syntax-special',
  char: 'text-code-syntax-string',
  className: 'text-code-syntax-special',
  comment: 'italic text-code-syntax-comment',
  constant: 'text-code-syntax-number',
  decorator: 'text-code-syntax-special',
  function: 'text-code-syntax-special',
  important: 'font-semibold text-code-syntax-keyword',
  keyword: 'font-medium text-code-syntax-keyword',
  number: 'text-code-syntax-number',
  operator: 'text-code-syntax-special',
  regex: 'text-code-syntax-string',
  string: 'text-code-syntax-string',
  symbol: 'text-code-syntax-number',
};

function syntaxClass(token: Prism.Token): string {
  const aliases = Array.isArray(token.alias) ? token.alias : token.alias ? [token.alias] : [];
  for (const candidate of [token.type, ...aliases]) {
    const normalized = candidate === 'class-name' ? 'className' : candidate;
    if (syntaxTokenClasses[normalized]) return syntaxTokenClasses[normalized];
  }
  return 'text-code-foreground';
}

type SyntaxSegment = { text: string; className: string };

// The colour the `<pre>` already paints. A segment that only asks for it needs
// no element of its own — see `SyntaxCodeBlock`.
const CODE_DEFAULT_CLASS = 'text-code-foreground';

function flattenSyntax(
  tokens: (string | Prism.Token)[],
  inherited: string,
  out: SyntaxSegment[],
): void {
  for (const token of tokens) {
    if (typeof token === 'string') {
      if (token) out.push({ text: token, className: inherited });
      continue;
    }
    const className = syntaxClass(token);
    if (Array.isArray(token.content)) {
      flattenSyntax(token.content as (string | Prism.Token)[], className, out);
    } else if (typeof token.content === 'string') {
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
    const parts = segment.text.split('\n');
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
function splitGutter(value: string): { gutters: string[]; code: string } | null {
  const rawLines = value.split('\n');
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
      codeLines.push('');
      continue;
    }
    if (line === '') {
      gutters.push('');
      codeLines.push('');
      continue;
    }
    return null;
  }
  if (numbered < 2) return null;
  return { gutters, code: codeLines.join('\n') };
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
  const lines = segmentsToLines(applyMarks(highlightSegments(source, language), marks.ranges));

  return (
    <div
      className={`relative overflow-hidden bg-code ${bare ? '' : `${compact ? 'my-2' : 'my-3'} ${frameless ? '' : 'border border-code-edge'}`}`}
    >
      {/* An enclosing card (a tool result) owns ONE copy control for the whole
          body, so a frameless block does not add a second, third, … chip. */}
      {!frameless && <CopyButton value={copyValue ?? source} />}
      <pre className={`${compact ? 'py-2 text-meta ' : 'py-2.5 text-ui '} m-0 max-w-full overflow-x-auto overscroll-x-contain font-mono text-code-foreground`}>
        <code className="block min-w-max [tab-size:2]">
          {lines.map((segments, index) => (
            <div key={index} className={`flex w-fit min-w-full whitespace-pre px-3 ${frameless ? '' : 'first:pr-16'}`}>
              {gutter && (
                <span className="mr-3 shrink-0 select-none text-right text-code-duration" aria-hidden="true">
                  {gutter.gutters[index] ?? ''}
                </span>
              )}
              <span className="min-w-0">
                {segments.length === 0
                  ? ' '
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
  // Justification is a WIDTH trade. Word-spacing is the only slack a justified line has, so an
  // atom the line breaker refuses to split — an inline `code` path, a URL — is paid for by
  // stretching the handful of words that DID fit, and that is where the rivers come from.
  // `overflow-wrap: anywhere` does NOT rescue it: it is a LAST-RESORT break (CSS Text 3 §5.5),
  // so a long path sitting after three words is kept whole and those three words absorb the
  // whole deficit (measured in Chromium at a 360px column: 32.9px between words against a
  // 4.2px space, an 8x stretch). The fix is to remove the unbreakable atom instead of removing
  // the justification: `break-all` on inline `code` and on links gives the breaker a stop at
  // every character, so the atom fills the line to the right margin and the gaps stay at their
  // natural width. Justification is therefore UNCONDITIONAL — a folded receipt's gist and its
  // `ntr[…]`-carrying metric bullets are all one flush-both-margins column, matching the TUI
  // (`markdown-layout/justify-line-runs` → lanterna `justifyLine`).
  const runningText =
    'hyphens-auto [hyphenate-limit-chars:6_3_3] text-pretty text-justify';
  // A heading inside a tool result card is a STRUCTURAL divider — one file in a
  // multi-file `cat`, one occurrence in an index, one step in a batch — not a
  // document hierarchy. The card body is `text-meta` (10px), so `### path · L12-30`
  // rendered at `text-body`/`text-title` towered over the very rows it labels. Nested
  // headings therefore step DOWN to the card's own scale and separate by weight,
  // case and rule instead of by size.
  const heading = (nestedClass: string, compactClass: string, fullClass: string) =>
    nested ? nestedClass : compact ? compactClass : fullClass;
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
            <blockquote className={`${compact ? 'my-2 pl-3' : 'my-3 pl-4'} border-l-2 border-answer-edge text-dialog-hint`}>
              {quote}
            </blockquote>
          ),
          code: ({ children: inline }) => (
            <code className="mx-px inline rounded-none bg-result-path px-0.5 py-px font-mono font-medium text-result-path-foreground break-all">
              {inline}
            </code>
          ),
          h1: ({ children: heading1 }) => (
            <h1 className={`${heading('mb-1 mt-3 text-ui', 'mb-1.5 mt-4 text-subhead', 'mb-2 mt-6 text-head')} border-b-2 border-answer-edge pb-1 font-semibold tracking-[-0.015em] text-heading-1`}>
              {heading1}
            </h1>
          ),
          h2: ({ children: heading2 }) => (
            <h2 className={`${heading('mb-1 mt-3 text-ui', 'mb-1 mt-3.5 text-title', 'mb-1.5 mt-5 text-subhead')} border-b border-answer-edge pb-0.5 font-semibold tracking-[-0.01em] text-heading-2`}>
              {heading2}
            </h2>
          ),
          h3: ({ children: heading3 }) => (
            <h3 className={`${heading('mb-0.5 mt-2.5 text-meta', 'mb-1 mt-3 text-body', 'mb-1 mt-4 text-title')} font-semibold text-heading-3`}>
              {heading3}
            </h3>
          ),
          h4: ({ children: heading4 }) => (
            <h4 className={`${heading('mb-0.5 mt-2 text-meta', 'mb-0.5 mt-2.5 text-body', 'mb-1 mt-3.5 text-body')} font-semibold text-heading-3`}>
              {heading4}
            </h4>
          ),
          h5: ({ children: heading5 }) => (
            <h5 className={`${heading('mb-0.5 mt-2 text-chip uppercase tracking-[0.06em]', 'mb-0.5 mt-2.5 text-ui', 'mb-1 mt-3 text-ui')} font-semibold text-heading-3`}>
              {heading5}
            </h5>
          ),
          h6: ({ children: heading6 }) => (
            <h6 className={`${heading('mb-0.5 mt-2 text-chip', 'mb-0.5 mt-2.5 text-meta', 'mb-1 mt-3 text-meta')} font-semibold uppercase tracking-[0.08em] text-heading-3`}>
              {heading6}
            </h6>
          ),
          hr: () => <hr className={`${compact ? 'my-3' : 'my-5'} border-answer-edge`} />,
          li: ({ children: item }) => (
            <li className={`${compact ? 'my-0.5 pl-0.5' : 'my-0.5 pl-1'} ${runningText}`}>{item}</li>
          ),
          ol: ({ children: list }) => (
            <ol className={`${compact ? 'my-2 pl-5' : 'my-3 pl-6'} list-decimal space-y-0.5`}>{list}</ol>
          ),
          p: ({ children: paragraph }) => (
            <p className={`${compact ? 'my-2' : 'my-2.5'} ${runningText}`}>{paragraph}</p>
          ),
          pre: ({ children: codeNode }) => {
            const raw = extractText(codeNode).replace(/\n$/, '');
            const language = codeLanguage(codeNode);
            if (language === 'diff' || language === 'patch' || language === 'udiff') {
              return <DiffBlock value={stripMarks(raw)} compact={compact} frameless={nested} />;
            }
            // A CSV/TSV artifact is DATA: `vis_attach` fences it as `vis-table` and
            // both surfaces paint a real grid — the TUI's table dialog, this table.
            if (language === 'vis-table') {
              return <DataTable body={stripMarks(raw)} compact={compact} frameless={nested} />;
            }
            // A PDF or an HTML page is a DOCUMENT: it never reaches the model, and
            // the fence carries a descriptor only. The card says what was produced;
            // the attachment rail below opens the bytes in a sandboxed frame.
            if (language === 'vis-doc') {
              return <DocCard body={stripMarks(raw)} compact={compact} frameless={nested} />;
            }
            return <SyntaxCodeBlock value={raw} language={language} compact={compact} frameless={nested} />;
          },
          strong: ({ children: strong }) => <strong className="font-semibold">{strong}</strong>,
          table: ({ children: table }) => (
            <div className={`${compact ? 'my-2' : 'my-3'} max-w-full overflow-x-auto overscroll-x-contain`}>
              {/* The table sits on the SAME step as the surrounding prose — hardcoding
                  `text-ui` made a tool result's table one step BIGGER than the compact
                  code blocks (`text-meta`) in the very same card. */}
              <table className={`w-full border-collapse ${compact ? 'text-meta' : 'text-ui'}`}>{table}</table>
            </div>
          ),
          // `align-top`: one wrapping cell makes the whole row tall, and middle-aligned
          // neighbours then float in the middle of that emptiness. Rows read as rows
          // when every cell starts on the first line.
          td: ({ children: cell }) => (
            <td className={`${compact ? 'px-1.5 py-1' : 'px-2 py-1.5'} border border-code-edge text-left align-top`}>{cell}</td>
          ),
          th: ({ children: cell }) => (
            <th className={`${compact ? 'px-1.5 py-1' : 'px-2 py-1.5'} border border-code-edge bg-code text-left align-top font-semibold`}>{cell}</th>
          ),
          ul: ({ children: list }) => (
            <ul className={`${compact ? 'my-2 pl-5' : 'my-3 pl-6'} list-disc space-y-0.5`}>{list}</ul>
          ),
        }}
      >
        {children}
      </ReactMarkdown>
    </div>
  );
});

function extractText(node: ReactNode): string {
  if (typeof node === 'string' || typeof node === 'number') return String(node);
  if (Array.isArray(node)) return node.map(extractText).join('');
  if (node && typeof node === 'object' && 'props' in node) {
    return extractText((node as { props: { children?: ReactNode } }).props.children);
  }
  return '';
}

function codeLanguage(node: ReactNode): string {
  if (!isValidElement<{ className?: string }>(node)) return '';
  return /(?:^|\s)language-([^\s]+)/.exec(node.props.className ?? '')?.[1] ?? '';
}

function jsonText(value: JsonValue | unknown): string {
  if (typeof value === 'string') return value;
  if (value == null) return '';
  try {
    return JSON.stringify(value, null, 2);
  } catch {
    return String(value);
  }
}

function stripAnsi(value: string): string {
  return value.replace(/\u001b\[[0-?]*[ -/]*[@-~]/g, '');
}

// The engine paints grep needles with reverse video (`ESC[7m … ESC[27m`), exactly
// like the TUI. Markdown cannot carry SGR, so the pair is rewritten into private-use
// sentinels that survive fence parsing and are turned back into a highlighted span
// by the code renderer (and dropped everywhere else).
const MARK_OPEN = '\u0091';
const MARK_CLOSE = '\u0092';
const MARK_SENTINELS = /[\u0091\u0092]/g;
const MARK_CLASS = 'bg-accent text-accent-foreground';

function markAnsiHighlights(value: string): string {
  const marked = value.replace(MARK_SENTINELS, '').replace(/\u001b\[([0-9;]*)m/g, (match, params: string) => {
    const codes = params === '' ? ['0'] : params.split(';');
    if (codes.includes('7')) return MARK_OPEN;
    if (codes.includes('27') || codes.includes('0')) return MARK_CLOSE;
    return match;
  });
  return stripAnsi(marked);
}

function stripMarks(value: string): string {
  return value.replace(MARK_SENTINELS, '');
}

// Pull the sentinels back out, leaving clean text plus the [from, to) offsets that
// were highlighted — so Prism only ever sees real source.
function extractMarks(value: string): { text: string; ranges: [number, number][] } {
  if (!value.includes(MARK_OPEN) && !value.includes(MARK_CLOSE)) return { text: value, ranges: [] };
  const ranges: [number, number][] = [];
  let text = '';
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
function applyMarks(segments: SyntaxSegment[], ranges: [number, number][]): SyntaxSegment[] {
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
        out.push({ text: segment.text.slice(cursor - start, hitFrom - start), className: segment.className });
      }
      out.push({ text: segment.text.slice(hitFrom - start, hitTo - start), className: MARK_CLASS });
      cursor = hitTo;
    }
    if (cursor < end) out.push({ text: segment.text.slice(cursor - start), className: segment.className });
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

const META_SEPARATOR = ' · ';
// Wire statuses a transcript row carries while its turn has NOT finished (the
// engine persists the row at submit; the gateway overlay renames `running` to
// `streaming`). Mirrors `IN_FLIGHT_ROW_STATUSES` in `SessionScreen`.
const IN_FLIGHT_STATUSES = new Set(['running', 'streaming', 'queued', 'pending']);

function modelPair(value?: { provider?: string; model?: string }): string | null {
  const provider = value?.provider?.replace(/^:/, '').trim();
  const model = value?.model?.trim().replaceAll('/', '-');
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
  const cost = typeof turn.cost === 'object' && turn.cost ? turn.cost : undefined;
  const model = modelPair(routing.actual) ?? modelPair({
    provider: turn.provider ?? cost?.provider,
    model: turn.model ?? cost?.model,
  });
  const usage = turnUsage(turn);
  const tokens = formatTokens(usage);
  const price = formatCost(usage.cost);
  const duration = formatDuration(turn.duration_ms);
  const parts = [model, tokens, price, duration].filter((part): part is string => Boolean(part));
  return parts.length ? parts.join(META_SEPARATOR) : null;
}

function turnFallbackNote(turn: TranscriptTurn): string | null {
  if (turn.meta_fallback_note?.trim()) return turn.meta_fallback_note.trim();
  const routing = turnRouting(turn);
  const fallbackTypes = new Set([
    'llm.routing/provider-fallback',
    'llm.routing/format-fallback',
  ]);
  // `wire/->wire` transforms Clojure's `:event/type` into `event_type`.
  // Accept `type` too so old persisted traces remain legible.
  const eventType = (item: Record<string, JsonValue>) =>
    String(item.event_type ?? item.type ?? '');
  const retryEvents = routing.trace.filter(
    (item) => eventType(item) === 'llm.routing/provider-retry',
  );
  if (!routing.fallback && !retryEvents.length) return null;

  const fallbackEvent = routing.trace.find((item) => fallbackTypes.has(eventType(item)));
  const event = fallbackEvent ?? retryEvents.at(-1);
  const retries = retryEvents.length;
  const from = modelPair(routing.selected) ?? 'previous model';
  const status = event?.status;
  const reason = event?.reason;
  const error = event?.error;
  const why = status != null
    ? String(status)
    : reason != null
      ? String(reason).replace(/^:/, '')
      : error != null
        ? (typeof error === 'string' ? error : jsonText(error))
        : null;
  const tail = [why, retries > 0 ? `retried ${retries}×` : null]
    .filter((part): part is string => Boolean(part));
  return `↳ from ${from}${tail.length ? ` — ${tail.join(', ')}` : ''}`;
}

function assistantUsage(turn: TranscriptTurn): boolean {
  const usage = turnUsage(turn);
  return usage.input > 0 || usage.output > 0 || usage.cost > 0;
}

function commandTurn(turn: TranscriptTurn): boolean {
  const request = (turn.user_request ?? turn.request ?? '').trimStart();
  return request.startsWith('/') || request.startsWith('!');
}

function toolLabel(name?: string): string {
  if (!name) return 'TOOL';
  return toolLabelOverrides[name] ?? name.toUpperCase();
}

function toolRole(role?: string): { border: string; text: string } {
  const normalized = role?.replace(/^:/, '');
  return (normalized && toolRoleClasses[normalized]) || { border: 'border-accent', text: 'text-accent-ink' };
}

// A fenced block must not be closable by the content it wraps: file text, tool
// stdout, and pretty-printed JSON can all carry ``` runs of their own, and a
// fixed triple-backtick wrapper then closes EARLY — the rest of the payload
// renders as prose (headings, blockquotes) instead of code. CommonMark allows
// longer fences; pick the shortest safe one. Mirrors `strutil/fenced`.
function fenced(body: string, lang = ''): string {
  const longest = (body.match(/`+/g) ?? []).reduce((max, run) => Math.max(max, run.length), 0);
  const delimiter = '`'.repeat(Math.max(3, longest + 1));
  return `${delimiter}${lang}\n${body}\n${delimiter}`;
}

function resultBody(form: TranscriptForm): string {
  if (form.error != null) return jsonText(form.error);
  const rendered = form.result_render?.trimEnd();
  if (rendered) return markAnsiHighlights(rendered);
  if (form.result_summary?.trim()) return '';
  if (form.result == null || form.result === '') return '';
  const raw = jsonText(form.result);
  return typeof form.result === 'string' ? raw : fenced(raw, 'json');
}

function toolCards(form: TranscriptForm): TranscriptForm[] {
  if (form.silent || form.result === 'vis_silent' || form.result === 'vis_answer') return [];
  if (form.cards?.length) return form.cards.flatMap(toolCards);
  return form.tool_name ? [form] : [];
}

function compactToolSummary(name: string | undefined, summary: string): string {
  if (!name || !['patch', 'struct_patch', 'write'].includes(name)) return summary;

  return summary
    .replace(
      /(^| · )(?:(?:update|add|delete|replace|overwrite)\s+|\(no change\)\s+)/g,
      '$1',
    )
    .replaceAll(' · ', ', ');
}

function ToolSummary({ children, className }: { children: string; className: string }) {
  return (
    <span
      className={`min-w-0 flex-1 truncate text-chip font-medium ${className}`}
      title={children}
    >
      <ReactMarkdown
        remarkPlugins={[remarkGfm]}
        allowedElements={['p', 'strong', 'em', 'del', 'code']}
        unwrapDisallowed
        components={{
          p: ({ children: content }) => <>{content}</>,
          strong: ({ children: content }) => <strong className="font-bold">{content}</strong>,
          em: ({ children: content }) => <em>{content}</em>,
          del: ({ children: content }) => <del>{content}</del>,
          code: ({ children: code }) => (
            <code className="mx-px inline rounded-none bg-result-path px-0.5 py-px font-mono text-chip font-medium text-result-path-foreground">
              {code}
            </code>
          ),
        }}
      >
        {children}
      </ReactMarkdown>
    </span>
  );
}

const ToolCard = memo(function ToolCard({ form }: { form: TranscriptForm }) {
  const role = toolRole(form.tool_color_role);
  const resultText = resultBody(form);
  const failed = form.error != null;
  // Once any real outcome has arrived (body/result/render/duration) a stale
  // "Running…" placeholder from the gateway must not linger — the op is done.
  const hasOutcome =
    resultText !== '' ||
    form.result != null ||
    form.result_render != null ||
    form.duration_ms != null;
  const placeholderSummary = form.result_summary?.trim() === 'Running…';
  const rawSummary =
    placeholderSummary && hasOutcome
      ? ''
      : form.result_summary?.trim() || (failed ? 'Failed' : '');
  const running =
    !failed && !hasOutcome && (!form.result_summary || placeholderSummary);
  // A RUNNING call wears the headline its own tool authored (`shell`'s
  // `$ npm test (running)`) instead of the generic wait: the awaiting card is the
  // same card as the finished one, only its outcome is still missing…
  const pendingSummary = form.pending_summary?.trim();
  // …and it paints that tool's own pending BODY underneath — the same sections
  // its finished body is built from — so the card never changes shape when the
  // result lands, and no pending-only dialect exists.
  const body = resultText || (running ? (form.pending_render?.trimEnd() ?? '') : '');
  const summary = compactToolSummary(
    form.tool_name,
    running && pendingSummary ? pendingSummary : rawSummary,
  );
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
  // The running placeholder stays readable: the tool role colour is low-contrast
  // on the light surface, so a running summary uses the neutral result colour.
  const summaryClass = failed ? 'text-err' : running ? 'text-code-result' : role.text;
  const headline = (
    <div className="flex min-w-0 flex-1 items-baseline gap-1.5">
      <span className={`shrink-0 font-mono text-chip font-extrabold tracking-[0.06em] ${failed ? 'text-err' : role.text}`}>
        {toolLabel(form.tool_name)}
      </span>
      {summary && <ToolSummary className={summaryClass}>{summary}</ToolSummary>}
      {/* A finished call that produced NO summary and NO body still says so: an
          otherwise bare "RESULT 39ms" row reads as a rendering bug rather than as
          the empty result it is. `running` keeps the spinner-less placeholder
          quiet until the outcome actually lands. */}
      {!summary && !body && !running && !failed && (
        <span className="min-w-0 flex-1 truncate font-mono text-chip font-medium text-code-duration">none</span>
      )}
      {duration && <span className="shrink-0 font-mono text-chip tabular-nums text-code-duration">{duration}</span>}
    </div>
  );

  if (!body) {
    return (
      <div className={`border-l-2 ${failed ? 'border-err' : role.border} bg-result px-2 py-1`}>
        {headline}
      </div>
    );
  }

  return (
    <details
      className={`group min-w-0 border-l-2 ${failed ? 'border-err' : role.border} bg-result`}
      onToggle={(event) => {
        if (event.currentTarget.open) setWasOpened(true);
      }}
    >
      <summary className="flex min-h-6 list-none cursor-pointer select-none items-center gap-1.5 px-2 py-1 text-code-result hover:bg-hover [&::-webkit-details-marker]:hidden">
        <span className={`${disclosureClass} ${failed ? 'text-err' : role.text}`} aria-hidden="true">›</span>
        {headline}
        {/* ONE copy control per result card: the body's code blocks are frameless
            inside this card and render no chip of their own. */}
        <CopyButton value={body} className="shrink-0" label="Copy result" />
      </summary>
      {/* A tool result is SUBORDINATE to the answer it feeds: its body is `text-meta`
          (10px), the step its compact code blocks and diffs already render at, so the
          card is internally uniform and steps down from the answer's `text-ui`. */}
      {wasOpened && (
        <div className={`min-w-0 overflow-hidden border-t border-code-edge bg-result px-2.5 py-1.5 text-meta text-code-result ${failed ? 'text-code-error-result' : ''}`}>
          {failed ? <pre className="m-0 overflow-x-auto whitespace-pre-wrap break-words font-mono text-meta ">{body}</pre> : <Markdown compact nested>{body}</Markdown>}
        </div>
      )}
    </details>
  );
});

function formCode(form: TranscriptForm): string {
  const nativeTool = form.tool_name && form.tool_name !== 'python_execution';
  // A running native call must show the invocation actually submitted, not its
  // prettified display mirror. Python keeps the canonical formatted surface.
  // Exception: a tool may AUTHOR the pending surface for its own call (marked by
  // `display_language`, e.g. `shell` rendering its commands as a bash block); that
  // block is what the finished card keeps showing, so the raw JSON never appears.
  const authored = (form.display_language ?? '').trim() !== '';
  const source = nativeTool && !authored
    ? form.code ?? form.source ?? form.src ?? form.display_code
    : form.display_code ?? form.code ?? form.source ?? form.src;
  return typeof source === 'string' ? source.trim() : '';
}

/** Highlighting language for a form's code block; python is the default surface. */
function formCodeLanguage(form: TranscriptForm): string {
  const language = (form.display_language ?? '').trim();
  return language || 'python';
}

function formIsRunning(form: TranscriptForm): boolean {
  const summary = form.result_summary?.trim();
  return (
    form.error == null
    && form.result == null
    && form.result_render == null
    && form.duration_ms == null
    && (!summary || summary === 'Running…')
  );
}

// Mirrors `form/running-code-tools`: the only native tools whose submitted source
// stays on screen while the call is still running. Everything else spins behind
// its op-card instead of flashing a bordered code frame for 200ms.
const RUNNING_CODE_TOOLS = new Set(['python_execution', 'shell']);

function showFormCode(form: TranscriptForm, code: string): boolean {
  if (!code) return false;
  if (!form.tool_name) return true;
  // Keep completed native invocations compact behind their result card. A long
  // `shell`/`python_execution` call is the exception while it runs: the submitted
  // command IS the most important fact on screen, and `block.started` carries that
  // exact source plus the Running… sentinel. Python remains evidence after
  // completion as well, whether it passed or failed.
  // …and never once the tool AUTHORED its own pending card body
  // (`pending_render`, mirroring `form/show-running-tool-code?`): that body
  // already IS the submitted command, rendered the way the card renders it, so a
  // raw invocation beside it would say the same thing twice in JSON.
  if (form.pending_render?.trim()) return false;
  return (
    form.tool_name === 'python_execution'
    || (formIsRunning(form) && RUNNING_CODE_TOOLS.has(form.tool_name))
  );
}

const PYTHON_PREVIEW_LINES = 5;

const CollapsibleFormCode = memo(function CollapsibleFormCode(
  {
    value,
    label,
    colorRole,
    language = 'python',
    bare = false,
  }: {
    value: string;
    label: string;
    colorRole?: string;
    language?: string;
    bare?: boolean;
  },
) {
  const [expanded, setExpanded] = useState(false);
  const lines = value.split(/\r?\n/);
  const hiddenLines = Math.max(0, lines.length - PYTHON_PREVIEW_LINES);
  const collapsible = hiddenLines > 0;
  const visibleValue = collapsible && !expanded ? lines.slice(0, PYTHON_PREVIEW_LINES).join('\n') : value;
  const role = toolRole(colorRole ?? 'tool-color/shell');

  // Same frame as the result cards this program produced (see `FormTrace`) and
  // the same tool-coloured rail the TUI paints for its code band — program and
  // results read as ONE stack. The disclosure row is a HEADER (top of the frame,
  // content reveals below it), identical to the `ToolCard` result headline and to
  // the TUI's THINKING accordion: one rule everywhere, so a row always labels the
  // block beneath it and the collapse control never scrolls away with the body.
  return (
    <div className={bare ? 'min-w-0' : 'mb-1 min-w-0 overflow-hidden border border-dialog-edge bg-dialog-edge shadow-[2px_2px_0_var(--dialog-shadow)]'}>
      <div className={`min-w-0 border-l-2 ${role.border} bg-code`}>
        {/* The header row OWNS the copy control (right edge), exactly like the
            `ToolCard` result headline — never a chip floating over the source.
            It is rendered even when the program is too short to collapse, so a
            4-line snippet and a 40-line one carry the same chrome. */}
        <div className="flex min-h-6 items-center gap-1.5 border-b border-code-edge pr-1.5">
          {collapsible ? (
            <button
              type="button"
              data-disclosure-toggle
              className="flex min-h-6 min-w-0 flex-1 cursor-pointer select-none items-center gap-1.5 px-2 py-1 text-left transition-colors hover:bg-hover"
              aria-expanded={expanded}
              onClick={() => setExpanded((current) => !current)}
            >
              <span
                className={`${disclosureClass} ${role.text} ${expanded ? 'rotate-90' : ''}`}
                aria-hidden="true"
              >
                ›
              </span>
              <span className={`truncate font-mono text-chip font-extrabold tracking-[0.06em] ${role.text}`}>
                {expanded ? label : `${label} +${hiddenLines} more`}
              </span>
            </button>
          ) : (
            <span className={`min-w-0 flex-1 select-none truncate px-2 py-1 font-mono text-chip font-extrabold tracking-[0.06em] ${role.text}`}>
              {label}
            </span>
          )}
          <CopyButton value={value} className="shrink-0" label="Copy code" />
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

const CardGrid = memo(function CardGrid(
  { cards, live = false, bare = false }: { cards: TranscriptForm[]; live?: boolean; bare?: boolean },
) {
  if (!cards.length) return null;

  // ONE framed stack per RUN of op-cards, whatever produced them: a python block
  // that printed several results, several native calls in one iteration, or a run
  // of consecutive tool-only iterations (see `IterationTrace`). A frame per call is
  // what read as "some cards are joined, some are not" - one run of work, one frame.
  return (
    <div
      className={`grid grid-cols-[minmax(0,1fr)] gap-px${bare ? '' : ' overflow-hidden border border-dialog-edge bg-dialog-edge shadow-[2px_2px_0_var(--dialog-shadow)]'}${live ? ` ${transcriptRiseClass}` : ''}`}
      aria-label={`${cards.length} ${cards.length === 1 ? 'result' : 'results'}`}
    >
      {cards.map((card, cardIndex) => (
        <ToolCard
          key={`${card.scope ?? card.tool_name ?? 'tool'}-${cardIndex}`}
          form={card}
        />
      ))}
    </div>
  );
});

const FormTrace = memo(function FormTrace(
  { form, live = false }: { form: TranscriptForm; live?: boolean },
) {
  if (form.silent || form.result === 'vis_silent' || form.result === 'vis_answer') return null;
  const code = formCode(form);
  const showCode = showFormCode(form, code);
  const cards = toolCards(form);
  if (!showCode && !cards.length) return null;
  const codeLabel = form.tool_name === 'python_execution' ? 'PYTHON' : toolLabel(form.tool_name);

  return (
    <div className={live ? `min-w-0 ${transcriptRiseClass}` : 'min-w-0'}>
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
          <CollapsibleFormCode value={code} label={codeLabel} colorRole={form.tool_color_role} language={formCodeLanguage(form)} bare />
          <CardGrid cards={cards} bare />
        </div>
      ) : (
        <>
          {showCode && <CollapsibleFormCode value={code} label={codeLabel} colorRole={form.tool_color_role} language={formCodeLanguage(form)} />}
          <CardGrid cards={cards} />
        </>
      )}
    </div>
  );
});

const ENCRYPTED_REASONING_PLACEHOLDER =
  '[provider returned encrypted reasoning; plaintext reasoning is unavailable]';

/** Mirrors com.blockether.vis.internal.render/normalize-reasoning. */
function normalizeReasoning(value: string): string {
  return value
    .replace(/[ \t\r\f\v]+\r?\n/g, '\n')
    .replace(/(?:\r?\n){2,}/g, '\n')
    .replace(/([.!?…]["')\]]?)\r?\n(?=\S)/g, '$1\n\n')
    .trim();
}

const REASONING_PREVIEW_LINES = 3;
// Mirrors com.blockether.vis.internal.render/reasoning-collapse-min-hidden (3).
// A disclosure that buys back one or two clipped rows is pure friction — you
// uncollapse just to read one more line — so a barely-overflowing trace renders
// inline, in full, with no toggle at all. Same rule as the TUI band and the
// Clojure transcript split; keep the three in step.
const REASONING_COLLAPSE_MIN_HIDDEN = 3;

// Collapsed-height measurement for THINKING bands, batched across the whole
// transcript. One shared observer and one animation-frame flush prevent a
// long session from doing N independent layout reads on rotation.
const bandMeasures = new WeakMap<Element, () => void>();
const observedBands = new Set<Element>();
const pendingBands = new Set<Element>();
let bandFrame: number | null = null;
let bandObserver: ResizeObserver | null = null;

function flushBands() {
  bandFrame = null;
  if (isViewportRotating()) return;
  const targets = [...pendingBands];
  pendingBands.clear();
  for (const band of targets) bandMeasures.get(band)?.();
}

function scheduleBands(bands: Iterable<Element>) {
  for (const band of bands) pendingBands.add(band);
  if (bandFrame !== null || typeof window === 'undefined') return;
  bandFrame = window.requestAnimationFrame(flushBands);
}

function observeBand(band: Element, measure: () => void): () => void {
  if (typeof ResizeObserver === 'undefined') return () => {};
  bandMeasures.set(band, measure);
  observedBands.add(band);
  if (!bandObserver) {
    bandObserver = new ResizeObserver((entries) =>
      scheduleBands(entries.map((entry) => entry.target)),
    );
    onViewportRotation((phase) => {
      if (phase === 'end') scheduleBands(observedBands);
    });
  }
  bandObserver.observe(band);
  return () => {
    bandObserver?.unobserve(band);
    observedBands.delete(band);
    pendingBands.delete(band);
    bandMeasures.delete(band);
  };
}

export const ThinkingBand = memo(function ThinkingBand({ children }: { children: string }) {
  const normalized = normalizeReasoning(children);
  const bodyRef = useRef<HTMLDivElement>(null);
  const [isExpandRequested, setExpandRequested] = useState(false);
  const [hiddenRows, setHiddenRows] = useState(0);

  useLayoutEffect(() => {
    const body = bodyRef.current;
    if (!body) return;

    const measure = () => {
      const lineHeight = Number.parseFloat(window.getComputedStyle(body).lineHeight) || 20;
      const previewHeight = lineHeight * REASONING_PREVIEW_LINES;
      const hiddenHeight = Math.max(0, body.scrollHeight - previewHeight);
      const nextHiddenRows = Math.ceil(hiddenHeight / lineHeight);
      setHiddenRows(nextHiddenRows >= REASONING_COLLAPSE_MIN_HIDDEN ? nextHiddenRows : 0);
    };

    measure();
    return observeBand(body, measure);
  }, [normalized]);

  // Collapsing is derived, not stored: a block with nothing hidden is never expanded.
  const expanded = isExpandRequested && hiddenRows > 0;
  if (!normalized || normalized === ENCRYPTED_REASONING_PLACEHOLDER) return null;
  const collapsible = hiddenRows >= REASONING_COLLAPSE_MIN_HIDDEN;

  return (
    <section className="my-2 min-w-0 bg-thinking-surface px-3 py-2 text-ui text-thinking">
      {collapsible && (
        <button
          type="button"
          data-disclosure-toggle
          className="mb-1 flex min-h-6 w-full items-center gap-1.5 text-left font-mono text-chip font-bold not-italic tracking-[0.07em] text-thinking transition-colors hover:text-dialog-hint-key"
          aria-expanded={expanded}
          onClick={() => setExpandRequested((value) => !value)}
        >
          <span aria-hidden="true">{expanded ? '▾' : '▸'}</span>
          <span>{expanded ? 'THINKING' : `THINKING +${hiddenRows} more`}</span>
        </button>
      )}
      <div
        ref={bodyRef}
        className={`${collapsible && !expanded ? 'max-h-[3.75rem] overflow-hidden' : ''} min-w-0 italic`}
      >
        <Markdown compact hardBreaks>{normalized}</Markdown>
      </div>
    </section>
  );
});


// ONE artifact a tool call produced (a matplotlib figure, a `vis_attach`ed
// image). The gateway ships descriptors only, never bytes, so the picture is
// pulled from the attachment endpoint on first paint — with the auth headers an
// `<img src>` cannot carry, hence the object URL. This is the app's twin of the
// TUI's inline image: the SAME produced artifact, painted where it was made.
const AttachmentTile = memo(function AttachmentTile({
  client,
  sid,
  attachment,
}: {
  client: GatewayClient;
  sid: string;
  attachment: IterationAttachment;
}) {
  const [url, setUrl] = useState<string | null>(null);
  // Bumped when the browser refuses the URL we handed it — the client's object
  // URL cache is bounded, so a picture parked off-screen long enough can have
  // been revoked under it. Re-asking repopulates the cache; it is not a retry
  // loop, because a genuinely broken artifact gives up after the second try.
  const [attempt, setAttempt] = useState(0);
  const [failed, setFailed] = useState(false);
  const iterationId = attachment.iteration_id ?? '';
  const index = attachment.index ?? 0;
  const isVideo = attachmentIsVideo(attachment);
  const isPlayable = isVideo || attachmentIsImage(attachment);
  const name = attachment.filename || 'attachment';

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
      <div className="mt-2 min-w-0 truncate font-mono text-chip text-footer-muted">{`↗ ${name}`}</div>
    );
  }

  // ONE reserved box for the whole life of the slot — see `lib/media-frame`. A
  // failure that arrives after the bytes were requested says so INSIDE the box
  // it already holds: collapsing to a text line here would shove the reader
  // just as hard as growing did.
  const slot: MediaSlotState = failed ? 'failed' : url ? 'ready' : 'pending';

  return (
    <figure className="mt-2.5 min-w-0">
      <div className={mediaSlotFrame(slot)}>
        {failed ? (
          <div className="flex h-full w-full items-center bg-thinking-surface px-2 font-mono text-chip text-footer-muted">
            {`✗ ${name}`}
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
            className={mediaContentClass}
          />
        )}
      </div>
      <figcaption className="mt-1 truncate font-mono text-chip text-footer-muted">{name}</figcaption>
    </figure>
  );
});

// Non-image artifacts (a scratch .py, a csv, a pdf) are RECORDED, not painted:
// the capture tap writes them to the session DB and they NEVER enter the
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
    const name = attachment.filename || 'attachment';
    const key = `${name}:${attachment.size ?? 0}`;
    const seen = byIdentity.get(key);
    if (seen) seen.count += 1;
    else {
      byIdentity.set(key, {
        key,
        name,
        media: attachment.media_type ?? '',
        size: attachment.size,
        count: 1,
      });
    }
  });
  return [...byIdentity.values()];
}

// The reader for a document artifact. Nothing is fetched until it is asked for:
// a transcript holding a dozen reports must not download and decode a dozen PDFs
// to paint. The bytes then land inside `DocPreview`'s sandboxed frame, which is
// a separate document with its own CSS scope and an opaque origin — untrusted
// markup can neither restyle the app nor read its storage. See `DocArtifact`.
const AttachmentDocTile = memo(function AttachmentDocTile({
  client,
  sid,
  attachment,
}: {
  client: GatewayClient;
  sid: string;
  attachment: IterationAttachment;
}) {
  const [url, setUrl] = useState<string | null>(null);
  const [failed, setFailed] = useState(false);
  const [wanted, setWanted] = useState(false);
  const iterationId = attachment.iteration_id ?? '';
  const index = attachment.index ?? 0;
  const name = attachment.filename || 'document';

  useEffect(() => {
    if (!wanted || !iterationId || !sid) return;
    let alive = true;
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
      mime={attachment.media_type ?? ''}
      sizeLabel={attachmentBytes(attachment.size)}
      url={url}
      failed={failed}
      onOpen={(open) => {
        if (open) setWanted(true);
      }}
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
  const media = attachments.filter(
    (entry) => attachmentIsPlayable(entry) || attachmentIsDoc(entry),
  );
  const page = pageBySize(media, (entry) => entry.size, pages, RAIL_PAGE);
  const playable = page.shown.filter(attachmentIsPlayable);
  const docs = page.shown.filter(attachmentIsDoc);
  const files = recordedFiles(
    attachments.filter((entry) => !attachmentIsPlayable(entry) && !attachmentIsDoc(entry)),
  );
  const total = files.reduce((sum, file) => sum + file.count, 0);
  const head = files[0];
  const rest = head ? total - head.count : 0;

  return (
    <>
      {playable.map((attachment) => (
        <AttachmentTile
          key={`${attachment.iteration_id ?? 'iter'}-${attachment.index}`}
          client={client}
          sid={sid}
          attachment={attachment}
        />
      ))}
      {docs.map((attachment) => (
        <AttachmentDocTile
          key={`doc-${attachment.iteration_id ?? 'iter'}-${attachment.index}`}
          client={client}
          sid={sid}
          attachment={attachment}
        />
      ))}
      {page.rest.length > 0 && (
        <button
          type="button"
          onClick={() => setPages((current) => current + 1)}
          aria-label={`Load ${page.restLabel} of attachments`}
          className="mt-2 flex min-h-11 w-full min-w-0 items-center gap-1.5 border border-dialog-edge px-2 text-left font-mono text-chip text-footer-muted hover:bg-hover focus-visible:outline-2 focus-visible:outline-accent mouse:min-h-8"
        >
          <span aria-hidden="true" className="shrink-0 opacity-70">↓</span>
          <span className="min-w-0 truncate">Load {page.restLabel}</span>
        </button>
      )}
      {head && (
        <div className="mt-2 min-w-0">
          <button
            type="button"
            aria-expanded={open}
            onClick={() => setOpen((current) => !current)}
            className="flex min-h-8 w-full min-w-0 items-center gap-1.5 text-left font-mono text-chip text-footer-muted"
          >
            <span aria-hidden="true" className="shrink-0 opacity-70">{open ? '▾' : '▸'}</span>
            <span className="min-w-0 truncate">
              ↗ {head.name}
              {head.count > 1 ? ` ×${head.count}` : ''}
            </span>
            {rest > 0 && <span className="shrink-0 opacity-70">+{rest} more</span>}
          </button>
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
                  ].filter(Boolean).join(' · ')}
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
// A fixed batch size cannot be right: segment weight spans three orders of
// magnitude (a one-line reply vs. a run of 400 tool cards), so any constant is
// either a stall on light turns or a dropped frame on heavy ones. Measured on
// device a flat 12/frame cost 14-20 long frames and one 106 ms frame on a
// 400-call session. Aim each step at a slice of the frame instead: bill it for
// the render plus the forced layout the scroll hold already does, and back off
// hard the moment the frame it landed in actually dropped — a pure "grow while
// frames stay whole" window overshoots (it reached 24 and spent 15 frames over
// 25 ms), because once the tree is memoised the cost really is roughly linear
// in batch size.
const SEGMENT_RAMP_START = 4;
const SEGMENT_RAMP_MIN = 2;
const SEGMENT_RAMP_MAX = 32;
/** Per-step work budget, leaving the rest of a 60 Hz frame for style and paint. */
const RAMP_BUDGET_MS = 6;
/** A step whose whole frame took longer than this dropped one; halve on sight. */
const RAMP_DROPPED_FRAME_MS = 32;

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

/** The scroller this trace lives in, or the document's, so the ramp can hold it. */
function scrollParent(node: HTMLElement | null): HTMLElement | null {
  for (let el = node?.parentElement ?? null; el; el = el.parentElement) {
    const overflowY = window.getComputedStyle(el).overflowY;
    if (overflowY === 'auto' || overflowY === 'scroll') return el;
  }
  return (document.scrollingElement as HTMLElement | null) ?? null;
}

function traceEntry(iteration: TranscriptIteration, index: number) {
  return {
    iteration,
    index,
    thinking: iteration.thinking?.trim() ?? '',
    prose: iteration.assistant_prose?.trim() ?? '',
    forms: iteration.forms ?? [],
    attachments: iteration.attachments ?? [],
  };
}

type TraceEntry = ReturnType<typeof traceEntry>;
type TraceSegmentData = { key: string; head: TraceEntry; items: TraceEntry[]; closed: boolean };
type Chunk =
  | { kind: 'code'; key: string; form: TranscriptForm }
  | { kind: 'cards'; key: string; cards: TranscriptForm[] };

// Consecutive TOOL-ONLY iterations are one run of work, not N bubbles: the model
// kept calling tools without saying anything in between. Mirrors the TUI
// (`render/merge-iteration-entries`): a narrated iteration may OPEN a run (its
// thinking / prose renders above the cards), an interior narrated call closes it,
// and so does an iteration that produced attachments (those render last).
function buildSegments(iterations: TranscriptIteration[]): TraceSegmentData[] {
  const visible = iterations
    .map((iteration, index) => traceEntry(iteration, index))
    .filter(({ thinking, prose, forms, attachments }) =>
      thinking
      || prose
      || attachments.length
      || forms.some((form) => showFormCode(form, formCode(form)) || toolCards(form).length),
    );

  const segments: TraceSegmentData[] = [];
  visible.forEach((entry) => {
    const open = segments.at(-1);
    if (open && !open.closed && !entry.thinking && !entry.prose) open.items.push(entry);
    else {
      segments.push({
        key: String(entry.iteration.id ?? entry.iteration.position ?? entry.index),
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
// re-rendering settled cards. `segment` comes from a memoised `buildSegments`,
// so its identity only changes when the transcript really did.
const TraceSegment = memo(function TraceSegment({
  segment,
  live,
  client,
  sid,
}: {
  segment: TraceSegmentData;
  live: boolean;
  client?: GatewayClient;
  sid?: string;
}) {
  // Inside a segment, adjacent code-less forms pool into ONE grid; a python
  // block keeps its own frame under its source and starts a new pool after it.
  const chunks = useMemo(() => {
    const built: Chunk[] = [];
    segment.items.forEach((entry) => {
      entry.forms.forEach((form, formIndex) => {
        if (form.silent || form.result === 'vis_silent' || form.result === 'vis_answer') return;
        const key = `${entry.index}-${formIndex}-${form.scope ?? form.tool_name ?? 'form'}`;
        if (showFormCode(form, formCode(form))) {
          built.push({ kind: 'code', key, form });
          return;
        }
        const cards = toolCards(form);
        if (!cards.length) return;
        const pool = built.at(-1);
        if (pool?.kind === 'cards') pool.cards.push(...cards);
        else built.push({ kind: 'cards', key, cards: [...cards] });
      });
    });
    return built;
  }, [segment]);
  const attachments = useMemo(
    () => segment.items.flatMap((entry) => entry.attachments),
    [segment],
  );

  return (
    <section className={live ? `min-w-0 ${transcriptEnterClass}` : 'min-w-0'}>
      {segment.head.thinking && <ThinkingBand>{segment.head.thinking}</ThinkingBand>}
      {segment.head.prose && (
        <div className="my-2.5 text-ui text-vis-message">
          <Markdown>{segment.head.prose}</Markdown>
        </div>
      )}
      {/* Chunk-to-chunk breathing room: each chunk is one call (its program
          glued to its own results), so the ONLY whitespace in the stack
          falls BETWEEN calls. */}
      {chunks.length > 0 && (
        <div className="grid min-w-0 gap-2.5">
          {chunks.map((chunk) => (chunk.kind === 'code'
            ? <FormTrace key={chunk.key} form={chunk.form} live={live} />
            : <CardGrid key={chunk.key} cards={chunk.cards} live={live} />))}
        </div>
      )}
      {client && sid && <AttachmentRail client={client} sid={sid} attachments={attachments} />}
    </section>
  );
});

export const IterationTrace = memo(function IterationTrace({
  iterations,
  live = false,
  client,
  sid,
}: {
  iterations: TranscriptIteration[];
  live?: boolean;
  client?: GatewayClient;
  sid?: string;
}) {
  const rootRef = useRef<HTMLDivElement>(null);
  const scrollerRef = useRef<HTMLElement | null>(null);
  // What this trace measured in the frame that asks for more segments: its own
  // height, and how far the reader was from the end. `null` means this render
  // was not a ramp step.
  const rampFromRef = useRef<{
    own: number;
    gap: number;
  } | null>(null);
  // Identity in the ramp queue, so only the bottom-most trace backfills at once.
  const [rampId] = useState(() => Symbol('trace-ramp'));
  useEffect(() => () => releaseRamp(rampId), [rampId]);
  // Adaptive ramp step: how many segments the next frame mounts, when the
  // current one started (0 = none in flight), and what the last one cost.
  const stepRef = useRef({ size: SEGMENT_RAMP_START, startedAt: 0, work: 0 });
  const [mountedSegments, setMountedSegments] = useState(SEGMENT_FIRST_PAINT);

  const segments = useMemo(() => buildSegments(iterations), [iterations]);

  const rampDone = mountedSegments >= segments.length;

  // Before the browser paints the segments the ramp just mounted: a reader at
  // the bottom stays at the bottom. Holding anyone ELSE's line is deliberately
  // not done here — see `lib/reader-gesture`; `SessionScreen` anchors the whole
  // scroller once, and a second opinion in this effect double-billed every
  // frame a "↑ Load earlier" shared with a ramp step.
  useLayoutEffect(() => {
    const from = rampFromRef.current;
    rampFromRef.current = null;
    const scroller = scrollerRef.current;
    const root = rootRef.current;
    // Reading this height forces the layout of everything the step just mounted:
    // the honest price of this batch, charged to it below instead of leaking into
    // the next frame.
    const grew = from && root ? root.offsetHeight - from.own : 0;
    if (from && scroller && root && (grew !== 0 || from.gap <= 8)) {
      // Whether the reader was following the end is measured BEFORE the batch
      // lands, in the frame that scheduled it. Deriving it afterwards by
      // subtracting this trace's growth silently assumed the growth was appended
      // below the reader — true for the newest trace, false for the 400 traces a
      // "load earlier" prepends ABOVE them. That assumption turned every 2 000 px
      // batch into "they are within 2 000 px of the end, so pin them there" and
      // fired 16 unwanted jumps worth 46 000 px in one load.
      // Pin only a reader who was already at the end AND is not touching the
      // scroller: a slow drag travels less than this tolerance in one frame, so
      // the pin would otherwise swallow the drag itself.
      if (from.gap <= 8 && !readerOwnsScroll()) scroller.scrollTop = scroller.scrollHeight;
    }

    // Price this batch: everything above is its render and its forced layout.
    // The frame it lands in is judged separately, in the next ramp frame.
    const step = stepRef.current;
    if (step.startedAt > 0) step.work = performance.now() - step.startedAt;
  }, [mountedSegments]);

  // A chunk per frame, so the work the first paint skipped never lands as one
  // long frame either.
  useEffect(() => {
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
      // Grow on the measured work, but if that frame was actually dropped, the
      // estimate was wrong about style/paint: halve and re-learn from there.
      // After waiting, that span is somebody else's work: do not price on it.
      const step = stepRef.current;
      if (step.startedAt > 0 && !waited) {
        const frameCost = performance.now() - step.startedAt;
        const scaled = Math.round((step.size * RAMP_BUDGET_MS) / Math.max(step.work, 0.5));
        const grown = Math.min(Math.ceil(step.size * 1.5), Math.max(1, scaled));
        const next = frameCost > RAMP_DROPPED_FRAME_MS ? Math.floor(step.size / 2) : grown;
        step.size = Math.min(SEGMENT_RAMP_MAX, Math.max(SEGMENT_RAMP_MIN, next));
      }

      const scroller = scrollerRef.current ?? scrollParent(rootRef.current);
      scrollerRef.current = scroller;
      const root = rootRef.current;
      rampFromRef.current =
        scroller && root
          ? {
              own: root.offsetHeight,
              // Measured here, before the batch lands, because only now does it
              // mean "the reader is following the end".
              gap: scroller.scrollHeight - scroller.scrollTop - scroller.clientHeight,
            }
          : null;
      step.startedAt = performance.now();
      setMountedSegments((count) => count + step.size);
    };
    frame = window.requestAnimationFrame(tick);
    return () => window.cancelAnimationFrame(frame);
  }, [mountedSegments, rampDone, rampId]);

  if (!segments.length) return null;
  const shown = rampDone ? segments : segments.slice(segments.length - mountedSegments);


  return (
    <div ref={rootRef} className="mb-2.5 grid gap-2.5">
      {shown.map((segment) => (
        <TraceSegment key={segment.key} segment={segment} live={live} client={client} sid={sid} />
      ))}
    </div>
  );
});

export const ContentBlockView = memo(function ContentBlockView({ block }: { block: ContentBlock }) {
  switch (block.type) {
    case 'prose':
      return block.markdown ? <Markdown>{block.markdown}</Markdown> : null;
    case 'code':
      return <Markdown>{fenced(block.text ?? '', block.language ?? '')}</Markdown>;
    case 'reasoning':
      return block.text ? <ThinkingBand>{block.text}</ThinkingBand> : null;
    case 'tool': {
      const form: TranscriptForm = {
        tool_name: block.tool ?? 'tool',
        result_summary: block.status,
        result_render: block.output == null ? undefined : jsonText(block.output),
        error: block.error,
      };
      return <ToolCard form={form} />;
    }
    case 'error':
      return (
        <div className="my-2 flex gap-2 border border-warn-edge bg-warn-surface px-2.5 py-2 font-mono text-meta text-err">
          <strong>{block.code}</strong><span>{block.message}</span>
        </div>
      );
    case 'attachment':
      return (
        <div className="my-2 w-fit border border-dialog-edge bg-panel px-2.5 py-1.5 font-mono text-meta text-dialog-foreground">
          ↗ {block.name ?? 'Attachment'} <small className="ml-2 text-dialog-hint">{block.media_type}</small>
        </div>
      );
    case 'notice':
      return <div className="my-2 border border-dialog-edge bg-panel px-2.5 py-2 font-mono text-meta text-dialog-hint">{block.message}</div>;
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
  return '';
}

function runningTurnPhase(turn: TranscriptTurn): string {
  const iterations = turn.iterations ?? [];
  const iteration = iterations.length;
  const request = (turn.user_request ?? turn.request ?? '').trim();
  if (iteration === 0) {
    if (request.startsWith('!&')) return 'Vis is starting a command';
    if (request.startsWith('!')) return 'Vis is running a command';
    if (request.startsWith('/')) return `Vis is running: ${request.split(/\s+/, 1)[0]}`;
    return 'Vis is waiting for an update';
  }
  const last = iterations.at(-1);
  const suffix = `(iter ${iteration})`;
  if (last?.error != null) return `Vis is retrying ${suffix}`;
  if (last?.forms?.length) return `Vis is running code ${suffix}`;
  if (last?.thinking?.trim()) return `Vis is thinking ${suffix}`;
  return `Vis is working ${suffix}`;
}

function LiveProgress({ phase, startedAt }: { phase: string; startedAt?: number }) {
  const [now, setNow] = useState(() => Date.now());

  useEffect(() => {
    const timer = window.setInterval(() => setNow(Date.now()), 100);
    return () => window.clearInterval(timer);
  }, []);

  const elapsed = formatDuration(Math.max(0, now - (startedAt ?? now))) ?? '0ms';

  return (
    <>
      <div
        className="mt-5 truncate whitespace-nowrap font-mono text-ui text-vis-message"
        aria-hidden="true"
      >
        <Spinner />
        <span>&nbsp;&nbsp;{phase}...&nbsp;&nbsp;{elapsed}</span>
      </div>
      <span className="sr-only" role="status">{phase}</span>
    </>
  );
}

export const AssistantMessage = memo(function AssistantMessage({
  turn,
  streaming = false,
  activity,
  startedAt,
  settled = false,
  client,
  sid,
}: {
  turn: TranscriptTurn;
  streaming?: boolean;
  activity?: string;
  startedAt?: number;
  /**
   * The caller KNOWS this row's turn is over (the gateway is not running this
   * session, or its terminal frame already landed) even though the persisted
   * row still reads `running` — persistence lags the terminal event. Without
   * it the placeholder row paints the live ticker, and "Vis is thinking..."
   * kept spinning at the bottom for a turn that had long finished.
   */
  settled?: boolean;
  client?: GatewayClient;
  sid?: string;
}) {
  const blocks = turn.content ?? [];
  const fallback = blocks.length ? '' : fallbackAnswer(turn);
  const cancelled = turn.status === 'cancelled' || turn.prior_outcome === 'cancelled';
  // A row still IN FLIGHT has no footer to show. Usage, cost and duration only
  // exist once the turn ends, so mid-turn the summary degrades to a bare
  // `provider/model` (the gateway stamps it from the last completed iteration's
  // routing) — a finished-looking meta line under a turn that is still working.
  const inFlight = streaming || IN_FLIGHT_STATUSES.has(String(turn.status ?? ''));
  const meta = !inFlight && !commandTurn(turn) && (!cancelled || assistantUsage(turn))
    ? turnMetaSummary(turn)
    : null;
  const fallbackNote = meta && !cancelled ? turnFallbackNote(turn) : null;

  // Keep a turn an ordinary paint subtree: both a `content-visibility`
  // placeholder (which corrects its guessed height above the reader) and a
  // `contain:layout style` boundary (which WebKit rasterizes only once it
  // enters the viewport) made big turns arrive late while scrolling — the
  // shift and the white bands. See the note at the top of this file.
  return (
    <article className="mt-4 w-full" aria-busy={streaming}>
      <div className={`mb-1 font-mono text-meta font-bold ${cancelled ? 'text-dialog-hint' : 'text-vis-role'}`}>Vis</div>
      <div className="min-w-0">
        <IterationTrace iterations={turn.iterations ?? []} live={streaming} client={client} sid={sid} />
        {/* Message prose sits on the SAME canonical step as the trace it grows out of:
            tool results, thinking bands and code cards are all `text-ui` (11px), so an
            answer at `text-body` (12px) was one px of drift, not a hierarchy. The role
            label (`text-meta`) and the meta footer (`text-chip`) still step down from it. */}
        <div className={`bg-answer text-ui ${cancelled ? 'italic text-cancelled-foreground' : 'text-answer-foreground'}`}>
          {blocks.map((block) => <ContentBlockView key={block.id} block={block} />)}
          {fallback && <Markdown>{fallback}</Markdown>}
          {!streaming && !blocks.length && !fallback && turn.status !== 'completed' && turn.status !== 'running' && (
            <span>{cancelled ? 'Cancelled by user.' : turn.status ?? 'No response'}</span>
          )}
        </div>
        {streaming ? (
          <LiveProgress phase={activity ?? 'Vis is working'} startedAt={startedAt} />
        ) : turn.status === 'running' && !settled ? (
          <LiveProgress phase={runningTurnPhase(turn)} startedAt={turn.created_at} />
        ) : null}
        {meta && (
          <footer className="mt-5 min-w-0 text-right font-mono text-chip text-footer-muted">
            <div className="overflow-hidden text-ellipsis whitespace-nowrap" title={meta}>{meta}</div>
            {fallbackNote && (
              <div className="overflow-hidden text-ellipsis whitespace-nowrap italic text-footer-muted" title={fallbackNote}>
                {fallbackNote}
              </div>
            )}
          </footer>
        )}
      </div>
    </article>
  );
});

export const UserMessage = memo(function UserMessage(
  { children, attachments }: { children: string; attachments?: GatewayAttachment[] },
) {
  const parts = parseUserMessage(children);
  // Persisted user images re-render from DB-owned base64 (survives a restart even
  // after the original clipboard/temp source file is gone). Tool artifacts render
  // in the assistant trace, so only the `user` rail belongs in the user bubble.
  const mediaAttachments = (attachments ?? []).filter(
    (a) => (a.source ?? 'user') === 'user'
      && !!a.base64
      && (!!a.media_type?.startsWith('image/') || !!a.media_type?.startsWith('video/')),
  );
  // The user bubble keeps its OWN best-effort rule, decided in JS. `Markdown` can justify
  // unconditionally because it owns elements to scope `break-all` to (inline `code`, links);
  // this bubble is ONE raw-text run, so `break-all` here would chop ordinary words too. A
  // pasted path or URL is therefore still an unbreakable atom, and a non-text part (an image
  // chip, a collapsed paste) is an atom too, so either one sends the whole bubble ragged
  // instead of letting one line stretch to a river.
  const isJustifiable = parts.every(
    (part) => part.type === 'text' && !/\S{24,}/u.test(part.text),
  );
  return (
    <article className="mt-4 w-full [contain:layout_style]">
      <div className="mb-1 font-mono text-meta font-bold text-you-role">You</div>
      <div className={`block w-full whitespace-pre-wrap break-words hyphens-auto [hyphenate-limit-chars:6_3_3] border-l-2 border-you-role bg-code px-3 py-2 text-ui text-pretty text-you-message-foreground ${isJustifiable ? 'text-justify' : 'text-left'}`}>
        {parts.map((part) => part.type === 'text' ? (
          <span key={part.key}>{part.text}</span>
        ) : part.type === 'image' ? (
          <span key={part.key} className="my-1 mr-1 inline-flex items-center gap-1 border border-code-edge bg-code px-2 py-1 align-middle font-mono text-meta text-dialog-hint first:mt-0">
            {part.summary}
          </span>
        ) : (
          // Full-bleed to the bubble's own `px-3`: the rules then read as the
          // bubble's dividers instead of a stray box hairline-close to the
          // sentence above and below it. Vertical margin is the paste's ONLY
          // separation from that prose, so it stays wider than the block's own
          // padding.
          <details key={part.key} className="group -mx-3 my-3 block max-w-none border-y border-code-edge bg-code text-code-foreground first:mt-0 last:mb-0">
            <summary className="cursor-pointer list-none select-none px-3 py-2 font-mono text-meta font-semibold text-accent-ink marker:hidden [&::-webkit-details-marker]:hidden">
              <span className="mr-1.5 inline-block text-dialog-hint transition-transform group-open:rotate-90">▸</span>
              {part.summary}
            </summary>
            <pre className="max-h-[min(28rem,60dvh)] overflow-auto overscroll-contain border-t border-code-edge px-3 py-2 font-mono text-meta [tab-size:2]">
              <code>{part.content}</code>
            </pre>
          </details>
        ))}
      </div>
      {mediaAttachments.length > 0 && (
        <div className="mt-2 flex flex-col items-start gap-2">
          {mediaAttachments.map((att, i) => {
            const key = att.id ?? `att-${i}`;
            const src = att.base64.startsWith('data:')
              ? att.base64
              : `data:${att.media_type};base64,${att.base64}`;
            // The clip the user sent replays from the SAME DB-owned bytes as a
            // picture, so it survives a restart after the source file is gone.
            //
            // These bytes are inline, so there is no pulse to swap out — but a
            // picture whose box is its own decoded size still reserves NOTHING
            // until it decodes, and on iOS that decode happens as the bubble
            // nears the viewport, i.e. mid-scroll. Same reserved frame as a
            // produced artifact, for the same reason.
            return att.media_type?.startsWith('video/') ? (
              <div key={key} className={`${mediaFrameClass} overflow-hidden rounded border border-code-edge`}>
                <video
                  src={src}
                  controls
                  playsInline
                  preload="metadata"
                  className={mediaContentClass}
                />
              </div>
            ) : (
              <ExpandableImage
                key={key}
                src={src}
                alt={att.filename ?? 'attachment'}
                frameClassName={`${mediaFrameClass} overflow-hidden rounded border border-code-edge`}
                className={mediaContentClass}
              />
            );
          })}
        </div>
      )}
    </article>
  );
});
