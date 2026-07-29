import {
  isValidElement,
  memo,
  useEffect,
  useLayoutEffect,
  useRef,
  useState,
  type MouseEvent,
  type ReactNode,
} from 'react';
import Prism from 'prismjs';
import 'prismjs/components/prism-bash';
import 'prismjs/components/prism-clojure';
import 'prismjs/components/prism-css';
import 'prismjs/components/prism-diff';
import 'prismjs/components/prism-java';
import 'prismjs/components/prism-json';
import 'prismjs/components/prism-markdown';
import 'prismjs/components/prism-python';
import 'prismjs/components/prism-typescript';
import 'prismjs/components/prism-jsx';
import 'prismjs/components/prism-tsx';
import 'prismjs/components/prism-yaml';
import ReactMarkdown from 'react-markdown';
import remarkGfm from 'remark-gfm';
import { parseUserMessage } from '../lib/paste';
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

type DiffLineKind = 'meta' | 'hunk' | 'add' | 'del' | 'ctx';

function diffLineKind(line: string): DiffLineKind {
  if (line.startsWith('+++') || line.startsWith('---')) return 'meta';
  if (line.startsWith('@@')) return 'hunk';
  if (line.startsWith('+')) return 'add';
  if (line.startsWith('-')) return 'del';
  return 'ctx';
}

function codeLanguage(node: ReactNode): string {
  if (!isValidElement<{ className?: string }>(node)) return '';
  return /(?:^|\s)language-([\w-]+)/.exec(node.props.className ?? '')?.[1]?.toLowerCase() ?? '';
}

const DiffBlock = memo(function DiffBlock({ value, compact, frameless = false }: { value: string; compact: boolean; frameless?: boolean }) {
  const lineClasses: Record<DiffLineKind, string> = {
    meta: 'text-code-duration',
    hunk: 'text-code-syntax-keyword',
    add: 'bg-code-ok text-code-success',
    del: 'bg-code-err text-code-error',
    ctx: 'text-code-foreground',
  };

  return (
    <div
      className={`${compact ? 'my-2' : 'my-3'} relative overflow-hidden bg-code ${frameless ? '' : 'border border-code-edge'}`}
      aria-label="Unified diff"
    >
      {!frameless && <CopyButton value={value} />}
      <pre
        className={`${compact ? 'text-meta ' : 'text-ui '} m-0 max-w-full overflow-x-auto overscroll-x-contain py-2 font-mono`}
      >
        {value.split('\n').map((line, index) => (
          <span
            className={`block min-w-full w-fit whitespace-pre px-3 ${frameless ? '' : 'first:pr-16'} ${lineClasses[diffLineKind(line)]}`}
            key={`${index}-${line}`}
          >
            {line || ' '}
          </span>
        ))}
      </pre>
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
  if (!grammar) return [{ text: value, className: 'text-code-foreground' }];
  const out: SyntaxSegment[] = [];
  flattenSyntax(Prism.tokenize(value, grammar), 'text-code-foreground', out);
  return out;
}

// Split a flat segment stream into per-line segment arrays, preserving the
// class of tokens (e.g. block comments) that span multiple newlines.
function segmentsToLines(segments: SyntaxSegment[]): SyntaxSegment[][] {
  const lines: SyntaxSegment[][] = [[]];
  for (const segment of segments) {
    const parts = segment.text.split('\n');
    parts.forEach((part, index) => {
      if (index > 0) lines.push([]);
      if (part) lines[lines.length - 1].push({ text: part, className: segment.className });
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
                  : segments.map((segment, segmentIndex) => (
                      <span className={segment.className} key={segmentIndex}>
                        {segment.text}
                      </span>
                    ))}
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
  return (
    <div className="min-w-0 break-words [&>:first-child]:mt-0 [&>:last-child]:mb-0">
      <ReactMarkdown
        remarkPlugins={[remarkGfm]}
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
          h1: ({ children: heading }) => (
            <h1 className={`${compact ? 'mb-1.5 mt-4 text-subhead' : 'mb-2 mt-6 text-head'} border-b-2 border-answer-edge pb-1 font-semibold tracking-[-0.015em] text-heading-1`}>
              {heading}
            </h1>
          ),
          h2: ({ children: heading }) => (
            <h2 className={`${compact ? 'mb-1 mt-3.5 text-title' : 'mb-1.5 mt-5 text-subhead'} border-b border-answer-edge pb-0.5 font-semibold tracking-[-0.01em] text-heading-2`}>
              {heading}
            </h2>
          ),
          h3: ({ children: heading }) => (
            <h3 className={`${compact ? 'mb-1 mt-3 text-body' : 'mb-1 mt-4 text-title'} font-semibold text-heading-3`}>
              {heading}
            </h3>
          ),
          h4: ({ children: heading }) => (
            <h4 className={`${compact ? 'mb-0.5 mt-2.5 text-body' : 'mb-1 mt-3.5 text-body'} font-semibold text-heading-3`}>
              {heading}
            </h4>
          ),
          h5: ({ children: heading }) => (
            <h5 className={`${compact ? 'mb-0.5 mt-2.5 text-ui' : 'mb-1 mt-3 text-ui'} font-semibold text-heading-3`}>
              {heading}
            </h5>
          ),
          h6: ({ children: heading }) => (
            <h6 className={`${compact ? 'mb-0.5 mt-2.5 text-meta' : 'mb-1 mt-3 text-meta'} font-semibold uppercase tracking-[0.08em] text-heading-3`}>
              {heading}
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
            return <SyntaxCodeBlock value={raw} language={language} compact={compact} frameless={nested} />;
          },
          strong: ({ children: strong }) => <strong className="font-semibold">{strong}</strong>,
          table: ({ children: table }) => (
            <div className={`${compact ? 'my-2' : 'my-3'} max-w-full overflow-x-auto overscroll-x-contain`}>
              <table className="w-full border-collapse text-ui">{table}</table>
            </div>
          ),
          td: ({ children: cell }) => <td className="border border-code-edge px-2 py-1.5 text-left">{cell}</td>,
          th: ({ children: cell }) => (
            <th className="border border-code-edge bg-code px-2 py-1.5 text-left font-semibold">{cell}</th>
          ),
          ul: ({ children: list }) => (
            <ul className={`${compact ? 'my-2 pl-5' : 'my-3 pl-6'} list-disc space-y-0.5`}>{list}</ul>
          ),
        }}
      >
        {hardBreaks ? children.replace(/\n/g, ' \n') : children}
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
  if (!routing.fallback) return null;

  const fallbackTypes = new Set([
    'llm.routing/provider-fallback',
    'llm.routing/format-fallback',
  ]);
  const event = routing.trace.find((item) => fallbackTypes.has(String(item.type ?? '')));
  const retries = routing.trace.filter((item) => item.type === 'llm.routing/provider-retry').length;
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
  const body = resultBody(form);
  const failed = form.error != null;
  // Once any real outcome has arrived (body/result/render/duration) a stale
  // "Running…" placeholder from the gateway must not linger — the op is done.
  const hasOutcome =
    body !== '' ||
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
  const summary = compactToolSummary(form.tool_name, rawSummary);
  const duration = formatDuration(form.duration_ms);
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
    <details className={`group min-w-0 border-l-2 ${failed ? 'border-err' : role.border} bg-result`}>
      <summary className="flex min-h-6 list-none cursor-pointer select-none items-center gap-1.5 px-2 py-1 text-code-result hover:bg-hover [&::-webkit-details-marker]:hidden">
        <span className={`${disclosureClass} ${failed ? 'text-err' : role.text}`} aria-hidden="true">›</span>
        {headline}
        {/* ONE copy control per result card: the body's code blocks are frameless
            inside this card and render no chip of their own. */}
        <CopyButton value={body} className="shrink-0" label="Copy result" />
      </summary>
      <div className={`min-w-0 overflow-hidden border-t border-code-edge bg-result px-3 py-2 text-ui text-code-result ${failed ? 'text-code-error-result' : ''}`}>
        {failed ? <pre className="m-0 overflow-x-auto whitespace-pre-wrap break-words font-mono text-meta ">{body}</pre> : <Markdown compact nested>{body}</Markdown>}
      </div>
    </details>
  );
});

function formCode(form: TranscriptForm): string {
  const source = form.display_code ?? form.code ?? form.source ?? form.src;
  return typeof source === 'string' ? source.trim() : '';
}

function showFormCode(form: TranscriptForm, code: string): boolean {
  if (!code) return false;
  if (!form.tool_name) return true;
  // The model's OWN program is always shown — succeeded or failed. On a failure
  // the result card carries the exception text but not the source that raised it,
  // so hiding the program left a bare message with nothing to read it against.
  // The TUI keeps the source under a failing `python_execution` too; same rule here.
  return form.tool_name === 'python_execution';
}

const PYTHON_PREVIEW_LINES = 5;

const CollapsiblePythonCode = memo(function CollapsiblePythonCode(
  { value, bare = false }: { value: string; bare?: boolean },
) {
  const [expanded, setExpanded] = useState(false);
  const lines = value.split(/\r?\n/);
  const hiddenLines = Math.max(0, lines.length - PYTHON_PREVIEW_LINES);
  const collapsible = hiddenLines > 0;
  const visibleValue = collapsible && !expanded ? lines.slice(0, PYTHON_PREVIEW_LINES).join('\n') : value;

  // Same frame as the result cards this program produced (see `FormTrace`) and
  // the same shell-coloured rail the TUI paints for its code band — program and
  // results read as ONE stack. The disclosure row is a HEADER (top of the frame,
  // content reveals below it), identical to the `ToolCard` result headline and to
  // the TUI's THINKING accordion: one rule everywhere, so a row always labels the
  // block beneath it and the collapse control never scrolls away with the body.
  return (
    <div className={bare ? 'min-w-0' : 'mb-1 min-w-0 overflow-hidden border border-dialog-edge bg-dialog-edge shadow-[2px_2px_0_var(--dialog-shadow)]'}>
      <div className="min-w-0 border-l-2 border-tool-shell bg-code">
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
                className={`${disclosureClass} text-tool-shell ${expanded ? 'rotate-90' : ''}`}
                aria-hidden="true"
              >
                ›
              </span>
              <span className="truncate font-mono text-chip font-extrabold tracking-[0.06em] text-tool-shell">
                {expanded ? 'PYTHON' : `PYTHON +${hiddenLines} more`}
              </span>
            </button>
          ) : (
            <span className="min-w-0 flex-1 select-none truncate px-2 py-1 font-mono text-chip font-extrabold tracking-[0.06em] text-tool-shell">
              PYTHON
            </span>
          )}
          <CopyButton value={value} className="shrink-0" label="Copy code" />
        </div>
        <SyntaxCodeBlock
          value={visibleValue}
          copyValue={value}
          language="python"
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
          <CollapsiblePythonCode value={code} bare />
          <CardGrid cards={cards} bare />
        </div>
      ) : (
        <>
          {showCode && <CollapsiblePythonCode value={code} />}
          <CardGrid cards={cards} />
        </>
      )}
    </div>
  );
});

const REASONING_PREVIEW_LINES = 3;
// Mirrors com.blockether.vis.internal.render/reasoning-collapse-min-hidden (3).
// A disclosure that buys back one or two clipped rows is pure friction — you
// uncollapse just to read one more line — so a barely-overflowing trace renders
// inline, in full, with no toggle at all. Same rule as the TUI band and the
// Clojure transcript split; keep the three in step.
const REASONING_COLLAPSE_MIN_HIDDEN = 3;
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

// Collapsed-height measurement for THINKING bands, batched across the whole
// transcript.
//
// One `ResizeObserver` per band is the obvious shape and the wrong one here: a
// rotation resizes EVERY band in the same frame, so the browser delivers N
// callbacks, each doing `getComputedStyle` + `scrollHeight` (a forced
// synchronous layout) followed by its own `setState`. A long session holds
// dozens of them, and that read-write-read-write thrash is what makes the
// transcript churn while the device is still turning.
//
// So: ONE observer for every band on screen, all measurements coalesced into a
// single animation frame (one layout flush, one React batch), and nothing
// measured while the viewport is mid-rotation — those widths are transitional
// and every intermediate answer is thrown away anyway. The pending set survives
// the skip and is replayed once, from settled geometry, when the flip ends.
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
    <section className="my-2 bg-thinking-surface px-3 py-2 text-ui text-thinking">
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
        className={`${collapsible && !expanded ? 'max-h-[3.75rem] overflow-hidden' : ''} italic`}
      >
        <Markdown compact hardBreaks>{normalized}</Markdown>
      </div>
    </section>
  );
});

function attachmentIsImage(attachment: IterationAttachment): boolean {
  const media = attachment.media_type ?? '';
  return media ? media.startsWith('image/') : attachment.kind === 'image';
}

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
  const isImage = attachmentIsImage(attachment);
  const name = attachment.filename || 'attachment';

  useEffect(() => {
    if (!isImage || !iterationId || !sid) return;
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
  }, [client, sid, iterationId, index, isImage, attempt]);

  // A non-image artifact reaching a tile is the failure path only — the rail
  // below routes files into the collapsed recorded-files row.
  if (!isImage || failed || !iterationId) {
    return (
      <div className="mt-2 min-w-0 truncate font-mono text-chip text-footer-muted">
        {failed ? `✗ ${name}` : `↗ ${name}`}
      </div>
    );
  }

  return (
    <figure className="mt-2.5 min-w-0">
      {url ? (
        <img
          src={url}
          alt={name}
          loading="lazy"
          decoding="async"
          onError={() => {
            if (attempt >= 2) {
              setFailed(true);
              return;
            }
            setUrl(null);
            setAttempt((current) => current + 1);
          }}
          className="block max-h-[60svh] w-auto max-w-full object-contain"
        />
      ) : (
        <div className="h-24 w-full animate-pulse bg-thinking-surface" aria-hidden="true" />
      )}
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

function attachmentBytes(bytes?: number): string {
  if (typeof bytes !== 'number' || !Number.isFinite(bytes) || bytes < 0) return '';
  if (bytes < 1024) return `${bytes}B`;
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)}KB`;
  return `${(bytes / 1024 / 1024).toFixed(1)}MB`;
}

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
  const images = attachments.filter(attachmentIsImage);
  const files = recordedFiles(attachments.filter((entry) => !attachmentIsImage(entry)));
  const total = files.reduce((sum, file) => sum + file.count, 0);
  const head = files[0];
  const rest = head ? total - head.count : 0;

  return (
    <>
      {images.map((attachment) => (
        <AttachmentTile
          key={`${attachment.iteration_id ?? 'iter'}-${attachment.index}`}
          client={client}
          sid={sid}
          attachment={attachment}
        />
      ))}
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
  const visible = iterations
    .map((iteration, index) => ({
      iteration,
      index,
      thinking: iteration.thinking?.trim() ?? '',
      prose: iteration.assistant_prose?.trim() ?? '',
      forms: iteration.forms ?? [],
      attachments: iteration.attachments ?? [],
    }))
    .filter(({ thinking, prose, forms, attachments }) =>
      thinking
      || prose
      || attachments.length
      || forms.some((form) => showFormCode(form, formCode(form)) || toolCards(form).length),
    );

  if (!visible.length) return null;

  type Entry = (typeof visible)[number];
  type Chunk =
    | { kind: 'code'; key: string; form: TranscriptForm }
    | { kind: 'cards'; key: string; cards: TranscriptForm[] };

  // Consecutive TOOL-ONLY iterations are one run of work, not N bubbles: the model
  // kept calling tools without saying anything in between. Mirrors the TUI
  // (`render/merge-iteration-entries`): a narrated iteration may OPEN a run (its
  // thinking / prose renders above the cards), an interior narrated call closes it,
  // and so does an iteration that produced attachments (those render last).
  const segments: { key: string; head: Entry; items: Entry[]; closed: boolean }[] = [];
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

  return (
    <div className="mb-2.5 grid gap-2.5">
      {segments.map((segment) => {
        // Inside a segment, adjacent code-less forms pool into ONE grid; a python
        // block keeps its own frame under its source and starts a new pool after it.
        const chunks: Chunk[] = [];
        segment.items.forEach((entry) => {
          entry.forms.forEach((form, formIndex) => {
            if (form.silent || form.result === 'vis_silent' || form.result === 'vis_answer') return;
            const key = `${entry.index}-${formIndex}-${form.scope ?? form.tool_name ?? 'form'}`;
            if (showFormCode(form, formCode(form))) {
              chunks.push({ kind: 'code', key, form });
              return;
            }
            const cards = toolCards(form);
            if (!cards.length) return;
            const pool = chunks.at(-1);
            if (pool?.kind === 'cards') pool.cards.push(...cards);
            else chunks.push({ kind: 'cards', key, cards: [...cards] });
          });
        });

        return (
          <section
            key={segment.key}
            className={live ? `min-w-0 ${transcriptEnterClass}` : 'min-w-0'}
          >
            {segment.head.thinking && <ThinkingBand>{segment.head.thinking}</ThinkingBand>}
            {segment.head.prose && (
              <div className="my-2.5 text-body text-vis-message">
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
            {client && sid && (
              <AttachmentRail
                client={client}
                sid={sid}
                attachments={segment.items.flatMap((entry) => entry.attachments)}
              />
            )}
          </section>
        );
      })}
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
    return 'Vis is calling the provider';
  }
  const last = iterations.at(-1);
  const suffix = `(iter ${iteration})`;
  if (last?.error != null) return `Vis is retrying ${suffix}`;
  if (last?.forms?.length) return `Vis is running code ${suffix}`;
  if (last?.thinking?.trim()) return `Vis is thinking ${suffix}`;
  return `Vis is working ${suffix}`;
}

const SPINNER_FRAMES = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'];

function LiveProgress({ phase, startedAt }: { phase: string; startedAt?: number }) {
  const [now, setNow] = useState(() => Date.now());

  useEffect(() => {
    const timer = window.setInterval(() => setNow(Date.now()), 100);
    return () => window.clearInterval(timer);
  }, []);

  const elapsed = formatDuration(Math.max(0, now - (startedAt ?? now))) ?? '0ms';
  const frame = SPINNER_FRAMES[Math.floor(now / 100) % SPINNER_FRAMES.length];

  return (
    <>
      <div
        className="mt-5 truncate whitespace-nowrap font-mono text-ui text-vis-message"
        aria-hidden="true"
      >
        <span className="motion-reduce:hidden">{frame}</span>
        <span className="hidden motion-reduce:inline">●</span>
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
  const meta = !commandTurn(turn) && (!cancelled || assistantUsage(turn))
    ? turnMetaSummary(turn)
    : null;
  const fallbackNote = meta && !cancelled ? turnFallbackNote(turn) : null;

  return (
    <article className="mt-4 w-full [contain:layout_style]" aria-busy={streaming}>
      <div className={`mb-1 font-mono text-meta font-bold ${cancelled ? 'text-dialog-hint' : 'text-vis-role'}`}>Vis</div>
      <div className="min-w-0">
        <IterationTrace iterations={turn.iterations ?? []} live={streaming} client={client} sid={sid} />
        <div className={`bg-answer text-body ${cancelled ? 'italic text-cancelled-foreground' : 'text-answer-foreground'}`}>
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
  const imageAttachments = (attachments ?? []).filter(
    (a) => (a.source ?? 'user') === 'user' && !!a.base64 && !!a.media_type?.startsWith('image/'),
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
      <div className={`inline-block max-w-full whitespace-pre-wrap break-words hyphens-auto [hyphenate-limit-chars:6_3_3] border-l-2 border-you-role bg-code px-3 py-2 text-body text-pretty text-you-message-foreground ${isJustifiable ? 'text-justify' : 'text-left'}`}>
        {parts.map((part) => part.type === 'text' ? (
          <span key={part.key}>{part.text}</span>
        ) : part.type === 'image' ? (
          <span key={part.key} className="my-0.5 inline-flex items-center gap-1 border border-code-edge bg-code px-1.5 py-0.5 align-middle font-mono text-meta text-dialog-hint first:mt-0">
            {part.summary}
          </span>
        ) : (
          <details key={part.key} className="group my-1 block max-w-full border-y border-code-edge bg-code text-code-foreground first:mt-0 last:mb-0">
            <summary className="cursor-pointer list-none select-none px-2 py-1 font-mono text-meta font-semibold text-accent-ink marker:hidden [&::-webkit-details-marker]:hidden">
              <span className="mr-1 inline-block text-dialog-hint transition-transform group-open:rotate-90">▸</span>
              {part.summary}
            </summary>
            <pre className="max-h-[min(28rem,60dvh)] overflow-auto overscroll-contain border-t border-code-edge px-2 py-2 font-mono text-meta [tab-size:2]">
              <code>{part.content}</code>
            </pre>
          </details>
        ))}
      </div>
      {imageAttachments.length > 0 && (
        <div className="mt-2 flex flex-col items-start gap-2">
          {imageAttachments.map((att, i) => (
            <img
              key={att.id ?? `att-${i}`}
              src={att.base64.startsWith('data:') ? att.base64 : `data:${att.media_type};base64,${att.base64}`}
              alt={att.filename ?? 'attachment'}
              className="max-h-[min(28rem,60dvh)] max-w-full w-auto rounded border border-code-edge object-contain"
            />
          ))}
        </div>
      )}
    </article>
  );
});
