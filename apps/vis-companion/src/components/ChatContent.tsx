import {
  isValidElement,
  memo,
  useEffect,
  useLayoutEffect,
  useRef,
  useState,
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
import type {
  ContentBlock,
  JsonValue,
  TranscriptForm,
  TranscriptIteration,
  TranscriptTurn,
} from '../lib/types';

const disclosureClass =
  'inline-block shrink-0 text-[11px] transition-transform duration-150 group-open:rotate-90';

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
  shell_run: 'SHELL RUN',
  shell_bg: 'SHELL BACKGROUND',
};

function CopyButton({ value }: { value: string }) {
  const [copied, setCopied] = useState(false);

  async function copy() {
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
      className="absolute right-2 top-2 z-10 border border-dialog-edge bg-button px-1.5 py-0.5 font-mono text-[9px] text-button-foreground transition-colors hover:bg-hover"
      onClick={copy}
      aria-label="Copy code"
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

function DiffBlock({ value, compact }: { value: string; compact: boolean }) {
  const lineClasses: Record<DiffLineKind, string> = {
    meta: 'text-code-duration',
    hunk: 'text-code-syntax-keyword',
    add: 'bg-code-ok text-code-success',
    del: 'bg-code-err text-code-error',
    ctx: 'text-code-foreground',
  };

  return (
    <div
      className={`${compact ? 'my-2' : 'my-3'} relative overflow-hidden border border-code-edge bg-code`}
      aria-label="Unified diff"
    >
      <CopyButton value={value} />
      <pre
        className={`${compact ? 'text-[10px] leading-4' : 'text-[11px] leading-5'} m-0 max-w-full overflow-x-auto py-2 font-mono`}
      >
        {value.split('\n').map((line, index) => (
          <span
            className={`block min-w-full w-fit whitespace-pre px-3 first:pr-16 ${lineClasses[diffLineKind(line)]}`}
            key={`${index}-${line}`}
          >
            {line || ' '}
          </span>
        ))}
      </pre>
    </div>
  );
}

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

function renderSyntaxToken(token: string | Prism.Token, key: string): ReactNode {
  if (typeof token === 'string') return token;
  const content = Array.isArray(token.content)
    ? token.content.map((child, index) => renderSyntaxToken(child, `${key}-${index}`))
    : renderSyntaxToken(token.content, `${key}-content`);
  return <span className={syntaxClass(token)} key={key}>{content}</span>;
}

function SyntaxCodeBlock({
  value,
  language,
  compact,
}: {
  value: string;
  language: string;
  compact: boolean;
}) {
  const normalized = languageAliases[language] ?? language;
  const grammar = Prism.languages[normalized];
  const tokens = grammar ? Prism.tokenize(value, grammar) : [value];

  return (
    <div className={`${compact ? 'my-2' : 'my-3'} relative overflow-hidden border border-code-edge bg-code`}>
      <CopyButton value={value} />
      <pre className={`${compact ? 'p-2.5 text-[10px] leading-4' : 'p-3 text-[11px] leading-5'} m-0 max-w-full overflow-x-auto font-mono text-code-foreground`}>
        <code className="block min-w-max pr-10 [tab-size:2]">
          {tokens.map((token, index) => renderSyntaxToken(token, `${normalized || 'text'}-${index}`))}
        </code>
      </pre>
    </div>
  );
}

export function Markdown({
  children,
  compact = false,
  hardBreaks = false,
}: {
  children: string;
  compact?: boolean;
  hardBreaks?: boolean;
}) {
  return (
    <div className="min-w-0 break-words [&>:first-child]:mt-0 [&>:last-child]:mb-0">
      <ReactMarkdown
        remarkPlugins={[remarkGfm]}
        components={{
          a: ({ children: label, ...props }) => (
            <a
              {...props}
              className="font-medium text-link underline underline-offset-3 hover:text-link-hover"
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
          h1: ({ children: heading }) => (
            <h1 className={`${compact ? 'mb-1 mt-3 text-sm' : 'mb-1.5 mt-5 text-xl'} font-semibold leading-tight tracking-[-0.015em] text-heading-1`}>
              {heading}
            </h1>
          ),
          h2: ({ children: heading }) => (
            <h2 className={`${compact ? 'mb-1 mt-3 text-[13px]' : 'mb-1.5 mt-5 text-[17px]'} font-semibold leading-tight tracking-[-0.01em] text-heading-2`}>
              {heading}
            </h2>
          ),
          h3: ({ children: heading }) => (
            <h3 className={`${compact ? 'mb-1 mt-2.5 text-xs' : 'mb-1.5 mt-4 text-[15px]'} font-semibold leading-tight text-heading-3`}>
              {heading}
            </h3>
          ),
          h4: ({ children: heading }) => (
            <h4 className={`${compact ? 'mb-1 mt-2.5 text-xs' : 'mb-1.5 mt-4 text-sm'} font-semibold leading-tight text-heading-3`}>
              {heading}
            </h4>
          ),
          hr: () => <hr className={`${compact ? 'my-3' : 'my-5'} border-answer-edge`} />,
          li: ({ children: item }) => <li className={compact ? 'my-0.5 pl-0.5' : 'my-0.5 pl-1'}>{item}</li>,
          ol: ({ children: list }) => (
            <ol className={`${compact ? 'my-2 pl-5' : 'my-3 pl-6'} list-decimal space-y-0.5`}>{list}</ol>
          ),
          p: ({ children: paragraph }) => (
            <p className={compact ? 'my-2 leading-5' : 'my-2.5 leading-6'}>{paragraph}</p>
          ),
          pre: ({ children: codeNode }) => {
            const raw = extractText(codeNode).replace(/\n$/, '');
            const language = codeLanguage(codeNode);
            if (language === 'diff' || language === 'patch' || language === 'udiff') {
              return <DiffBlock value={raw} compact={compact} />;
            }
            return <SyntaxCodeBlock value={raw} language={language} compact={compact} />;
          },
          strong: ({ children: strong }) => <strong className="font-semibold">{strong}</strong>,
          table: ({ children: table }) => (
            <div className={`${compact ? 'my-2' : 'my-3'} max-w-full overflow-x-auto`}>
              <table className="w-full border-collapse text-[11px]">{table}</table>
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
        {hardBreaks ? children.replace(/\n/g, '  \n') : children}
      </ReactMarkdown>
    </div>
  );
}

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

function formatDuration(value?: number): string | null {
  if (value == null || !Number.isFinite(value) || value <= 0) return null;
  const milliseconds = Math.trunc(value);
  if (milliseconds < 1_000) return `${milliseconds}ms`;
  if (milliseconds < 60_000) return `${(milliseconds / 1_000).toFixed(1)}s`;
  const minutes = Math.floor(milliseconds / 60_000);
  return `${minutes}m ${Math.floor((milliseconds % 60_000) / 1_000)}s`;
}

const META_SEPARATOR = '  ·  ';

function finiteNumber(...values: unknown[]): number | undefined {
  return values.find((value): value is number => typeof value === 'number' && Number.isFinite(value));
}

function humanizeCount(value: number): string {
  const count = Math.trunc(value);
  if (count < 1_000) return String(count);
  const scale = count < 1_000_000 ? 1_000 : 1_000_000;
  const unit = count < 1_000_000 ? 'k' : 'M';
  const tenths = Math.round(count / (scale / 10));
  const whole = Math.floor(tenths / 10);
  const fraction = tenths % 10;
  return `${whole}${fraction ? `.${fraction}` : ''}${unit}`;
}

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
  const input = finiteNumber(turn.tokens?.input, turn.input_tokens);
  const output = finiteNumber(turn.tokens?.output, turn.output_tokens);
  const cached = finiteNumber(turn.tokens?.cached, turn.input_cache_read_tokens);
  const tokens = (input && input > 0) || (output && output > 0)
    ? `${humanizeCount(input ?? 0)}→${humanizeCount(output ?? 0)}${cached && cached > 0 ? ` (cached ${humanizeCount(cached)})` : ''}`
    : null;
  const totalCost = finiteNumber(
    turn.total_cost,
    typeof turn.cost === 'number' ? turn.cost : undefined,
    cost?.total_cost,
  );
  const price = totalCost && totalCost > 0
    ? `~$${totalCost.toFixed(totalCost >= 1 ? 2 : totalCost >= 0.0001 ? 4 : 6)}`
    : null;
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
  const cost = typeof turn.cost === 'object' && turn.cost ? turn.cost : undefined;
  return [
    turn.tokens?.input,
    turn.tokens?.output,
    turn.input_tokens,
    turn.output_tokens,
    turn.total_cost,
    typeof turn.cost === 'number' ? turn.cost : undefined,
    cost?.total_cost,
  ].some((value) => typeof value === 'number' && value > 0);
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
  return (normalized && toolRoleClasses[normalized]) || { border: 'border-accent', text: 'text-accent' };
}

function resultBody(form: TranscriptForm): string {
  if (form.error != null) return jsonText(form.error);
  const rendered = form.result_render?.trimEnd();
  if (rendered) return stripAnsi(rendered);
  if (form.result_summary?.trim()) return '';
  if (form.result == null || form.result === '') return '';
  const raw = jsonText(form.result);
  return typeof form.result === 'string' ? raw : `\`\`\`json\n${raw}\n\`\`\``;
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
      className={`min-w-0 flex-1 truncate text-[9px] font-medium leading-3 ${className}`}
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
            <code className="mx-px inline rounded-none bg-result-path px-0.5 py-px font-mono text-[8px] font-medium text-result-path-foreground">
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

function ToolCard({ form }: { form: TranscriptForm }) {
  const role = toolRole(form.tool_color_role);
  const body = resultBody(form);
  const failed = form.error != null;
  const rawSummary = form.result_summary?.trim() || (failed ? 'Failed' : '');
  const summary = compactToolSummary(form.tool_name, rawSummary);
  const duration = formatDuration(form.duration_ms);
  const headline = (
    <div className="flex min-w-0 flex-1 items-baseline gap-1.5">
      <span className={`shrink-0 font-mono text-[8px] font-extrabold tracking-[0.06em] ${failed ? 'text-err' : role.text}`}>
        {toolLabel(form.tool_name)}
      </span>
      {summary && <ToolSummary className={failed ? 'text-err' : role.text}>{summary}</ToolSummary>}
      {duration && <span className="shrink-0 font-mono text-[8px] tabular-nums text-code-duration">{duration}</span>}
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
    <details className={`group border-l-2 ${failed ? 'border-err' : role.border} bg-result`}>
      <summary className="flex min-h-6 list-none cursor-pointer select-none items-center gap-1.5 px-2 py-1 text-code-result hover:bg-hover [&::-webkit-details-marker]:hidden">
        <span className={`${disclosureClass} ${failed ? 'text-err' : role.text}`} aria-hidden="true">›</span>
        {headline}
      </summary>
      <div className={`border-t border-code-edge bg-result px-3 py-2 text-[11px] text-code-result ${failed ? 'text-code-error-result' : ''}`}>
        {failed ? <pre className="m-0 overflow-x-auto whitespace-pre-wrap break-words font-mono text-[10px] leading-4">{body}</pre> : <Markdown compact>{body}</Markdown>}
      </div>
    </details>
  );
}

function formCode(form: TranscriptForm): string {
  const source = form.display_code ?? form.code ?? form.source ?? form.src;
  return typeof source === 'string' ? source.trim() : '';
}

function showFormCode(form: TranscriptForm, code: string): boolean {
  if (!code) return false;
  if (form.error != null) return true;
  if (!form.tool_name) return true;
  return form.tool_name === 'python_execution';
}

function FormTrace({ form }: { form: TranscriptForm }) {
  if (form.silent || form.result === 'vis_silent' || form.result === 'vis_answer') return null;
  const code = formCode(form);
  const showCode = showFormCode(form, code);
  const cards = toolCards(form);
  if (!showCode && !cards.length) return null;

  return (
    <div className="min-w-0">
      {showCode && form.comment?.trim() && (
        <div className="mb-1 bg-thinking-surface px-3 py-1.5 text-[10px] italic leading-4 text-thinking">
          <Markdown compact>{form.comment.trim()}</Markdown>
        </div>
      )}
      {showCode && <SyntaxCodeBlock value={code} language="python" compact />}
      {cards.length > 0 && (
        <div
          className="grid gap-px overflow-hidden border border-dialog-edge bg-dialog-edge shadow-[2px_2px_0_var(--dialog-shadow)]"
          aria-label={`${cards.length} ${cards.length === 1 ? 'result' : 'results'}`}
        >
          {cards.map((card, cardIndex) => (
            <ToolCard
              key={`${card.scope ?? card.tool_name ?? 'tool'}-${cardIndex}`}
              form={card}
            />
          ))}
        </div>
      )}
    </div>
  );
}

const REASONING_PREVIEW_LINES = 6;
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

export function ThinkingBand({ children }: { children: string }) {
  const normalized = normalizeReasoning(children);
  const bodyRef = useRef<HTMLDivElement>(null);
  const [expanded, setExpanded] = useState(false);
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
    const observer = new ResizeObserver(measure);
    observer.observe(body);
    return () => observer.disconnect();
  }, [normalized]);

  useEffect(() => {
    if (!hiddenRows) setExpanded(false);
  }, [hiddenRows]);

  if (!normalized || normalized === ENCRYPTED_REASONING_PLACEHOLDER) return null;
  const collapsible = hiddenRows >= REASONING_COLLAPSE_MIN_HIDDEN;

  return (
    <section className="my-2 bg-thinking-surface px-3 py-2 text-[11px] leading-5 text-thinking">
      {collapsible && (
        <button
          type="button"
          data-disclosure-toggle
          className="mb-1 flex min-h-6 w-full items-center gap-1.5 text-left font-mono text-[9px] font-bold not-italic tracking-[0.07em] text-thinking transition-colors hover:text-dialog-hint-key"
          aria-expanded={expanded}
          onClick={() => setExpanded((value) => !value)}
        >
          <span aria-hidden="true">{expanded ? '▾' : '▸'}</span>
          <span>{expanded ? 'THINKING' : `THINKING  +${hiddenRows} more`}</span>
        </button>
      )}
      <div
        ref={bodyRef}
        className={`${collapsible && !expanded ? 'max-h-[7.5rem] overflow-hidden' : ''} italic`}
      >
        <Markdown compact hardBreaks>{normalized}</Markdown>
      </div>
    </section>
  );
}

export const IterationTrace = memo(function IterationTrace({
  iterations,
}: {
  iterations: TranscriptIteration[];
}) {
  const visible = iterations
    .map((iteration, index) => ({
      iteration,
      index,
      thinking: iteration.thinking?.trim() ?? '',
      prose: iteration.assistant_prose?.trim() ?? '',
      forms: iteration.forms ?? [],
    }))
    .filter(({ thinking, prose, forms }) =>
      thinking || prose || forms.some((form) => showFormCode(form, formCode(form)) || toolCards(form).length),
    );

  if (!visible.length) return null;

  return (
    <div className="mb-2.5 grid gap-2.5">
      {visible.map(({ iteration, index, thinking, prose, forms }) => (
        <section key={iteration.id ?? iteration.position ?? index} className="min-w-0">
          {thinking && <ThinkingBand>{thinking}</ThinkingBand>}
          {prose && (
            <div className="my-2.5 text-[12px] leading-5 text-vis-message">
              <Markdown>{prose}</Markdown>
            </div>
          )}
          {forms.map((form, formIndex) => (
            <FormTrace
              key={`${form.scope ?? form.tool_name ?? 'form'}-${formIndex}`}
              form={form}
            />
          ))}
        </section>
      ))}
    </div>
  );
});

export function ContentBlockView({ block }: { block: ContentBlock }) {
  switch (block.type) {
    case 'prose':
      return block.markdown ? <Markdown>{block.markdown}</Markdown> : null;
    case 'code':
      return <Markdown>{`\`\`\`${block.language ?? ''}\n${block.text ?? ''}\n\`\`\``}</Markdown>;
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
        <div className="my-2 flex gap-2 border border-warn-edge bg-warn-surface px-2.5 py-2 font-mono text-[10px] text-err">
          <strong>{block.code}</strong><span>{block.message}</span>
        </div>
      );
    case 'attachment':
      return (
        <div className="my-2 w-fit border border-dialog-edge bg-panel px-2.5 py-1.5 font-mono text-[10px] text-dialog-foreground">
          ↗ {block.name ?? 'Attachment'} <small className="ml-2 text-dialog-hint">{block.media_type}</small>
        </div>
      );
    case 'notice':
      return <div className="my-2 border border-dialog-edge bg-panel px-2.5 py-2 font-mono text-[10px] text-dialog-hint">{block.message}</div>;
    default:
      return null;
  }
}

function fallbackAnswer(turn: TranscriptTurn): string {
  const iterations = turn.iterations ?? [];
  for (let index = iterations.length - 1; index >= 0; index -= 1) {
    const answer = iterations[index].answer?.trim();
    if (answer) return answer;
  }
  return '';
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
        className="mt-5 truncate whitespace-nowrap font-mono text-[11px] leading-5 text-vis-message"
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
}: {
  turn: TranscriptTurn;
  streaming?: boolean;
  activity?: string;
  startedAt?: number;
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
      <div className={`mb-1 font-mono text-[10px] font-bold ${cancelled ? 'text-dialog-hint' : 'text-vis-role'}`}>Vis</div>
      <div className="min-w-0">
        <IterationTrace iterations={turn.iterations ?? []} />
        <div className={`bg-answer text-[13px] leading-6 ${cancelled ? 'italic text-cancelled-foreground' : 'text-answer-foreground'}`}>
          {blocks.map((block) => <ContentBlockView key={block.id} block={block} />)}
          {fallback && <Markdown>{fallback}</Markdown>}
          {!streaming && !blocks.length && !fallback && turn.status !== 'completed' && (
            <span>{cancelled ? 'Cancelled by user.' : turn.status ?? 'No response'}</span>
          )}
        </div>
        {streaming && <LiveProgress phase={activity ?? 'Vis is working'} startedAt={startedAt} />}
        {meta && (
          <footer className="mt-5 min-w-0 text-right font-mono text-[9px] leading-4 text-footer-muted">
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

export const UserMessage = memo(function UserMessage({ children }: { children: string }) {
  const parts = parseUserMessage(children);
  return (
    <article className="mt-4 w-full [contain:layout_style]">
      <div className="mb-1 font-mono text-[10px] font-bold text-you-role">You</div>
      <div className="inline-block max-w-full whitespace-pre-wrap break-words bg-you-message px-3 py-2.5 text-[13px] leading-5 text-you-message">
        {parts.map((part) => part.type === 'text' ? (
          <span key={part.key}>{part.text}</span>
        ) : (
          <details key={part.key} className="my-1 block max-w-full border-y border-code-edge bg-code text-code-foreground first:mt-0 last:mb-0">
            <summary className="cursor-pointer list-none select-none px-2 py-1 font-mono text-[10px] font-semibold text-accent marker:hidden [&::-webkit-details-marker]:hidden">
              <span className="mr-1 inline-block text-dialog-hint transition-transform group-open:rotate-90">▸</span>
              {part.summary}
            </summary>
            <pre className="max-h-[min(28rem,60dvh)] overflow-auto border-t border-code-edge px-2 py-2 font-mono text-[10px] leading-4 [tab-size:2]">
              <code>{part.content}</code>
            </pre>
          </details>
        ))}
      </div>
    </article>
  );
});
