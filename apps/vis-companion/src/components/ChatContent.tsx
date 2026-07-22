import { useMemo, useState, type ReactNode } from 'react';
import ReactMarkdown from 'react-markdown';
import remarkGfm from 'remark-gfm';
import type { ContentBlock, JsonValue, TranscriptForm, TranscriptIteration, TranscriptTurn } from '../lib/types';

const summaryClass =
  'flex min-h-9 list-none cursor-pointer select-none items-center gap-2 px-3 py-2 [&::-webkit-details-marker]:hidden';
const detailClass = 'group text-xs text-white/60';
const disclosureClass = 'inline-block transition-transform duration-150 group-open:rotate-90';
const toolDetailClass =
  'grid gap-px border-t border-edge/60 bg-edge [&>pre]:max-h-80 [&>pre]:overflow-auto [&>pre]:whitespace-pre-wrap [&>pre]:break-words [&>pre]:bg-ink/80 [&>pre]:p-3 [&>pre]:font-mono [&>pre]:text-[11px] [&>pre]:leading-relaxed [&>pre]:text-white/70';

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
      className="absolute right-2 top-2 z-10 rounded-md border border-edge bg-panel-2/90 px-2 py-1 font-mono text-[10px] text-white/60 backdrop-blur transition-colors hover:text-white"
      onClick={copy}
      aria-label="Copy code"
    >
      {copied ? 'Copied' : 'Copy'}
    </button>
  );
}

export function Markdown({ children }: { children: string }) {
  return (
    <div className="min-w-0 break-words [&>:first-child]:mt-0 [&>:last-child]:mb-0">
      <ReactMarkdown
        remarkPlugins={[remarkGfm]}
        components={{
          a: ({ children: label, ...props }) => (
            <a
              {...props}
              className="text-accent underline decoration-accent/40 underline-offset-3"
              target="_blank"
              rel="noreferrer"
            >
              {label}
            </a>
          ),
          blockquote: ({ children: quote }) => (
            <blockquote className="my-3 border-l-2 border-accent/50 pl-4 text-white/65">{quote}</blockquote>
          ),
          h1: ({ children: heading }) => <h1 className="mb-2 mt-6 text-2xl font-semibold leading-tight">{heading}</h1>,
          h2: ({ children: heading }) => <h2 className="mb-2 mt-6 text-xl font-semibold leading-tight">{heading}</h2>,
          h3: ({ children: heading }) => <h3 className="mb-2 mt-5 text-lg font-semibold leading-tight">{heading}</h3>,
          h4: ({ children: heading }) => <h4 className="mb-2 mt-5 font-semibold leading-tight">{heading}</h4>,
          hr: () => <hr className="my-5 border-edge" />,
          li: ({ children: item }) => <li className="my-1 pl-1">{item}</li>,
          ol: ({ children: list }) => <ol className="my-3 list-decimal space-y-1 pl-6">{list}</ol>,
          p: ({ children: paragraph }) => <p className="my-3 leading-7">{paragraph}</p>,
          pre: ({ children: codeNode }) => {
            const raw = extractText(codeNode).replace(/\n$/, '');
            return (
              <div className="relative my-4 overflow-hidden rounded-xl border border-edge bg-panel/70">
                <CopyButton value={raw} />
                <pre className="m-0 max-w-full overflow-x-auto p-4 font-mono text-xs leading-relaxed text-white/90">
                  {codeNode}
                </pre>
              </div>
            );
          },
          strong: ({ children: strong }) => <strong className="font-semibold text-white">{strong}</strong>,
          table: ({ children: table }) => (
            <div className="my-4 max-w-full overflow-x-auto">
              <table className="w-full border-collapse text-sm">{table}</table>
            </div>
          ),
          td: ({ children: cell }) => <td className="border border-edge px-3 py-2 text-left">{cell}</td>,
          th: ({ children: cell }) => <th className="border border-edge bg-panel-2 px-3 py-2 text-left font-semibold">{cell}</th>,
          ul: ({ children: list }) => <ul className="my-3 list-disc space-y-1 pl-6">{list}</ul>,
        }}
      >
        {children}
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

function formatDuration(value?: number): string | null {
  if (value == null || !Number.isFinite(value)) return null;
  if (value < 1_000) return `${Math.round(value)}ms`;
  if (value < 60_000) return `${(value / 1_000).toFixed(value < 10_000 ? 1 : 0)}s`;
  const minutes = Math.floor(value / 60_000);
  return `${minutes}m ${Math.round((value % 60_000) / 1_000)}s`;
}

function formSource(form: TranscriptForm): string {
  const source = form.src ?? form.source ?? form.code;
  return typeof source === 'string' ? source : '';
}

function formLabel(form: TranscriptForm): string {
  return form.tool_name ?? form.tag ?? form.scope ?? 'operation';
}

function isVisibleForm(form: TranscriptForm): boolean {
  if (form.silent) return false;
  const source = formSource(form).trimStart();
  if (form.result === 'vis_silent' || form.result === 'vis_answer') return false;
  return !source.startsWith('(done');
}

function ToolRow({ form }: { form: TranscriptForm }) {
  const source = formSource(form);
  const result = form.error ?? form.stdout ?? form.result_render ?? form.result_summary ?? form.result;
  const failed = form.error != null;

  return (
    <details className={`${detailClass} border-t border-edge/60 first:border-t-0`}>
      <summary className={summaryClass}>
        <span className={`w-4 text-center ${failed ? 'text-err' : 'text-ok'}`} aria-hidden="true">
          {failed ? '×' : '✓'}
        </span>
        <span className="min-w-0 flex-1 truncate font-mono">{formLabel(form)}</span>
        {formatDuration(form.duration_ms) && (
          <span className="shrink-0 font-mono text-[10px] text-white/35">{formatDuration(form.duration_ms)}</span>
        )}
        <span className={disclosureClass} aria-hidden="true">›</span>
      </summary>
      <div className={toolDetailClass}>
        {source && <pre>{source}</pre>}
        {result != null && result !== '' && <pre className={failed ? '!text-err' : ''}>{jsonText(result)}</pre>}
      </div>
    </details>
  );
}

function Reasoning({ children }: { children: string }) {
  return (
    <details className={`${detailClass} mb-3`}>
      <summary className="flex list-none cursor-pointer select-none items-center gap-2 py-1 hover:text-white/80 [&::-webkit-details-marker]:hidden">
        <span className="font-mono text-accent" aria-hidden="true">⌁</span>
        Reasoning
        <span className={disclosureClass} aria-hidden="true">›</span>
      </summary>
      <div className="mb-3 ml-2 mt-2 border-l border-edge py-0.5 pl-4 text-[13px] leading-relaxed text-white/55">
        <Markdown>{children}</Markdown>
      </div>
    </details>
  );
}

function IterationTrace({ iterations }: { iterations: TranscriptIteration[] }) {
  const forms = useMemo(
    () => iterations.flatMap((iteration) => iteration.forms ?? []).filter(isVisibleForm),
    [iterations],
  );
  const thinking = useMemo(
    () => iterations.map((iteration) => iteration.thinking?.trim()).filter(Boolean).join('\n\n'),
    [iterations],
  );

  if (!forms.length && !thinking) return null;
  return (
    <div className="mb-3">
      {thinking && <Reasoning>{thinking}</Reasoning>}
      {forms.length > 0 && (
        <details className={`${detailClass} mt-2`}>
          <summary className="flex list-none cursor-pointer select-none items-center gap-2 py-1 hover:text-white/80 [&::-webkit-details-marker]:hidden">
            <span className="font-mono text-accent" aria-hidden="true">›_</span>
            {forms.length} {forms.length === 1 ? 'operation' : 'operations'}
            <span className={disclosureClass} aria-hidden="true">›</span>
          </summary>
          <div className="ml-2 mt-2 overflow-hidden rounded-xl border border-edge/75 bg-panel/60">
            {forms.map((form, index) => <ToolRow key={`${form.scope ?? index}`} form={form} />)}
          </div>
        </details>
      )}
    </div>
  );
}

export function ContentBlockView({ block }: { block: ContentBlock }) {
  switch (block.type) {
    case 'prose':
      return block.markdown ? <Markdown>{block.markdown}</Markdown> : null;
    case 'code':
      return <Markdown>{`\`\`\`${block.language ?? ''}\n${block.text ?? ''}\n\`\`\``}</Markdown>;
    case 'reasoning':
      return block.text ? <Reasoning>{block.text}</Reasoning> : null;
    case 'tool': {
      const failed = block.status === 'failed';
      return (
        <details className={`${detailClass} my-2 overflow-hidden rounded-xl border border-edge/75 bg-panel/60`}>
          <summary className={summaryClass}>
            <span className={`w-4 text-center ${failed ? 'text-err' : 'text-ok'}`}>
              {failed ? '×' : block.status === 'running' ? '…' : '✓'}
            </span>
            <span className="min-w-0 flex-1 truncate font-mono">{block.tool ?? 'tool'}</span>
            <span className="shrink-0 font-mono text-[10px] text-white/35">{block.status}</span>
            <span className={disclosureClass}>›</span>
          </summary>
          <div className={toolDetailClass}>
            {block.input != null && <pre>{jsonText(block.input)}</pre>}
            {block.output != null && <pre>{jsonText(block.output)}</pre>}
            {block.error != null && <pre className="!text-err">{jsonText(block.error)}</pre>}
          </div>
        </details>
      );
    }
    case 'error':
      return (
        <div className="my-3 flex gap-2 rounded-xl border border-err/30 bg-err/5 px-3 py-2.5 text-sm text-err">
          <strong>{block.code}</strong><span>{block.message}</span>
        </div>
      );
    case 'attachment':
      return (
        <div className="my-3 w-fit rounded-lg border border-edge bg-panel px-3 py-2 text-xs">
          ↗ {block.name ?? 'Attachment'} <small className="ml-2 text-white/40">{block.media_type}</small>
        </div>
      );
    case 'notice':
      return <div className="my-3 rounded-xl border border-edge bg-panel px-3 py-2.5 text-sm text-white/60">{block.message}</div>;
    default:
      return null;
  }
}

function fallbackAnswer(turn: TranscriptTurn): string {
  const iterations = turn.iterations ?? [];
  for (let index = iterations.length - 1; index >= 0; index -= 1) {
    const prose = iterations[index].assistant_prose?.trim();
    if (prose) return prose;
  }
  return '';
}

export function AssistantMessage({ turn }: { turn: TranscriptTurn }) {
  const blocks = turn.content ?? [];
  const fallback = blocks.length ? '' : fallbackAnswer(turn);
  const duration = formatDuration(turn.duration_ms);
  const model = turn.model ?? (typeof turn.cost === 'object' && turn.cost ? String(turn.cost.model ?? '') : '');

  return (
    <article className="mt-6 flex items-start gap-3 max-sm:gap-2.5">
      <div className="grid size-7 shrink-0 place-items-center rounded-lg border border-accent/35 bg-accent/10 font-mono text-xs font-bold text-accent shadow-lg shadow-accent/10 max-sm:size-6 max-sm:rounded-md max-sm:text-[10px]" aria-hidden="true">v</div>
      <div className="min-w-0 flex-1">
        <IterationTrace iterations={turn.iterations ?? []} />
        <div className="text-[15px] leading-7 text-white/95">
          {blocks.map((block) => <ContentBlockView key={block.id} block={block} />)}
          {fallback && <Markdown>{fallback}</Markdown>}
          {!blocks.length && !fallback && turn.status !== 'completed' && (
            <span className="text-sm text-white/45">{turn.status ?? 'No response'}</span>
          )}
        </div>
        {(duration || model) && (
          <div className="mt-3 flex gap-3 font-mono text-[10px] text-white/30">
            {model && <span>{model}</span>}{duration && <span>{duration}</span>}
          </div>
        )}
      </div>
    </article>
  );
}

export function UserMessage({ children }: { children: string }) {
  return (
    <article className="ml-auto w-fit max-w-[86%] rounded-[1.25rem_1.25rem_0.35rem_1.25rem] border border-edge bg-panel-2 px-4 py-3 text-[15px] leading-6 text-white shadow-xl shadow-black/20 max-sm:max-w-[91%]">
      <div className="whitespace-pre-wrap break-words">{children}</div>
    </article>
  );
}
