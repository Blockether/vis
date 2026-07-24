import {
  useCallback,
  useEffect,
  useLayoutEffect,
  useRef,
  useState,
  type ClipboardEvent as ReactClipboardEvent,
  type MouseEvent as ReactMouseEvent,
} from 'react';
import { AssistantMessage, UserMessage } from '../components/ChatContent';
import { Banner } from '../components/ui';
import { Capacitor } from '@capacitor/core';
import {
  attachmentsFromFiles,
  captureCameraAttachment,
  type PendingAttachment,
} from '../lib/attachments';
import type { GatewayClient } from '../lib/gateway';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import {
  collapsePastePlaceholders,
  createComposerPaste,
  expandPastePlaceholders,
  shouldCollapsePaste,
  type ComposerPaste,
} from '../lib/paste';
import type {
  GatewayCapabilities,
  FileSuggestion,
  QueuedTurn,
  QueuePausedInfo,
  Session,
  SlashCommand,
  SseEvent,
  TranscriptForm,
  TranscriptIteration,
  TranscriptTurn,
  VoiceModelState,
} from '../lib/types';
import { startWavRecording, type WavRecording } from '../lib/voice';

interface LiveActivity {
  kind: string;
  iteration?: number;
  command?: string;
  operation?: string;
  label?: string;
}

interface LiveTurn {
  id?: string;
  request: string;
  answer: string;
  iterations: TranscriptIteration[];
  activity?: LiveActivity;
  startedAt: number;
  cancelling?: boolean;
  status: 'running' | 'failed' | 'cancelled';
}

const TERMINAL_EVENTS = new Set(['turn.completed', 'turn.failed', 'turn.cancelled']);
const LIVE_BODY_THROTTLE_MS = 150;
const INITIAL_VISIBLE_TURNS = 24;

function stringField(event: SseEvent, key: string): string {
  const value = event[key];
  return typeof value === 'string' ? value : '';
}

function applyText(current: string, event: SseEvent): string {
  const cumulative = stringField(event, 'cumulative');
  return cumulative || current + stringField(event, 'text');
}

function eventIteration(event: SseEvent): number {
  const value = event.iteration;
  const parsed = typeof value === 'number' ? value : Number(value);
  return Number.isFinite(parsed) ? parsed : 0;
}

function compactLabel(value: string, fallback: string): string {
  const label = value.split('\n', 1)[0].trim();
  if (!label) return fallback;
  return label.length > 64 ? `${label.slice(0, 61)}…` : label;
}

function commandPhase(request: string): string | null {
  const text = request.trim();
  if (text.startsWith('!&')) {
    return `Vis is starting: ${compactLabel(text.slice(2), '…')}`;
  }
  if (text.startsWith('!')) {
    return `Vis is running: ${compactLabel(text.slice(1), '…')}`;
  }
  if (text.startsWith('/')) {
    return `Vis is running: ${compactLabel(text.split(/\s+/, 1)[0], 'command')}`;
  }
  return null;
}

function liveProgressPhase(turn: LiveTurn): string {
  if (turn.cancelling) return 'Vis is cancelling';

  const last = turn.iterations.at(-1);
  const activity = turn.activity;
  const iteration = Math.max(
    turn.iterations.length,
    activity?.iteration == null ? 0 : activity.iteration + 1,
  );

  if (last?.error != null) return 'Vis is retrying';
  if (iteration === 0) return commandPhase(turn.request) ?? 'Vis is calling the provider';

  const suffix = `(iter ${iteration})`;
  switch (activity?.kind) {
    case 'shell-run':
      return `Vis is running: ${compactLabel(activity.command ?? '', '…')}`;
    case 'shell-bg':
      return `Vis is starting: ${compactLabel(activity.command ?? '', '…')}`;
    case 'slash':
      return `Vis is running: ${compactLabel(activity.command ?? '', 'command')}`;
    case 'provider-call':
      return `Vis is calling the provider ${suffix}`;
    case 'response-parse':
      return `Vis is parsing model response ${suffix}`;
    case 'tool':
    case 'tool-call':
      return `Vis is running: ${activity.operation || 'tool'}${activity.label ? ` ${compactLabel(activity.label, '')}` : ''} ${suffix}`;
    default:
      break;
  }

  if (last?.thinking?.trim()) return `Vis is thinking ${suffix}`;
  if (last?.forms?.length) return `Vis is running code ${suffix}`;
  return `Vis is working ${suffix}`;
}

function updateLiveIteration(
  turn: LiveTurn,
  position: number,
  update: (iteration: TranscriptIteration) => TranscriptIteration,
): LiveTurn {
  const index = turn.iterations.findIndex((iteration) => iteration.position === position);
  if (index < 0) {
    return {
      ...turn,
      iterations: [...turn.iterations, update({ position, forms: [] })].sort(
        (a, b) => (a.position ?? 0) - (b.position ?? 0),
      ),
    };
  }

  const iterations = [...turn.iterations];
  iterations[index] = update(iterations[index]);
  return { ...turn, iterations };
}

function formFromEvent(event: SseEvent, running = false): TranscriptForm {
  const cards = Array.isArray(event.cards) ? (event.cards as TranscriptForm[]) : undefined;
  return {
    block_id: stringField(event, 'block_id'),
    scope: stringField(event, 'scope') || undefined,
    code: stringField(event, 'code') || undefined,
    display_code: stringField(event, 'display_code') || undefined,
    comment: stringField(event, 'comment') || undefined,
    tool_name: stringField(event, 'tool_name') || undefined,
    tool_color_role: stringField(event, 'tool_color_role') || undefined,
    result_summary: stringField(event, 'result_summary') || (running ? 'Running…' : undefined),
    result_render: stringField(event, 'result_render') || undefined,
    result_kind: stringField(event, 'result_kind') || undefined,
    result: event.result as TranscriptForm['result'],
    error: event.error as TranscriptForm['error'],
    stdout: stringField(event, 'stdout') || undefined,
    cards,
    silent: event.silent === true,
    duration_ms: typeof event.duration_ms === 'number' ? event.duration_ms : undefined,
  };
}

function formIsRunningPlaceholder(form: TranscriptForm): boolean {
  return (
    form.result == null &&
    form.error == null &&
    form.duration_ms == null &&
    (!form.result_summary || form.result_summary === 'Running…')
  );
}

function formHasOutcome(form: TranscriptForm): boolean {
  return (
    form.result != null ||
    form.error != null ||
    form.duration_ms != null ||
    (!!form.result_summary && form.result_summary !== 'Running…')
  );
}

function upsertLiveForm(iteration: TranscriptIteration, next: TranscriptForm): TranscriptIteration {
  const forms = [...(iteration.forms ?? [])];
  const blockId = next.block_id;
  let index = forms.findIndex((form) => blockId && form.block_id === blockId);
  // Fallback: a completed form supersedes the still-running placeholder for the
  // same tool when block_id didn't line up (gateway replay / a started event
  // that shipped no block_id). Without this the 'X Running…' placeholder and the
  // finished card both render — the same op shown twice.
  if (index < 0 && formHasOutcome(next)) {
    index = forms.findIndex(
      (form) =>
        formIsRunningPlaceholder(form) &&
        (form.tool_name ?? '') === (next.tool_name ?? '') &&
        (form.scope ?? '') === (next.scope ?? ''),
    );
  }
  if (index < 0) forms.push(next);
  else {
    const defined = Object.fromEntries(
      Object.entries(next).filter(([, value]) => value !== undefined),
    ) as TranscriptForm;
    forms[index] = { ...forms[index], ...defined };
  }
  return { ...iteration, forms };
}

function reduceLiveEvent(turn: LiveTurn | null, event: SseEvent): LiveTurn | null {
  const type = event.type;
  if (type === 'turn.started') {
    return {
      id: stringField(event, 'turn_id'),
      request: stringField(event, 'request'),
      answer: '',
      iterations: [],
      startedAt: typeof event.started_at === 'number' ? event.started_at : Date.now(),
      status: 'running',
    };
  }
  if (!turn) return turn;

  if (type === 'content.block.delta') {
    const field = stringField(event, 'field');
    const blockId = stringField(event, 'block_id');
    const position = eventIteration(event);
    if (field === 'text') {
      const next = updateLiveIteration(turn, position, (iteration) => ({
        ...iteration,
        thinking: applyText(iteration.thinking ?? '', event),
      }));
      return { ...next, activity: undefined };
    }
    if (field === 'markdown' && blockId.includes(':assistant-prose:')) {
      const next = updateLiveIteration(turn, position, (iteration) => ({
        ...iteration,
        assistant_prose: applyText(iteration.assistant_prose ?? '', event),
      }));
      // The model's prose streamed first as a live `:content` ticker (turn.answer)
      // and now lands as this iteration's canonical prose. Mirror the TUI
      // (progress.clj drops `:content-stream`): clear the live answer so the same
      // text isn't rendered twice — once above the tool and once below it.
      return { ...next, answer: '', activity: undefined };
    }
    if (field === 'markdown') {
      return { ...turn, answer: applyText(turn.answer, event), activity: undefined };
    }
    return turn;
  }

  if (type === 'iteration.completed') {
    const position = eventIteration(event);
    const next = updateLiveIteration(turn, position, (iteration) => ({
      ...iteration,
      thinking: stringField(event, 'thinking') || iteration.thinking,
      assistant_prose: stringField(event, 'assistant_prose') || iteration.assistant_prose,
      error: undefined,
    }));
    // If this iteration finalized any prose, the live `:content` ticker that fed
    // it has been promoted into the iteration — drop it so it isn't duplicated.
    const promoted = next.iterations.find((i) => i.position === position)?.assistant_prose;
    return { ...next, answer: promoted ? '' : turn.answer, activity: undefined };
  }

  if (type === 'block.preview') {
    const position = eventIteration(event);
    const form = formFromEvent(event, false);
    const next = updateLiveIteration(turn, position, (iteration) => upsertLiveForm(iteration, form));
    return { ...next, activity: undefined };
  }

  if (type === 'block.started' || type === 'block.output') {
    const position = eventIteration(event);
    const form = formFromEvent(event, type === 'block.started');
    const next = updateLiveIteration(turn, position, (iteration) => upsertLiveForm(iteration, form));
    if (type === 'block.output') return { ...next, activity: undefined };
    return {
      ...next,
      activity: {
        kind: form.tool_name ? 'tool' : 'code',
        iteration: position,
        operation: form.tool_name || form.scope,
      },
    };
  }

  if (type === 'activity') {
    const kind = stringField(event, 'activity');
    const rawIteration = event.iteration;
    const iteration = typeof rawIteration === 'number'
      ? rawIteration
      : typeof rawIteration === 'string' && rawIteration.trim()
        ? Number(rawIteration)
        : undefined;
    return {
      ...turn,
      activity: kind ? {
        kind,
        iteration: Number.isFinite(iteration) ? iteration : undefined,
        command: stringField(event, 'cmd') || undefined,
        operation: stringField(event, 'op') || undefined,
        label: stringField(event, 'label') || undefined,
      } : undefined,
    };
  }

  if (type === 'iteration.error' || type === 'provider.retry') {
    const position = eventIteration(event);
    const next = updateLiveIteration(turn, position, (iteration) => ({
      ...iteration,
      error: (event.error_data ?? event.error ?? event.detail ?? 'retrying') as TranscriptIteration['error'],
    }));
    return { ...next, activity: undefined };
  }

  return turn;
}

function coalesceLiveEvents(events: SseEvent[]): SseEvent[] {
  const merged: SseEvent[] = [];
  for (const event of events) {
    const previous = merged.at(-1);
    const sameDelta = previous?.type === 'content.block.delta'
      && event.type === 'content.block.delta'
      && stringField(previous, 'field') === stringField(event, 'field')
      && stringField(previous, 'block_id') === stringField(event, 'block_id')
      && eventIteration(previous) === eventIteration(event);

    if (!previous || !sameDelta) {
      merged.push(event);
      continue;
    }

    const currentCumulative = stringField(event, 'cumulative');
    const previousCumulative = stringField(previous, 'cumulative');
    if (currentCumulative) {
      merged[merged.length - 1] = event;
    } else if (previousCumulative) {
      merged[merged.length - 1] = {
        ...previous,
        ...event,
        cumulative: previousCumulative + stringField(event, 'text'),
        text: '',
      };
    } else {
      merged[merged.length - 1] = {
        ...previous,
        ...event,
        cumulative: '',
        text: stringField(previous, 'text') + stringField(event, 'text'),
      };
    }
  }
  return merged;
}

const FALLBACK_SLASHES: SlashCommand[] = [
  { name: '/help', doc: 'Show the available slash commands.' },
  { name: '/new-session', doc: 'Create a new session. Optional text starts its first turn.' },
  { name: '/sessions', doc: 'Return to the session list.' },
  { name: '/clear', doc: 'Start a fresh session without deleting this transcript.' },
  { name: '/rename', doc: "Rename this session's title." },
  { name: '/export', doc: 'Export this session transcript to Markdown or HTML.' },
  { name: '/export-html', doc: 'Export this session transcript as styled HTML.' },
  { name: '/root', doc: 'Show or change the session filesystem root.' },
  { name: '/draft new', doc: 'Create an isolated draft workspace.' },
  { name: '/draft apply', doc: 'Apply the active draft workspace.' },
  { name: '/draft abandon', doc: 'Abandon the active draft workspace.' },
  { name: '/draft list', doc: 'List draft workspaces.' },
  { name: '/fs list', doc: 'List filesystem permissions.' },
  { name: '/fs add', doc: 'Add an allowed filesystem root.' },
  { name: '/reload', doc: 'Reload extensions, skills, prompts, and context files.' },
];

function mergeSlashCommands(remote: SlashCommand[]): SlashCommand[] {
  const byName = new Map<string, SlashCommand>();
  for (const command of [...FALLBACK_SLASHES, ...remote]) byName.set(command.name, command);
  return [...byName.values()].sort((a, b) => a.name.localeCompare(b.name));
}

// `@` file-mention trigger, mirroring the TUI (`file_suggest.clj` trigger-regex)
// VERBATIM: the `@` must begin a word (start of text or right after whitespace),
// and `@@` escapes to a literal `@`. `head` is the input text up to the caret.
const FILE_MENTION_REGEX = /(?:^|\s)@(?!@)(\S*)$/;

function fileMentionAt(head: string): { query: string; at: number } | null {
  const match = FILE_MENTION_REGEX.exec(head);
  if (!match) return null;
  const query = match[1] ?? '';
  return { query, at: head.length - query.length - 1 };
}

// Visible inline token inserted by the picker — quoted when the path has spaces,
// matching the TUI's `format-file-mention`.
function formatFileMention(path: string): string {
  return /\s/.test(path) ? `@"${path}"` : `@${path}`;
}

// Splice the picked `path` over the active `@token` at the caret, returning the
// new text and caret offset. Mirrors `file_suggest.clj` apply-mention.
function applyFileMention(
  text: string,
  caret: number,
  path: string,
): { text: string; caret: number } {
  const head = text.slice(0, caret);
  const mention = fileMentionAt(head);
  if (!mention) return { text, caret };
  const before = text.slice(0, mention.at);
  const after = text.slice(caret);
  const token = `${formatFileMention(path)} `;
  return { text: before + token + after, caret: before.length + token.length };
}

// Expand inline `@path` mentions into the SAME agent-facing read-this-file
// directive the TUI emits (`input.clj` file-mention->prompt-block), so the model
// knows the user attached a file. The visible transcript keeps the short `@path`
// token; only the outbound agent text carries the directive. `@@` stays literal.
const FILE_MENTION_EXPAND_REGEX =
  /(?<!\S)@(?:"([^"]+)"|([A-Za-z0-9][A-Za-z0-9._/-]*))/g;

function expandFileMentions(text: string): string {
  return text.replace(FILE_MENTION_EXPAND_REGEX, (_match, quoted, bare) => {
    const path = (quoted ?? bare) as string;
    return `[Attached File: ${path}]\nThe user attached this file. Read it (via the file/zipper tools) before answering.`;
  });
}

const LOADING_SPINNER_FRAMES = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'];

// Mirrors the TUI's `paint-content-loading!`: a centered Braille spinner that
// advances every 100ms next to "Loading session…" while an existing session
// hydrates. New-session creation never mounts this — it opens straight to the
// empty transcript, matching the TUI (which suppresses the spinner for a
// still-building `:build-id` tab).
function LoadingSession() {
  const [now, setNow] = useState(() => Date.now());
  useEffect(() => {
    const timer = window.setInterval(() => setNow(Date.now()), 100);
    return () => window.clearInterval(timer);
  }, []);
  const frame = LOADING_SPINNER_FRAMES[Math.floor(now / 100) % LOADING_SPINNER_FRAMES.length];
  return (
    <div
      className="flex min-h-[55vh] items-center justify-center font-mono text-[12px] text-white"
      role="status"
      aria-label="Loading session"
    >
      <span className="motion-reduce:hidden">{frame}</span>
      <span className="hidden motion-reduce:inline">●</span>
      <span>&nbsp;&nbsp;Loading session…</span>
    </div>
  );
}

// The session id is the durable handle a user pastes into `vis`/tools, so it is
// tap-to-copy rather than inert text — shown short with the full id on hover.
function CopyableId({ id, className }: { id: string; className: string }) {
  const [copied, setCopied] = useState(false);
  async function copy() {
    try {
      await navigator.clipboard.writeText(id);
      setCopied(true);
      window.setTimeout(() => setCopied(false), 1_200);
    } catch {
      // Clipboard access can be unavailable in an untrusted mobile webview.
    }
  }
  const short = id.length > 8 ? id.slice(0, 8) : id;
  return (
    <button
      type="button"
      onClick={copy}
      title={`Copy session id\n${id}`}
      aria-label="Copy session id"
      className={`group inline-flex min-w-0 items-center gap-1 border border-dialog-edge px-2 py-1 font-mono text-[10px] leading-none transition-[background-color,color,border-color] hover:bg-hover ${copied ? 'border-ok text-ok' : 'text-dialog-hint'} ${className}`}
    >
      <span aria-hidden="true" className="opacity-50 transition-opacity group-hover:opacity-100">#</span>
      <span className="truncate">{copied ? 'Copied' : short}</span>
    </button>
  );
}

// Copies the current shareable URL (origin + #/s/<sid>?gw=<gateway>) so another
// user can open the same session in their own paired app. Prefers the native
// iOS/Android share sheet, falling back to a clipboard copy with confirmation.
function ShareLink({ className }: { className: string }) {
  const [copied, setCopied] = useState(false);
  async function share() {
    const url = window.location.href;
    try {
      if (typeof navigator.share === 'function') {
        await navigator.share({ title: 'Vis session', url });
        return;
      }
      await navigator.clipboard.writeText(url);
      setCopied(true);
      window.setTimeout(() => setCopied(false), 1_500);
    } catch {
      // User dismissed the native share sheet, or clipboard is unavailable.
    }
  }
  return (
    <button
      type="button"
      onClick={share}
      title="Share this session"
      aria-label="Share this session"
      className={`group inline-flex shrink-0 items-center gap-1 border px-2 py-1 font-mono text-[10px] uppercase tracking-[0.08em] leading-none transition-[background-color,color,border-color,transform] duration-150 active:scale-[0.97] motion-reduce:transition-none ${copied ? 'border-ok text-ok' : 'border-dialog-edge text-dialog-hint hover:border-edge-strong hover:bg-hover hover:text-white'} ${className}`}
    >
      {copied ? (
        <>
          <svg viewBox="0 0 20 20" className="size-3" fill="none" stroke="currentColor" strokeWidth="2" aria-hidden="true">
            <path d="M5 10.5l3.5 3.5L15 6.5" strokeLinecap="round" strokeLinejoin="round" />
          </svg>
          <span>Copied</span>
        </>
      ) : (
        <>
          <svg viewBox="0 0 20 20" className="size-3" fill="none" stroke="currentColor" strokeWidth="1.6" aria-hidden="true">
            <path d="M7.5 10.5l5-3M7.5 9.5l5 3M6 10a2 2 0 11-4 0 2 2 0 014 0zM16 5a2 2 0 11-4 0 2 2 0 014 0zM16 15a2 2 0 11-4 0 2 2 0 014 0z" strokeLinecap="round" strokeLinejoin="round" />
          </svg>
          <span>Share</span>
        </>
      )}
    </button>
  );
}

export function SessionScreen({
  client,
  subscriptions,
  sid,
  onBack,
  onOpenSession,
  fresh = false,
}: {
  client: GatewayClient;
  subscriptions: SessionSubscriptionHub;
  sid: string;
  onBack: () => void;
  onOpenSession: (sid: string, fresh?: boolean) => void;
  fresh?: boolean;
}) {
  const [session, setSession] = useState<Session | null>(null);
  const [turns, setTurns] = useState<TranscriptTurn[]>([]);
  const [prompt, setPrompt] = useState('');
  const [error, setError] = useState<string | null>(null);
  const [loading, setLoading] = useState(!fresh);
  const [connected, setConnected] = useState(false);
  const [running, setRunning] = useState(false);
  const [liveTurn, setLiveTurn] = useState<LiveTurn | null>(null);
  const [queued, setQueued] = useState<QueuedTurn[]>([]);
  const [editingQueued, setEditingQueued] = useState<{ turnId: string; text: string } | null>(null);
  const [queuePaused, setQueuePaused] = useState<QueuePausedInfo | null>(null);
  const [showJump, setShowJump] = useState(false);
  const [visibleTurnCount, setVisibleTurnCount] = useState(INITIAL_VISIBLE_TURNS);
  const [slashCommands, setSlashCommands] = useState<SlashCommand[]>(FALLBACK_SLASHES);
  const [slashIndex, setSlashIndex] = useState(0);
  const [slashDismissed, setSlashDismissed] = useState(false);
  const [caret, setCaret] = useState(0);
  const [fileSuggestions, setFileSuggestions] = useState<FileSuggestion[]>([]);
  const [fileIndex, setFileIndex] = useState(0);
  const [fileDismissed, setFileDismissed] = useState(false);
  const [capabilities, setCapabilities] = useState<GatewayCapabilities | null>(null);
  const [attachments, setAttachments] = useState<PendingAttachment[]>([]);
  const [pastes, setPastes] = useState<Map<number, ComposerPaste>>(() => new Map());
  const [composerNotice, setComposerNotice] = useState<string | null>(null);
  const [voiceSupported, setVoiceSupported] = useState(false);
  const [voiceModel, setVoiceModel] = useState<VoiceModelState | null>(null);
  const [voicePhase, setVoicePhase] = useState<'idle' | 'recording' | 'transcribing'>('idle');
  const [voiceRequested, setVoiceRequested] = useState(false);
  const scrollRef = useRef<HTMLDivElement>(null);
  const transcriptRef = useRef<HTMLDivElement>(null);
  const composerRef = useRef<HTMLTextAreaElement>(null);
  const fileInputRef = useRef<HTMLInputElement>(null);
  const recordingRef = useRef<WavRecording | null>(null);
  const pasteCounterRef = useRef(0);
  const resizeScrollFrameRef = useRef<number | null>(null);
  const disclosureScrollFrameRef = useRef<number | null>(null);
  const prependScrollHeightRef = useRef<number | null>(null);
  const followingRef = useRef(true);
  const showJumpRef = useRef(false);
  const liveTurnRef = useRef<LiveTurn | null>(null);
  const runningRef = useRef(false);
  const turnsRef = useRef<TranscriptTurn[]>([]);
  // Keep the loading overlay up until a freshly opened session has been
  // scrolled to its bottom, so persisted history never flashes at the top first.
  const initialScrollPendingRef = useRef(!fresh);
  runningRef.current = running;
  turnsRef.current = turns;

  useEffect(() => {
    void recordingRef.current?.cancel();
    recordingRef.current = null;
    setTurns([]);
    setLiveTurn(null);
    setQueued([]);
    setQueuePaused(null);
    liveTurnRef.current = null;
    setSession(null);
    setAttachments([]);
    setPastes(new Map());
    pasteCounterRef.current = 0;
    setComposerNotice(null);
    setVoicePhase('idle');
    setVoiceRequested(false);
    setLoading(!fresh);
    setVisibleTurnCount(INITIAL_VISIBLE_TURNS);
    followingRef.current = true;
    initialScrollPendingRef.current = !fresh;
    showJumpRef.current = false;
  }, [sid, fresh]);

  const scrollToEnd = useCallback((behavior: ScrollBehavior = 'auto') => {
    const viewport = scrollRef.current;
    if (!viewport) return;
    viewport.scrollTo({ top: viewport.scrollHeight, behavior });
    followingRef.current = true;
    if (showJumpRef.current) {
      showJumpRef.current = false;
      setShowJump(false);
    }
  }, []);

  const loadTranscript = useCallback(async () => {
    try {
      const next = await client.transcript(sid);
      setTurns(next);
      setError(null);
      // With turns present, the scroll effect drops the overlay only after it
      // pins the viewport to the bottom; an empty transcript has nothing to
      // scroll, so reveal it immediately.
      if (!next.length) {
        initialScrollPendingRef.current = false;
        setLoading(false);
      }
      return next;
    } catch (cause) {
      setError((cause as Error).message);
      initialScrollPendingRef.current = false;
      setLoading(false);
      return null;
    }
  }, [client, sid]);

  useEffect(() => {
    const controller = new AbortController();
    void Promise.all([
      loadTranscript(),
      client.session(sid, controller.signal).then(setSession).catch(() => undefined),
    ]);
    return () => controller.abort();
  }, [client, sid, loadTranscript]);

  // Reconcile against the gateway's authoritative liveness. A streamed live turn
  // is only cleared by a terminal SSE event, but that event can be missed — the
  // turn finished in the TUI/another client before we subscribed, the ring
  // rewound past the completion, or the socket dropped mid-turn. Left alone the
  // bubble spins "working" forever while the session is actually idle. Poll the
  // session every few seconds; when the gateway says it is no longer live but we
  // still show a running turn, reload the transcript and drop the live bubble.
  useEffect(() => {
    let cancelled = false;
    const reconcile = async () => {
      if (document.visibilityState === 'hidden') return;
      let next: Session;
      try {
        next = await client.session(sid);
      } catch {
        return;
      }
      if (cancelled) return;
      setSession(next);
      const gatewayLive =
        next.live !== undefined ? next.live : next.status === 'running';
      const showsWork = liveTurnRef.current !== null || runningRef.current;
      if (!gatewayLive && showsWork) {
        // Batch the reload with clearing the live bubble so the persisted turn
        // (engine id ≠ live gateway id) never renders alongside it for a frame.
        let next: TranscriptTurn[] | null = null;
        try {
          next = await client.transcript(sid);
        } catch {
          next = null;
        }
        if (cancelled) return;
        if (next) setTurns(next);
        setRunning(false);
        setLiveTurn(null);
        liveTurnRef.current = null;
      }
    };
    const timer = window.setInterval(() => void reconcile(), 5000);
    const onVisible = () => {
      if (document.visibilityState === 'visible') void reconcile();
    };
    document.addEventListener('visibilitychange', onVisible);
    return () => {
      cancelled = true;
      window.clearInterval(timer);
      document.removeEventListener('visibilitychange', onVisible);
    };
  }, [client, sid, loadTranscript]);

  useEffect(() => {
    const controller = new AbortController();
    void client
      .slashes(controller.signal)
      .then((commands) => setSlashCommands(mergeSlashCommands(commands)))
      .catch(() => setSlashCommands(mergeSlashCommands([])));
    return () => controller.abort();
  }, [client]);

  useEffect(() => {
    const controller = new AbortController();
    let active = true;
    void (async () => {
      try {
        const next = await client.capabilities(controller.signal);
        if (!active) return;
        setCapabilities(next);
        setVoiceSupported(next.features.voice.enabled);
        setVoiceModel(next.features.voice.model);
      } catch {
        try {
          const model = await client.voiceModel(sid, false, controller.signal);
          if (!active) return;
          setVoiceSupported(model.status !== 'unavailable');
          setVoiceModel(model);
        } catch {
          if (!active) return;
          setVoiceSupported(false);
          setVoiceModel({ status: 'unavailable' });
        }
      }
    })();
    return () => {
      active = false;
      controller.abort();
    };
  }, [client, sid]);

  useEffect(() => {
    if (!voiceSupported || voiceModel?.status !== 'downloading') return;
    const timer = window.setInterval(() => {
      void client.voiceModel(sid).then(setVoiceModel).catch(() => undefined);
    }, 2000);
    return () => window.clearInterval(timer);
  }, [client, sid, voiceModel?.status, voiceSupported]);

  useEffect(() => () => {
    void recordingRef.current?.cancel();
    recordingRef.current = null;
  }, []);

  useEffect(() => {
    async function settle(event: SseEvent) {
      const type = event.type;
      setRunning(false);
      // Keep the streamed live turn on screen until the finished turn is
      // actually persisted in the transcript, otherwise it vanishes for a frame
      // (the persisted row lags the terminal event) and the view jumps.
      const finishedId = stringField(event, 'turn_id') || liveTurnRef.current?.id || '';
      // Fetch the finished transcript WITHOUT touching state, then apply the
      // turns and drop the live bubble in ONE synchronous (React-batched) update.
      // The persisted finished turn carries the engine's row id, not the live
      // turn's gateway id, so `visibleTurns` can't filter it out — if `setTurns`
      // rendered before `setLiveTurn(null)`, both would show for a frame (dup).
      let next: TranscriptTurn[] | null = null;
      try {
        next = await client.transcript(sid);
      } catch {
        next = null;
      }
      const has = (turns: TranscriptTurn[] | null) =>
        !finishedId || !!turns?.some((turn) => (turn.id ?? turn.turn_id) === finishedId);
      if (next && !has(next)) {
        await new Promise((resolve) => window.setTimeout(resolve, 300));
        try {
          next = await client.transcript(sid);
        } catch {
          /* keep the earlier snapshot */
        }
      }
      if (next) {
        setTurns(next);
        setError(null);
        setLoading(false);
        if (has(next)) {
          setLiveTurn(null);
          liveTurnRef.current = null;
        }
      }
      if (type === 'turn.failed') {
        setError(stringField(event, 'message') || stringField(event, 'error') || 'The turn failed.');
      }
    }


    // Match the TUI's 150 ms live-body throttle. One reducer pass and one React
    // state update replace hundreds of token-level updates during fast streams.
    const eventQueue: SseEvent[] = [];
    let timerId: number | null = null;
    const flushEvents = () => {
      timerId = null;
      const batch = coalesceLiveEvents(eventQueue.splice(0));
      if (!batch.length) return;

      // Queue-mirror + pause control frames (channel-agnostic, same events the
      // TUI consumes). Not live-turn events, so handle them outside the reducer.
      for (const event of batch) {
        const tid = stringField(event, 'turn_id');
        switch (event.type) {
          case 'turn.queued':
            setQueued((current) =>
              current.some((item) => item.turnId === tid)
                ? current
                : [...current, { turnId: tid, request: stringField(event, 'request') }]);
            break;
          case 'turn.queued.updated':
            setQueued((current) =>
              current.map((item) =>
                item.turnId === tid ? { ...item, request: stringField(event, 'request') } : item));
            break;
          case 'turn.queued.deleted':
          case 'turn.queued.drained':
            setQueued((current) => current.filter((item) => item.turnId !== tid));
            break;
          case 'queue.paused':
            setQueuePaused({
              reason: stringField(event, 'reason') || 'provider_unhealthy',
              held: Number(event.held ?? 0),
              fails: Number(event.fails ?? 0),
              isTransient: event.is_transient !== false,
              isBreakerOpen: event.is_breaker_open === true,
              retryAt: event.retry_at != null ? Number(event.retry_at) : null,
            });
            break;
          case 'queue.resumed':
            setQueuePaused(null);
            break;
          default:
            break;
        }
      }

      if (batch.some((event) => event.type === 'turn.started')) setRunning(true);
      setLiveTurn((turn) => {
        const reduced = batch.reduce(reduceLiveEvent, turn);
        liveTurnRef.current = reduced;
        return reduced;
      });

      let terminal: SseEvent | undefined;
      for (let index = batch.length - 1; index >= 0; index -= 1) {
        if (TERMINAL_EVENTS.has(batch[index].type)) {
          terminal = batch[index];
          break;
        }
      }
      if (terminal) void settle(terminal);
    };

    const unsubscribeConnection = subscriptions.subscribeConnection(setConnected);
    const unsubscribeEvents = subscriptions.subscribeSession(
      sid,
      (event) => {
        eventQueue.push(event);
        if (timerId !== null) return;
        const delay = TERMINAL_EVENTS.has(event.type) ? 0 : LIVE_BODY_THROTTLE_MS;
        timerId = window.setTimeout(flushEvents, delay);
      },
    );

    return () => {
      if (timerId !== null) window.clearTimeout(timerId);
      eventQueue.length = 0;
      unsubscribeEvents();
      unsubscribeConnection();
      setConnected(false);
    };
  }, [loadTranscript, sid, subscriptions]);

  useLayoutEffect(() => {
    const viewport = scrollRef.current;
    const previousHeight = prependScrollHeightRef.current;
    if (viewport && previousHeight !== null) {
      viewport.scrollTop += viewport.scrollHeight - previousHeight;
      prependScrollHeightRef.current = null;
      return;
    }
    if (followingRef.current) scrollToEnd('auto');
    if (initialScrollPendingRef.current && turns.length) {
      initialScrollPendingRef.current = false;
      // Reveal one frame later, after the browser paints the bottom-pinned
      // transcript, so opening a session lands on the latest turn.
      requestAnimationFrame(() => setLoading(false));
    }
  }, [turns, visibleTurnCount, liveTurn?.id, scrollToEnd]);

  // Deferred Markdown, fonts, and content-visibility can change the transcript's
  // measured height after React commits. Keep a newly opened/followed session at
  // its actual bottom as those measurements settle.
  useEffect(() => {
    const transcript = transcriptRef.current;
    if (!transcript || typeof ResizeObserver === 'undefined') return;

    const observer = new ResizeObserver(() => {
      if (!followingRef.current || resizeScrollFrameRef.current !== null) return;
      resizeScrollFrameRef.current = window.requestAnimationFrame(() => {
        resizeScrollFrameRef.current = null;
        if (followingRef.current) scrollToEnd('auto');
      });
    });
    observer.observe(transcript);

    return () => {
      observer.disconnect();
      if (resizeScrollFrameRef.current !== null) {
        window.cancelAnimationFrame(resizeScrollFrameRef.current);
        resizeScrollFrameRef.current = null;
      }
      if (disclosureScrollFrameRef.current !== null) {
        window.cancelAnimationFrame(disclosureScrollFrameRef.current);
        disclosureScrollFrameRef.current = null;
      }
    };
  }, [scrollToEnd, sid]);

  useEffect(() => {
    const textarea = composerRef.current;
    if (!textarea) return;
    textarea.style.height = 'auto';
    textarea.style.height = `${Math.min(textarea.scrollHeight, 112)}px`;
  }, [prompt]);

  function addAttachments() {
    const limits = capabilities?.features.attachments;
    const maximum = limits?.max_files ?? 8;
    if (maximum - attachments.length <= 0) {
      setComposerNotice(`You can attach up to ${maximum} images`);
      return;
    }
    // A persistent hidden <input type="file"> is the one attachment path that
    // works identically on the web and inside the iOS/Android WKWebView (it
    // shows the native Photos/Files sheet) — no Capacitor plugin required.
    fileInputRef.current?.click();
  }

  async function onFilesPicked(fileList: FileList | null) {
    const input = fileInputRef.current;
    if (input) input.value = '';
    const files = fileList ? Array.from(fileList) : [];
    if (!files.length) return;

    const limits = capabilities?.features.attachments;
    const maximum = limits?.max_files ?? 8;
    const remaining = maximum - attachments.length;
    if (remaining <= 0) {
      setComposerNotice(`You can attach up to ${maximum} images`);
      return;
    }
    try {
      const result = await attachmentsFromFiles(files, {
        maxFiles: remaining,
        maxFileBytes: limits?.max_file_bytes ?? 5 * 1024 * 1024,
        mediaTypes: limits?.media_types,
      });
      setAttachments((current) => [...current, ...result.attachments].slice(0, maximum));
      setComposerNotice(result.rejected.length ? result.rejected.join(' · ') : null);
    } catch (cause) {
      setComposerNotice((cause as Error).message);
    }
  }

  async function captureCamera() {
    const limits = capabilities?.features.attachments;
    const maximum = limits?.max_files ?? 8;
    if (attachments.length >= maximum) {
      setComposerNotice(`You can attach up to ${maximum} images`);
      return;
    }
    try {
      const shot = await captureCameraAttachment({
        maxFileBytes: limits?.max_file_bytes ?? 5 * 1024 * 1024,
      });
      if (shot) {
        setAttachments((current) => [...current, shot].slice(0, maximum));
        setComposerNotice(null);
      }
    } catch (cause) {
      const message = (cause as Error).message;
      if (!/cancel|dismiss|no image|user cancelled/i.test(message)) setComposerNotice(message);
    }
  }

  function removeAttachment(id: string) {
    setAttachments((current) => current.filter((attachment) => attachment.id !== id));
    setComposerNotice(null);
  }

  function removePaste(id: number) {
    const paste = pastes.get(id);
    if (!paste) return;
    setPrompt((current) => current.replace(paste.token, '').replace(/ {2,}/g, ' '));
    setPastes((current) => {
      const next = new Map(current);
      next.delete(id);
      return next;
    });
  }

  async function addPastedImages(files: File[]) {
    const limits = capabilities?.features.attachments;
    const maximum = limits?.max_files ?? 8;
    const remaining = maximum - attachments.length;
    if (remaining <= 0) {
      setComposerNotice(`You can attach up to ${maximum} images`);
      return;
    }
    try {
      const result = await attachmentsFromFiles(files, {
        maxFiles: remaining,
        maxFileBytes: limits?.max_file_bytes ?? 5 * 1024 * 1024,
        mediaTypes: limits?.media_types,
      });
      setAttachments((current) => [...current, ...result.attachments].slice(0, maximum));
      setComposerNotice(result.rejected.length ? result.rejected.join(' · ') : null);
    } catch (cause) {
      setComposerNotice((cause as Error).message);
    }
  }

  function handlePaste(event: ReactClipboardEvent<HTMLTextAreaElement>) {
    // Image paste (screenshots, copied pictures) — works on web and in the
    // iOS/Android WKWebView, which surface pasted images as clipboard files.
    const imageFiles = Array.from(event.clipboardData.files).filter((file) =>
      file.type.startsWith('image/'),
    );
    if (imageFiles.length) {
      event.preventDefault();
      void addPastedImages(imageFiles);
      return;
    }

    const content = event.clipboardData.getData('text/plain').replace(/\r\n?/g, '\n');
    if (!content || !shouldCollapsePaste(content)) return;
    event.preventDefault();

    const id = ++pasteCounterRef.current;
    const paste = createComposerPaste(id, content);
    const input = event.currentTarget;
    const start = input.selectionStart ?? prompt.length;
    const end = input.selectionEnd ?? start;
    const nextPrompt = `${prompt.slice(0, start)}${paste.token}${prompt.slice(end)}`;
    setPastes((current) => new Map(current).set(id, paste));
    setPrompt(nextPrompt);
    window.requestAnimationFrame(() => {
      const caret = start + paste.token.length;
      composerRef.current?.setSelectionRange(caret, caret);
    });
  }

  async function toggleVoice() {
    setVoiceRequested(true);
    setComposerNotice(null);

    if (recordingRef.current) {
      const recording = recordingRef.current;
      recordingRef.current = null;
      setVoicePhase('transcribing');
      try {
        const wav = await recording.stop();
        const transcript = await client.transcribeVoice(sid, wav);
        const text = transcript.text.trim();
        if (text) setPrompt((current) => `${current.trimEnd()}${current.trim() ? ' ' : ''}${text}`);
        requestAnimationFrame(() => composerRef.current?.focus());
      } catch (cause) {
        setComposerNotice((cause as Error).message);
      } finally {
        setVoicePhase('idle');
      }
      return;
    }

    if (voiceModel?.status === 'downloading') return;
    try {
      let model = voiceModel;
      if (model?.status !== 'ready') {
        model = await client.voiceModel(sid, true);
        setVoiceModel(model);
        if (model.status !== 'ready') return;
      }
      recordingRef.current = await startWavRecording();
      setVoicePhase('recording');
    } catch (cause) {
      setVoicePhase('idle');
      setComposerNotice((cause as Error).message);
    }
  }

  async function send() {
    const authoredRequest = prompt.trim();
    const request = expandFileMentions(expandPastePlaceholders(authoredRequest, pastes))
      || (attachments.length ? 'Please inspect the attached image(s).' : '');
    const displayRequest = collapsePastePlaceholders(authoredRequest, pastes) || request;
    if (!request || voicePhase !== 'idle') return;

    const [command = '', ...argParts] = authoredRequest.split(/\s+/);
    const args = argParts.join(' ');

    if (command === '/help') {
      setPrompt('/');
      setSlashDismissed(false);
      setSlashIndex(0);
      return;
    }

    if (command === '/sessions') {
      setPrompt('');
      onBack();
      return;
    }

    if (command === '/new-session' || command === '/clear') {
      setPrompt('');
      setError(null);
      setRunning(true);
      try {
        const created = await client.createSession({ channel: 'web' });
        if (command === '/new-session' && args) await client.submitTurn(created.id, args);
        onOpenSession(created.id, true);
      } catch (cause) {
        setPrompt(request);
        setError((cause as Error).message);
      } finally {
        setRunning(false);
      }
      return;
    }

    // A turn is already running: the gateway enqueues this behind it and mirrors
    // it back as `turn.queued`, which fills the tray. Keep the composer live.
    if (running || queued.length) {
      const pendingAttachments = attachments;
      const pendingPastes = pastes;
      setPrompt('');
      setAttachments([]);
      setPastes(new Map());
      setComposerNotice(null);
      setSlashDismissed(false);
      setError(null);
      try {
        await client.submitTurn(sid, request, {
          displayRequest,
          attachments: pendingAttachments.map(({ filename, media_type, base64 }) => ({
            filename,
            media_type,
            base64,
          })),
        });
      } catch (cause) {
        setPrompt(authoredRequest);
        setPastes(pendingPastes);
        setAttachments((current) => current.length ? current : pendingAttachments);
        setError((cause as Error).message);
        requestAnimationFrame(() => composerRef.current?.focus());
      }
      return;
    }

    const pendingAttachments = attachments;
    const pendingPastes = pastes;
    setPrompt('');
    setAttachments([]);
    setPastes(new Map());
    setComposerNotice(null);
    setSlashDismissed(false);
    setError(null);
    setRunning(true);
    setLiveTurn({
      request: displayRequest,
      answer: '',
      iterations: [],
      startedAt: Date.now(),
      status: 'running',
    });
    followingRef.current = true;
    requestAnimationFrame(() => scrollToEnd());

    try {
      const submitted = await client.submitTurn(sid, request, {
        displayRequest,
        attachments: pendingAttachments.map(({ filename, media_type, base64 }) => ({
          filename,
          media_type,
          base64,
        })),
      });
      setLiveTurn((turn) => turn ? { ...turn, id: submitted.turn_id ?? submitted.id } : turn);
    } catch (cause) {
      setRunning(false);
      setLiveTurn(null);
      setPrompt(authoredRequest);
      setPastes(pendingPastes);
      setAttachments((current) => current.length ? current : pendingAttachments);
      setError((cause as Error).message);
      requestAnimationFrame(() => composerRef.current?.focus());
    }
  }

  async function cancel() {
    setLiveTurn((turn) => turn ? { ...turn, cancelling: true, activity: undefined } : turn);
    try {
      await client.cancelCurrentTurn(sid);
    } catch (cause) {
      setLiveTurn((turn) => turn ? { ...turn, cancelling: false } : turn);
      setError((cause as Error).message);
    }
  }

  function handleDisclosureClick(event: ReactMouseEvent<HTMLDivElement>) {
    const target = event.target;
    const viewport = scrollRef.current;
    if (!(target instanceof Element) || !viewport) return;

    const disclosure = target.closest('summary, [data-disclosure-toggle]');
    if (!disclosure || !viewport.contains(disclosure)) return;

    const anchorTop = disclosure.getBoundingClientRect().top;
    followingRef.current = false;
    if (disclosureScrollFrameRef.current !== null) {
      window.cancelAnimationFrame(disclosureScrollFrameRef.current);
    }

    const preserveAnchor = () => {
      const activeViewport = scrollRef.current;
      if (!activeViewport || !disclosure.isConnected) return;
      const shift = disclosure.getBoundingClientRect().top - anchorTop;
      if (Math.abs(shift) > 0.5) activeViewport.scrollTop += shift;
    };

    disclosureScrollFrameRef.current = window.requestAnimationFrame(() => {
      preserveAnchor();
      disclosureScrollFrameRef.current = window.requestAnimationFrame(() => {
        disclosureScrollFrameRef.current = null;
        preserveAnchor();
        const activeViewport = scrollRef.current;
        if (!activeViewport) return;
        const distance =
          activeViewport.scrollHeight - activeViewport.scrollTop - activeViewport.clientHeight;
        const following = distance < 64;
        followingRef.current = following;
        if (showJumpRef.current !== !following) {
          showJumpRef.current = !following;
          setShowJump(!following);
        }
      });
    });
  }

  function handleScroll() {
    const viewport = scrollRef.current;
    if (!viewport) return;
    const distance = viewport.scrollHeight - viewport.scrollTop - viewport.clientHeight;
    const following = distance < 64;
    followingRef.current = following;
    if (showJumpRef.current !== !following) {
      showJumpRef.current = !following;
      setShowJump(!following);
    }
  }

  useEffect(() => {
    if (!running) return;
    const onKeyDown = (event: KeyboardEvent) => {
      if (event.key !== 'Escape') return;
      event.preventDefault();
      void cancel();
    };
    window.addEventListener('keydown', onKeyDown);
    return () => window.removeEventListener('keydown', onKeyDown);
  }, [running]);

  const slashText = prompt.trimStart();
  const slashOpen =
    !running &&
    !slashDismissed &&
    slashText.startsWith('/') &&
    !slashText.startsWith('//') &&
    !slashText.includes('\n');
  const slashQuery = slashText.toLowerCase();
  const slashMatches = slashOpen
    ? slashCommands.filter((command) => command.name.toLowerCase().startsWith(slashQuery)).slice(0, 8)
    : [];
  const selectedSlash = slashMatches[Math.min(slashIndex, Math.max(0, slashMatches.length - 1))];

  function completeSlash(command: SlashCommand) {
    const noArgs = new Set(['/help', '/sessions', '/clear']);
    setPrompt(command.name + (noArgs.has(command.name) ? '' : ' '));
    setSlashIndex(0);
    setSlashDismissed(noArgs.has(command.name));
    requestAnimationFrame(() => composerRef.current?.focus());
  }

  // `@` file-mention picker — the SAME fuzzy index the TUI composer uses,
  // served by GET /v1/sessions/:sid/suggest. The trigger smarts live here (never
  // the gateway), so a literal `@@` is never endangered.
  const caretPos = Math.min(caret, prompt.length);
  const fileMention = !running && !slashOpen ? fileMentionAt(prompt.slice(0, caretPos)) : null;
  const fileOpen = fileMention !== null && !fileDismissed;
  const fileQuery = fileMention?.query ?? '';
  const fileMatches = fileOpen ? fileSuggestions : [];
  const selectedFile = fileMatches[Math.min(fileIndex, Math.max(0, fileMatches.length - 1))];

  useEffect(() => {
    if (!fileOpen) {
      setFileSuggestions([]);
      return;
    }
    const controller = new AbortController();
    const timer = window.setTimeout(() => {
      void client
        .suggestFiles(sid, fileQuery, controller.signal)
        .then((rows) => setFileSuggestions(rows))
        .catch(() => {
          /* keep the last rows on a transient failure */
        });
    }, 90);
    return () => {
      controller.abort();
      window.clearTimeout(timer);
    };
  }, [client, sid, fileOpen, fileQuery]);

  function completeFile(path: string) {
    const spliced = applyFileMention(prompt, caretPos, path);
    setPrompt(spliced.text);
    setFileIndex(0);
    setFileDismissed(true);
    requestAnimationFrame(() => {
      const element = composerRef.current;
      if (!element) return;
      element.focus();
      element.setSelectionRange(spliced.caret, spliced.caret);
      setCaret(spliced.caret);
    });
  }

  const activePastes = Array.from(pastes.values()).filter((paste) => prompt.includes(paste.token));
  const title = session?.title?.trim() || 'Chat';
  const visibleStart = Math.max(0, turns.length - visibleTurnCount);
  const liveTurnId = liveTurn?.id;
  // While a live turn streams, drop the transcript's own copy of that same turn
  // (a running turn is persisted as a bare 'running' row) so it isn't rendered
  // twice — the live bubble owns it until `settle` confirms the finished row.
  const visibleTurns = turns
    .slice(visibleStart)
    .filter((turn) => {
      if (!liveTurn) return true;
      const id = turn.id ?? turn.turn_id;
      // Same turn by id — the live bubble owns it.
      if (liveTurnId && id === liveTurnId) return false;
      // The persisted 'running' row is the very turn being streamed live, even
      // when its id can't be matched (e.g. turn.started replayed without a
      // turn_id). Only one turn runs per session, so drop it to avoid a dup.
      if (turn.status === 'running') return false;
      return true;
    });
  const loadEarlierTurns = () => {
    const viewport = scrollRef.current;
    if (viewport) prependScrollHeightRef.current = viewport.scrollHeight;
    followingRef.current = false;
    setVisibleTurnCount((count) => Math.min(turns.length, count + INITIAL_VISIBLE_TURNS));
  };

  return (
    <section className="relative flex h-full min-h-0 flex-col overflow-hidden bg-ink transition-opacity duration-200 starting:opacity-0 motion-reduce:transition-none">
      <header className="z-10 flex min-h-12 shrink-0 items-stretch gap-0 border-b border-dialog-edge bg-panel-2 pt-[env(safe-area-inset-top)]">
        <button
          type="button"
          className="grid w-12 shrink-0 place-items-center border-r border-dialog-edge bg-dialog-title font-mono text-xl font-bold leading-none text-dialog-title-foreground transition-[background-color,transform] duration-150 active:scale-[0.96] hover:bg-accent-2 focus-visible:outline-none focus-visible:bg-accent-2 motion-reduce:transition-none sm:w-10"
          onClick={onBack}
          aria-label="Back to sessions"
        >
          <span aria-hidden="true">‹</span>
        </button>
        <div className="min-w-0 flex-1 self-center px-3">
          <h1 className="truncate font-mono text-xs font-bold text-white">{title}</h1>
          <div className="flex items-center gap-1.5 font-mono text-[10px] text-dialog-hint">
            <span
              className={`size-1.5 ${connected ? 'bg-ok' : 'animate-pulse bg-turn-edge motion-reduce:animate-none'}`}
            />
            {connected ? 'Gateway connected' : 'Reconnecting'}
          </div>
        </div>
        <div className="flex shrink-0 items-center gap-1.5 self-center pr-2 pl-1 sm:pr-3">
          <CopyableId id={sid} className="hidden max-w-[9rem] sm:inline-flex" />
          <ShareLink className="" />
        </div>
      </header>

      <div className="relative flex min-h-0 flex-1 flex-col">
      <div
        ref={scrollRef}
        className="min-h-0 flex-1 touch-pan-y overflow-x-hidden overflow-y-auto overscroll-contain scroll-pb-8 [overflow-anchor:none] [-webkit-overflow-scrolling:touch]"
        onClickCapture={handleDisclosureClick}
        onScroll={handleScroll}
        role="log"
      >
        <div
          ref={transcriptRef}
          className="mx-auto min-h-full w-full max-w-3xl px-[max(0.875rem,env(safe-area-inset-left))] pb-10 pr-[max(0.875rem,env(safe-area-inset-right))] pt-4 sm:px-6 sm:pt-6"
        >
          {error && <Banner kind="err">{error}</Banner>}

          <>
              {!turns.length && !liveTurn ? (
            <div className="flex min-h-[55vh] flex-col items-center justify-center text-center transition-[opacity,transform] duration-300 starting:translate-y-2 starting:opacity-0 motion-reduce:transition-none">
              <div className="grid size-11 place-items-center border border-dialog-edge bg-panel-2" aria-hidden="true">
                <img src="/vis-logo.png" alt="" className="h-5 w-6 object-contain" />
              </div>
              <h2 className="mb-1 mt-3 text-base font-semibold">Start a conversation</h2>
              <p className="max-w-sm text-xs leading-5 text-dialog-hint">
                This session is ready. Ask Vis to inspect, explain, or change your project.
              </p>
            </div>
              ) : null}

          {visibleStart > 0 && (
            <div className="mb-5 flex justify-center">
              <button
                type="button"
                className="border border-dialog-edge bg-panel px-3 py-1.5 font-mono text-[9px] font-bold text-dialog-hint transition-colors hover:border-accent hover:text-dialog-hint-key"
                onClick={loadEarlierTurns}
              >
                ↑ Load {Math.min(INITIAL_VISIBLE_TURNS, visibleStart)} earlier · {visibleStart} remaining
              </button>
            </div>
          )}

          {visibleTurns.map((turn, index) => {
            const request = turn.user_request ?? turn.request ?? '';
            return (
              <div
                className={`${index === 0 ? '' : 'mt-10'} [content-visibility:auto] [contain-intrinsic-size:auto_480px]`}
                key={turn.id ?? turn.turn_id}
              >
                {(request || (turn.attachments?.length ?? 0) > 0) && (
                  <UserMessage attachments={turn.attachments}>{request}</UserMessage>
                )}
                <AssistantMessage turn={turn} />
              </div>
            );
          })}

          {liveTurn && (
            <div className={turns.length ? 'mt-10' : ''} data-live="true">
              {liveTurn.request && <UserMessage>{liveTurn.request}</UserMessage>}
              <AssistantMessage
                turn={{
                  id: liveTurn.id ?? 'live',
                  request: liveTurn.request,
                  status: liveTurn.status,
                  iterations: liveTurn.iterations,
                  content: liveTurn.answer
                    ? [{ id: 'live-answer', type: 'prose', markdown: liveTurn.answer }]
                    : [],
                }}
                streaming={liveTurn.status === 'running'}
                activity={liveProgressPhase(liveTurn)}
                startedAt={liveTurn.startedAt}
              />
            </div>
          )}
          </>
        </div>
      </div>
        {loading && (
          <div className="absolute inset-0 z-10 flex items-center justify-center bg-ink transition-opacity duration-200">
            <LoadingSession />
          </div>
        )}
      </div>

      {showJump && (
        <button
          type="button"
          className="absolute bottom-24 left-1/2 z-20 -translate-x-1/2 border border-dialog-edge bg-button px-3 py-1.5 font-mono text-[10px] font-bold text-button-foreground shadow-[4px_4px_0_var(--dialog-shadow)] transition-[opacity,transform,background-color] duration-150 starting:translate-y-2 starting:opacity-0 active:scale-[0.97] motion-reduce:transition-none max-sm:bottom-24"
          onClick={() => scrollToEnd('smooth')}
        >
          ↓ Latest
        </button>
      )}

      <footer className="relative z-10 shrink-0 border-t border-dialog-edge bg-ink px-[max(0.5rem,env(safe-area-inset-left))] pb-[max(0.4rem,env(safe-area-inset-bottom))] pr-[max(0.5rem,env(safe-area-inset-right))] pt-1.5 sm:px-[max(1.5rem,calc((100%_-_46rem)/2))] sm:py-2">
        {fileMatches.length > 0 && (
          <div
            id="file-mention-list"
            role="listbox"
            aria-label="File mentions"
            className="absolute inset-x-2 bottom-full mb-1.5 max-h-[min(20rem,55dvh)] overflow-y-auto border border-dialog-edge bg-panel shadow-[6px_6px_0_var(--dialog-shadow)] transition-[opacity,transform] duration-150 starting:translate-y-2 starting:opacity-0 motion-reduce:transition-none sm:inset-x-[max(1.5rem,calc((100%_-_46rem)/2))] sm:shadow-[8px_8px_0_var(--dialog-shadow)]"
          >
            <div className="bg-dialog-title px-3 py-2 font-mono text-[10px] font-bold text-dialog-title-foreground">
              Attach a file
            </div>
            {fileMatches.map((file, index) => (
              <button
                key={file.name}
                type="button"
                role="option"
                aria-selected={index === fileIndex}
                className={`grid w-full grid-cols-[1fr_auto] items-center gap-3 border-t border-dialog-edge px-3 py-2 text-left transition-colors ${
                  index === fileIndex
                    ? 'bg-accent text-accent-foreground'
                    : 'text-dialog-foreground hover:bg-hover'
                }`}
                onPointerDown={(event) => event.preventDefault()}
                onClick={() => completeFile(file.name)}
              >
                <code className="truncate font-mono text-xs font-semibold text-accent-ink">
                  {file.name}
                </code>
                <span className="shrink-0 font-mono text-[10px] text-dialog-hint">
                  {[file.size, file.age, file.status && file.status !== 'clean' ? file.status : '']
                    .filter(Boolean)
                    .join(' · ')}
                </span>
              </button>
            ))}
          </div>
        )}

        {slashMatches.length > 0 && (
          <div
            id="slash-command-list"
            role="listbox"
            aria-label="Slash commands"
            className="absolute inset-x-2 bottom-full mb-1.5 max-h-[min(20rem,55dvh)] overflow-y-auto border border-dialog-edge bg-panel shadow-[6px_6px_0_var(--dialog-shadow)] transition-[opacity,transform] duration-150 starting:translate-y-2 starting:opacity-0 motion-reduce:transition-none sm:inset-x-[max(1.5rem,calc((100%_-_46rem)/2))] sm:shadow-[8px_8px_0_var(--dialog-shadow)]"
          >
            <div className="bg-dialog-title px-3 py-2 font-mono text-[10px] font-bold text-dialog-title-foreground">
              Slash commands
            </div>
            {slashMatches.map((command, index) => (
              <button
                key={command.name}
                type="button"
                role="option"
                aria-selected={index === slashIndex}
                className={`grid w-full grid-cols-[8.5rem_1fr] items-start gap-3 border-t border-dialog-edge px-3 py-2.5 text-left transition-colors sm:grid-cols-[11rem_1fr] ${
                  index === slashIndex
                    ? 'bg-accent text-accent-foreground'
                    : 'text-dialog-foreground hover:bg-hover'
                }`}
                onPointerDown={(event) => event.preventDefault()}
                onClick={() => completeSlash(command)}
              >
                <code className="break-words font-mono text-xs font-semibold text-accent-ink">
                  {command.name}
                </code>
                <span className="line-clamp-2 text-xs leading-5 text-dialog-hint">{command.doc}</span>
              </button>
            ))}
          </div>
        )}

        {queuePaused && (
          <div className="mb-1.5 flex flex-wrap items-center gap-x-2 gap-y-1 border border-warn-strong bg-warn-surface px-2.5 py-1.5 font-mono text-[10px] text-warn-strong">
            <span className="size-1.5 shrink-0 bg-warn-strong" aria-hidden="true" />
            <span className="font-bold text-warn-strong">
              {queuePaused.isBreakerOpen ? 'Provider unhealthy' : 'Queue paused'}
            </span>
            <span className="min-w-0 flex-1 truncate">
              {queuePaused.held} held · {queuePaused.reason.replace(/_/g, ' ')}
              {queuePaused.fails > 0 ? ` · ${queuePaused.fails} fail${queuePaused.fails > 1 ? 's' : ''}` : ''}
            </span>
            <button
              type="button"
              className="shrink-0 border border-warn-strong px-2 py-0.5 font-bold text-warn-strong transition-colors hover:bg-warn-strong hover:text-ink"
              onClick={() => { void client.resumeQueue(sid).catch(() => undefined); setQueuePaused(null); }}
            >
              Retry now
            </button>
          </div>
        )}

        {queued.length > 0 && (
          <div className="mb-1.5 border border-dialog-edge bg-panel">
            <div className="flex items-center gap-1.5 border-b border-dialog-edge bg-dialog-title px-2.5 py-1 font-mono text-[10px] font-bold text-dialog-title-foreground">
              <span aria-hidden="true">┌</span>
              Queued · {queued.length}
            </div>
            {queued.map((item, index) => {
              const editing = editingQueued?.turnId === item.turnId;
              return (
              <div
                key={item.turnId}
                className="flex items-center gap-2 border-t border-dialog-edge px-2.5 py-1 first:border-t-0 transition-[opacity,transform] duration-150 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none"
              >
                <span className="shrink-0 font-mono text-[10px] font-bold text-accent-ink">#{index + 1}</span>
                {editing ? (
                  <input
                    autoFocus
                    value={editingQueued.text}
                    onChange={(event) => setEditingQueued({ turnId: item.turnId, text: event.target.value })}
                    onKeyDown={(event) => {
                      if (event.key === 'Enter') {
                        event.preventDefault();
                        const text = editingQueued.text.trim();
                        if (text && text !== item.request) {
                          setQueued((current) =>
                            current.map((entry) => (entry.turnId === item.turnId ? { ...entry, request: text } : entry)));
                          void client.updateQueuedTurn(sid, item.turnId, text).catch(() => undefined);
                        }
                        setEditingQueued(null);
                      } else if (event.key === 'Escape') {
                        event.preventDefault();
                        setEditingQueued(null);
                      }
                    }}
                    onBlur={() => setEditingQueued(null)}
                    className="min-w-0 flex-1 border border-accent bg-input px-1 py-0.5 font-mono text-[11px] text-dialog-foreground outline-none"
                    aria-label={`Edit queued message ${index + 1}`}
                  />
                ) : (
                  <button
                    type="button"
                    onClick={() => setEditingQueued({ turnId: item.turnId, text: item.request })}
                    className="min-w-0 flex-1 truncate text-left font-mono text-[11px] text-dialog-foreground transition-colors hover:text-accent-ink"
                    title="Tap to edit"
                  >
                    {item.request || '(empty)'}
                  </button>
                )}
                <button
                  type="button"
                  className="grid size-6 shrink-0 place-items-center text-dialog-hint transition-colors hover:bg-warn-surface hover:text-err"
                  onClick={() => {
                    setEditingQueued((current) => (current?.turnId === item.turnId ? null : current));
                    setQueued((current) => current.filter((entry) => entry.turnId !== item.turnId));
                    void client.deleteQueuedTurn(sid, item.turnId).catch(() => undefined);
                  }}
                  aria-label={`Remove queued message ${index + 1}`}
                >
                  ×
                </button>
              </div>
              );
            })}
          </div>
        )}

        <div className="relative border border-dialog-edge bg-input shadow-[3px_3px_0_var(--dialog-shadow)] transition-colors focus-within:border-accent">
          {activePastes.length > 0 && (
            <div className="flex gap-1 overflow-x-auto border-b border-dialog-edge px-1.5 py-1 [scrollbar-width:thin]">
              {activePastes.map((paste) => (
                <span key={paste.id} className="inline-flex min-h-7 shrink-0 items-center border border-code-edge bg-code font-mono text-[9px] text-accent-ink">
                  <span className="max-w-56 truncate px-2">{paste.token}</span>
                  <button
                    type="button"
                    className="grid min-h-7 w-7 place-items-center border-l border-code-edge text-dialog-hint transition-colors hover:bg-warn-surface hover:text-err"
                    onClick={() => removePaste(paste.id)}
                    aria-label={`Remove pasted block ${paste.id}`}
                  >
                    ×
                  </button>
                </span>
              ))}
            </div>
          )}
          {attachments.length > 0 && (
            <div className="flex gap-1.5 overflow-x-auto border-b border-dialog-edge px-1.5 py-1.5 [scrollbar-width:thin]">
              {attachments.map((attachment) => (
                <div
                  key={attachment.id}
                  className="group relative flex min-w-0 max-w-40 shrink-0 items-center gap-1.5 border border-dialog-edge bg-panel pr-6 transition-[opacity,transform] duration-150 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none"
                >
                  <img
                    src={attachment.previewUrl}
                    alt=""
                    className="size-8 shrink-0 object-cover"
                  />
                  <span className="truncate font-mono text-[9px] text-dialog-hint-key">
                    {attachment.filename}
                  </span>
                  <button
                    type="button"
                    className="absolute inset-y-0 right-0 grid w-6 place-items-center text-xs text-dialog-hint transition-colors hover:bg-warn-surface hover:text-err"
                    onClick={() => removeAttachment(attachment.id)}
                    aria-label={`Remove ${attachment.filename}`}
                  >
                    ×
                  </button>
                </div>
              ))}
            </div>
          )}

          {(composerNotice || voicePhase !== 'idle' || (voiceRequested && voiceModel?.status !== 'ready')) && (
            <div className="pointer-events-none absolute bottom-full left-0 mb-1 flex max-w-full items-center gap-1.5 border border-dialog-edge bg-panel px-2 py-1 font-mono text-[9px] text-dialog-hint shadow-[3px_3px_0_var(--dialog-shadow)] transition-[opacity,transform] duration-150 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none">
              {voicePhase === 'recording' ? (
                <><span className="size-1.5 animate-pulse bg-err motion-reduce:animate-none" /> Listening · tap the microphone to finish</>
              ) : voicePhase === 'transcribing' ? (
                <><span className="size-1.5 animate-pulse bg-accent motion-reduce:animate-none" /> Transcribing on the gateway…</>
              ) : composerNotice ? composerNotice : voiceModel?.status === 'downloading' ? (
                <>Downloading voice model{voiceModel.progress == null ? '…' : ` · ${Math.round(voiceModel.progress)}%`}</>
              ) : voiceModel?.status === 'failed' ? (
                <>Voice model failed{voiceModel.error ? ` · ${voiceModel.error}` : ''}</>
              ) : voiceModel?.status === 'absent' ? (
                <>Tap the microphone to install the local voice model</>
              ) : null}
            </div>
          )}

          <div className="flex items-end gap-1 p-1">
            <input
              ref={fileInputRef}
              type="file"
              accept={(capabilities?.features.attachments.media_types ?? ['image/*']).join(',')}
              multiple
              className="hidden"
              onChange={(event) => void onFilesPicked(event.target.files)}
            />

            <button
              type="button"
              className="grid size-11 shrink-0 place-items-center text-dialog-hint transition-[background-color,color,transform] duration-150 hover:bg-hover hover:text-dialog-hint-key active:scale-[0.94] disabled:text-muted motion-reduce:transition-none sm:size-9"
              onClick={() => void addAttachments()}
              disabled={attachments.length >= (capabilities?.features.attachments.max_files ?? 8)}
              aria-label="Add images"
              title="Add images"
            >
              <svg viewBox="0 0 24 24" className="size-5" fill="none" stroke="currentColor" strokeWidth="1.8" aria-hidden="true">
                <path d="M12 5v14M5 12h14" strokeLinecap="square" />
              </svg>
            </button>

            {Capacitor.isNativePlatform() && (
              <button
                type="button"
                className="grid size-11 shrink-0 place-items-center text-dialog-hint transition-[background-color,color,transform] duration-150 hover:bg-hover hover:text-dialog-hint-key active:scale-[0.94] disabled:text-muted motion-reduce:transition-none sm:size-9"
                onClick={() => void captureCamera()}
                disabled={attachments.length >= (capabilities?.features.attachments.max_files ?? 8)}
                aria-label="Take photo"
                title="Take photo"
              >
                <svg viewBox="0 0 24 24" className="size-5" fill="none" stroke="currentColor" strokeWidth="1.8" aria-hidden="true">
                  <path d="M3 8h3l1.5-2h9L17 8h4v11H3z" strokeLinejoin="round" />
                  <circle cx="12" cy="13" r="3.2" />
                </svg>
              </button>
            )}

            <textarea
              ref={composerRef}
              rows={1}
              value={prompt}
              disabled={voicePhase === 'recording'}
              placeholder={voicePhase === 'recording' ? 'Listening…' : running ? 'Message Vis — queues behind the running turn' : 'Message Vis or type /'}
              aria-label="Message Vis"
              aria-controls={slashMatches.length ? 'slash-command-list' : undefined}
              aria-expanded={slashMatches.length > 0}
              className="h-11 min-h-11 max-h-28 min-w-0 flex-1 resize-none overflow-y-auto border-0 bg-transparent px-1.5 py-2.5 text-base leading-6 text-dialog-foreground outline-none placeholder:text-dialog-hint disabled:text-cancelled-foreground sm:h-9 sm:min-h-9 sm:py-2 sm:text-[12px] sm:leading-5"
              onPaste={handlePaste}
              onSelect={(event) =>
                setCaret((event.target as HTMLTextAreaElement).selectionStart ?? 0)
              }
              onChange={(event) => {
                setPrompt(event.target.value);
                setCaret(event.target.selectionStart ?? event.target.value.length);
                setSlashIndex(0);
                setSlashDismissed(false);
                setFileIndex(0);
                setFileDismissed(false);
              }}
              onKeyDown={(event) => {
                if (fileMatches.length) {
                  if (event.key === 'ArrowDown' || event.key === 'ArrowUp') {
                    event.preventDefault();
                    const delta = event.key === 'ArrowDown' ? 1 : -1;
                    setFileIndex(
                      (current) => (current + delta + fileMatches.length) % fileMatches.length,
                    );
                    return;
                  }
                  if ((event.key === 'Tab' || event.key === 'Enter') && selectedFile) {
                    event.preventDefault();
                    completeFile(selectedFile.name);
                    return;
                  }
                  if (event.key === 'Escape') {
                    event.preventDefault();
                    setFileDismissed(true);
                    return;
                  }
                }
                if (slashMatches.length && (event.key === 'ArrowDown' || event.key === 'ArrowUp')) {
                  event.preventDefault();
                  const delta = event.key === 'ArrowDown' ? 1 : -1;
                  setSlashIndex((current) => (current + delta + slashMatches.length) % slashMatches.length);
                  return;
                }
                if (slashMatches.length && event.key === 'Tab' && selectedSlash) {
                  event.preventDefault();
                  completeSlash(selectedSlash);
                  return;
                }
                if (slashMatches.length && event.key === 'Escape') {
                  event.preventDefault();
                  setSlashDismissed(true);
                  return;
                }
                if (event.key === 'Enter' && !event.shiftKey && !event.nativeEvent.isComposing) {
                  event.preventDefault();
                  if (selectedSlash && slashText.toLowerCase() !== selectedSlash.name.toLowerCase()) {
                    completeSlash(selectedSlash);
                  } else {
                    void send();
                  }
                }
              }}
            />

            {voiceSupported && !(prompt.trim() || attachments.length) && (
              <button
                type="button"
                className={`grid size-11 shrink-0 place-items-center transition-[background-color,color,transform] duration-150 active:scale-[0.94] disabled:text-muted motion-reduce:transition-none sm:size-9 ${
                  voicePhase === 'recording'
                    ? 'animate-pulse bg-warn-surface text-err motion-reduce:animate-none'
                    : 'text-dialog-hint hover:bg-hover hover:text-dialog-hint-key'
                }`}
                onClick={() => void toggleVoice()}
                disabled={voicePhase === 'transcribing' || voiceModel?.status === 'downloading'}
                aria-label={voicePhase === 'recording' ? 'Finish dictation' : 'Dictate message'}
                title={voicePhase === 'recording' ? 'Finish dictation' : 'Dictate message'}
              >
                <svg viewBox="0 0 24 24" className="size-5" fill="none" stroke="currentColor" strokeWidth="1.8" aria-hidden="true">
                  <rect x="9" y="3" width="6" height="11" rx="3" />
                  <path d="M5.5 11.5a6.5 6.5 0 0 0 13 0M12 18v3M8.5 21h7" strokeLinecap="square" />
                </svg>
              </button>
            )}
            {(!!(prompt.trim() || attachments.length) || (!running && !voiceSupported)) && (
              <button
                type="button"
                className="grid size-11 shrink-0 place-items-center border border-dialog-edge bg-dialog-title font-bold text-dialog-title-foreground transition-[background-color,color,transform] duration-150 hover:bg-accent-2 active:scale-[0.94] disabled:scale-100 disabled:bg-button disabled:text-dialog-hint motion-reduce:transition-none sm:size-9"
                onClick={send}
                disabled={(!prompt.trim() && !attachments.length) || voicePhase !== 'idle'}
                aria-label={running ? 'Queue message' : 'Send message'}
                title={running ? 'Queue behind the running turn' : 'Send'}
              >
                {running ? '+' : '↑'}
              </button>
            )}
            {running && (
              <button
                type="button"
                className="grid size-11 shrink-0 place-items-center border border-err bg-cancelled transition-[background-color,transform] duration-150 hover:bg-warn-surface active:scale-[0.94] motion-reduce:transition-none sm:size-9"
                onClick={cancel}
                aria-label="Stop response"
              >
                <span className="size-2.5 bg-err" />
              </button>
            )}
          </div>
        </div>
      </footer>
    </section>
  );
}
