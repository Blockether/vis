import {
  useCallback,
  useEffect,
  useLayoutEffect,
  useMemo,
  useRef,
  useState,
  type ClipboardEvent as ReactClipboardEvent,
  type MouseEvent as ReactMouseEvent,
} from 'react';
import { AssistantMessage, transcriptEnterClass, UserMessage } from '../components/ChatContent';
import { Banner } from '../components/ui';
import { ProviderRouterDialog } from './RouterScreen';
import { attachmentsFromFiles, type PendingAttachment } from '../lib/attachments';
import type { GatewayClient } from '../lib/gateway';
import { queuedTurnFromWire } from '../lib/gateway';
import { exactCost, formatCost, formatTokens, sessionUsage } from '../lib/usage';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import {
  collapsePastePlaceholders,
  createComposerPaste,
  expandPastePlaceholders,
  shouldCollapsePaste,
  type ComposerPaste,
} from '../lib/paste';
import {
  draftMessageKey,
  flushDraftMessages,
  peekDraftMessage,
  readDraftMessage,
  watchDraftMessageExits,
  writeDraftMessage,
} from '../lib/draft-messages';
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
  ModelPref,
  GatewayAttachment,
} from '../lib/types';
import { startWavRecording, type WavRecording } from '../lib/voice';
import { onWake } from '../lib/wake';
import {
  applyScrollAnchor,
  isViewportRotating,
  onViewportRotation,
  scrollAnchorFor,
  type ScrollAnchor,
} from '../lib/viewport';
import { answeredTurnCount, markSessionRead } from '../lib/unread';
import { shareableSessionLink } from '../lib/router';
import { Capacitor } from '@capacitor/core';

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
  // Bytes of the images this device just sent. The gateway's live rail carries
  // none (persisted rows own them), so the bubble would otherwise be text-only.
  attachments?: GatewayAttachment[];
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
    activity?.iteration == null ? 0 : activity.iteration,
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
    const startedId = stringField(event, 'turn_id');
    return {
      id: startedId,
      request: stringField(event, 'request'),
      answer: '',
      iterations: [],
      startedAt: typeof event.started_at === 'number' ? event.started_at : Date.now(),
      status: 'running',
      // `turn.started` for the turn we optimistically painted must not drop the
      // attachments we are already showing (the event has no bytes).
      attachments: turn && (!turn.id || turn.id === startedId) ? turn.attachments : undefined,
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
  { name: '/cd', doc: "Show or change the session's filesystem root (the directory Vis works in)." },
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

// Matches the veil's `duration-200`. Kept in JS because the veil has to stay
// MOUNTED for the length of its own fade-out (see the reveal effect below).
const VEIL_FADE_MS = 200;

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
      className="flex min-h-[55vh] items-center justify-center font-mono text-body text-white"
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
      className={`group inline-flex h-6 min-w-0 items-center gap-1 border border-dialog-edge px-2 font-mono text-chip transition-[background-color,color,border-color] hover:bg-hover ${copied ? 'border-ok text-ok' : 'text-dialog-hint'} ${className}`}
    >
      <span aria-hidden="true" className="opacity-50 transition-opacity group-hover:opacity-100">#</span>
      <span className="truncate">{copied ? 'Copied' : short}</span>
    </button>
  );
}

// Hands out a link to this session so another user can open it in their own
// paired app. Two surfaces, two truths:
//
//   * Web: `window.location.href` is a real https URL and the Web Share API
//     works, so offer the system share sheet.
//   * Capacitor (iOS/Android): the origin is `capacitor://localhost` — not an
//     openable URL — and `navigator.share` is NOT implemented by WKWebView: it
//     rejects with NotAllowedError (or silently no-ops), which the old code
//     swallowed, so the button did nothing at all. Native therefore copies the
//     registered `vis://s/<sid>?gw=<id>` deep link instead.
//
// Every path ends in visible feedback; failure is never silent.
async function copyText(text: string): Promise<boolean> {
  try {
    await navigator.clipboard.writeText(text);
    return true;
  } catch {
    // Untrusted webview / no permission — fall back to the legacy selection copy.
  }
  try {
    const area = document.createElement('textarea');
    area.value = text;
    area.setAttribute('readonly', '');
    area.className = 'fixed top-0 left-0 size-px opacity-0';
    document.body.appendChild(area);
    area.select();
    area.setSelectionRange(0, text.length);
    const ok = document.execCommand('copy');
    document.body.removeChild(area);
    return ok;
  } catch {
    return false;
  }
}

function ShareLink({ className }: { className: string }) {
  const [state, setState] = useState<'idle' | 'copied' | 'shared' | 'failed'>('idle');
  const flashRef = useRef<ReturnType<typeof setTimeout> | null>(null);
  function flash(next: 'copied' | 'shared' | 'failed') {
    setState(next);
    if (flashRef.current) clearTimeout(flashRef.current);
    flashRef.current = setTimeout(() => setState('idle'), 1_800);
  }
  useEffect(() => () => {
    if (flashRef.current) clearTimeout(flashRef.current);
  }, []);
  async function share() {
    const url = shareableSessionLink();
    if (!Capacitor.isNativePlatform() && typeof navigator.share === 'function') {
      try {
        await navigator.share({ title: 'Vis session', url });
        flash('shared');
        return;
      } catch (error) {
        // A dismissed sheet is not a failure — say nothing and leave it alone.
        if (error instanceof DOMException && error.name === 'AbortError') return;
        // Anything else (unsupported, blocked) falls through to the copy.
      }
    }
    flash((await copyText(url)) ? 'copied' : 'failed');
  }
  const label = state === 'copied' ? 'Copied' : state === 'shared' ? 'Shared' : state === 'failed' ? 'Failed' : 'Share';
  const done = state === 'copied' || state === 'shared';
  const tone = done
    ? 'border-ok bg-ok/15 text-ok'
    : state === 'failed'
      ? 'border-err bg-err/15 text-err'
      : 'border-dialog-title bg-dialog-title text-dialog-title-foreground hover:bg-accent-2';
  return (
    <button
      type="button"
      onClick={share}
      title="Share this session"
      aria-label="Share this session"
      className={`group inline-flex h-6 shrink-0 items-center gap-1 border px-2 font-mono text-chip font-bold uppercase tracking-[0.08em] transition-[background-color,color,border-color,transform,translate,scale,rotate] duration-150 active:scale-[0.97] motion-reduce:transition-none ${tone} ${className}`}
    >
      {done ? (
        <svg viewBox="0 0 20 20" className="size-3" fill="none" stroke="currentColor" strokeWidth="2" aria-hidden="true">
          <path d="M5 10.5l3.5 3.5L15 6.5" strokeLinecap="round" strokeLinejoin="round" />
        </svg>
      ) : (
        <svg viewBox="0 0 20 20" className="size-3" fill="none" stroke="currentColor" strokeWidth="1.6" aria-hidden="true">
          <path d="M7.5 10.5l5-3M7.5 9.5l5 3M6 10a2 2 0 11-4 0 2 2 0 014 0zM16 5a2 2 0 11-4 0 2 2 0 014 0zM16 15a2 2 0 11-4 0 2 2 0 014 0z" strokeLinecap="round" strokeLinejoin="round" />
        </svg>
      )}
      {/* All four labels live in the same grid cell, so the button is sized once by
         the widest of them and never resizes/jumps when the state flips. */}
      <span aria-live="polite" className="grid justify-items-center">
        <span aria-hidden="true" className="invisible col-start-1 row-start-1">Copied</span>
        <span className="col-start-1 row-start-1">{label}</span>
      </span>
    </button>
  );
}

export function SessionScreen({
  client,
  subscriptions,
  sid,
  onBack,
  onOpenSession,
  onManageProviders,
  fresh = false,
}: {
  client: GatewayClient;
  subscriptions: SessionSubscriptionHub;
  sid: string;
  onBack: () => void;
  onOpenSession: (sid: string, fresh?: boolean) => void;
  /** Open this gateway's settings, where provider accounts and OAuth live. */
  onManageProviders?: () => void;
  fresh?: boolean;
}) {
  // Every screen-level snapshot is seeded from the client's cache: reopening a
  // session paints its last known transcript on the FIRST frame and revalidates
  // underneath, instead of holding the loading sheet over an empty view.
  const [session, setSession] = useState<Session | null>(() => client.cachedSession(sid));
  const [turns, setTurns] = useState<TranscriptTurn[]>(() => client.cachedTranscript(sid) ?? []);
  // Whether the turns on screen were confirmed against the gateway during THIS
  // visit. Cached rows paint the first frame, but a cached 'running' row is a
  // placeholder with no outcome: rendered before confirmation it spins and
  // counts elapsed time for a turn that may already be cancelled or done.
  const [turnsFresh, setTurnsFresh] = useState(false);
  // The composer holds a DRAFT MESSAGE, not a keystroke buffer: unsent text
  // survives leaving for the session list, backgrounding, and killing the app.
  // (A "draft" in this system is an isolated agent workspace — different thing.)
  // The seed is synchronous off the in-memory mirror so a revisit paints what you
  // typed on the FIRST frame; the effect below covers the cold start, where
  // storage still has to be read.
  const draftMessageId = draftMessageKey(client.base, sid);
  const [prompt, setPrompt] = useState(() => peekDraftMessage(draftMessageId).text);
  const [draftMessageReady, setDraftMessageReady] = useState(false);
  const [error, setError] = useState<string | null>(null);
  // The session's provider/model pick. Read once from the gateway so the header
  // chip is right on open, then written through by the router dialog.
  const [modelPref, setModelPref] = useState<ModelPref | null>(null);
  // The gateway's default route, shown when this session pins nothing.
  const [defaultPref, setDefaultPref] = useState<ModelPref | null>(null);
  const [routerOpen, setRouterOpen] = useState(false);
  const [loading, setLoading] = useState(!fresh);
  // The veil outlives `loading` by one transition so it can dissolve.
  const [veiled, setVeiled] = useState(!fresh);
  const [connected, setConnected] = useState(false);
  const [running, setRunning] = useState(false);
  const [liveTurn, setLiveTurn] = useState<LiveTurn | null>(null);
  const [queued, setQueued] = useState<QueuedTurn[]>(() => client.cachedQueuedTurns(sid) ?? []);
  // Turn ids with a queue mutation in flight. The gateway is the ONE writer of the
  // queue tray (rows appear on `turn.queued` and leave on `.updated`/`.deleted`/
  // `.drained`), so an edit or removal is NOT applied optimistically — it is
  // marked busy until the daemon's own event lands. Mirroring the intent locally
  // is exactly how a row could disappear while the gateway still ran it.
  const [queueBusy, setQueueBusy] = useState<ReadonlySet<string>>(() => new Set());
  const [editingQueued, setEditingQueued] = useState<{ turnId: string; text: string } | null>(null);
  const [queuePaused, setQueuePaused] = useState<QueuePausedInfo | null>(null);
  // The pause banner is gateway state too: it clears on `queue.resumed`, never
  // because we asked. This only disables the button while the request is out.
  const [resumingQueue, setResumingQueue] = useState(false);
  const [showJump, setShowJump] = useState(false);
  const [visibleTurnCount, setVisibleTurnCount] = useState(INITIAL_VISIBLE_TURNS);
  // Reading IS being here. While this transcript is on screen its FINISHED turns
  // count as read, so an answer that lands while you watch never raises a badge
  // in the session list — but one that lands with the screen backgrounded does.
  // Both halves must count answers only: `turn_count` includes the turn that is
  // running right now (the gateway persists it at submit), and marking that as
  // read would pre-read the answer before it exists.
  const readTurns = Math.max(
    answeredTurnCount(session),
    turns.filter((turn) => turn.status !== 'running' && turn.status !== 'pending').length,
  );
  useEffect(() => {
    if (document.visibilityState !== 'hidden') markSessionRead(sid, readTurns);
    // Coming back to a screen that stayed mounted through a suspend is also a read.
    return onWake(() => markSessionRead(sid, readTurns));
  }, [sid, readTurns]);
  // Turns that exist on the gateway BEFORE the window we hold. The transcript is
  // fetched newest-page-first (a long session is tens of megabytes whole), so
  // "earlier" can mean rows we have but hide, or rows we have not read yet.
  const [earlierRemaining, setEarlierRemaining] = useState(
    () => client.transcriptWindow(sid).offset,
  );
  const [loadingEarlier, setLoadingEarlier] = useState(false);
  const [slashCommands, setSlashCommands] = useState<SlashCommand[]>(FALLBACK_SLASHES);
  const [slashIndex, setSlashIndex] = useState(0);
  const [slashDismissed, setSlashDismissed] = useState(false);
  const [caret, setCaret] = useState(0);
  const [fileSuggestions, setFileSuggestions] = useState<FileSuggestion[]>([]);
  const [fileIndex, setFileIndex] = useState(0);
  const [fileDismissed, setFileDismissed] = useState(false);
  const [capabilities, setCapabilities] = useState<GatewayCapabilities | null>(null);
  const [attachments, setAttachments] = useState<PendingAttachment[]>([]);
  const [pastes, setPastes] = useState<Map<number, ComposerPaste>>(
    () =>
      new Map(peekDraftMessage(draftMessageId).pastes.map((paste) => [paste.id, paste])),
  );
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
  const pasteCounterRef = useRef(peekDraftMessage(draftMessageId).counter);
  const resizeScrollFrameRef = useRef<number | null>(null);
  const disclosureScrollFrameRef = useRef<number | null>(null);
  const prependScrollHeightRef = useRef<number | null>(null);
  // Open/settle window: while it is in the future the transcript is still being
  // measured and every scroll inside it is ours, not the user's.
  const settleUntilRef = useRef(0);
  const settleTimersRef = useRef<number[]>([]);
  const followingRef = useRef(true);
  const showJumpRef = useRef(false);
  const liveTurnRef = useRef<LiveTurn | null>(null);
  // turn id -> the image bytes this device sent with it. A queued message drains
  // into a live turn minutes later, and the SSE rail never replays attachments,
  // so the sender keeps them until the persisted transcript row takes over.
  const sentAttachmentsRef = useRef<Map<string, GatewayAttachment[]>>(new Map());
  const rememberSent = (turnId: string | undefined, sent: GatewayAttachment[]) => {
    if (!turnId || !sent.length) return;
    const map = sentAttachmentsRef.current;
    map.set(turnId, sent);
    // Bounded: these are base64 pixels and only the newest turns can still be live.
    while (map.size > 8) {
      const oldest = map.keys().next();
      if (oldest.done) break;
      map.delete(oldest.value);
    }
  };
  const runningRef = useRef(false);
  const turnsRef = useRef<TranscriptTurn[]>([]);
  const cancelRef = useRef<() => void>(() => undefined);
  // Keep the loading overlay up until a freshly opened session has been
  // scrolled to its bottom, so persisted history never flashes at the top first.
  const initialScrollPendingRef = useRef(!fresh);
  // Mirror the latest render values for async callbacks. Written in an effect so
  // render itself stays pure.
  useEffect(() => {
    runningRef.current = running;
    turnsRef.current = turns;
    cancelRef.current = () => void cancel();
  });

  // Switching session identity resets this screen's whole view state. React's
  // alternative is remounting via `key`, which would also tear down the live SSE
  // subscription mid-stream, so the reset stays explicit here.
  useEffect(() => {
    void recordingRef.current?.cancel();
    recordingRef.current = null;
    setTurns(client.cachedTranscript(sid) ?? []);
    setTurnsFresh(false);
    setEarlierRemaining(client.transcriptWindow(sid).offset);
    setLoadingEarlier(false);
    setLiveTurn(null);
    sentAttachmentsRef.current.clear();
    setQueued(client.cachedQueuedTurns(sid) ?? []);
    setQueueBusy(new Set());
    setQueuePaused(null);
    liveTurnRef.current = null;
    setSession(client.cachedSession(sid));
    setAttachments([]);
    setPastes(new Map());
    pasteCounterRef.current = 0;
    setComposerNotice(null);
    setVoicePhase('idle');
    setVoiceRequested(false);
    setLoading(!fresh);
    setVeiled(!fresh);
    setVisibleTurnCount(INITIAL_VISIBLE_TURNS);
    followingRef.current = true;
    initialScrollPendingRef.current = !fresh;
    settleUntilRef.current = 0;
    showJumpRef.current = false;
    setRouterOpen(false);
    setModelPref(null);
  }, [sid, fresh]);

  // The header chip shows whatever model this session actually runs on, so read
  // the gateway's answer rather than assuming the global default.
  useEffect(() => {
    let live = true;
    void client
      .sessionModel(sid)
      .then((pref) => {
        if (live) setModelPref(pref);
      })
      .catch(() => {
        /* A missing pick is not an error worth interrupting the transcript for. */
      });
    return () => {
      live = false;
    };
  }, [client, sid]);

  // An unpinned session runs on the gateway default, so the chip names THAT
  // model rather than the placeholder word. Re-read when the picker closes: the
  // default may have just been changed from it.
  useEffect(() => {
    let live = true;
    void client
      .defaultModel()
      .then((pref) => {
        if (live) setDefaultPref(pref);
      })
      .catch(() => {
        /* Without a readable default the chip simply falls back to the pin. */
      });
    return () => {
      live = false;
    };
  }, [client, routerOpen]);

  const markQueueBusy = useCallback((turnId: string, busy: boolean) => {
    setQueueBusy((current) => {
      const next = new Set(current);
      if (busy) {
        next.add(turnId);
      } else {
        next.delete(turnId);
      }
      return next;
    });
  }, []);

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

  // Opening a session must LAND on the latest turn, and one scrollTo cannot do
  // that: the transcript keeps growing after the first paint (deferred Markdown,
  // fonts, images, code blocks). Worse, the scroll event produced by that first
  // scrollTo is delivered AFTER the growth, so `handleScroll` measured a large
  // distance-to-bottom and cleared `followingRef` — which then vetoed the
  // ResizeObserver's catch-up scroll and left the session parked mid-history.
  // Pin instead: re-scroll on a settle schedule and own every scroll event in
  // that window.
  const pinToEnd = useCallback(() => {
    settleTimersRef.current.forEach((id) => window.clearTimeout(id));
    settleTimersRef.current = [];
    settleUntilRef.current = Date.now() + 1200;
    scrollToEnd('auto');
    for (const delay of [60, 160, 320, 600, 1000]) {
      settleTimersRef.current.push(
        window.setTimeout(() => {
          if (Date.now() > settleUntilRef.current) return;
          scrollToEnd('auto');
        }, delay),
      );
    }
  }, [scrollToEnd]);

  // A real gesture ends the pin at once — never fight the finger.
  const releasePin = useCallback(() => {
    settleUntilRef.current = 0;
    settleTimersRef.current.forEach((id) => window.clearTimeout(id));
    settleTimersRef.current = [];
  }, []);

  // Rotation reflows the whole transcript: every wrapped line changes width, so
  // the pixel `scrollTop` you were reading at stops pointing at the same turn
  // and the reader lands somewhere else entirely. The browser's own scroll
  // anchoring cannot rescue it — this scroller runs `[overflow-anchor:none]` so
  // that loading earlier turns stays stable — so anchor it ourselves: remember
  // the turn at the top edge before the flip and put it back after, or simply
  // stay pinned to the bottom when that is where the reader was.
  const scrollAnchorRef = useRef<ScrollAnchor | null>(null);

  const captureScrollAnchor = useCallback(() => {
    const viewport = scrollRef.current;
    const transcript = transcriptRef.current;
    if (!viewport || !transcript) return;
    // Following the live turn needs no anchor: the bottom IS the anchor.
    scrollAnchorRef.current = followingRef.current
      ? null
      : scrollAnchorFor(viewport, transcript);
  }, []);

  const restoreScrollAnchor = useCallback(() => {
    const viewport = scrollRef.current;
    if (!viewport) return;
    if (applyScrollAnchor(viewport, scrollAnchorRef.current)) return;
    if (followingRef.current) scrollToEnd('auto');
  }, [scrollToEnd]);

  // The snapshot is taken when the orientation flips and replayed on every
  // settle, because the reflow arrives over several frames (composer autosize,
  // re-wrapped code blocks, images) rather than in one.
  useEffect(
    () =>
      onViewportRotation((phase) => {
        if (phase === 'start') captureScrollAnchor();
        else restoreScrollAnchor();
      }),
    [captureScrollAnchor, restoreScrollAnchor],
  );

  // Pass the session's meta `row` and the transcript is re-read ONLY when that
  // row says a turn was persisted since the copy already on screen — a long
  // session's transcript is tens of megabytes, so a blind re-read is the most
  // expensive thing this screen can do. Called with no row it always refetches.
  const loadTranscript = useCallback(
    async (row?: Session | null) => {
      try {
        const next =
          row === undefined
            ? await client.transcript(sid)
            : await client.transcriptIfMoved(sid, row);
        if (!next) {
          // Unchanged: the gateway just told us the cache IS its current answer.
          setTurnsFresh(true);
          // Unchanged: keep the painted turns, their object identities, and the
          // scroll position exactly as they are.
          if (!turnsRef.current.length) {
            initialScrollPendingRef.current = false;
            setLoading(false);
          }
          return turnsRef.current;
        }
        setTurns(next);
        setTurnsFresh(true);
        setEarlierRemaining(client.transcriptWindow(sid).offset);
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
    },
    [client, sid],
  );

  useEffect(() => {
    const controller = new AbortController();
    // Meta FIRST: it is a tiny payload whose stamp decides whether the transcript
    // has to be read back at all. Re-entering a session that has not moved then
    // costs one small request and no re-render, instead of refetching, reparsing
    // and re-rendering the whole history every time you walk back into it.
    void (async () => {
      let row: Session | null = null;
      try {
        row = await client.session(sid, controller.signal);
        setSession(row);
      } catch {
        /* Unreachable gateway: fall through, the transcript read reports it. */
      }
      if (controller.signal.aborted) return;
      await loadTranscript(row);
    })();
    // The queue tray paints ONLY gateway truth, and SSE carries just the
    // deltas that happen while we are subscribed — so read the existing
    // backlog on open. Without this, messages queued from the TUI (or
    // before a browser reload) are invisible here until they drain.
    void client
      .queuedTurns(sid, controller.signal)
      .then(setQueued)
      .catch(() => undefined);
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
    // A request still in flight when the OS suspends the webview never settles
    // and never rejects. A plain boolean latch would then block every later
    // tick for the life of the app — the "restart it before I see anything"
    // bug. Stamp the start instead and treat an older one as lost.
    const STALE_RECONCILE_MS = 20_000;
    let inflightSince: number | null = null;
    const reconcileOnce = async () => {
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
      // Safety net: on wake we ALWAYS refetch the transcript and check whether
      // the streamed live turn has already been persisted while we were
      // backgrounded. iOS/Android suspend fetch-body streams silently, so the
      // terminal event that would have cleared the live bubble may have been
      // dropped. If the persisted turn now exists, drop the live bubble; if
      // the gateway is idle but we still show work, do the same.
      const liveId = liveTurnRef.current?.id ?? '';
      let nextTurns: TranscriptTurn[] | null = null;
      try {
        // Gated on the row this tick just read: an idle session costs one tiny
        // request per tick instead of re-reading its whole transcript.
        nextTurns = await client.transcriptIfMoved(sid, next);
      } catch {
        nextTurns = null;
      }
      if (cancelled) return;
      if (nextTurns) {
        setTurns(nextTurns);
        setTurnsFresh(true);
      }
      // Same reconcile for the queue: a `turn.queued`/`.deleted` frame dropped
      // by a suspended stream would otherwise leave the tray lying until the
      // row drained. Gateway truth wins outright — we never merge in a local
      // guess.
      try {
        const backlog = await client.queuedTurns(sid);
        if (cancelled) return;
        setQueued(backlog);
      } catch {
        /* Keep the last known backlog; the next tick retries. */
      }
      const persisted =
        !!liveId &&
        !!nextTurns?.some((turn) => (turn.id ?? turn.turn_id) === liveId);
      const showsWork = liveTurnRef.current !== null || runningRef.current;
      if (persisted || (!gatewayLive && showsWork)) {
        setRunning(false);
        setLiveTurn(null);
        liveTurnRef.current = null;
      }
    };
    // Never stack: each reconcile is two sequential round-trips (session +
    // transcript), so on a slow gateway a fixed 5s tick would overlap and pile
    // requests up. One in flight at a time — a skipped tick self-heals 5s later.
    const reconcile = async () => {
      if (inflightSince !== null && Date.now() - inflightSince < STALE_RECONCILE_MS) return;
      inflightSince = Date.now();
      try {
        await reconcileOnce();
      } finally {
        inflightSince = null;
      }
    };
    const timer = window.setInterval(() => void reconcile(), 5000);
    // Every wake signal, DOM *and* native resume (a Capacitor iOS webview can
    // resume without firing a single DOM event). Force the multiplexed SSE
    // stream to reconnect so a silently frozen background socket does not sit
    // there forever, and drop a latch a suspended request left behind.
    const stopWake = onWake(() => {
      inflightSince = null;
      subscriptions.resync();
      void reconcile();
    });
    return () => {
      cancelled = true;
      window.clearInterval(timer);
      stopWake();
    };
  }, [client, sid, loadTranscript, subscriptions]);

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
    let inflight = false;
    const timer = window.setInterval(() => {
      // Same anti-stacking rule as the reconcile poll: one request in flight,
      // and nothing at all while the app is backgrounded.
      if (inflight || document.visibilityState === 'hidden') return;
      inflight = true;
      void client
        .voiceModel(sid)
        .then(setVoiceModel)
        .catch(() => undefined)
        .finally(() => {
          inflight = false;
        });
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
      // Refresh the meta row alongside the transcript: the transcript read stamps
      // itself with the freshest row it can see, so the next reconcile tick knows
      // this copy is current and skips its own read.
      void client
        .session(sid)
        .then(setSession)
        .catch(() => undefined);
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
                : [...current, queuedTurnFromWire(event as unknown as Record<string, unknown>)]);
            break;
          case 'turn.queued.updated':
            setQueued((current) =>
              current.map((item) =>
                item.turnId === tid
                  ? { ...item, ...queuedTurnFromWire(event as unknown as Record<string, unknown>) }
                  : item));
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
  }, [client, loadTranscript, sid, subscriptions]);

  useLayoutEffect(() => {
    const viewport = scrollRef.current;
    const previousHeight = prependScrollHeightRef.current;
    if (viewport && previousHeight !== null) {
      viewport.scrollTop += viewport.scrollHeight - previousHeight;
      prependScrollHeightRef.current = null;
      return;
    }
    if (initialScrollPendingRef.current && turns.length) {
      initialScrollPendingRef.current = false;
      pinToEnd();
      // Reveal one frame later, after the browser paints the bottom-pinned
      // transcript, so opening a session lands on the latest turn.
      requestAnimationFrame(() => setLoading(false));
      return;
    }
    if (followingRef.current) scrollToEnd('auto');
  }, [turns, visibleTurnCount, liveTurn?.id, scrollToEnd, pinToEnd]);

  // The veil must DISSOLVE, not vanish. Unmounting it the instant the transcript
  // is ready swaps a full-bleed `bg-ink` sheet for the whole transcript inside a
  // single frame — and reopening a *cached* session is exactly that worst case:
  // the turns are already painted, so the veil is only up for a frame or two and
  // its removal reads as a jump rather than a load. Holding it mounted at
  // `opacity-0` for one transition lets the transcript cross-fade in underneath.
  useEffect(() => {
    if (loading) {
      setVeiled(true);
      return;
    }
    if (!veiled) return;
    const timer = window.setTimeout(() => setVeiled(false), VEIL_FADE_MS);
    return () => window.clearTimeout(timer);
  }, [loading, veiled]);

  // Deferred Markdown, fonts, and content-visibility can change the transcript's
  // measured height after React commits. Keep a newly opened/followed session at
  // its actual bottom as those measurements settle.
  //
  // The SCROLLER itself must be observed too, not just its content. Focusing the
  // composer raises the keyboard, which shrinks the shell and therefore the
  // scroller's `clientHeight` while the transcript's own height never changes —
  // so a content-only observer stays silent, `scrollTop` is left where it was,
  // and the bottom of the conversation slides under the keyboard. That is the
  // "I tapped the input and got scrolled up" jump: nothing scrolled, the window
  // shrank around a reader who was pinned to the end.
  useEffect(() => {
    const transcript = transcriptRef.current;
    const viewport = scrollRef.current;
    if (!transcript || typeof ResizeObserver === 'undefined') return;

    const observer = new ResizeObserver(() => {
      if (!followingRef.current || resizeScrollFrameRef.current !== null) return;
      resizeScrollFrameRef.current = window.requestAnimationFrame(() => {
        resizeScrollFrameRef.current = null;
        if (followingRef.current) scrollToEnd('auto');
      });
    });
    observer.observe(transcript);
    if (viewport) observer.observe(viewport);

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
      settleTimersRef.current.forEach((id) => window.clearTimeout(id));
      settleTimersRef.current = [];
    };
  }, [scrollToEnd, sid]);

  // Autosize the composer WITHOUT thrashing layout. The naive pattern
  // (`height='auto'` then read `scrollHeight` on every keystroke) invalidates
  // the footer → section → chat scroller and forces a synchronous reflow of
  // the ENTIRE transcript per keypress — that was the typing lag. With the box
  // height left untouched, reading `scrollHeight` only lays out the textarea's
  // own content, so the common case (no height change) costs nothing upstream.
  const promptLengthRef = useRef(0);
  useEffect(() => {
    const textarea = composerRef.current;
    if (!textarea) return;
    const shrunk = prompt.length < promptLengthRef.current;
    promptLengthRef.current = prompt.length;
    const needed = Math.min(textarea.scrollHeight, 112);
    if (needed > textarea.clientHeight + 1) {
      // Content wrapped past the current box — grow (one cheap targeted write).
      textarea.style.height = `${needed}px`;
    } else if (shrunk && textarea.style.height) {
      // Text got shorter while grown: remeasure from natural height so the box
      // shrinks back. Only this rare path pays the full reset + reflow.
      textarea.style.height = 'auto';
      textarea.style.height = `${Math.min(textarea.scrollHeight, 112)}px`;
    }
  }, [prompt]);

  // Cold start: read the stored draft message and adopt it only while the
  // composer is still untouched, so typing that raced the read is never
  // overwritten. Until it resolves nothing is recorded — writing the empty
  // initial composer first would erase the very message we are about to restore.
  useEffect(() => {
    let cancelled = false;
    setDraftMessageReady(false);
    watchDraftMessageExits();
    void readDraftMessage(draftMessageId).then((message) => {
      if (cancelled) return;
      if (message.text) {
        setPrompt((current) => current || message.text);
        setPastes((current) =>
          current.size ? current : new Map(message.pastes.map((paste) => [paste.id, paste])),
        );
        pasteCounterRef.current = Math.max(pasteCounterRef.current, message.counter);
      }
      setDraftMessageReady(true);
    });
    // Leaving the screen is one of the moments the message must be on disk.
    return () => {
      cancelled = true;
      void flushDraftMessages();
    };
  }, [draftMessageId]);

  // Record every change. Sending clears the composer, which clears the message.
  useEffect(() => {
    if (!draftMessageReady) return;
    writeDraftMessage(draftMessageId, {
      text: prompt,
      pastes: pastes.values(),
      counter: pasteCounterRef.current,
    });
  }, [draftMessageReady, draftMessageId, prompt, pastes]);

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
        // A transcript that comes back empty is a REAL outcome (a muted or
        // hijacked mic records perfect silence), so it has to say so: dropping it
        // silently is what makes the button feel dead.
        if (text) setPrompt((current) => `${current.trimEnd()}${current.trim() ? ' ' : ''}${text}`);
        else setComposerNotice('No speech recognised — nothing was captured.');
        requestAnimationFrame(() => composerRef.current?.focus());
      } catch (cause) {
        setComposerNotice((cause as Error).message);
      } finally {
        setVoicePhase('idle');
      }
      return;
    }

    if (voiceModel?.status === 'downloading') {
      setComposerNotice('Voice model is still downloading — dictation starts when it lands.');
      return;
    }

    // The microphone is acquired FIRST, inside the click's own user-gesture
    // window. iOS/WKWebView only honours getUserMedia + AudioContext.resume()
    // there; probing the model first put a network round trip in between, and
    // the context then came up suspended — onaudioprocess never fired, the WAV
    // was silence, and the empty transcript looked like a dead button. That
    // round trip is slowest exactly while a turn streams, which is when this
    // was reported.
    let recording: WavRecording | null = null;
    try {
      recording = await startWavRecording();
      let model = voiceModel;
      if (model?.status !== 'ready') {
        model = await client.voiceModel(sid, true);
        setVoiceModel(model);
        if (model.status !== 'ready') {
          await recording.cancel();
          setComposerNotice(
            model.status === 'downloading'
              ? 'Downloading the voice model — dictation starts when it lands.'
              : model.status === 'failed'
                ? `Voice model failed${model.error ? ` · ${model.error}` : ''}`
                : 'Voice model is not ready yet.',
          );
          return;
        }
      }
      recordingRef.current = recording;
      setVoicePhase('recording');
    } catch (cause) {
      await recording?.cancel().catch(() => {});
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
        const sent: GatewayAttachment[] = pendingAttachments.map(
          ({ filename, media_type, base64 }) => ({ filename, media_type, base64 }),
        );
        const submitted = await client.submitTurn(sid, request, {
          displayRequest,
          attachments: sent,
        });
        rememberSent(submitted.turn_id ?? submitted.id, sent);
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
    const sent: GatewayAttachment[] = pendingAttachments.map(
      ({ filename, media_type, base64 }) => ({ filename, media_type, base64 }),
    );
    setLiveTurn({
      request: displayRequest,
      answer: '',
      iterations: [],
      startedAt: Date.now(),
      status: 'running',
      attachments: sent.length ? sent : undefined,
    });
    followingRef.current = true;
    requestAnimationFrame(() => scrollToEnd());

    try {
      const submitted = await client.submitTurn(sid, request, {
        displayRequest,
        attachments: sent,
      });
      const submittedId = submitted.turn_id ?? submitted.id;
      rememberSent(submittedId, sent);
      setLiveTurn((turn) => turn ? { ...turn, id: submittedId } : turn);
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
    // One stop is one stop. A second press (button re-tap, Escape) while the
    // request is in flight would re-announce a state the transcript is already
    // showing as "Vis is cancelling".
    if (liveTurnRef.current?.cancelling) return;
    setLiveTurn((turn) => {
      const next = turn ? { ...turn, cancelling: true, activity: undefined } : turn;
      liveTurnRef.current = next;
      return next;
    });
    try {
      await client.cancelCurrentTurn(sid);
    } catch (cause) {
      // The stop never landed, so the affordance has to come back.
      setLiveTurn((turn) => {
        const next = turn ? { ...turn, cancelling: false } : turn;
        liveTurnRef.current = next;
        return next;
      });
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

  // Tapping the composer must not move the conversation. The keyboard slides up
  // over ~300ms and the shell shrinks with it, so the scroller loses height in
  // several steps while the transcript keeps its own — the reader who was parked
  // at the end ends up looking at the middle. Worse, the first shrink can be
  // observed as a large distance-to-bottom and clear `followingRef`, which then
  // vetoes the catch-up. Re-pin instead, on the same settle schedule the session
  // opens with, but ONLY for a reader who was already at the bottom: someone
  // reading history and tapping reply keeps their place.
  function handleComposerFocus() {
    if (followingRef.current) pinToEnd();
  }

  function handleScroll() {
    const viewport = scrollRef.current;
    if (!viewport) return;
    // Inside the settle window the only scrolls are the ones we issued; a stale
    // event delivered after the transcript grew must not cancel following.
    if (Date.now() <= settleUntilRef.current) {
      followingRef.current = true;
      if (showJumpRef.current) {
        showJumpRef.current = false;
        setShowJump(false);
      }
      return;
    }
    const distance = viewport.scrollHeight - viewport.scrollTop - viewport.clientHeight;
    const following = distance < 64;
    followingRef.current = following;
    if (showJumpRef.current !== !following) {
      showJumpRef.current = !following;
      setShowJump(!following);
    }
    // Keep the rotation anchor fresh: iOS can deliver the orientation signal
    // AFTER the reflow, and by then the top-most turn is already unreadable.
    // Scrolls during a rotation are the reflow's own, never the reader's.
    if (!isViewportRotating()) captureScrollAnchor();
  }

  useEffect(() => {
    if (!running) return;
    const onKeyDown = (event: KeyboardEvent) => {
      if (event.key !== 'Escape') return;
      event.preventDefault();
      cancelRef.current();
    };
    window.addEventListener('keydown', onKeyDown);
    return () => window.removeEventListener('keydown', onKeyDown);
  }, [running]);

  const slashText = prompt.trimStart();
  const slashOpen =
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
  const fileMention = !slashOpen ? fileMentionAt(prompt.slice(0, caretPos)) : null;
  const fileOpen = fileMention !== null && !fileDismissed;
  const fileQuery = fileMention?.query ?? '';
  const fileMatches = fileOpen ? fileSuggestions : [];
  const selectedFile = fileMatches[Math.min(fileIndex, Math.max(0, fileMatches.length - 1))];

  useEffect(() => {
    // `fileMatches` above already renders nothing while the menu is closed, so
    // this effect never has to clear suggestion state.
    if (!fileOpen) return;
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
  // Cumulative session usage — the SAME fold the TUI footer runs over its message
  // vector (`footer/session-usage`), so both surfaces read one number. Memoized on
  // the transcript identity: it only moves when a turn lands, never per keystroke.
  const usage = useMemo(() => sessionUsage(turns), [turns]);
  const usageTokens = formatTokens(usage);
  const usageCost = formatCost(usage.cost);
  const usageTitle = [
    `${usage.input.toLocaleString()} input`,
    `${usage.output.toLocaleString()} output`,
    usage.cached > 0 ? `${usage.cached.toLocaleString()} cached` : null,
    usage.cost > 0 ? exactCost(usage.cost) : null,
    `${usage.turns} turn${usage.turns === 1 ? '' : 's'}`,
  ]
    .filter((part): part is string => Boolean(part))
    .join(' · ');
  const visibleStart = Math.max(0, turns.length - visibleTurnCount);
  // Everything older than the first bubble on screen, wherever it lives.
  const earlierTotal = visibleStart + earlierRemaining;
  const liveTurnId = liveTurn?.id;
  // While a live turn streams, drop the transcript's own copy of that same turn
  // (a running turn is persisted as a bare 'running' row) so it isn't rendered
  // twice — the live bubble owns it until `settle` confirms the finished row.
  const visibleTurns = useMemo(
    () =>
      turns.slice(visibleStart).filter((turn) => {
        // A persisted 'running' row is a placeholder, not a result. Painted from
        // the cache — reopening a session you already left — it resurrects the
        // working spinner and its elapsed clock for a turn that has since been
        // cancelled or finished, until the refetch lands seconds later. Only a
        // transcript confirmed against the gateway this visit may show one.
        if (turn.status === 'running' && !turnsFresh) return false;
        if (!liveTurn) return true;
        const id = turn.id ?? turn.turn_id;
        // Same turn by id — the live bubble owns it.
        if (liveTurnId && id === liveTurnId) return false;
        // The persisted 'running' row is the very turn being streamed live, even
        // when its id can't be matched (e.g. turn.started replayed without a
        // turn_id). Only one turn runs per session, so drop it to avoid a dup.
        if (turn.status === 'running') return false;
        return true;
      }),
    [turns, visibleStart, liveTurn, liveTurnId, turnsFresh],
  );
  // Memoized rows keep their element IDENTITY across composer keystrokes
  // (prompt/caret state), so React bails out of the whole transcript subtree
  // instead of re-reconciling every turn wrapper on each keypress — that
  // reconciliation was what made `/` and `@` completion typing lag.
  const turnRows = useMemo(
    () =>
      visibleTurns.map((turn, index) => {
        const request = turn.user_request ?? turn.request ?? '';
        // No content-visibility deferral: the DOM is already bounded to
        // INITIAL_VISIBLE_TURNS by pagination, and deferred turns rendered
        // white placeholder bands (plus a synchronous render hitch) when
        // fast-scrolling up into them on iOS — the 480px intrinsic-size guess
        // never matched the real height, so the scroll position shifted too.
        return (
          <div
            className={index === 0 ? '' : 'mt-10'}
            key={turn.id ?? turn.turn_id}
          >
            {(request || (turn.attachments?.length ?? 0) > 0) && (
              <UserMessage attachments={turn.attachments}>{request}</UserMessage>
            )}
            <AssistantMessage turn={turn} />
          </div>
        );
      }),
    [visibleTurns],
  );
  const liveRow = useMemo(
    () => {
      if (!liveTurn) return null;
      // A screenshot just sent lives only in this device's memory until the turn
      // is persisted: the live rail and the queue tray ship no attachment bytes.
      const liveAttachments = liveTurn.attachments
        ?? (liveTurn.id ? sentAttachmentsRef.current.get(liveTurn.id) : undefined);
      return (
        <div
          className={`${turns.length ? 'mt-10 ' : ''}${transcriptEnterClass}`}
          data-live="true"
        >
          {(liveTurn.request || (liveAttachments?.length ?? 0) > 0) && (
            <UserMessage attachments={liveAttachments}>{liveTurn.request}</UserMessage>
          )}
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
      );
    },
    [liveTurn, turns.length],
  );
  // Pin the viewport to the content it is already showing, so rows added ABOVE
  // do not shove it. The layout effect reads this height on the next paint.
  const anchorPrepend = () => {
    const viewport = scrollRef.current;
    if (viewport) prependScrollHeightRef.current = viewport.scrollHeight;
    followingRef.current = false;
  };

  // "Earlier" has two sources: rows already fetched but hidden by the render
  // window, and history still on the gateway. Reveal local rows first — that is
  // free — and only page the daemon once they run out.
  const loadEarlierTurns = () => {
    if (visibleStart > 0) {
      anchorPrepend();
      setVisibleTurnCount((count) => Math.min(turns.length, count + INITIAL_VISIBLE_TURNS));
      return;
    }
    if (loadingEarlier || earlierRemaining <= 0) return;
    setLoadingEarlier(true);
    const held = turns.length;
    void client
      .transcriptEarlier(sid)
      .then((older) => {
        if (!older) return;
        anchorPrepend();
        // Keep every bubble that was on screen, plus the page that just landed.
        setVisibleTurnCount((count) => count + Math.max(0, older.length - held));
        setTurns(older);
        setEarlierRemaining(client.transcriptWindow(sid).offset);
      })
      .catch((cause: unknown) => setError((cause as Error).message))
      .finally(() => setLoadingEarlier(false));
  };

  return (
    <section className="relative flex h-full min-h-0 flex-col overflow-hidden bg-ink transition-[opacity,transform,translate,scale,rotate] duration-200 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none">
      <header className="z-10 flex min-h-13 shrink-0 items-stretch gap-0 border-b border-dialog-edge bg-panel-2 pt-[env(safe-area-inset-top)]">
        <button
          type="button"
          className="grid w-11 shrink-0 place-items-center border-r border-dialog-edge bg-dialog-title font-mono text-subhead font-bold text-dialog-title-foreground transition-[background-color,transform,translate,scale,rotate] duration-150 active:scale-[0.96] hover:bg-accent-2 focus-visible:outline-none focus-visible:bg-accent-2 motion-reduce:transition-none sm:w-10"
          onClick={onBack}
          aria-label="Back to sessions"
        >
          <span aria-hidden="true">‹</span>
        </button>
        <div className="min-w-0 flex-1 self-center px-3 py-1.5">
          <h1 className="truncate font-mono text-body font-bold text-white">{title}</h1>
          <div className="flex items-center gap-1.5 font-mono text-meta text-dialog-hint">
            <span
              className={`size-1.5 ${connected ? 'bg-ok' : 'animate-pulse bg-turn-edge motion-reduce:animate-none'}`}
            />
            {connected ? 'Connected' : 'Reconnecting'}
          </div>
        </div>
        <div className="flex shrink-0 items-center gap-1 self-center pr-2 pl-1 sm:pr-3">
          <CopyableId id={sid} className="hidden max-w-[9rem] sm:inline-flex" />
          <ShareLink className="" />
        </div>
      </header>

      {routerOpen && (
        <ProviderRouterDialog
          client={client}
          sid={sid}
          onClose={() => setRouterOpen(false)}
          onPicked={setModelPref}
          onManageProviders={onManageProviders}
        />
      )}

      <div className="relative flex min-h-0 flex-1 flex-col">
      <div
        ref={scrollRef}
        className="min-h-0 flex-1 overflow-x-hidden overflow-y-auto overscroll-contain scroll-pb-8 bg-ink [overflow-anchor:none]"
        onClickCapture={handleDisclosureClick}
        onScroll={handleScroll}
        onPointerDown={releasePin}
        onWheel={releasePin}
        onTouchMove={releasePin}
        role="log"
      >
        <div
          ref={transcriptRef}
          className="mx-auto min-h-full w-full max-w-3xl px-[max(0.875rem,env(safe-area-inset-left))] pb-10 pr-[max(0.875rem,env(safe-area-inset-right))] pt-4 sm:px-6 sm:pt-6"
        >
          {error && <Banner kind="err">{error}</Banner>}

          <>
              {!turns.length && !liveTurn ? (
            <div className="flex min-h-[55vh] flex-col items-center justify-center text-center transition-[opacity,transform,translate,scale,rotate] duration-300 starting:translate-y-2 starting:opacity-0 motion-reduce:transition-none">
              <div className="grid size-9 place-items-center border border-dialog-edge bg-panel-2" aria-hidden="true">
                <img src="/vis-logo.png" alt="" className="h-5 w-6 object-contain" />
              </div>
              <h2 className="mb-1 mt-3 text-title font-semibold">Start a conversation</h2>
              <p className="max-w-sm text-body text-dialog-hint">
                This session is ready. Ask Vis to inspect, explain, or change your project.
              </p>
            </div>
              ) : null}

          {earlierTotal > 0 && (
            <div className="mb-5 flex justify-center">
              <button
                type="button"
                className="border border-dialog-edge bg-panel px-3 py-1.5 font-mono text-chip font-bold text-dialog-hint transition-colors hover:border-accent hover:text-dialog-hint-key"
                onClick={loadEarlierTurns}
                disabled={loadingEarlier}
              >
                {loadingEarlier
                  ? 'Loading earlier…'
                  : visibleStart > 0
                    ? `↑ Load ${Math.min(INITIAL_VISIBLE_TURNS, earlierTotal)} earlier · ${earlierTotal} remaining`
                    : `↑ Load earlier · ${earlierTotal} remaining`}
              </button>
            </div>
          )}

          {turnRows}

          {liveRow}
          </>
        </div>
      </div>
        {veiled && (
          <div
            aria-hidden={!loading}
            className={`absolute inset-0 z-10 flex items-center justify-center bg-ink transition-opacity duration-200 motion-reduce:transition-none ${
              loading ? 'opacity-100' : 'pointer-events-none opacity-0'
            }`}
          >
            <LoadingSession />
          </div>
        )}
      </div>

      <footer className="relative z-10 shrink-0 border-t border-dialog-edge bg-ink px-[max(0.875rem,env(safe-area-inset-left))] pb-[calc(0.5rem+var(--safe-bottom,env(safe-area-inset-bottom)))] pr-[max(0.875rem,env(safe-area-inset-right))] pt-1.5 sm:px-[max(1.5rem,calc((100%_-_46rem)/2))] sm:py-2">
        {/* Anchored to the footer's top edge, so it always clears the queue
            tray and composer no matter how tall they grow. Hidden while a
            completion list occupies the same strip. */}
        {showJump && !fileMatches.length && !slashMatches.length && (
          <button
            type="button"
            className="absolute bottom-full left-1/2 z-20 mb-2 -translate-x-1/2 border border-dialog-edge bg-button px-3 py-1.5 font-mono text-meta font-bold text-button-foreground shadow-[4px_4px_0_var(--dialog-shadow)] transition-[opacity,transform,translate,scale,rotate,background-color] duration-150 starting:translate-y-2 starting:opacity-0 active:scale-[0.97] motion-reduce:transition-none"
            onClick={() => scrollToEnd('smooth')}
          >
            ↓ Latest
          </button>
        )}

        {fileMatches.length > 0 && (
          <div
            id="file-mention-list"
            role="listbox"
            aria-label="File mentions"
            className="absolute inset-x-2 bottom-full mb-1.5 max-h-[min(20rem,55dvh)] overflow-y-auto border border-dialog-edge bg-panel shadow-[6px_6px_0_var(--dialog-shadow)] transition-[opacity,transform,translate,scale,rotate] duration-150 starting:translate-y-2 starting:opacity-0 motion-reduce:transition-none sm:inset-x-[max(1.5rem,calc((100%_-_46rem)/2))] sm:shadow-[8px_8px_0_var(--dialog-shadow)]"
          >
            <div className="bg-dialog-title px-3 py-2 font-mono text-meta font-bold text-dialog-title-foreground">
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
                <code className="truncate font-mono text-body font-semibold text-accent-ink">
                  {file.name}
                </code>
                <span className="shrink-0 font-mono text-meta text-dialog-hint">
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
            className="absolute inset-x-2 bottom-full mb-1.5 max-h-[min(20rem,55dvh)] overflow-y-auto border border-dialog-edge bg-panel shadow-[6px_6px_0_var(--dialog-shadow)] transition-[opacity,transform,translate,scale,rotate] duration-150 starting:translate-y-2 starting:opacity-0 motion-reduce:transition-none sm:inset-x-[max(1.5rem,calc((100%_-_46rem)/2))] sm:shadow-[8px_8px_0_var(--dialog-shadow)]"
          >
            <div className="bg-dialog-title px-3 py-2 font-mono text-meta font-bold text-dialog-title-foreground">
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
                <code className="break-words font-mono text-body font-semibold text-accent-ink">
                  {command.name}
                </code>
                <span className="line-clamp-2 text-body text-dialog-hint">{command.doc}</span>
              </button>
            ))}
          </div>
        )}

        {queuePaused && (
          <div className="mb-1.5 flex flex-wrap items-center gap-x-2 gap-y-1 border border-warn-strong bg-warn-surface px-2.5 py-1.5 font-mono text-meta text-warn-strong">
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
              disabled={resumingQueue}
              className="shrink-0 border border-warn-strong px-2 py-0.5 font-bold text-warn-strong transition-colors hover:bg-warn-strong hover:text-ink disabled:cursor-not-allowed disabled:opacity-50 disabled:hover:bg-transparent disabled:hover:text-warn-strong"
              onClick={() => {
                setResumingQueue(true);
                void client
                  .resumeQueue(sid)
                  .catch((cause) => setError((cause as Error).message))
                  .finally(() => setResumingQueue(false));
              }}
            >
              {resumingQueue ? 'Retrying…' : 'Retry now'}
            </button>
          </div>
        )}

        {queued.length > 0 && (
          <div className="mb-1.5 border border-dialog-edge bg-panel">
            <div className="flex items-center gap-1.5 border-b border-dialog-edge bg-dialog-title px-2.5 py-1 font-mono text-meta font-bold text-dialog-title-foreground">
              <span aria-hidden="true">┌</span>
              Queued · {queued.length}
            </div>
            {queued.map((item, index) => {
              const editing = editingQueued?.turnId === item.turnId;
              const busy = queueBusy.has(item.turnId);
              return (
              <div
                key={item.turnId}
                className={`flex items-center gap-2 border-t border-dialog-edge px-2.5 py-1 first:border-t-0 transition-[opacity,transform,translate,scale,rotate] duration-150 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none${busy ? ' opacity-50' : ''}`}
              >
                <span className="shrink-0 font-mono text-meta font-bold text-accent-ink">#{index + 1}</span>
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
                          // The gateway owns the row: it is rewritten here only when
                          // the daemon confirms with `turn.queued.updated`.
                          markQueueBusy(item.turnId, true);
                          void client.updateQueuedTurn(sid, item.turnId, text)
                            .catch((cause) => setError((cause as Error).message))
                            .finally(() => markQueueBusy(item.turnId, false));
                        }
                        setEditingQueued(null);
                      } else if (event.key === 'Escape') {
                        event.preventDefault();
                        setEditingQueued(null);
                      }
                    }}
                    onBlur={() => setEditingQueued(null)}
                    className="min-w-0 flex-1 border border-accent bg-input px-1 py-0.5 font-mono text-ui text-dialog-foreground outline-none"
                    aria-label={`Edit queued message ${index + 1}`}
                  />
                ) : (
                  <button
                    type="button"
                    disabled={busy}
                    onClick={() => setEditingQueued({ turnId: item.turnId, text: item.request })}
                    className="flex min-w-0 flex-1 items-center gap-1 text-left font-mono text-ui text-dialog-foreground transition-colors hover:text-accent-ink disabled:cursor-not-allowed"
                    title="Tap to edit"
                  >
                    {/* Image chips first: a queued screenshot reads as its filename,
                        never as the raw /var/folders path the OS pasted. */}
                    {item.attachments.map((attachment) => (
                      <span
                        key={attachment.filename}
                        className="inline-flex shrink-0 items-center gap-1 border border-dialog-edge bg-input px-1 text-chip text-dialog-hint"
                        title={`${attachment.filename}${attachment.sizeLabel ? ` · ${attachment.sizeLabel}` : ''}`}
                      >
                        <span className="max-w-[7rem] truncate">{attachment.filename}</span>
                      </span>
                    ))}
                    <span className="min-w-0 flex-1 truncate">
                      {item.preview || (item.attachments.length ? '' : '(empty)')}
                    </span>
                  </button>
                )}
                <button
                  type="button"
                  disabled={busy}
                  className="grid size-6 shrink-0 place-items-center text-dialog-hint transition-colors hover:bg-warn-surface hover:text-err disabled:cursor-not-allowed disabled:hover:bg-transparent disabled:hover:text-dialog-hint"
                  onClick={() => {
                    setEditingQueued((current) => (current?.turnId === item.turnId ? null : current));
                    // Removal is the gateway's to make: the row leaves the tray on
                    // `turn.queued.deleted`. A rejected delete (already started)
                    // therefore keeps showing the truth instead of hiding a turn
                    // that still runs.
                    markQueueBusy(item.turnId, true);
                    void client.deleteQueuedTurn(sid, item.turnId)
                      .catch((cause) => setError((cause as Error).message))
                      .finally(() => markQueueBusy(item.turnId, false));
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
            <div className="flex gap-1 overflow-x-auto overscroll-x-contain border-b border-dialog-edge px-1.5 py-1 [scrollbar-width:thin]">
              {activePastes.map((paste) => (
                <span key={paste.id} className="inline-flex min-h-7 shrink-0 items-center border border-code-edge bg-code font-mono text-chip text-accent-ink">
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
            <div className="flex gap-1.5 overflow-x-auto overscroll-x-contain border-b border-dialog-edge px-1.5 py-1.5 [scrollbar-width:thin]">
              {attachments.map((attachment) => (
                <div
                  key={attachment.id}
                  className="group relative flex min-w-0 max-w-40 shrink-0 items-center gap-1.5 border border-dialog-edge bg-panel pr-6 transition-[opacity,transform,translate,scale,rotate] duration-150 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none"
                >
                  <img
                    src={attachment.previewUrl}
                    alt=""
                    className="size-8 shrink-0 object-cover"
                  />
                  <span className="truncate font-mono text-chip text-dialog-hint-key">
                    {attachment.filename}
                  </span>
                  <button
                    type="button"
                    className="absolute inset-y-0 right-0 grid w-6 place-items-center text-body text-dialog-hint transition-colors hover:bg-warn-surface hover:text-err"
                    onClick={() => removeAttachment(attachment.id)}
                    aria-label={`Remove ${attachment.filename}`}
                  >
                    ×
                  </button>
                </div>
              ))}
            </div>
          )}

          {(composerNotice || voicePhase !== 'idle' || voiceModel?.status === 'downloading'
            || (voiceRequested && voiceModel?.status !== 'ready')) && (
            <div className="pointer-events-none absolute bottom-full left-0 mb-1 flex max-w-full items-center gap-1.5 border border-dialog-edge bg-panel px-2 py-1 font-mono text-chip text-dialog-hint shadow-[3px_3px_0_var(--dialog-shadow)] transition-[opacity,transform,translate,scale,rotate] duration-150 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none">
              {voicePhase === 'recording' ? (
                <><span className="size-1.5 animate-pulse bg-err motion-reduce:animate-none" /> Listening · tap the microphone to finish</>
              ) : voicePhase === 'transcribing' ? (
                <><span className="size-1.5 animate-pulse bg-accent motion-reduce:animate-none" /> Transcribing on your machine…</>
              ) : composerNotice ? composerNotice : voiceModel?.status === 'downloading' ? (
                <>{voiceModel.phase === 'extracting' ? 'Unpacking voice model' : 'Downloading voice model'}{voiceModel.progress == null ? '…' : ` · ${Math.round(voiceModel.progress)}%`}</>
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
              className="grid h-8 w-7 shrink-0 place-items-center text-dialog-hint transition-[background-color,color,transform,translate,scale,rotate] duration-150 hover:bg-hover hover:text-dialog-hint-key active:scale-[0.94] disabled:text-muted motion-reduce:transition-none sm:h-7 sm:w-6"
              onClick={() => void addAttachments()}
              disabled={attachments.length >= (capabilities?.features.attachments.max_files ?? 8)}
              aria-label="Add images"
              title="Add images"
            >
              <svg viewBox="0 0 24 24" className="size-3.5" fill="none" stroke="currentColor" strokeWidth="1.8" aria-hidden="true">
                <path d="M12 5v14M5 12h14" strokeLinecap="square" />
              </svg>
            </button>

            {voiceSupported && (
              <button
                type="button"
                className={`grid h-8 w-7 shrink-0 place-items-center transition-[background-color,color,transform,translate,scale,rotate] duration-150 active:scale-[0.94] disabled:text-muted motion-reduce:transition-none sm:h-7 sm:w-6 ${
                  voicePhase === 'recording'
                    ? 'animate-pulse bg-warn-surface text-err motion-reduce:animate-none'
                    : 'text-dialog-hint hover:bg-hover hover:text-dialog-hint-key'
                }`}
                onClick={() => void toggleVoice()}
                disabled={voicePhase === 'transcribing' || voiceModel?.status === 'downloading'}
                aria-label={voicePhase === 'recording' ? 'Finish dictation' : 'Dictate message'}
                title={voicePhase === 'recording' ? 'Finish dictation' : 'Dictate message'}
              >
                <svg viewBox="0 0 24 24" className="size-3.5" fill="none" stroke="currentColor" strokeWidth="1.8" aria-hidden="true">
                  <rect x="9" y="3" width="6" height="11" rx="3" />
                  <path d="M5.5 11.5a6.5 6.5 0 0 0 13 0M12 18v3M8.5 21h7" strokeLinecap="square" />
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
              className="h-8 min-h-8 max-h-20 min-w-0 flex-1 resize-none overflow-y-auto border-0 bg-transparent px-1 py-2 text-ui text-dialog-foreground outline-none placeholder:text-dialog-hint disabled:text-cancelled-foreground sm:h-7 sm:min-h-7 sm:py-[0.4375rem] sm:text-meta"
              onPaste={handlePaste}
              onFocus={handleComposerFocus}
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

            {(!running || !!(prompt.trim() || attachments.length)) && (
              <button
                type="button"
                className="grid size-8 shrink-0 place-items-center border border-dialog-edge bg-dialog-title text-ui font-bold text-dialog-title-foreground transition-[background-color,color,transform,translate,scale,rotate] duration-150 hover:bg-accent-2 active:scale-[0.94] disabled:scale-100 disabled:bg-button disabled:text-dialog-hint motion-reduce:transition-none sm:size-7"
                onClick={send}
                disabled={(!prompt.trim() && !attachments.length) || voicePhase !== 'idle'}
                aria-label={running ? 'Queue message' : 'Send message'}
                title={running ? 'Queue behind the running turn' : 'Send'}
              >
                {'↑'}
              </button>
            )}
            {/* The stop affordance retires the moment the cancel is accepted: the
                live bubble then carries the single "Vis is cancelling" line, and
                the finished turn carries "Cancelled by user." — one state at a
                time, never a button offering to cancel a cancel. */}
            {running && !liveTurn?.cancelling && (
              <button
                type="button"
                className="grid size-8 shrink-0 place-items-center border border-err bg-cancelled transition-[background-color,transform,translate,scale,rotate] duration-150 hover:bg-warn-surface active:scale-[0.94] motion-reduce:transition-none sm:size-7"
                onClick={cancel}
                aria-label="Stop response"
              >
                <span className="size-1.5 bg-err" />
              </button>
            )}
          </div>
        </div>

        {/* Composer strip, in the TUI footer's own reading order: the router chip
            sits LEFT directly under the input, cumulative session usage (tokens,
            then cost) rides the RIGHT edge. The chip truncates first so the
            numbers survive a narrow phone. */}
        <div className="flex w-full items-center gap-2 pt-1">
          <button
            type="button"
            className="group inline-flex min-w-0 shrink items-center gap-1.5 px-1 py-1 font-mono text-chip font-bold uppercase tracking-[0.09em] text-dialog-hint transition-colors duration-150 hover:text-accent-ink focus-visible:text-accent-ink focus-visible:outline-none motion-reduce:transition-none"
            onClick={() => setRouterOpen(true)}
            aria-label="Change provider and model"
            title={
              modelPref?.model ?? defaultPref?.model
                ? `${modelPref?.provider ?? defaultPref?.provider ?? ''}/${modelPref?.model ?? defaultPref?.model ?? ''}`
                : 'Change provider and model'
            }
          >
            <span aria-hidden="true" className="text-accent-ink/80 transition-colors duration-150 group-hover:text-accent-ink motion-reduce:transition-none">◇</span>
            <span className="truncate">{modelPref?.model ?? defaultPref?.model ?? 'model'}</span>
            <span aria-hidden="true" className="opacity-40 transition-opacity duration-150 group-hover:opacity-100 motion-reduce:transition-none">▾</span>
          </button>

          {(usageTokens || usageCost) && (
            <span
              className="ml-auto flex shrink-0 items-center gap-1.5 px-1 py-1 font-mono text-chip tabular-nums text-dialog-hint"
              title={`Session usage — ${usageTitle}`}
            >
              {usageTokens && <span className="whitespace-nowrap">{usageTokens}</span>}
              {usageTokens && usageCost && (
                <span aria-hidden="true" className="opacity-40">·</span>
              )}
              {usageCost && <span className="whitespace-nowrap text-accent-ink">{usageCost}</span>}
            </span>
          )}
        </div>
      </footer>
    </section>
  );
}
