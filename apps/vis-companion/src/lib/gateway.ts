// Typed client for the vis gateway HTTP/SSE API. This is the companion's twin
// of src/com/blockether/vis/internal/gateway/client.clj — the SAME daemon the
// TUI and other channels drive, reached over LAN / Tailscale / cloudflared.
//
// Auth: any non-loopback (or --require-token) gateway demands a bearer token;
// we send it on every request. A 401 surfaces as GatewayError so the UI can
// prompt a re-pair.

import { activityProjectionFromWire, type ActivityProjection } from './activity';
import gatewaySchema from '../../../../packages/vis-contract/resources/vis-contract/schema/gateway.json';
import { unreadTurnCount } from './unread';
import type { CouncilRoom, RoomInvitation, RoomMember, RoomsStatus } from './rooms';
import type { PushGateway } from './relay';
import {
  ATTACHMENT_MEMORY_BUDGET,
  cacheVictims,
  readCachedAttachment,
  writeCachedAttachment,
} from './attachment-cache';
import {
  clearDraftMessage,
  dirtySessionIds,
  draftMessageKey,
  flushDraftMessages,
  hydrateDraftMessages,
} from './draft-messages';
import { approxBytes, registerMemoryOwner, registerMemorySource, type MemoryCell } from './perf';
import { flattenSettings, replaceSetting } from './setting-tree';
import { randomUuid } from './uuid';
import type {
  ArchiveView,
  AuthFlow,
  AuthVerdict,
  BandWindow,
  ModelPref,
  QueuedAttachment,
  QueuedTurn,
  QueuedTurnDeliver,
  QueuePausedInfo,
  ProviderLimits,
  ProviderResetOutcome,
  ProviderPreset,
  ProviderStatus,
  RouterProvider,
  GatewayAttachment,
  GatewayCapabilities,
  GatewayHealth,
  GatewayOverview,
  GatewayConn,
  FileSuggestion,
  FileWindow,
  GatewayStatus,
  IterationAttachment,
  SessionArtifactRow,
  Session,
  SessionGoal,
  SessionGroup,
  SessionGroupPage,
  Project,
  SessionUsage,
  Subagent,
  SettingsResponse,
  SettingsTarget,
  SlashCommand,
  SseEvent,
  SubmittedTurn,
  Toggle,
  TranscriptIteration,
  TranscriptTurn,
  ForkPoint,
  PushDevice,
  PushDeviceInput,
  PushStatus,
  SpeechJob,
  SpeechVoice,
  SpeechVoices,
  VoiceJob,
  VoiceModelState,
  VoiceProgress,
  VoiceTranscript,
  McpAuthFlow,
  McpAuthStatus,
  McpServer,
  McpServerInput,
  McpServersResponse,
  McpTestResult,
  BrowseEntry,
  BrowseListing,
  SessionAlert,
  ExtensionReload,
} from './types';
import { PROTOCOL_HEADERS } from './compat';
import { withSavedAttachment } from './artifacts';
import { inputViewsFromWire, type HumanInputRequest } from './human-input';
import type { ViewAction, ViewActionOutcome } from './view';
import { liveViewsFromWire, type LiveLogPage, type LiveView } from './live-view';
import type {
  ImproveCreate,
  ImprovePage,
  ImproveRecord,
  ImproveSettings,
  ImproveUpdate,
} from './improve';
import type {
  Automation,
  AutomationInput,
  AutomationList,
  AutomationPatch,
  AutomationRun,
  AutomationRunList,
  AutomationSecret,
  AutomationSecretKind,
} from './automations';
import {
  flushSnapshots,
  hydrateSnapshots,
  installSnapshotFlushOnHide,
  scheduleSnapshotFlush,
  type SnapshotStores,
} from './snapshot-store';
import {
  startGatewayRequestDiagnostic,
  type GatewayRequestDiagnostic,
  type GatewayRequestDiagnosticFinish,
  type GatewayRequestDiagnosticStart,
  type GatewayStreamReason,
} from './diagnostics';

export class GatewayError extends Error {
  status: number;
  body: unknown;
  constructor(status: number, message: string, body?: unknown) {
    super(message);
    this.name = 'GatewayError';
    this.status = status;
    this.body = body;
  }
}

/** Local, non-secret refusals that are safe to display in the sign-in UI. */
export class GatewayOAuthError extends GatewayError {
  readonly reason: 'pairing-required' | 'invalid-address';
  constructor(reason: GatewayOAuthError['reason']) {
    super(
      0,
      {
        'pairing-required': 'Pair this gateway before starting sign-in.',
        'invalid-address': 'Use an HTTP or HTTPS gateway address for sign-in.',
      }[reason],
    );
    this.reason = reason;
  }
}

/** HTTP status a gateway refuses an unsupported client protocol with. */
export const INCOMPATIBLE_STATUS = 426;

let incompatibleListener: ((error: GatewayError) => void) | null = null;

/**
 * Watch for a gateway REFUSING this build's wire protocol.
 *
 * The compatibility verdict is normally read once, from `/healthz` at connect
 * time — but a gateway can raise its floor while this app is already running
 * (someone updates Vis on the machine and bounces the daemon). From that moment
 * every ordinary call answers 426, and without this the app would keep painting
 * them as unrelated request failures instead of the one screen that explains
 * which half is behind and how to fix it.
 *
 * Module-level on purpose: clients are constructed per connection, all over the
 * app, and a 426 from any of them says the same thing about this build.
 */
export function onGatewayIncompatible(listener: (error: GatewayError) => void): () => void {
  incompatibleListener = listener;
  return () => {
    if (incompatibleListener === listener) incompatibleListener = null;
  };
}

const turnSubmissionListeners = new Set<(sid: string) => void>();

/**
 * Hear each message that a gateway accepted from this device.
 *
 * The gateway ranks a session by its newest message as soon as it accepts it. A list
 * behind the open session hears this and reads its new order before the reader goes
 * back to it. Module-level on purpose: the composer and the list each hold their own
 * client.
 */
export function onTurnSubmitted(listener: (sid: string) => void): () => void {
  turnSubmissionListeners.add(listener);
  return () => {
    turnSubmissionListeners.delete(listener);
  };
}

// One transcript-search hit inside a session: which SIDE it landed on (the
// user's own request, the assistant's answer, or the reasoning aside it thought
// out loud), a short preview snippet, and when it happened. Several travel per
// session, best band first.
export interface SessionMatchHit {
  side: 'request' | 'reply' | 'thinking';
  snippet: string;
  at: number | null;
}

// Where ONE row of a search answer matched, plus up to a handful of preview
// snippets. Only those small windows travel — never the conversation.
// `requestSnippet`/`replySnippet` are the first hit of each side, kept for
// callers that want a single line.
//
// `rank` is the gateway's own relevance band — 0 the session's TITLE, 1 the
// user's own words, 2 the assistant's answer, 3 its thinking. It says WHERE the
// query hit; where the row sits is the answer's own freshest-first order.
export interface SessionMatch {
  sessionId: string;
  rank: number;
  inTitle: boolean;
  inRequest: boolean;
  inReply: boolean;
  inThinking: boolean;
  requestSnippet: string | null;
  replySnippet: string | null;
  hits: SessionMatchHit[];
}

interface RawSessionMatch {
  rank?: number;
  is_in_title?: boolean;
  is_in_request?: boolean;
  is_in_reply?: boolean;
  is_in_thinking?: boolean;
  request_snippet?: string | null;
  reply_snippet?: string | null;
  hits?: { side?: string; snippet?: string | null; at?: number | null }[];
}

/**
 * One answer of THE session search (`GET /v1/sessions/actions/search`), the one
 * every client asks. A blank query answers the RECENTS, a query the sessions whose
 * title or transcript matched it. `sessions` are `GET /v1/sessions` rows in the
 * gateway's own freshest-first order; `matches` says where each matched row hit,
 * in the same order, and recents carry none. `nextCursor` continues the answer.
 */
export interface SessionSearch {
  sessions: Session[];
  matches: SessionMatch[];
  total: number;
  nextCursor: string | null;
  hasMore: boolean;
}

/** A search row's `match`, in the shape the app paints. */
function sessionMatch(sessionId: string, m: RawSessionMatch): SessionMatch {
  return {
    sessionId,
    rank: Number(m.rank ?? 0),
    inTitle: Boolean(m.is_in_title),
    inRequest: Boolean(m.is_in_request),
    inReply: Boolean(m.is_in_reply),
    inThinking: Boolean(m.is_in_thinking),
    requestSnippet: m.request_snippet ?? null,
    replySnippet: m.reply_snippet ?? null,
    hits: (m.hits ?? [])
      .filter((h) => Boolean(h.snippet?.trim()))
      .map((h) => ({
        side:
          h.side === 'request'
            ? ('request' as const)
            : h.side === 'thinking'
              ? ('thinking' as const)
              : ('reply' as const),
        snippet: h.snippet as string,
        at: h.at ?? null,
      })),
  };
}

/**
 * How long a `/v1/router` payload stays good. Assembling it costs the daemon a
 * live auth + limits probe per provider, so five minutes of reuse turns "open
 * the model picker" from a multi-second wait into an instant paint.
 */
export const ROUTER_TTL_MS = 5 * 60 * 1000;

/**
 * How long a machine's settings payloads stay warm after a sweep.
 *
 * Toggles, MCP servers, capabilities and the device list are read for EVERY
 * paired machine on launch, on wake and whenever the paired list changes. They
 * change about once a week, so without a stamp the sweep would re-ask four
 * questions per machine every time the app came back to the front.
 */
const PANEL_TTL_MS = 5 * 60 * 1000;

/**
 * Hard deadline for ONE gateway request, body included.
 *
 * A suspended iOS/Android webview does not FAIL its in-flight HTTP. The OS
 * freezes the socket and, after the resume, the promise neither resolves nor
 * rejects — ever. Every screen that awaits one then waits forever: the session
 * transcript is the visible casualty, because its loading veil is gated on
 * exactly this call, so a phone that comes back after a few minutes away sits
 * on a spinner that only a force-quit clears. No layer below `fetch` reports
 * that, so the bound lives here. Long enough that a slow phone link never trips
 * it, short enough that a dead one self-heals without the user restarting.
 *
 * The event stream has its own, longer watchdog (a live SSE body is *meant* to
 * stay open); every endpoint reached through `request` answers immediately —
 * `auth/poll` included, which the daemon documents as non-blocking.
 */
const REQUEST_TIMEOUT_MS = 30_000;

/**
 * Bounds for ONE event-stream attempt.
 *
 * A live SSE body is *meant* to stay open, so the body phase gets the long
 * heartbeat-based bound. The CONNECT phase is the dangerous one and used to
 * have no bound at all: a webview resumed onto a frozen keep-alive socket
 * issues the request and never hears back — no headers, no error — so the body
 * watchdog was never armed, the reconnect sat there forever, and the header
 * stayed "connecting" until something else in the app forced a fresh attempt.
 */
const SSE_CONNECT_TIMEOUT_MS = 10_000;
const SSE_STALL_TIMEOUT_MS = 45_000;

/**
 * How long a candidate ADDRESS gets to prove it is still there.
 *
 * `/healthz` is answered before the gateway looks at anything, so an address
 * that cannot produce it in a few seconds is not one the app should move onto.
 * Spending the full request budget on each silent candidate is what made an
 * address sweep cost half a minute per dead address, on every wake.
 */
const PROBE_TIMEOUT_MS = 5_000;

/**
 * How many ordinary requests may be in flight to ONE gateway at once.
 *
 * A webview gives an HTTP/1.1 origin about six sockets, and this app holds two
 * of them open for the live session and fleet streams. A resumed screen fires
 * its whole poll set at once, so those bursts took every socket and the stream
 * reopening behind them never got its headers inside `SSE_CONNECT_TIMEOUT_MS`:
 * the app painted `Reconnecting` while the gateway was healthy and answering
 * every poll. Diagnostics from one phone show the aborted stream opens sitting
 * at a median of thirteen concurrent requests, against six for the ones that
 * connected. So polls queue behind a cap that leaves the streams their sockets.
 *
 * Live streams do NOT pass through the gate — they are the traffic it protects.
 */
const MAX_INFLIGHT_PER_GATEWAY = 4;

/** Requests on the wire per gateway base URL, and who is waiting for a slot. */
type GatewaySlots = { active: number; waiting: Array<() => void> };
const inflight = new Map<string, GatewaySlots>();

function slotsOf(base: string): GatewaySlots {
  let gate = inflight.get(base);
  if (!gate) {
    gate = { active: 0, waiting: [] };
    inflight.set(base, gate);
  }
  return gate;
}

/**
 * Give the slot back — handing it straight to the next waiter, so the cap holds
 * without a gap for a newcomer to slip through. Calling it twice is harmless.
 */
function releaseGatewaySlot(gate: GatewaySlots): () => void {
  let isReleased = false;
  return () => {
    if (isReleased) return;
    isReleased = true;
    const next = gate.waiting.shift();
    if (next) next();
    else gate.active -= 1;
  };
}

/**
 * Take a slot on `base` if this gateway has one free RIGHT NOW, synchronously:
 * a request that nothing is holding up must reach the wire in the same tick it
 * was asked for, the way a tap on `Retry` expects.
 */
function takeGatewaySlot(base: string): (() => void) | null {
  const gate = slotsOf(base);
  if (gate.active >= MAX_INFLIGHT_PER_GATEWAY) return null;
  gate.active += 1;
  return releaseGatewaySlot(gate);
}

/**
 * Wait for this gateway's next free slot, then take it. An `urgent` request is one
 * the reader is waiting on, such as the transcript of the session being opened: it
 * goes to the FRONT of the queue instead of behind the polls the same navigation
 * fired. It still counts against the cap, so the streams keep their sockets.
 */
async function awaitGatewaySlot(base: string, urgent = false): Promise<() => void> {
  const gate = slotsOf(base);
  await new Promise<void>((resume) => {
    if (urgent) gate.waiting.unshift(resume);
    else gate.waiting.push(resume);
  });
  return releaseGatewaySlot(gate);
}

/**
 * Deadline for ONE transcription round trip, SCALED to the audio it carries.
 *
 * `transcribeVoice` bypasses `request` (it posts raw WAV bytes, not JSON), so it
 * used to be the one call in the client with no bound at all — and the most
 * exposed one, because it is issued exactly when a dictation ends, which on a
 * phone is often the moment the screen locks. A frozen socket then left
 * `voicePhase` pinned at `transcribing` forever: the mic button disabled, the
 * send button disabled, the audio trapped in a promise that never settles.
 *
 * The bound cannot be a flat 30s: transcription is real work on the daemon and
 * a 15-minute dictation legitimately takes minutes. So it tracks the payload —
 * 16 kHz mono Int16 is 32 kB per second of speech (src/lib/voice.ts).
 */
const VOICE_TIMEOUT_FLOOR_MS = 60_000;
const VOICE_BYTES_PER_SECOND = 32_000;
const VOICE_TIMEOUT_PER_SECOND_MS = 500;

function voiceTimeoutMs(bytes: number): number {
  return (
    VOICE_TIMEOUT_FLOOR_MS + Math.ceil(bytes / VOICE_BYTES_PER_SECOND) * VOICE_TIMEOUT_PER_SECOND_MS
  );
}

/**
 * A job whose stream says nothing for this long is dead, not slow: the gateway
 * pushes a frame per phase and per percentage and a heartbeat comment between
 * them, so a live engine always reaches us.
 */
const VOICE_STALL_TIMEOUT_MS = 120_000;

/**
 * A synthesis job is polled, not streamed: the client wants the AUDIO and nothing in
 * between, so it asks the job resource where it is rather than opening a second SSE
 * connection for a progress bar nobody renders.
 */
const SPEECH_JOB_POLL_MS = 400;
const SPEECH_JOB_TIMEOUT_MS = 120_000;

/**
 * SSE `event:` name of every frame on a transcription job's stream.
 *
 * Mirror of `com.blockether.vis.contract.gateway/voice-job-event`
 * and of `features.voice.progress_event` in `GET /v1/capabilities`; a
 * cross-channel test pins the two spellings together. It exists because this
 * client reads TWO unrelated SSE resources: a session's ordered event LOG (`id:`
 * cursor, engine event types, replayed from that cursor, open for the session's
 * life) and ONE transcription job's state (no cursor, no replay, this single
 * name, ends on the terminal frame). Keying off the name is what keeps a job
 * frame out of the session reducer, and a session event out of the progress
 * notice.
 */
export const VOICE_JOB_EVENT = 'voice.job';

/**
 * Read an SSE body and hand every `data:` payload to `onData`, together with
 * its frame's `event:` name (null when the frame named none).
 *
 * Both live streams in this client parse frames by hand, because a native
 * `EventSource` cannot carry the bearer header in a Capacitor webview, so the
 * framing is written once, here. The event NAME is part of that framing: a
 * caller that dropped it would have to guess a frame's meaning from the shape of
 * its JSON, and would accept anything that merely looked like its own payload.
 * `onChunk` fires on every read, heartbeat comments included, which is exactly
 * what a stall watchdog has to count.
 */
async function readSseFrames(
  body: ReadableStream<Uint8Array>,
  onData: (json: string, event: string | null) => void,
  onChunk?: () => void,
  signal?: AbortSignal,
  onHeartbeat?: () => void,
): Promise<void> {
  const reader = body.getReader();
  const decoder = new TextDecoder();
  let buffer = '';
  for (;;) {
    const { value, done } = await reader.read();
    // A reader retired during a network handoff can wake after its replacement.
    // Its bytes belong to the old attempt and must never enter the current cursor.
    if (signal?.aborted) break;
    if (done) break;
    if (value?.byteLength) onChunk?.();
    buffer += decoder.decode(value, { stream: true }).replace(/\r\n/g, '\n');
    let boundary: number;
    while ((boundary = buffer.indexOf('\n\n')) >= 0) {
      const frame = buffer.slice(0, boundary);
      buffer = buffer.slice(boundary + 2);
      const lines = frame.split('\n').map((line) => line.trimStart());
      if (lines.some((line) => line.startsWith(':'))) onHeartbeat?.();
      const named = lines.find((line) => line.startsWith('event:'));
      const event = named ? named.slice(6).trim() || null : null;
      for (const line of lines) {
        if (!line.startsWith('data:')) continue;
        const json = line.slice(5).trim();
        if (json) onData(json, event);
      }
    }
  }
}

/** Track stream timing without keeping event data or heartbeat text. */
function trackSseActivity() {
  let phase: 'connect' | 'read' = 'connect';
  let lastByteAt: number | null = null;
  let lastHeartbeatAt: number | null = null;
  return {
    opened() {
      phase = 'read';
    },
    chunk() {
      lastByteAt = Date.now();
    },
    heartbeat() {
      lastHeartbeatAt = Date.now();
    },
    snapshot() {
      const now = Date.now();
      return {
        stream_phase: phase,
        last_byte_age_ms: lastByteAt === null ? null : Math.max(0, now - lastByteAt),
        last_heartbeat_age_ms: lastHeartbeatAt === null ? null : Math.max(0, now - lastHeartbeatAt),
      };
    },
    retryReason(status: number, stalled: boolean, closed: boolean): GatewayStreamReason {
      if (stalled) return phase === 'connect' ? 'connect_timeout' : 'stream_stall';
      if (closed) return 'eof';
      return status >= 400 ? 'http_error' : 'network_error';
    },
  };
}

/** The gateway's canonical `error.message`, or the bare status when it sent none. */
function errorText(parsed: unknown, status: number): string {
  const message = (parsed as { error?: { message?: unknown } } | undefined)?.error?.message;
  return typeof message === 'string' && message ? message : `HTTP ${status}`;
}

/** Add the engine selector every voice/speech endpoint shares. */
function withEngine(path: string, engine?: string | null): string {
  if (!engine) return path;
  return `${path}${path.includes('?') ? '&' : '?'}engine=${encodeURIComponent(engine)}`;
}

/** Router rows per gateway base URL, shared by every screen and client instance. */
const routerCache = new Map<string, { at: number; rows: RouterProvider[] }>();
const providerLimitsListeners = new Map<
  string,
  Set<(providerId: string, limits: ProviderLimits) => void>
>();
/** Setting writes per gateway base URL, so other mounted screens read the new value. */
const settingListeners = new Map<string, Set<(updated: Toggle) => void>>();

/**
 * Hear every setting write on the gateway at `base`. A row in one screen can change
 * what another screen shows: Settings turns off simplified thinking modes, and the
 * open session's reasoning chip must list the exact levels at once.
 */
export function onSettingChange(base: string, receive: (updated: Toggle) => void): () => void {
  let listeners = settingListeners.get(base);
  if (!listeners) {
    listeners = new Set();
    settingListeners.set(base, listeners);
  }
  listeners.add(receive);
  return () => {
    listeners.delete(receive);
    if (!listeners.size) settingListeners.delete(base);
  };
}
// A retry on another screen/client still represents the same account operation.
const providerResetInflight = new Map<string, Promise<ProviderResetOutcome>>();
/** In-flight router reads per base URL, so concurrent opens cost one request. */
const routerInflight = new Map<string, Promise<RouterProvider[]>>();

/** When each gateway's settings panels were last warmed, per base URL. */
const panelWarmed = new Map<string, number>();

/** When each gateway last answered its stable feature negotiation payload. */
const capabilityReads = new Map<string, number>();

/** One shared capabilities read per gateway, regardless of how many screens mount. */
type CapabilityFlight = {
  controller: AbortController;
  promise: Promise<GatewayCapabilities>;
};
const capabilityFlights = new Map<string, CapabilityFlight>();

/**
 * Last-known payload per gateway+resource, kept for the tab's lifetime so a
 * screen that REMOUNTS — switching tabs, backing out of a session, reopening
 * one — paints its previous frame instead of flashing an empty skeleton.
 * Volatile resources revalidate against their own validator; stable machine
 * facts such as capabilities and devices have an explicit freshness window.
 */
const snapshots = new Map<string, unknown>();

/**
 * One machine's push facts: who it will wake, and whether it can wake anyone.
 */
export interface DevicesState {
  devices: PushDevice[];
  push: PushStatus;
}

/**
 * How long ONE read of a machine's device list answers for every caller.
 *
 * Reported as: every paired machine is hit with four or five requests before a
 * single row is painted. Three of them were this one question asked by three
 * callers — the launch sweep, push registration asking whether the machine can
 * sign for this device at all, and the notifications panel the moment it opens.
 * So it is asked once and shared. A minute is far shorter than anything that
 * can change the answer: only this app puts this device on that list or takes
 * it off, and both of those invalidate the window here.
 */
const DEVICES_FRESH_MS = 60_000;

/** When each gateway's device list was last read, keyed like its snapshot. */
const deviceReads = new Map<string, number>();

/** A device-list read already in the air, so overlapping callers share it. */
const deviceFlights = new Map<string, Promise<DevicesState>>();
/** Background transcript reads already in flight, shared by every client instance. */
const transcriptPrefetches = new Map<string, Promise<boolean>>();
/** Shared reads finish into the cache even when one reader leaves. */
const transcriptReads = new Map<string, Promise<TranscriptTurn[]>>();

/** References from unread rows near the viewport, shared across client instances. */
const retainedTranscripts = new Map<string, number>();

/** Last active-list row a background read completed for, so polls stay cheap. */
const transcriptPrefetchStamps = new Map<string, string>();

/** Session lists that already answered in this run. Only their changes hold a list back. */
const listedSessions = new Set<string>();

/**
 * Freshness stamp of the transcript snapshot we hold, per gateway+session. A
 * long session's transcript is TENS OF MEGABYTES; refetching it on a timer, or
 * on every re-entry, is by far the most expensive thing this client can do. The
 * transcript only moves when a turn is persisted, and that always bumps the meta
 * row — so this string turns a whole-transcript revalidation into a comparison
 * against the tiny `/v1/sessions/:id` payload we already fetch.
 */
const transcriptStamps = new Map<string, string>();

/** Turns per windowed transcript fetch — the page the UI pulls and pushes. */
export const TRANSCRIPT_PAGE = 24;

/** One windowed transcript response. */
export interface TranscriptPage {
  turns: TranscriptTurn[];
  /** Turns in the session, not in this page. */
  total: number;
  /** 0-based start of this window in the oldest-first list. */
  offset: number;
  /** Older turns exist before this window. */
  hasMore: boolean;
}

/** How much of a session's history a client currently holds. */
export interface TranscriptWindow {
  offset: number;
  total: number;
}

/**
 * Oldest row held per transcript snapshot, so "load earlier" knows where to
 * continue and the UI can say how much history is still on the gateway.
 */
const transcriptWindows = new Map<string, TranscriptWindow>();

/**
 * Screens waiting to hear that an artifact in one session grew a NEW VERSION,
 * keyed by that session's transcript snapshot.
 *
 * A revision is the one transcript change no revalidation can see: it is
 * appended to an iteration that already exists, so the turn count and the meta
 * row are exactly where they were and `transcriptIfMoved` correctly decides
 * nothing moved. The client therefore says so itself, at the moment it files the
 * descriptor — which is why the sheet shows the new cut, and the comments on it,
 * without refetching tens of megabytes of transcript.
 */
const revisionWatchers = new Map<string, Set<(turns: TranscriptTurn[]) => void>>();

function transcriptStamp(row: Session | null | undefined): string {
  if (!row) return '';
  return `${row.turn_count}\u0000${row.modified_at ?? ''}`;
}

function sessionIsActive(row: Session): boolean {
  return row.live;
}

function sessionTurnCount(row: Session): number {
  const count = Number(row.turn_count ?? 0);
  return Number.isFinite(count) && count > 0 ? count : 0;
}

/** Did this row finish an answer after the reader saw its `previous` version? */
function sessionSettled(previous: Session | undefined, next: Session): boolean {
  return (
    !!previous &&
    !sessionIsActive(next) &&
    (sessionIsActive(previous) || sessionTurnCount(next) > sessionTurnCount(previous))
  );
}

/** Does this row show a finished answer that the reader has not opened yet? */
function sessionHasNewAnswer(previous: Session | undefined, next: Session): boolean {
  return !sessionIsActive(next) && (unreadTurnCount(next) > 0 || sessionSettled(previous, next));
}

function transcriptPrefetchStamp(row: Session): string {
  return [transcriptStamp(row), row.live, row.status ?? '', row.current_turn_id ?? ''].join(
    '\u0000',
  );
}

/** Normal transcript budget per machine. Visible unread rows remain protected until they leave the viewport. */
export const SESSION_CACHE_LIMIT = 10;

/**
 * Only resource kinds whose cardinality follows session count need an explicit
 * bound. Machine-wide facts are one row per gateway and must never be evicted by
 * a burst of model or transcript writes.
 */
const SNAPSHOT_KIND_LIMITS = new Map<string, number>([
  ['session', SESSION_CACHE_LIMIT],
  ['transcript', SESSION_CACHE_LIMIT],
  ['queued', SESSION_CACHE_LIMIT],
  ['live', SESSION_CACHE_LIMIT],
  // Streaming bubbles are memory-only but can retain whole generated answers.
  ['running-turn', SESSION_CACHE_LIMIT],
  // The head list seeds model pins for up to one complete session-list window.
  ['model', 100],
]);

function snapshotParts(key: string): { base: string; kind: string } {
  const first = key.indexOf('\u0000');
  if (first < 0) return { base: '', kind: key };
  const second = key.indexOf('\u0000', first + 1);
  return {
    base: key.slice(0, first),
    kind: key.slice(first + 1, second < 0 ? key.length : second),
  };
}

function dropSnapshot(key: string): void {
  snapshots.delete(key);
  if (snapshotParts(key).kind !== 'transcript') return;
  transcriptStamps.delete(key);
  transcriptWindows.delete(key);
  transcriptPrefetchStamps.delete(key);
}

/** Keep visible answers before older LRU entries. Only visible rows can exceed the normal budget. */
function trimSnapshotKind(key: string): void {
  const target = snapshotParts(key);
  const limit = SNAPSHOT_KIND_LIMITS.get(target.kind);
  if (limit === undefined) return;
  const matching = Array.from(snapshots.keys()).filter((candidate) => {
    const parts = snapshotParts(candidate);
    return parts.base === target.base && parts.kind === target.kind;
  });
  const expendable = matching.filter((candidate) =>
    !retainedTranscripts.has(candidate.replace('\u0000session\u0000', '\u0000transcript\u0000')),
  );
  for (const oldest of expendable.slice(0, Math.max(0, matching.length - limit))) {
    dropSnapshot(oldest);
  }
}

function normalizeSnapshotLimits(): void {
  for (const key of Array.from(snapshots.keys())) trimSnapshotKind(key);
}
/**
 * The snapshot caches as ONE durable unit (see `snapshot-store.ts`).
 *
 * Hydrated at module load, before any screen can read a cache: the OS kills a
 * backgrounded webview routinely, so "reopening the app" is normally a COLD
 * start, and without this every session re-downloaded its transcript over the
 * phone's network before it could paint a single row. With it, the last known
 * rows are on the first frame and the meta row's stamp decides whether anything
 * has to be fetched at all.
 */
const snapshotStores: SnapshotStores = {
  snapshots,
  stamps: transcriptStamps,
  windows: transcriptWindows,
};
hydrateSnapshots(snapshotStores);
normalizeSnapshotLimits();
installSnapshotFlushOnHide(snapshotStores);

/** Snapshot kinds whose key ends in a session id: the memory heatmap's rows. */
const SESSION_SNAPSHOT_KINDS = new Set([
  'goal',
  'live',
  'model',
  'queued',
  'running-turn',
  'session',
  'transcript',
]);

registerMemorySource('gateway snapshots', function* (): Iterable<MemoryCell> {
  const titles = new Map<string, string>();
  for (const [key, value] of snapshots) {
    const [, kind, sid] = key.split('\u0000');
    const title = kind === 'session' ? (value as Session | null)?.title : undefined;
    if (sid && title) titles.set(sid, title);
  }
  for (const [key, value] of snapshots) {
    const [, kind = key, sid] = key.split('\u0000');
    const session = SESSION_SNAPSHOT_KINDS.has(kind) ? sid : undefined;
    yield {
      source: kind,
      session,
      title: session ? titles.get(session) : undefined,
      bytes: approxBytes(value),
      entries: Array.isArray(value) ? value.length : 1,
    };
  }
  for (const [key, watchers] of revisionWatchers) {
    yield { source: 'revision watchers', session: key.split('\u0000')[2], bytes: 0, entries: watchers.size };
  }
});

// ── Replay cursors, one per gateway and session ─────────────────────
//
// WHERE THIS DEVICE'S EVENT STREAM GOT TO, kept across launches.
//
// A cursor used to live only in the subscription hub's in-memory Map, so a cold
// start had none and asked for `-1` on every watched session at once. `-1` is
// NOT a cheap live-only subscribe: the gateway reads it as REWIND
// (`gateway/server.clj resolve-sse-cursor`) and answers with the running turn's
// whole `turn.started`-to-now replay. Measured against three running turns on one
// machine: 6,062,080 bytes on connect, where resuming from an in-range cursor
// costs 98,304 bytes over the same window — and the app asks for up to
// `MAX_SUBSCRIBED_SESSIONS` of them, on a phone, every time it is reopened.
//
// Remembering the cursors is what turns a relaunch back into a resume. An
// unusable one needs no check here: the gateway clamps a cursor above its
// high-water mark or below its ring floor to the same rewind, so a daemon that
// restarted or a ring that moved past us still heals in one connect.
const SESSION_CURSORS_KEY = 'vis.sessionCursors.v1';

// One entry per watched session per paired machine, each a small integer. The
// bound is what stops an install that has opened thousands of sessions from
// keeping a row for every one of them; eviction costs a single rewind.
const MAX_PERSISTED_CURSORS = 256;

// Cursors advance on nearly every streamed frame, so the writes are coalesced.
// Losing the last couple of seconds to an OS kill costs a few seconds of replay.
const CURSOR_FLUSH_MS = 2_000;

function hydrateSessionCursors(): Array<[string, number]> {
  try {
    const raw = (globalThis.localStorage ?? null)?.getItem(SESSION_CURSORS_KEY);
    if (!raw) return [];
    const parsed = JSON.parse(raw) as unknown;
    if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) return [];
    return Object.entries(parsed as Record<string, unknown>)
      .filter(
        (entry): entry is [string, number] =>
          Number.isSafeInteger(entry[1]) && (entry[1] as number) >= 0,
      )
      .slice(-MAX_PERSISTED_CURSORS);
  } catch {
    // Private mode, a blocked store or a corrupt blob: a rewind is slow, never broken.
    return [];
  }
}

const sessionCursors = new Map<string, number>(hydrateSessionCursors());

let cursorFlushTimer: ReturnType<typeof setTimeout> | null = null;

/** Write the replay cursors now. Safe to call from a teardown handler. */
function flushSessionCursors(): void {
  if (cursorFlushTimer !== null) {
    clearTimeout(cursorFlushTimer);
    cursorFlushTimer = null;
  }
  try {
    const store = globalThis.localStorage ?? null;
    if (!store) return;
    store.setItem(SESSION_CURSORS_KEY, JSON.stringify(Object.fromEntries(sessionCursors)));
  } catch {
    // Out of quota or a hostile embedder: persistence is an optimisation.
  }
}

function scheduleCursorFlush(): void {
  if (cursorFlushTimer !== null) return;
  cursorFlushTimer = setTimeout(() => {
    cursorFlushTimer = null;
    flushSessionCursors();
  }, CURSOR_FLUSH_MS);
}

/** Persist the caches NOW — used when the app is being torn down. */
export function persistGatewayCaches(): void {
  flushSnapshots(snapshotStores);
  flushSessionCursors();
}

function readSnapshot<T>(key: string): T | null {
  if (!snapshots.has(key)) return null;
  const value = snapshots.get(key) as T;
  // Map iteration order IS the LRU order: re-insert to mark this entry as used.
  snapshots.delete(key);
  snapshots.set(key, value);
  return value;
}

function writeSnapshot(key: string, value: unknown): void {
  snapshots.delete(key);
  snapshots.set(key, value);
  trimSnapshotKind(key);
  scheduleSnapshotFlush(snapshotStores);
}

/**
 * Structural equality over decoded JSON. `JSON.parse` builds a BRAND-NEW object
 * graph for every response, so identity alone reports "changed" for a payload
 * that is byte-for-byte what we already hold — and React then re-renders a whole
 * transcript that did not move.
 */
function sameJson(a: unknown, b: unknown): boolean {
  if (a === b) return true;
  if (typeof a !== 'object' || typeof b !== 'object' || a === null || b === null) return false;
  if (Array.isArray(a) || Array.isArray(b)) {
    if (!Array.isArray(a) || !Array.isArray(b) || a.length !== b.length) return false;
    return a.every((item, index) => sameJson(item, b[index]));
  }
  const left = a as Record<string, unknown>;
  const right = b as Record<string, unknown>;
  const keys = Object.keys(left);
  if (keys.length !== Object.keys(right).length) return false;
  return keys.every(
    (key) => Object.prototype.hasOwnProperty.call(right, key) && sameJson(left[key], right[key]),
  );
}

/**
 * Splice a freshly fetched list onto the one we already painted: KEEP the old
 * object for every row whose content is unchanged, and the old ARRAY when
 * nothing changed at all. React bails out of an identical state write, and
 * `memo`'d rows keep their identity — so re-entering a session or the periodic
 * liveness refetch costs no re-render instead of re-parsing every markdown
 * block in the history.
 */
/**
 * One page of the session list, pinned to the exact rows its ETag was issued for.
 *
 * The gateway hashes the whole record it answers with — the rows, the count and the
 * overview — into the validator, so a 304 revalidates all of it, not just the rows.
 */
type SessionsWindow = {
  etag: string;
  /** The cursor this window was ASKED for; `HEAD_CURSOR` is the top of the list. */
  after: string;
  rows: Session[];
  /** The gateway's own count of the WHOLE list this window is the head of. */
  total: number;
  /** Stable project and fleet totals, present on the head window only. */
  overview: GatewayOverview | null;
};

/**
 * ONE PROJECT'S PAGE, exactly as the gateway cut it (`listProjectPage`).
 *
 * `total` is the PROJECT's own count — what the group's header prints — so the
 * pager's arithmetic and the header's are one number, never two.
 */
export interface ProjectPage {
  rows: Session[];
  total: number;

  /** Cursor of this page's last row: what the page AFTER it is asked for, `""` at the end. */
  nextCursor: string;
  /**
   * The project's sessions PARKED on an unanswered human-input request, complete
   * however deep in the project they sit (`state/list-sessions-page`). They stand
   * BESIDE the window rather than in it — the ordering never lifts them, so a page
   * never moves when a turn asks or is answered — and the group pins them above the
   * rows it paints. A parked row that is also IN the window is here too; the group
   * paints it once.
   */
  awaiting: Session[];
  /**
   * The project's sessions FILED in a group, complete and outside the window
   * (`?grouped=aside`). A group is a shelf the reader paints whole: painting it
   * from the current page instead showed a filed session as ungrouped until the
   * reader happened to page down to it, and `total`/`nextCursor` above therefore
   * count the LOOSE sessions only.
   *
   * A read that carries a BAND WINDOW is answered with the rows of the bands that
   * window paints and no others: a shelf off the page is nobody's to render
   * (`listProjectPage`).
   */
  grouped: Session[];
}
/**
 * The project windows ONE reader is holding, and the validator each was issued
 * under (`listProjectPage`).
 *
 * Deeper windows belong to the project group and disappear when it unmounts.
 * Each project's head is also snapshotted for cold paint; its validator stays
 * attached to the exact gateway, root, page size and draft overlay it answered.
 */
export type ProjectWindows = Map<string, { etag: string; page: ProjectPage }>;

/**
 * How much of a machine's list this device holds: its newest window, and nothing
 * below it.
 *
 * Measured on a 1043-session gateway: the whole list is 825 KB (152 KB gzip), a
 * 100-row window 84.7 KB (16.2 KB gzip) in 446 ms, a 20-row window 19.2 KB
 * (4.6 KB gzip) in 123 ms. A phone paints fifteen rows, so twenty is the screen
 * plus the scroll under it. Nothing deeper needs this window: a project's pages
 * are cut by the gateway at the size the screen measured (`listProjectPage`),
 * the projects and their counts arrive in the overview beside it, and the runs
 * parked on a human arrive there too (see `listSessions`).
 */
const SESSIONS_PAGE = 20;

// Keep one screen-sized head per project, not the wide reads used to jump pages.
const MAX_PROJECT_HEAD_ROWS = 100;

/** The first window of the list: the one page that is asked for with no cursor. */
const HEAD_CURSOR = '';

function reconcileRows<T>(previous: T[] | null, next: T[]): T[] {
  if (!previous) return next;
  let changed = previous.length !== next.length;
  const merged = next.map((row, index) => {
    const old = previous[index];
    if (old !== undefined && sameJson(old, row)) return old;
    changed = true;
    return row;
  });
  return changed ? merged : previous;
}

/** Single-payload variant: keep the cached object when the wire repeats itself. */
function reconcileRow<T>(previous: T | null, next: T): T {
  return previous !== null && sameJson(previous, next) ? previous : next;
}

function sessionGoalFromWire(raw: unknown): SessionGoal | null {
  if (!raw || typeof raw !== 'object' || Array.isArray(raw)) return null;
  const g = raw as Record<string, unknown>;
  if (
    typeof g.id !== 'string' ||
    !g.id ||
    typeof g.objective !== 'string' ||
    !g.objective.trim() ||
    g.objective.length > gatewaySchema.$defs.session_goal.properties.objective.maxLength ||
    typeof g.status !== 'string' ||
    !gatewaySchema.$defs.session_goal.properties.status.oneOf.some(
      (status) => status.const === g.status,
    ) ||
    !(
      g.reason === null ||
      (typeof g.reason === 'string' &&
        g.reason.length <= gatewaySchema.$defs.session_goal.properties.reason.maxLength)
    ) ||
    !(
      g.iteration_budget === null ||
      (Number.isSafeInteger(g.iteration_budget) && (g.iteration_budget as number) > 0)
    ) ||
    !['iterations_used', 'tokens_used', 'time_used_ms', 'created_at', 'updated_at'].every(
      (k) => Number.isSafeInteger(g[k]) && (g[k] as number) >= 0,
    ) ||
    !['revision', 'version'].every((k) => Number.isSafeInteger(g[k]) && (g[k] as number) >= 1)
  )
    return null;
  return g as unknown as SessionGoal;
}

/**
 * Facts the LIST row establishes that a SINGLE-session payload cannot carry.
 *
 * `GET /v1/sessions/:sid` answers a lean row. Measured against a live gateway,
 * every row it serves omits exactly these three keys, which only the list and
 * the project pages carry. Taking such a row wholesale into the cache DELETED
 * them — and a row with no `workspace` groups under the empty path, so the
 * project it belongs to grew a second, nameless header beside the real one.
 *
 * Only these keys are held. Every OTHER key a payload omits is the gateway
 * saying the field is not set, and must still be allowed to clear the cache.
 */
const LIST_ONLY_SESSION_KEYS = ['workspace', 'is_unread', 'unread_answers'] as const;

/** Keep what a lean payload cannot carry; everything it does carry still wins. */
function withHeldListFacts(previous: Session | null, next: Session): Session {
  if (!previous) return next;
  const held = LIST_ONLY_SESSION_KEYS.filter((key) => !(key in next) && key in previous);
  if (held.length === 0) return next;
  const merged: Record<string, unknown> = { ...next };
  for (const key of held) merged[key] = (previous as Record<string, unknown>)[key];
  return merged as Session;
}

function reconcileSession(
  previous: Session | null,
  next: Session,
  pending?: SessionGoal | null,
): Session {
  const incoming = withHeldListFacts(previous, next);
  const oldGoal = sessionGoalFromWire(previous?.goal);
  const goal = pending && pending.revision > (oldGoal?.revision ?? 0) ? pending : oldGoal;
  const nextGoal = sessionGoalFromWire(incoming.goal);
  const row =
    goal && goal.revision > (nextGoal?.revision ?? 0)
      ? { ...incoming, goal }
      : incoming.goal == null
        ? incoming
        : { ...incoming, goal: nextGoal };
  return reconcileRow(previous, row);
}

/**
 * One live queue delta a screen has already applied: the row as the last live frame
 * left it (`turn.queued`, or `turn.queued.updated` after an edit or a `→` mark), or
 * `null` for a row that LEFT the queue (`turn.queued.drained` / `.deleted` / `.sent`).
 */
export interface QueueDelta {
  at: number;
  row: QueuedTurn | null;
}

/**
 * Fold a `?status=queued` re-read into the deltas that arrived while it was in
 * flight.
 *
 * The queue has two sources and they cross. Rows LEAVE the queue only on live
 * frames — the gateway appends `turn.queued.drained` and `.deleted` with
 * `:store? false`, so no replay, no poll and no snapshot ever repeats them. A
 * backlog read that left before the head drained therefore answers with a row
 * that the drain frame has already removed, and, resolving afterwards, puts it
 * back permanently: the tray shows "Queued" for the turn whose answer is
 * streaming right above it.
 *
 * So a read is authoritative only for rows it could actually have seen. A delta
 * older than the read start is settled (the gateway knew) and is forgotten; a
 * delta NEWER than it wins over the read: a newer row replaces the read's copy in
 * place (an edit, or a `→` mark or unmark), and a newer removal drops it. `forget`
 * names the ids whose removal the read has just written back into the cache, to be
 * dropped there too.
 *
 * `deltas` is the caller's live journal and is pruned in place.
 */
export function mergeQueueBacklog(
  rows: QueuedTurn[],
  deltas: Map<string, QueueDelta>,
  readStartedAt: number,
): { rows: QueuedTurn[]; forget: string[] } {
  const byId = new Map(rows.map((row) => [row.turnId, row]));
  const appended: QueuedTurn[] = [];
  const forget: string[] = [];
  for (const [tid, delta] of [...deltas]) {
    if (delta.at < readStartedAt) {
      deltas.delete(tid);
      continue;
    }
    if (delta.row) {
      if (byId.has(tid)) byId.set(tid, delta.row);
      else appended.push(delta.row);
    } else {
      byId.delete(tid);
      forget.push(tid);
    }
  }
  const kept = rows.flatMap((row) => {
    const current = byId.get(row.turnId);
    return current ? [current] : [];
  });
  return {
    rows: [...kept, ...appended],
    forget,
  };
}

function normalizeBase(url: string): string {
  return url.replace(/\/+$/, '');
}
function errorOfDiagnostic(cause: unknown): string {
  return cause instanceof Error ? cause.message : String(cause);
}

function diagnosticGateway(base: string): string {
  try {
    return new URL(base).origin;
  } catch {
    return 'invalid gateway';
  }
}

type GatewayRequestDiagnosticExtras = Omit<
  GatewayRequestDiagnosticStart,
  'gateway' | 'method' | 'path'
>;

function diagnosticPath(path: string): string {
  try {
    return new URL(path, 'https://gateway.invalid').pathname;
  } catch {
    return path.split(/[?#]/u, 1)[0] || '/';
  }
}

function diagnosticSessionId(path: string): string | undefined {
  const encoded = /^\/v1\/sessions\/([^/]+)/u.exec(path)?.[1];
  if (!encoded) return undefined;
  try {
    return decodeURIComponent(encoded);
  } catch {
    return encoded;
  }
}

function startRequestDiagnostic(
  base: string,
  method: string,
  path: string,
  extras: GatewayRequestDiagnosticExtras = { transport: 'fetch' },
): GatewayRequestDiagnostic {
  const safePath = diagnosticPath(path);
  const sessionId = diagnosticSessionId(safePath);
  return startGatewayRequestDiagnostic({
    gateway: diagnosticGateway(base),
    method,
    path: safePath,
    ...(sessionId ? { session_id: sessionId } : {}),
    ...extras,
  });
}

type FinishRequestDiagnosticOptions = {
  status?: number;
  failure?: { cause: unknown };
  signal?: AbortSignal;
  timedOut?: boolean;
  outcome?: GatewayRequestDiagnosticFinish['outcome'];
  stream?: Pick<
    GatewayRequestDiagnosticFinish,
    'stream_phase' | 'last_byte_age_ms' | 'last_heartbeat_age_ms'
  >;
};

function finishRequestDiagnostic(
  diagnostic: GatewayRequestDiagnostic,
  {
    status = 0,
    failure,
    signal,
    timedOut = false,
    outcome: explicitOutcome,
    stream,
  }: FinishRequestDiagnosticOptions,
): void {
  const outcome =
    explicitOutcome ??
    (signal?.aborted
      ? 'cancelled'
      : timedOut
        ? 'timeout'
        : status >= 400
          ? 'http_error'
          : failure
            ? 'network_error'
            : status === 304
              ? 'not_modified'
              : 'success');
  const diagnosticError =
    !failure || outcome === 'cancelled'
      ? undefined
      : outcome === 'http_error'
        ? `HTTP ${status}`
        : errorOfDiagnostic(failure.cause);
  const level =
    outcome === 'success' || outcome === 'not_modified' || outcome === 'cancelled'
      ? 'info'
      : outcome === 'closed'
        ? 'warn'
        : 'error';
  diagnostic.finish(level, {
    status,
    outcome,
    ...(diagnosticError ? { error: diagnosticError } : {}),
    ...stream,
  });
}

/**
 * One gateway queued-turn payload (a `/v1/sessions/:id/turns` row OR a
 * `turn.queued` / `.updated` SSE frame — same keys) → the row the tray paints.
 *
 * The gateway resolves image attachments once, at submit time, so the tray never
 * has to re-derive them: `request_preview` is the path-free prose and
 * `attachment_previews` the byte-free chips. Without this a message authored by
 * dropping a screenshot rendered as its raw `/var/folders/…/clipboard-….png`.
 * `request` stays verbatim so editing a row starts from what was authored.
 */
export function queuedTurnFromWire(row: Record<string, unknown>): QueuedTurn {
  const request = typeof row.request === 'string' ? row.request : '';
  const preview = typeof row.request_preview === 'string' ? row.request_preview : '';
  const rawAttachments = Array.isArray(row.attachment_previews) ? row.attachment_previews : [];
  const attachments: QueuedAttachment[] = rawAttachments.map((entry) => {
    const item = (entry ?? {}) as Record<string, unknown>;
    return {
      filename: typeof item.filename === 'string' ? item.filename : 'image',
      mediaType: typeof item.media_type === 'string' ? item.media_type : 'image',
      sizeLabel: typeof item.size_label === 'string' ? item.size_label : '',
    };
  });
  return {
    turnId: String(row.turn_id ?? ''),
    request,
    preview: preview || request,
    attachments,
    deliver: row.deliver === 'next_iteration' ? 'next_iteration' : 'turn_end',
  };
}

/**
 * True when a queued message can run only as its own turn: a slash command
 * (`/name …`) or a bang shell line (`!cmd`). The gateway refuses `→` on these rows
 * (`409 not-steerable`); the tray mirrors `gateway-contract/command-request?` so it
 * never offers the control. A bare `!` is prose.
 */
export function isCommandRequest(request: string): boolean {
  const text = request.trimStart();
  return /^\/[A-Za-z]/.test(text) || (text.startsWith('!') && text.slice(1).trim() !== '');
}

/** The gateway-owned hold paired with a queued backlog, or no hold at all. */
function queuePausedFromWire(value: unknown): QueuePausedInfo | null {
  if (!value || typeof value !== 'object' || Array.isArray(value)) return null;
  const row = value as Record<string, unknown>;
  return {
    reason: typeof row.reason === 'string' ? row.reason : 'turn_failed',
    held: Math.max(0, Number(row.held ?? 0)),
  };
}

function attachmentPayloadBlob(attachment: GatewayAttachment): Blob {
  const encoded = attachment.base64.startsWith('data:')
    ? attachment.base64.slice(attachment.base64.indexOf(',') + 1)
    : attachment.base64;
  const binary = atob(encoded);
  const bytes = new Uint8Array(binary.length);
  for (let index = 0; index < binary.length; index += 1) bytes[index] = binary.charCodeAt(index);
  return new Blob([bytes], { type: attachment.media_type });
}

type AttachmentSource = { blob: Blob; url: string };

export class GatewayClient {
  readonly base: string;
  private readonly token?: string;
  // Shared with the snapshot cache: a late read from another client instance must
  // not resurrect a confirmed deletion. Session UUIDs are never reused.
  private static readonly deletedSessions = new Set<string>();
  /** Last canonical queue hold read with this session's backlog. */
  private readonly queuePaused = new Map<string, QueuePausedInfo | null>();
  // (session, iteration, index) → the produced artifact's downloaded Blob and its
  // object URL. Text readers consume the Blob directly; media elements consume the
  // URL. One source owns both so neither path downloads or re-reads the other.
  private readonly attachmentSources = new Map<string, Promise<AttachmentSource>>();
  // What each of those rows actually COSTS, learned when its bytes land. A bound
  // counted in entries alone cannot tell 24 thumbnails from 24 video clips.
  private readonly attachmentSizes = new Map<string, number>();
  // How many MOUNTED tiles are painting each key right now. Eviction REVOKES an
  // object URL, and a revoked URL is a permanently broken `<img>` — so a picture
  // that is on screen must never be the one handed back to the collector.
  private readonly attachmentHolds = new Map<string, number>();
  // Last conditional-GET validator for the session LIST, per gateway: the head
  // window keyed by the cursor it was asked for, plus the rows it was built from.
  // Static because the screens build a throwaway client whenever the connection
  // object changes, and the snapshot cache it pairs with is module-level too.
  // Pinning by IDENTITY is what makes it safe: any other code path that rewrites the
  // sessions snapshot (a local delete, a rename) swaps that array, the pin misses,
  // and the next poll is an unconditional read instead of a 304 restoring stale rows.
  private static readonly sessionsValidators = new Map<
    string,
    { full: Session[]; windows: Map<string, SessionsWindow> }
  >();
  /** Backing store refreshed by the head of every list read. */
  private overview: GatewayOverview | null;
  constructor(conn: GatewayConn) {
    this.base = normalizeBase(conn.url);
    this.token = conn.token;
    this.overview = this.cachedProjectsOverview();
    registerMemoryOwner('gateway client', this, (client) => client.memoryCells());
  }

  /** What this instance holds, for the memory overlay (`perf.ts`). */
  private *memoryCells(): Iterable<MemoryCell> {
    yield { source: 'gateway clients', bytes: 0, entries: 1 };
    for (const [key, bytes] of this.attachmentSizes) {
      yield { source: 'attachments', session: key.split('\u0000')[0], bytes, entries: 1 };
    }
    for (const [key, sent] of this.sentAttachments) {
      yield {
        source: 'sent attachments',
        session: key.split('\u0000')[0],
        bytes: approxBytes(sent),
        entries: sent.length,
      };
    }
  }

  /**
   * The replay cursor this device was last served for one session, or `null`
   * when it has never streamed that session from this gateway.
   */
  cachedSessionCursor(sid: string): number | null {
    return sessionCursors.get(this.snapshotKey('cursor', sid)) ?? null;
  }

  /** Remember how far one session's stream has been delivered. */
  private rememberSessionCursor(sid: string, cursor: number): void {
    if (!Number.isSafeInteger(cursor) || cursor < 0) return;
    const key = this.snapshotKey('cursor', sid);
    if (sessionCursors.get(key) === cursor) return;
    // Re-insert so Map order IS the LRU order the bound below is applied along.
    sessionCursors.delete(key);
    sessionCursors.set(key, cursor);
    for (const oldest of Array.from(sessionCursors.keys()).slice(
      0,
      Math.max(0, sessionCursors.size - MAX_PERSISTED_CURSORS),
    )) {
      sessionCursors.delete(oldest);
    }
    scheduleCursorFlush();
  }

  /**
   * The cursor to ASK this gateway for, for one subscribed session.
   *
   * A caller that holds no cursor passes the rewind sentinel, which is the only
   * honest thing it can say about a session it has not streamed yet. This is
   * where "I have no cursor" becomes "resume where this device left off": the
   * remembered cursor, or the sentinel when there is none to use.
   */
  private resumeCursor(sid: string, requested: number): number {
    if (requested >= 0) return requested;
    return this.cachedSessionCursor(sid) ?? requested;
  }

  /** Cache key for one of this gateway's snapshot-able payloads. */
  private snapshotKey(kind: string, sid?: string): string {
    return sid ? `${this.base}\u0000${kind}\u0000${sid}` : `${this.base}\u0000${kind}`;
  }

  private headers(extra?: HeadersInit): Headers {
    const h = new Headers(extra);
    if (this.token) h.set('Authorization', `Bearer ${this.token}`);
    // Announce which wire protocol this build speaks on EVERY request, so a
    // gateway that no longer serves us answers 426 with a real explanation
    // instead of a shape we would misread.
    for (const [k, v] of Object.entries(PROTOCOL_HEADERS)) h.set(k, v);
    return h;
  }

  /**
   * One request, reported in full: status and validator, not just the parsed
   * body. `304 Not Modified` is NOT an error here — it is the success case of a
   * revalidation, so it returns early, before the body read, with no data.
   * `urgent` queues it ahead of other waiting requests; see `awaitGatewaySlot`.
   */
  private async requestFull<T>(
    method: string,
    path: string,
    body?: unknown,
    signal?: AbortSignal,
    extraHeaders?: Record<string, string>,
    urgent = false,
  ): Promise<{
    status: number;
    data: T | undefined;
    etag: string | null;
    headers: Headers;
  }> {
    const oauth =
      /^\/v1\/(?:providers\/[^/]+|mcp\/servers\/[^/]+)\/auth\/(?:start|complete|poll|cancel)$/.test(
        path,
      );
    // OAuth uses the paired gateway's HTTP(S) transport, like other app requests.
    if (oauth) {
      const destination = new URL(this.base);
      const loopback = ['localhost', '127.0.0.1', '[::1]'].includes(destination.hostname);
      if (!['http:', 'https:'].includes(destination.protocol)) {
        throw new GatewayOAuthError('invalid-address');
      }
      if (!loopback && !this.token?.trim()) throw new GatewayOAuthError('pairing-required');
    }
    // Queue for one of this gateway's few sockets BEFORE the clock starts: the
    // wait is this app's own backpressure, not a slow gateway, and reporting it
    // as a timeout would blame the machine for the app's own burst.
    const release = takeGatewaySlot(this.base) ?? (await awaitGatewaySlot(this.base, urgent));
    const diagnostic = startRequestDiagnostic(this.base, method, path);
    let exchangeStatus = 0;
    let exchangeFailure: { cause: unknown } | undefined;
    const headers = this.headers(extraHeaders);
    // A Blob is a RECORDING (or any raw upload) and travels as itself: it carries its
    // own media type and JSON-encoding it would destroy it.
    const isRaw = body instanceof Blob;
    if (body !== undefined && !isRaw) headers.set('Content-Type', 'application/json');
    // Bound the whole exchange, not just the connect: a resumed request usually
    // parks on the BODY read, with its headers already delivered.
    const deadline = new AbortController();
    const timer = window.setTimeout(() => deadline.abort(), REQUEST_TIMEOUT_MS);
    // A caller that aborted (screen unmounted, session switched) is not a stall,
    // and must keep reporting itself as one.
    const stalled = () => deadline.signal.aborted && !signal?.aborted;
    const seconds = Math.round(REQUEST_TIMEOUT_MS / 1000);
    const attempt = linkSignals(signal ? [signal, deadline.signal] : [deadline.signal]);
    const attemptSignal = attempt.signal;
    try {
      let res: Response;
      try {
        res = await raceAbort(
          fetch(this.base + path, {
            method,
            headers,
            body: body === undefined ? undefined : isRaw ? (body as Blob) : JSON.stringify(body),
            signal: attemptSignal,
            ...(oauth ? { redirect: 'error' as const, cache: 'no-store' as const } : {}),
          }),
          attemptSignal,
        );
        exchangeStatus = res.status;
      } catch (e) {
        throw stalled()
          ? new GatewayError(0, `gateway did not answer within ${seconds}s`)
          : new GatewayError(0, `network error: ${(e as Error).message}`);
      }
      if (res.status === 304)
        return {
          status: 304,
          data: undefined,
          etag: res.headers.get('ETag'),
          headers: res.headers,
        };
      let text: string;
      try {
        text = await raceAbort(res.text(), attemptSignal);
      } catch (e) {
        throw stalled()
          ? new GatewayError(0, `gateway stopped sending after ${seconds}s`)
          : new GatewayError(0, `network error: ${(e as Error).message}`);
      }
      let parsed: unknown = undefined;
      if (text) {
        try {
          parsed = JSON.parse(text);
        } catch {
          parsed = text;
        }
      }
      if (!res.ok) {
        const error = new GatewayError(res.status, errorText(parsed, res.status), parsed);
        // A refused protocol is not this call's problem, it is the whole
        // connection's: announce it so the app can re-read the verdict and show
        // the screen, rather than let one failed request explain it alone.
        if (res.status === INCOMPATIBLE_STATUS) incompatibleListener?.(error);
        throw error;
      }
      return {
        status: res.status,
        data: parsed as T,
        etag: res.headers.get('ETag'),
        headers: res.headers,
      };
    } catch (cause) {
      exchangeFailure = { cause };
      throw cause;
    } finally {
      finishRequestDiagnostic(diagnostic, {
        status: exchangeStatus,
        failure: exchangeFailure,
        signal,
        timedOut: deadline.signal.aborted && !signal?.aborted,
      });
      window.clearTimeout(timer);
      attempt.release();
      release();
    }
  }

  private async request<T>(
    method: string,
    path: string,
    body?: unknown,
    signal?: AbortSignal,
    urgent = false,
  ): Promise<T> {
    return (await this.requestFull<T>(method, path, body, signal, undefined, urgent)).data as T;
  }

  // ── Health / status ─────────────────────────────────────────────
  status(signal?: AbortSignal): Promise<GatewayStatus> {
    return this.request<GatewayStatus>('GET', '/v1/admin/status', undefined, signal);
  }

  async ping(signal?: AbortSignal): Promise<boolean> {
    // A candidate address gets a SHORT question (`PROBE_TIMEOUT_MS`): this is
    // asked of every address the app knows, including the ones that are simply
    // not on this network any more.
    const deadline = new AbortController();
    const timer = window.setTimeout(() => deadline.abort(), PROBE_TIMEOUT_MS);
    const probe = linkSignals(signal ? [signal, deadline.signal] : [deadline.signal]);
    try {
      await this.request(
        'GET',
        '/healthz',
        undefined,
        probe.signal,
      );
      return true;
    } catch (e) {
      // A token-gated gateway still answers /healthz; a 401 means "reachable
      // but unauthorized", which is a connection we should flag distinctly.
      if (e instanceof GatewayError && e.status === 401) throw e;
      return false;
    } finally {
      window.clearTimeout(timer);
      probe.release();
    }
  }

  /**
   * The gateway's stable, opaque instance id — names WHICH gateway this is
   * (deterministic across restarts and independent of LAN/Tailscale/cloudflared
   * host), never grants access. Used to build clean shareable session links.
   */
  async identify(signal?: AbortSignal): Promise<string | null> {
    try {
      return (await this.health(signal)).id ?? null;
    } catch {
      return null;
    }
  }

  /**
   * `/healthz` is open even to a client the gateway refuses to serve, so this
   * is how the app learns WHY it was refused — and how it detects the reverse
   * case, a gateway too old to know it is too old.
   */
  health(signal?: AbortSignal): Promise<GatewayHealth> {
    return this.request<GatewayHealth>('GET', '/healthz', undefined, signal);
  }

  /**
   * Last capabilities payload seen for THIS gateway — the first frame for every
   * session and settings panel. The payload is also durable across an app kill.
   */
  cachedCapabilities(): GatewayCapabilities | null {
    return readSnapshot<GatewayCapabilities>(this.snapshotKey('capabilities'));
  }

  /**
   * Stable feature negotiation for one gateway, shared by every client instance.
   *
   * A fresh answer is reused for the settings-panel cadence instead of asking the
   * same machine whenever a session mounts. Concurrent readers join one request.
   * `force` belongs to address recovery: it must prove the endpoint still answers
   * rather than mistaking a cached payload for network reachability.
   */
  async capabilities(
    signal?: AbortSignal,
    opts?: { force?: boolean },
  ): Promise<GatewayCapabilities> {
    const key = this.snapshotKey('capabilities');
    const held = readSnapshot<GatewayCapabilities>(key);
    if (!opts?.force && held && Date.now() - (capabilityReads.get(key) ?? 0) < PANEL_TTL_MS)
      return held;

    let flight = capabilityFlights.get(key);
    if (flight?.controller.signal.aborted) {
      capabilityFlights.delete(key);
      flight = undefined;
    }
    if (!flight) {
      const controller = new AbortController();
      const promise = this.request<GatewayCapabilities>(
        'GET',
        '/v1/capabilities',
        undefined,
        controller.signal,
      ).then((response) => {
        const answer = reconcileRow(readSnapshot<GatewayCapabilities>(key), response);
        writeSnapshot(key, answer);
        capabilityReads.set(key, Date.now());
        return answer;
      });
      const created = { controller, promise };
      capabilityFlights.set(key, created);
      const forget = () => {
        if (capabilityFlights.get(key) === created) capabilityFlights.delete(key);
      };
      void promise.then(forget, forget);
      flight = created;
    }

    if (opts?.force && signal) {
      // Address recovery DOES own its probe: a wake aborts the socket frozen on
      // the old network and the next force call replaces the aborted flight.
      const cancel = () => flight.controller.abort();
      if (signal.aborted) cancel();
      else {
        signal.addEventListener('abort', cancel, { once: true });
        const detach = () => signal.removeEventListener('abort', cancel);
        void flight.promise.then(detach, detach);
      }
    } else {
      // A session does not own this machine-wide read. Unmounting one composer
      // must not cancel the answer another screen or the launch sweep awaits.
      void signal;
    }
    return flight.promise;
  }

  // ── Projects overview ───────────────────────────────────────────

  /** Last project totals seen for THIS gateway — paint them before revalidation. */
  cachedProjectsOverview(): GatewayOverview | null {
    return readSnapshot<GatewayOverview>(this.snapshotKey('projects-overview'));
  }

  /** Project totals carried by the most recent session-list head. */
  projectsOverview(): GatewayOverview | null {
    return this.overview;
  }

  // ── Native push devices ─────────────────────────────────────────
  /**
   * Last device list seen for THIS gateway. The notifications panel is opened over
   * and over on an answer that rarely changes, so it paints this and revalidates
   * instead of asking `Checking…` every time (see `lib/notify-verdict.ts`).
   */
  cachedDevices(): DevicesState | null {
    return readSnapshot(this.snapshotKey('devices'));
  }

  /**
   * `GET /v1/devices` — one question per machine, however many callers ask it.
   *
   * A read younger than `DEVICES_FRESH_MS` is answered from the snapshot and a
   * read already in flight is joined rather than duplicated, so the launch
   * sweep, push registration and the panel that opens on top of them cost the
   * machine a single request between them.
   */
  async devices(signal?: AbortSignal): Promise<DevicesState> {
    const key = this.snapshotKey('devices');
    const held = readSnapshot<DevicesState>(key);
    if (held && Date.now() - (deviceReads.get(key) ?? 0) < DEVICES_FRESH_MS) {
      return held;
    }
    const flight = deviceFlights.get(key);
    if (flight) return flight;
    const reading = this.request<DevicesState>('GET', '/v1/devices', undefined, signal)
      .then((response) => {
        writeSnapshot(key, response);
        writeSnapshot(this.snapshotKey('devices-unsupported'), false);
        deviceReads.set(key, Date.now());
        return response;
      })
      .catch((error: unknown) => {
        // A gateway too old to carry the route answers 404/501, and will answer
        // it again tomorrow. Remembering the refusal is what keeps the panel
        // ABSENT on the next open instead of painting itself and then deleting
        // itself, shoving everything below it up the screen.
        if (error instanceof GatewayError && (error.status === 404 || error.status === 501))
          writeSnapshot(this.snapshotKey('devices-unsupported'), true);
        throw error;
      })
      .finally(() => {
        deviceFlights.delete(key);
      });
    deviceFlights.set(key, reading);
    return reading;
  }

  /** Whether THIS gateway has already said it carries no `/v1/devices`. */
  isDevicesUnsupported(): boolean {
    return readSnapshot<boolean>(this.snapshotKey('devices-unsupported')) === true;
  }

  /**
   * Idempotent: re-registering the same token refreshes it, never duplicates.
   *
   * The answer names the row that was just written, so it is merged into the
   * held list instead of being re-read: the panel reloading after a press asks
   * this machine nothing.
   */
  async registerDevice(input: PushDeviceInput): Promise<{ device: PushDevice; push: PushStatus }> {
    const response = await this.request<{ device: PushDevice; push: PushStatus }>(
      'POST',
      '/v1/devices',
      input,
    );
    const key = this.snapshotKey('devices');
    const held = readSnapshot<DevicesState>(key);
    if (held) {
      writeSnapshot(key, {
        devices: [
          ...held.devices.filter(
            (device) => device.token_preview !== response.device.token_preview,
          ),
          response.device,
        ],
        push: response.push,
      });
    }
    return response;
  }

  /** What the list says has changed, so the next read of it asks again. */
  async unregisterDevice(token: string): Promise<{ is_removed: boolean }> {
    const response = await this.request<{ is_removed: boolean }>(
      'DELETE',
      `/v1/devices/${encodeURIComponent(token)}`,
    );
    deviceReads.delete(this.snapshotKey('devices'));
    return response;
  }

  /**
   * This gateway as push registration sees it (`lib/relay.ts`): whether it can
   * sign a push to this device at all, and the two calls that put the device on
   * its list or take it off again.
   */
  pushTarget(): PushGateway {
    return {
      status: async () => (await this.devices()).push,
      register: (input) => this.registerDevice(input),
      unregister: (id) => this.unregisterDevice(id),
    };
  }

  // ── Engines: whether this MACHINE can listen and speak ──────────
  //
  // Session-less, like the voices below: a model on disk is a fact about the machine, so
  // settings can ask - and start the download - before any conversation exists.

  /**
   * Whether one listening engine is ready, still downloading (with progress), or failed.
   * `start` POSTs instead: prepare that exact engine and begin its download.
   */
  voiceModel({
    start = false,
    signal,
    engine,
  }: {
    start?: boolean;
    signal?: AbortSignal;
    engine?: string | null;
  } = {}): Promise<VoiceModelState> {
    return this.request<VoiceModelState>(
      start ? 'POST' : 'GET',
      withEngine('/v1/voice/model', engine),
      undefined,
      signal,
    );
  }

  /** [[voiceModel]] for the speaking direction, optionally scoped to one voice. */
  speechModel({
    start = false,
    signal,
    engine,
    voice,
    isLicenseAccepted = false,
  }: {
    start?: boolean;
    signal?: AbortSignal;
    engine?: string | null;
    voice?: string;
    isLicenseAccepted?: boolean;
  } = {}): Promise<VoiceModelState> {
    const query = new URLSearchParams();
    if (voice) query.set('voice_id', voice);
    if (isLicenseAccepted) query.set('is_license_accepted', 'true');
    const path = `/v1/speech/model${query.size ? `?${query.toString()}` : ''}`;
    return this.request<VoiceModelState>(
      start ? 'POST' : 'GET',
      withEngine(path, engine),
      undefined,
      signal,
    );
  }

  // ── Voices: what this MACHINE can speak with ────────────────────
  //
  // No session in these paths on purpose. A cloning voice is a stored recording, so it
  // belongs to the machine, and the screen that manages voices is settings — which is
  // reading a machine and not a session.

  /** Every voice one speaking engine can use, plus whether it can learn another one. */
  speechVoices({
    signal,
    engine,
  }: { signal?: AbortSignal; engine?: string | null } = {}): Promise<SpeechVoices> {
    return this.request<SpeechVoices>(
      'GET',
      withEngine('/v1/speech/voices', engine),
      undefined,
      signal,
    );
  }

  /**
   * Create a voice by UPLOADING the recording that is it. The clip travels as the body;
   * everything said ABOUT it travels in the query, including its own transcript — the
   * model is told the words, which is what makes the clone track the voice instead of
   * guessing them.
   */
  async importSpeechVoice(
    clip: Blob,
    about: { name: string; lang?: string; text?: string },
    { signal, engine }: { signal?: AbortSignal; engine?: string | null } = {},
  ): Promise<SpeechVoice> {
    const query = new URLSearchParams({ name: about.name });
    if (about.lang) query.set('lang', about.lang);
    if (about.text) query.set('text', about.text);
    const answer = await this.request<{ voice: SpeechVoice }>(
      'POST',
      withEngine(`/v1/speech/voices?${query.toString()}`, engine),
      clip,
      signal,
    );
    return answer.voice;
  }

  /** Take an imported voice back. 404 means the catalogue on screen is stale. */
  async forgetSpeechVoice(
    id: string,
    { signal, engine }: { signal?: AbortSignal; engine?: string | null } = {},
  ): Promise<void> {
    await this.request(
      'DELETE',
      withEngine(`/v1/speech/voices/${encodeURIComponent(id)}`, engine),
      undefined,
      signal,
    );
  }

  /**
   * The sound of ONE voice, so a catalogue can be heard and not only read.
   *
   * `requestBody` rather than `request`, for the same reason [[speakText]] uses it: a WAV
   * is not text. A 404 here is not a broken client — it is a voice with nothing to play
   * yet, and what to do about that belongs to the screen.
   */
  async speechVoiceSample(
    id: string,
    { signal, engine }: { signal?: AbortSignal; engine?: string | null } = {},
  ): Promise<Blob> {
    return this.requestBody(
      'GET',
      withEngine(`/v1/speech/voices/${encodeURIComponent(id)}/sample`, engine),
      { signal },
      (response) => response.blob(),
    );
  }

  /**
   * Speak a line on the machine and hand back the audio. A null session uses
   * the machine-level route for settings previews, without creating a conversation.
   *
   * Two answers, one call: a short line comes back AS the bytes in a single round
   * trip, a long one answers 202 with a job that is followed to its audio here. The
   * caller only ever wanted the sound, and where the threshold sits is the gateway's
   * to publish (`features.speech.inline_max_chars`), never this client's to guess.
   *
   * `requestBody` rather than `request`: `request` reads every answer as text, and
   * a WAV is not text.
   */
  async speakText(
    sid: string | null,
    text: string,
    {
      voice,
      engine,
      signal,
    }: {
      voice?: string | null;
      engine?: string | null;
      signal?: AbortSignal;
    } = {},
  ): Promise<Blob> {
    const base = sid === null ? '/v1/speech' : `/v1/sessions/${encodeURIComponent(sid)}/speech`;
    const answer = await this.requestBody(
      'POST',
      withEngine(base, engine),
      {
        body: JSON.stringify(voice ? { text, voice } : { text }),
        contentType: 'application/json',
        signal,
      },
      async (response) =>
        response.status === 202
          ? { kind: 'job' as const, job: (await response.json()) as SpeechJob }
          : { kind: 'audio' as const, blob: await response.blob() },
    );
    if (answer.kind === 'audio') return answer.blob;
    const finished = await this.awaitSpeechJob(base, answer.job, signal);
    const blob = await this.requestBody(
      'GET',
      `${base}/jobs/${encodeURIComponent(finished.id)}/audio`,
      { signal },
      (response) => response.blob(),
    );
    // The audio is ours now, so the machine may forget the job. A failure here costs
    // nothing - finished jobs expire on their own.
    void this.request(
      'DELETE',
      `${base}/jobs/${encodeURIComponent(finished.id)}`,
      undefined,
      signal,
    ).catch(() => undefined);
    return blob;
  }

  /** One fully-consumed request with a caller-selected body decoder. */
  private async requestBody<T>(
    method: string,
    path: string,
    options: { body?: BodyInit; contentType?: string; signal?: AbortSignal },
    read: (response: Response) => Promise<T>,
  ): Promise<T> {
    const diagnostic = startRequestDiagnostic(this.base, method, path);
    const headers = this.headers();
    if (options.contentType) headers.set('Content-Type', options.contentType);
    const deadline = new AbortController();
    const timer = window.setTimeout(() => deadline.abort(), REQUEST_TIMEOUT_MS);
    const attempt = linkSignals(
      options.signal ? [options.signal, deadline.signal] : [deadline.signal],
    );
    const attemptSignal = attempt.signal;
    let status = 0;
    let failure: { cause: unknown } | undefined;
    try {
      const res = await raceAbort(
        fetch(this.base + path, {
          method,
          headers,
          body: options.body,
          signal: attemptSignal,
        }),
        attemptSignal,
      );
      status = res.status;
      if (!res.ok) {
        const text = await raceAbort(res.text(), attemptSignal).catch(() => '');
        let parsed: unknown;
        try {
          parsed = text ? JSON.parse(text) : undefined;
        } catch {
          parsed = text;
        }
        throw new GatewayError(res.status, errorText(parsed, res.status), parsed);
      }
      return await raceAbort(read(res), attemptSignal);
    } catch (cause) {
      const error =
        deadline.signal.aborted && !options.signal?.aborted
          ? new GatewayError(
              0,
              `gateway did not finish the response within ${Math.round(REQUEST_TIMEOUT_MS / 1000)}s`,
            )
          : cause instanceof GatewayError
            ? cause
            : new GatewayError(0, `network error: ${errorOfDiagnostic(cause)}`);
      failure = { cause: error };
      throw error;
    } finally {
      finishRequestDiagnostic(diagnostic, {
        status,
        failure,
        signal: options.signal,
        timedOut: deadline.signal.aborted && !options.signal?.aborted,
      });
      window.clearTimeout(timer);
      attempt.release();
    }
  }

  /** Follow one synthesis job to its end, or say why it will never get there. */
  private async awaitSpeechJob(
    base: string,
    job: SpeechJob,
    signal?: AbortSignal,
  ): Promise<SpeechJob> {
    const path = `${base}/jobs/${encodeURIComponent(job.id)}`;
    const deadline = Date.now() + SPEECH_JOB_TIMEOUT_MS;
    let latest = job;
    while (!latest.is_done) {
      if (Date.now() > deadline) {
        throw new GatewayError(0, 'the machine did not finish speaking in time');
      }
      await new Promise((resolve) => setTimeout(resolve, SPEECH_JOB_POLL_MS));
      latest = await this.request<SpeechJob>('GET', path, undefined, signal);
    }
    if (latest.error) throw new GatewayError(0, latest.error);
    return latest;
  }

  /**
   * Upload the recording and get the JOB back (HTTP 202), reporting the bytes as
   * they leave. This is the only part of a transcription the client can measure
   * itself, and on a phone it is the slow half.
   *
   * XHR, not `fetch`: `fetch` still cannot report upload progress in a WebView.
   */
  private uploadVoice(
    sid: string,
    wav: Blob,
    onUploaded: (percent: number) => void,
    signal?: AbortSignal,
    engine?: string | null,
  ): Promise<VoiceJob> {
    const budget = voiceTimeoutMs(wav.size);
    const seconds = Math.round(budget / 1000);
    const path = withEngine(`/v1/sessions/${encodeURIComponent(sid)}/voice`, engine);
    return new Promise<VoiceJob>((resolve, reject) => {
      if (signal?.aborted) {
        reject(signal.reason ?? new DOMException('Aborted', 'AbortError'));
        return;
      }
      const diagnostic = startRequestDiagnostic(this.base, 'POST', path, { transport: 'xhr' });
      const xhr = new XMLHttpRequest();
      const onAbort = () => xhr.abort();
      const done = () => signal?.removeEventListener('abort', onAbort);
      const finish = (failure?: { cause: unknown }, timedOut = false) => {
        done();
        finishRequestDiagnostic(diagnostic, {
          status: xhr.status,
          failure,
          signal,
          timedOut,
        });
      };
      xhr.open('POST', `${this.base}${path}`);
      xhr.timeout = budget;
      this.headers({ 'Content-Type': 'audio/wav' }).forEach((value, key) =>
        xhr.setRequestHeader(key, value),
      );
      if (xhr.upload) {
        xhr.upload.onprogress = (event: ProgressEvent) => {
          if (event.lengthComputable && event.total > 0) {
            onUploaded(Math.round((event.loaded / event.total) * 100));
          }
        };
      }
      xhr.onload = () => {
        let parsed: unknown;
        try {
          parsed = xhr.responseText ? JSON.parse(xhr.responseText) : undefined;
        } catch {
          parsed = xhr.responseText;
        }
        if (xhr.status >= 200 && xhr.status < 300) {
          finish();
          onUploaded(100);
          resolve(parsed as VoiceJob);
          return;
        }
        const error = new GatewayError(xhr.status, errorText(parsed, xhr.status), parsed);
        finish({ cause: error });
        reject(error);
      };
      xhr.onerror = () => {
        const error = new GatewayError(0, 'network error: upload failed');
        finish({ cause: error });
        reject(error);
      };
      xhr.ontimeout = () => {
        const error = new GatewayError(0, `transcription did not answer within ${seconds}s`);
        finish({ cause: error }, true);
        reject(error);
      };
      xhr.onabort = () => {
        const error = signal?.reason ?? new DOMException('Aborted', 'AbortError');
        finish({ cause: error });
        reject(error);
      };
      signal?.addEventListener('abort', onAbort, { once: true });
      try {
        xhr.send(wav);
      } catch (cause) {
        finish({ cause });
        reject(cause);
      }
    });
  }

  /**
   * Follow ONE job's own event stream to the end and return the terminal job.
   *
   * Nothing is polled: the gateway pushes a frame the instant the engine moves,
   * and the closing frame carries the transcript, so the percentage a human
   * reads is never a poll interval stale and there is no "ask again" to time.
   */
  private async voiceJobStream(
    sid: string,
    jobId: string,
    onJob: (job: VoiceJob) => void,
    signal?: AbortSignal,
  ): Promise<VoiceJob> {
    const path = `/v1/sessions/${encodeURIComponent(sid)}/voice/jobs/${encodeURIComponent(jobId)}/events`;
    const diagnostic = startRequestDiagnostic(this.base, 'GET', path, {
      transport: 'sse',
      stream: 'voice_job',
    });
    const watchdog = new AbortController();
    // Released by `watchdog.abort()` when the stream ends.
    const streamSignal = signal ? linkSignals([signal, watchdog.signal]).signal : watchdog.signal;
    const seen: { job: VoiceJob | null } = { job: null };
    let stalled = false;
    let status = 0;
    let failure: { cause: unknown } | undefined;
    let timer: ReturnType<typeof setTimeout> | null = null;
    const armStall = () => {
      if (timer) clearTimeout(timer);
      timer = setTimeout(() => {
        stalled = true;
        watchdog.abort();
      }, VOICE_STALL_TIMEOUT_MS);
    };
    try {
      armStall();
      const response = await raceAbort(
        fetch(this.base + path, {
          headers: this.headers({ Accept: 'text/event-stream' }),
          signal: streamSignal,
        }),
        streamSignal,
      );
      status = response.status;
      if (!response.ok || !response.body) {
        let parsed: unknown;
        try {
          parsed = await raceAbort(response.json(), streamSignal);
        } catch {
          parsed = undefined;
        }
        throw new GatewayError(response.status, errorText(parsed, response.status), parsed);
      }
      await raceAbort(
        readSseFrames(
          response.body,
          (json, event) => {
            // This stream carries `voice.job` frames and nothing else. Any other
            // name is not this job's progress and must never be reported as it.
            if (event !== VOICE_JOB_EVENT) return;
            let job: VoiceJob;
            try {
              job = JSON.parse(json) as VoiceJob;
            } catch {
              return;
            }
            if (!job || typeof job !== 'object' || !job.id) return;
            seen.job = job;
            onJob(job);
          },
          armStall,
          streamSignal,
        ),
        streamSignal,
      );
      const job = seen.job;
      if (!job?.is_done) {
        throw new GatewayError(0, 'transcription ended before the transcript');
      }
      return job;
    } catch (cause) {
      const error =
        stalled && !signal?.aborted
          ? new GatewayError(
              0,
              `transcription stopped reporting for ${Math.round(VOICE_STALL_TIMEOUT_MS / 1000)}s`,
            )
          : cause;
      failure = { cause: error };
      throw error;
    } finally {
      finishRequestDiagnostic(diagnostic, {
        status,
        failure,
        signal,
        timedOut: stalled && !signal?.aborted,
      });
      if (timer) clearTimeout(timer);
      watchdog.abort();
    }
  }

  /** Drop a collected job. Finished jobs also expire on the gateway by themselves. */
  async forgetVoiceJob(sid: string, jobId: string): Promise<void> {
    try {
      await this.request(
        'DELETE',
        `/v1/sessions/${encodeURIComponent(sid)}/voice/jobs/${encodeURIComponent(jobId)}`,
      );
    } catch {
      // Housekeeping: the transcript is already in the composer.
    }
  }

  /**
   * Transcribe a recording, SAYING WHERE IT IS the whole way: `uploading` while
   * the bytes travel, then the gateway job's own `queued` / `preparing` /
   * `transcribing` percentage until the text arrives.
   *
   * The dictation used to be one opaque POST that returned the text minutes
   * later — indistinguishable from a hang, and its socket was the only thing
   * holding the result, so a locked screen lost the words.
   */
  async transcribeVoice(
    sid: string,
    wav: Blob,
    opts: {
      onProgress?: (progress: VoiceProgress) => void;
      signal?: AbortSignal;
      engine?: string | null;
    } = {},
  ): Promise<VoiceTranscript> {
    const { onProgress, signal, engine } = opts;
    // A reporting callback is a UI detail; it can never fail a transcription.
    const report = (progress: VoiceProgress) => {
      try {
        onProgress?.(progress);
      } catch {
        /* ignored */
      }
    };
    report({ phase: 'uploading', progress: 0 });
    const accepted = await this.uploadVoice(
      sid,
      wav,
      (percent) => report({ phase: 'uploading', progress: percent }),
      signal,
      engine,
    );
    report({
      phase: accepted.phase ?? 'queued',
      progress: accepted.progress ?? 0,
      engine: accepted.engine,
    });

    // The upload is the only half this client can measure; the rest is PUSHED
    // from the job's own stream, frame by frame, until the terminal one.
    const job = accepted.is_done
      ? accepted
      : await this.voiceJobStream(
          sid,
          accepted.id,
          (tick) =>
            report({
              phase: tick.phase,
              progress: tick.progress ?? 0,
              engine: tick.engine,
            }),
          signal,
        );
    void this.forgetVoiceJob(sid, job.id);
    if (job.phase === 'failed' || job.error) {
      throw new GatewayError(0, job.error || 'transcription failed');
    }
    return { text: job.text ?? '' };
  }

  // ── Settings (shared feature-toggle registry, same as TUI) ──────
  /** Last settings payload seen for this gateway — paint it, then revalidate. */
  private settingsQuery(target?: SettingsTarget): string {
    const query = new URLSearchParams({ scope: target?.scope ?? 'global' });
    if (target?.target_id) query.set('target_id', target.target_id);
    return query.toString();
  }

  private settingsKey(kind: string, target?: SettingsTarget, id = ''): string {
    return this.snapshotKey(kind, `${this.settingsQuery(target)}/${id}`);
  }

  cachedSettings(target?: SettingsTarget): SettingsResponse | null {
    const cached = readSnapshot<SettingsResponse>(this.settingsKey('settings', target));
    return this.hasSettingsCatalogRevision(cached) ? cached : null;
  }

  /** Register paired identities with the primary; its durable order is authoritative. */
  async machineOrder(ids: string[], signal?: AbortSignal): Promise<string[]> {
    const response = await this.request<{ machine_ids: string[] }>(
      'POST',
      '/v1/machines/order',
      { machine_ids: ids },
      signal,
    );
    if (
      !Array.isArray(response.machine_ids) ||
      response.machine_ids.some((id) => typeof id !== 'string')
    )
      throw new Error('Invalid machine order response');
    return response.machine_ids;
  }

  async settings(
    signal?: AbortSignal,
    target?: SettingsTarget,
    contextSessionId?: string,
  ): Promise<SettingsResponse> {
    // Naming the open session marks the rows its own scopes decide (`overridden_by`).
    const context = contextSessionId
      ? `&context_session_id=${encodeURIComponent(contextSessionId)}`
      : '';
    const response = await this.request<SettingsResponse>(
      'GET',
      `/v1/settings?channel=all&${this.settingsQuery(target)}${context}`,
      undefined,
      signal,
    );
    this.cacheSettingsCatalog(response, target);
    return response;
  }

  /**
   * One toggle by id, exactly as it was last seen. The composer footer paints
   * this on its FIRST frame: without it the reasoning chip is simply absent
   * until a round trip lands, on every session open, for a value that changes
   * once in a blue moon.
   */
  cachedSetting(id: string, target?: SettingsTarget): Toggle | null {
    return readSnapshot<Toggle>(this.settingsKey('setting', target, id));
  }

  /**
   * One toggle by id. `/v1/settings` only lists what the settings sheet shows,
   * so screen-owned knobs (reasoning effort lives in the composer footer) are
   * read one at a time here. The answer is snapshotted for the seed above.
   */
  async setting(id: string, signal?: AbortSignal, target?: SettingsTarget): Promise<Toggle> {
    const toggle = await this.request<Toggle>(
      'GET',
      `/v1/settings/${encodeURIComponent(id)}?${this.settingsQuery(target)}`,
      undefined,
      signal,
    );
    writeSnapshot(this.settingsKey('setting', target, id), toggle);
    return toggle;
  }

  async setSetting(
    id: string,
    action: 'toggle' | 'cycle' | 'value' | 'inherit',
    value?: import('./types').SettingValue,
    target?: SettingsTarget,
  ): Promise<Toggle> {
    const updated = await this.request<Toggle>('POST', '/v1/settings', {
      id,
      action,
      value,
      scope: target?.scope ?? 'global',
      target_id: target?.target_id,
    });
    // The by-id seed the composer reads is the same fact, so keep it in step —
    // otherwise cycling reasoning effort here would repaint the OLD word on the
    // next open until the revalidation landed.
    writeSnapshot(this.settingsKey('setting', target, id), updated);
    // Patch the one toggle that changed instead of dropping the snapshot, so
    // reopening the dialog paints the NEW value rather than a blank sheet.
    const cached = this.cachedSettings(target);
    if (cached) {
      writeSnapshot(this.settingsKey('settings', target), {
        ...cached,
        groups: (cached.groups ?? []).map((group) => ({
          ...group,
          toggles: replaceSetting(group.toggles, updated),
        })),
      });
    }
    for (const receive of settingListeners.get(this.base) ?? []) receive(updated);
    return updated;
  }

  /**
   * Run trusted extension code again where `target` runs. Machine settings reload
   * machine extensions only. Reading the catalog with `settings` never runs extension
   * code. Gateways without this route answer 404.
   */
  async reloadExtensions(target?: SettingsTarget): Promise<ExtensionReload> {
    return await this.request<ExtensionReload>('POST', '/v1/extensions/reload', {
      scope: target?.scope ?? 'global',
      target_id: target?.target_id,
    });
  }

  private hasSettingsCatalogRevision(data: SettingsResponse | null): data is SettingsResponse {
    return (
      !!data &&
      Array.isArray(data.groups) &&
      typeof data.revision === 'string' &&
      !!data.revision.trim()
    );
  }

  private cacheSettingsCatalog(saved: SettingsResponse, target?: SettingsTarget): void {
    if (!this.hasSettingsCatalogRevision(saved))
      throw new GatewayError(
        INCOMPATIBLE_STATUS,
        'This machine returned an invalid settings catalog. Update the gateway and reconnect.',
      );
    writeSnapshot(this.settingsKey('settings', target), saved);
    for (const group of saved.groups) {
      for (const setting of flattenSettings(group.toggles)) {
        writeSnapshot(this.settingsKey('setting', target, setting.id), setting);
      }
    }
  }

  rooms(signal?: AbortSignal): Promise<RoomsStatus> {
    return this.request('GET', '/v1/council/rooms', undefined, signal);
  }

  disconnectRooms(relay_url: string): Promise<RoomsStatus> {
    return this.request('POST', '/v1/council/rooms/disconnect', { relay_url });
  }

  joinRoom(invite_url: string): Promise<unknown> {
    return this.request('POST', '/v1/council/rooms/join', { invite_url });
  }

  registerRooms(relay_url: string, admin_token: string): Promise<RoomsStatus> {
    return this.request('POST', '/v1/council/rooms/register', { relay_url, admin_token });
  }

  createRoom(relay_url: string, name: string): Promise<CouncilRoom> {
    return this.request('POST', '/v1/council/rooms', { relay_url, name });
  }

  inviteToRoom(roomId: string): Promise<RoomInvitation> {
    return this.request('POST', `/v1/council/rooms/${encodeURIComponent(roomId)}/invites`, {});
  }

  revokeRoomInvite(roomId: string, inviteId: string): Promise<unknown> {
    return this.request('DELETE', `/v1/council/rooms/${encodeURIComponent(roomId)}/invites/${encodeURIComponent(inviteId)}`);
  }

  roomMembers(roomId: string): Promise<RoomMember[]> {
    return this.request('GET', `/v1/council/rooms/${encodeURIComponent(roomId)}/members`);
  }

  removeRoomMember(roomId: string, machineId: string): Promise<unknown> {
    return this.request('DELETE', `/v1/council/rooms/${encodeURIComponent(roomId)}/members/${encodeURIComponent(machineId)}`);
  }

  deleteRoom(roomId: string): Promise<unknown> {
    return this.request('DELETE', `/v1/council/rooms/${encodeURIComponent(roomId)}`);
  }

  // ── Improve: project issues and governed review ─────────────────

  improveSettings(signal?: AbortSignal): Promise<ImproveSettings> {
    return this.request('GET', '/v1/improve/settings', undefined, signal);
  }

  setImproveSettings(settings: Partial<ImproveSettings>): Promise<ImproveSettings> {
    return this.request('PATCH', '/v1/improve/settings', settings);
  }

  improveProjects(signal?: AbortSignal): Promise<GatewayOverview> {
    return this.request('GET', '/v1/projects/overview', undefined, signal);
  }

  improveRecords(projectId: string | null, after = 0, signal?: AbortSignal): Promise<ImprovePage> {
    const query = new URLSearchParams({
      project_id: projectId ?? '',
      after: String(after),
      limit: '200',
    });
    return this.request('GET', `/v1/improve?${query}`, undefined, signal);
  }

  improveRecord(id: number, signal?: AbortSignal): Promise<ImproveRecord> {
    return this.request('GET', `/v1/improve/${id}`, undefined, signal);
  }

  createImproveRecord(record: ImproveCreate): Promise<ImproveRecord> {
    return this.request('POST', '/v1/improve', record);
  }

  updateImproveRecord(id: number, changes: ImproveUpdate): Promise<ImproveRecord> {
    return this.request('PATCH', `/v1/improve/${id}`, changes);
  }

  reviewImprove(): Promise<unknown> {
    return this.request('POST', '/v1/improve/review', {});
  }

  // ── Automations: prompts that run on a schedule or a webhook ────

  automations(signal?: AbortSignal): Promise<AutomationList> {
    return this.request('GET', '/v1/automations', undefined, signal);
  }

  createAutomation(input: AutomationInput): Promise<Automation> {
    return this.request('POST', '/v1/automations', input);
  }

  automationRuns(automationId: string, signal?: AbortSignal): Promise<AutomationRunList> {
    const query = new URLSearchParams({ automation_id: automationId, limit: '20' });
    return this.request('GET', `/v1/automations/runs?${query}`, undefined, signal);
  }

  updateAutomation(automationId: string, changes: AutomationPatch): Promise<Automation> {
    return this.request('PATCH', `/v1/automations/${encodeURIComponent(automationId)}`, changes);
  }

  runAutomation(automationId: string): Promise<AutomationRun> {
    return this.request('POST', `/v1/automations/${encodeURIComponent(automationId)}/run`, {});
  }

  /** The only answer that carries a secret value. No other route reads it back. */
  createAutomationSecret(
    automationId: string,
    kind: AutomationSecretKind,
  ): Promise<AutomationSecret> {
    return this.request('POST', `/v1/automations/${encodeURIComponent(automationId)}/secrets`, {
      kind,
    });
  }

  deleteAutomation(automationId: string): Promise<unknown> {
    return this.request('DELETE', `/v1/automations/${encodeURIComponent(automationId)}`);
  }

  // ── Gateway-owned MCP servers ───────────────────────────────────

  /**
   * Last server list seen for THIS gateway — paint it, then revalidate.
   *
   * The panel opened on `null` every single time, so every visit to Settings
   * flashed an empty MCP band and moved the panels under it when the rows
   * landed. A row carries no secret — `McpServer` is the sanitized spec, env
   * and headers stay on the machine — so the answer from the last visit is the
   * honest first frame.
   */
  cachedMcpServers(target?: SettingsTarget): McpServer[] | null {
    return readSnapshot<McpServer[]>(this.settingsKey('mcp-servers', target));
  }

  async mcpServers(signal?: AbortSignal, target?: SettingsTarget): Promise<McpServer[]> {
    const servers =
      (await this.request<McpServersResponse>('GET', `/v1/mcp/servers?${this.settingsQuery(target)}`, undefined, signal))
        .servers ?? [];
    writeSnapshot(this.settingsKey('mcp-servers', target), servers);
    return servers;
  }

  /**
   * Keep the seed in step with a row this device just changed, so reopening the
   * panel paints what the press did instead of the state before it.
   */
  private rememberMcpServer(server: McpServer, target?: SettingsTarget): McpServer {
    const held = this.cachedMcpServers(target);
    if (held)
      writeSnapshot(
        this.settingsKey('mcp-servers', target),
        held.some((row) => row.name === server.name)
          ? held.map((row) => (row.name === server.name ? server : row))
          : [...held, server],
      );
    return server;
  }

  async saveMcpServer(name: string, server: McpServerInput, target?: SettingsTarget): Promise<McpServer> {
    return this.rememberMcpServer(
      await this.request<McpServer>('POST', '/v1/mcp/servers', { name, server, ...target }), target,
    );
  }

  async setMcpServerEnabled(name: string, enabled: boolean, target?: SettingsTarget): Promise<McpServer> {
    return this.rememberMcpServer(
      await this.request<McpServer>(
        'POST',
        `/v1/mcp/servers/${encodeURIComponent(name)}/actions/enable`,
        { enabled, ...target },
      ), target,
    );
  }

  async deleteMcpServer(name: string, target?: SettingsTarget): Promise<void> {
    await this.request('DELETE', `/v1/mcp/servers/${encodeURIComponent(name)}?${this.settingsQuery(target)}`);
    const held = this.cachedMcpServers(target);
    if (held)
      writeSnapshot(
        this.settingsKey('mcp-servers', target),
        held.filter((row) => row.name !== name),
      );
  }

  // Kill/start are RUNTIME ops, not config edits: they work for hand-written
  // servers too, because stopping a runaway child process is not rewriting
  // somebody's `vis.yml`. A kill holds until `startMcpServer`.
  async killMcpServer(name: string): Promise<McpServer> {
    return this.rememberMcpServer(
      await this.request<McpServer>(
        'POST',
        `/v1/mcp/servers/${encodeURIComponent(name)}/actions/kill`,
      ),
    );
  }

  async startMcpServer(name: string): Promise<McpServer> {
    return this.rememberMcpServer(
      await this.request<McpServer>(
        'POST',
        `/v1/mcp/servers/${encodeURIComponent(name)}/actions/start`,
      ),
    );
  }

  // Tokens and PKCE stay here on the paired gateway. Every client takes the
  // loopback callback: a phone's native receiver binds that port and opens the
  // browser itself; web/TUI clients use it directly. No relay option.
  async mcpAuthStart(name: string): Promise<McpAuthFlow> {
    return this.request<McpAuthFlow>(
      'POST',
      `/v1/mcp/servers/${encodeURIComponent(name)}/auth/start`,
      { callback_mode: 'loopback' },
    );
  }

  async mcpAuthComplete(name: string, flowId: string, input: string): Promise<McpAuthFlow> {
    return this.request<McpAuthFlow>(
      'POST',
      `/v1/mcp/servers/${encodeURIComponent(name)}/auth/complete`,
      { flow_id: flowId, input },
    );
  }

  async mcpAuthPoll(name: string, flowId: string): Promise<McpAuthFlow> {
    return this.request<McpAuthFlow>(
      'POST',
      `/v1/mcp/servers/${encodeURIComponent(name)}/auth/poll`,
      { flow_id: flowId },
    );
  }

  async mcpAuthCancel(name: string, flowId: string): Promise<void> {
    await this.request('POST', `/v1/mcp/servers/${encodeURIComponent(name)}/auth/cancel`, {
      flow_id: flowId,
    });
  }

  async mcpAuthLogout(name: string): Promise<McpAuthStatus> {
    return this.request<McpAuthStatus>(
      'POST',
      `/v1/mcp/servers/${encodeURIComponent(name)}/auth/logout`,
    );
  }

  async testMcpServer(name: string, server: McpServerInput): Promise<McpTestResult> {
    return this.request<McpTestResult>('POST', '/v1/mcp/servers/actions/test', {
      name,
      server,
    });
  }

  // ── Router: providers, models, auth ─────────────────────────────
  // `/v1/router` is the WHOLE picker payload in one call — the same one the
  // TUI's router dialog renders. Auth is driven step-by-step over HTTP: the
  // daemon owns the PKCE verifier, the device code, and the credential file,
  // so no token ever reaches this device.
  //
  // Assembling that payload costs the daemon a real auth/limits probe per
  // provider (seconds on a cold gateway), so the answer is cached here for
  // ROUTER_TTL_MS, shared by every screen, prefetched at connect time, and
  // served stale-while-revalidating: opening the picker paints instantly and
  // any refresh lands underneath. Every mutation below drops the entry.

  /**
   * Cached rows at ANY age — paint these first, then revalidate.
   *
   * The memory map is the hot layer; the snapshot under it is all a COLD start
   * has. Without it every relaunch opened Providers on `Checking provider
   * sign-in…` for a payload that only moves when somebody signs in or out.
   */
  cachedRouter(): RouterProvider[] | null {
    return (
      routerCache.get(this.base)?.rows ?? readSnapshot<RouterProvider[]>(this.snapshotKey('router'))
    );
  }

  /** True when the cached rows are younger than the TTL. */
  isRouterFresh(): boolean {
    const entry = routerCache.get(this.base);
    return !!entry && Date.now() - entry.at < ROUTER_TTL_MS;
  }

  /** Forget the cached fleet so the next read re-probes the daemon. */
  invalidateRouter(): void {
    routerCache.delete(this.base);
    routerInflight.delete(this.base);
    // The seed goes with it: a provider just signed out of must never paint as
    // signed in on the next open, which is exactly what a kept snapshot would
    // do until the re-probe landed.
    snapshots.delete(this.snapshotKey('router'));
  }

  /**
   * Warm the router cache in the background. Fire-and-forget: never throws,
   * never blocks a render, and collapses into any request already in flight.
   */
  prefetchRouter(): void {
    if (this.isRouterFresh()) return;
    void this.router().catch(() => undefined);
  }

  /**
   * Warm every settings panel of this machine, fire-and-forget.
   *
   * Settings is opened on ONE machine at a time but the fleet is swept, because
   * the flicker is a cold panel, not a slow one: whichever machine the reader
   * opens has already answered. Each read is TTL-stamped (`PANEL_TTL_MS`) and
   * `devices` has its own freshness window, so a wake costs nothing when the
   * fleet was swept a minute ago.
   */
  prefetchPanels(): void {
    if (Date.now() - (panelWarmed.get(this.base) ?? 0) < PANEL_TTL_MS) return;
    panelWarmed.set(this.base, Date.now());
    const quietly = (work: Promise<unknown>) => void work.catch(() => undefined);
    quietly(this.settings());
    quietly(this.capabilities());
    quietly(this.mcpServers());
    quietly(this.devices());
    this.prefetchRouter();
  }

  async router(signal?: AbortSignal, opts?: { force?: boolean }): Promise<RouterProvider[]> {
    const key = this.base;
    if (opts?.force) this.invalidateRouter();
    else {
      const entry = routerCache.get(key);
      if (entry && Date.now() - entry.at < ROUTER_TTL_MS) return entry.rows;
    }

    // One shared request per gateway: three screens opening at once cost the
    // daemon one probe, and an aborted caller never cancels the others.
    let inflight = routerInflight.get(key);
    if (!inflight) {
      inflight = this.request<{ providers: RouterProvider[] }>('GET', '/v1/router')
        .then((response) => {
          const rows = response.providers;
          routerCache.set(key, { at: Date.now(), rows });
          writeSnapshot(this.snapshotKey('router'), rows);
          return rows;
        })
        .finally(() => {
          routerInflight.delete(key);
        });
      routerInflight.set(key, inflight);
    }
    // The shared request is deliberately NOT tied to one caller's signal:
    // callers check `signal.aborted` after awaiting instead.
    void signal;
    return inflight;
  }

  // ── Fleet membership ────────────────────────────────────────────
  //
  // Adding a provider is a DAEMON operation: config and credentials live on the
  // machine that talks to the model, so the phone names a preset and the
  // gateway writes it. No key ever travels on these two calls.

  /**
   * Provider presets this machine can still add. The daemon answers with what
   * is NOT configured yet, so the picker can never offer a duplicate.
   */
  async providerPresets(signal?: AbortSignal): Promise<ProviderPreset[]> {
    const response = await this.request<{ presets?: ProviderPreset[] }>(
      'GET',
      '/v1/provider-presets',
      undefined,
      signal,
    );
    return response.presets ?? [];
  }

  /**
   * Put a preset into this machine's fleet. `baseUrl` only means anything for a
   * LOCAL preset, whose address the user owns. The answer IS the new fleet, so
   * the caller repaints from it instead of racing a second read.
   */
  async addProvider(providerId: string, baseUrl?: string): Promise<RouterProvider[]> {
    const response = await this.request<{ providers: RouterProvider[] }>('POST', '/v1/providers', {
      id: providerId,
      base_url: baseUrl,
    });
    this.invalidateRouter();
    return response.providers;
  }

  /**
   * Drop a provider AND its stored credential, and answer with the fleet that
   * remains.
   */
  async removeProvider(providerId: string): Promise<RouterProvider[]> {
    const response = await this.request<{ providers: RouterProvider[] }>(
      'DELETE',
      `/v1/providers/${encodeURIComponent(providerId)}`,
    );
    this.invalidateRouter();
    return response.providers;
  }

  async setDefaultModel(provider: string, model: string): Promise<void> {
    await this.request<{ default_provider: string; default_model: string }>('PATCH', '/v1/router', {
      role: 'primary',
      provider,
      model,
    });
    this.invalidateRouter();
  }

  /**
   * Tag the FALLBACK provider+model: the router's second root, used when the
   * default one cannot serve the turn. The daemon REFUSES a fallback on the
   * default's own provider (400) — a fallback is only useful somewhere else.
   */
  async setFallbackModel(provider: string, model: string): Promise<void> {
    await this.request<{ fallback_provider: string; fallback_model: string }>(
      'PATCH',
      '/v1/router',
      {
        role: 'fallback',
        provider,
        model,
      },
    );
    this.invalidateRouter();
  }

  /** Drop the fallback tag: a blank `provider` on the fallback role clears it. */
  async clearFallbackModel(): Promise<void> {
    await this.request<{ fallback_provider: string | null }>('PATCH', '/v1/router', {
      role: 'fallback',
      provider: '',
      model: '',
    });
    this.invalidateRouter();
  }

  /**
   * Set the default thinking of sessions on `provider`. `setting` is
   * `reasoning_level` or `reasoning_effort`; a null `value` removes the default.
   * A session that has its own value keeps it.
   */
  async setProviderThinking(
    provider: string,
    setting: 'reasoning_level' | 'reasoning_effort',
    value: string | null,
  ): Promise<void> {
    await this.request<{ providers: RouterProvider[] }>('PATCH', '/v1/router', {
      role: 'thinking',
      provider,
      setting,
      value: value ?? '',
    });
    this.invalidateRouter();
  }

  /**
   * This session's pinned provider/model as last seen — the header chip's first
   * frame. `null` here means BOTH "no pin" and "never read"; either way the
   * fetch below is still issued and reconciles on top.
   *
   * The session LIST already carries `model_pref` per row (the gateway soul
   * reads it off the same `session_soul` row), so `seedSessionModels` normally
   * fills this before a session is ever opened; the cached list is the fallback
   * for a seed evicted from the snapshot store.
   */
  cachedSessionModel(sid: string): ModelPref | null {
    const seeded = readSnapshot<ModelPref>(this.snapshotKey('model', sid));
    if (seeded) return seeded;
    return this.cachedSessions()?.find((row) => row.id === sid)?.model_pref ?? null;
  }

  /**
   * Record each row's pin as the per-session seed. Rows without one only clear a
   * seed that exists, so an unpinned fleet does not fill the snapshot store with
   * nulls it would then have to persist.
   */
  private seedSessionModels(rows: Session[]): void {
    for (const row of rows) {
      if (!row?.id) continue;
      const key = this.snapshotKey('model', row.id);
      const pref = row.model_pref ?? null;
      if (pref || readSnapshot<ModelPref>(key)) writeSnapshot(key, pref);
    }
  }

  async sessionModel(sid: string, signal?: AbortSignal): Promise<ModelPref | null> {
    const response = await this.request<{ model?: ModelPref }>(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}/model`,
      undefined,
      signal,
    );
    const pref = response.model ?? null;
    writeSnapshot(this.snapshotKey('model', sid), pref);
    return pref;
  }

  /**
   * Record a pin this client did NOT write — the gateway's own
   * `session.model_updated` broadcast, raised whenever ANOTHER surface repoints
   * the same session (the TUI picker, a second device, an embedded caller).
   *
   * The gateway is the ONE writer of the pin, so the snapshot has to follow its
   * broadcast: it is the header chip's first frame, and leaving it stale paints
   * a reopened session with a model that session no longer runs on.
   *
   * Blank provider AND model = the override was cleared (`state.clj` labels a
   * cleared pref as empty strings), so store `null` — the chip then falls back
   * to the gateway default instead of rendering an empty pin.
   */
  noteSessionModel(sid: string, pref: ModelPref | null): ModelPref | null {
    const provider = pref?.provider?.trim() || undefined;
    const model = pref?.model?.trim() || undefined;
    const next = provider || model ? { provider, model } : null;
    writeSnapshot(this.snapshotKey('model', sid), next);
    return next;
  }

  /**
   * The default as last seen — same first-frame job as above. With `sid`, this
   * session's own default comes first: a project `.vis/config.yml` overlay can set it.
   */
  cachedDefaultModel(sid?: string): ModelPref | null {
    return (
      (sid ? readSnapshot<ModelPref>(this.snapshotKey('model-default', sid)) : null) ??
      readSnapshot<ModelPref>(this.snapshotKey('model-default'))
    );
  }

  /**
   * The DEFAULT provider+model — what a session with no pin actually runs on.
   * `sessionModel` answers only the explicit pin (null for "default"), so any
   * surface that names the live model needs this fallback.
   *
   * With `sid`, the session's own `/model` answer comes first. A project
   * `.vis/config.yml` overlay can set that session's default, and the gateway
   * default does not show it (issue #311). A failed read, or a gateway that names
   * no session default, falls back to the gateway default.
   *
   * The gateway default rides `/v1/router`, which is a real auth/limits probe per
   * provider on a cold daemon — seconds. Hence the snapshot: the chip names the
   * model at once and this answer only ever corrects it.
   */
  async defaultModel(signal?: AbortSignal, sid?: string): Promise<ModelPref | null> {
    if (sid) {
      const own = await this.request<{ default?: ModelPref | null } | null>(
        'GET',
        `/v1/sessions/${encodeURIComponent(sid)}/model`,
        undefined,
        signal,
      ).then(
        (response) => (response?.default?.model ? response.default : null),
        () => undefined,
      );
      const key = this.snapshotKey('model-default', sid);
      if (own !== undefined && (own || readSnapshot<ModelPref>(key))) writeSnapshot(key, own);
      if (own) return own;
    }
    const rows = await this.router(signal);
    const row =
      rows.find((p) => p.is_default && p.default_model) ?? rows.find((p) => p.default_model);
    if (!row?.default_model) return null;
    const pref = { provider: row.id, model: row.default_model };
    writeSnapshot(this.snapshotKey('model-default'), pref);
    return pref;
  }

  async setSessionModel(sid: string, provider: string, model: string): Promise<ModelPref | null> {
    const response = await this.request<{ model?: ModelPref }>(
      'PATCH',
      `/v1/sessions/${encodeURIComponent(sid)}/model`,
      { provider, model },
    );
    const pref = response.model ?? null;
    writeSnapshot(this.snapshotKey('model', sid), pref);
    return pref;
  }

  /** Begin OAuth. Device and reachable browser callbacks finish through polling. */
  startProviderAuth(providerId: string): Promise<AuthFlow> {
    return this.request<AuthFlow>(
      'POST',
      `/v1/providers/${encodeURIComponent(providerId)}/auth/start`,
    );
  }

  async completeProviderAuth(
    providerId: string,
    flowId: string,
    redirectUrl: string,
  ): Promise<AuthVerdict> {
    const verdict = await this.request<AuthVerdict>(
      'POST',
      `/v1/providers/${encodeURIComponent(providerId)}/auth/complete`,
      { flow_id: flowId, redirect_url: redirectUrl },
    );
    this.invalidateRouter();
    return verdict;
  }

  /** Finish an `api-key` flow: the DAEMON persists the key in its own config. */
  async submitProviderKey(
    providerId: string,
    flowId: string,
    apiKey: string,
  ): Promise<AuthVerdict> {
    const verdict = await this.request<AuthVerdict>(
      'POST',
      `/v1/providers/${encodeURIComponent(providerId)}/auth/complete`,
      { flow_id: flowId, api_key: apiKey },
    );
    this.invalidateRouter();
    return verdict;
  }

  async pollProviderAuth(providerId: string, flowId: string): Promise<AuthVerdict> {
    const verdict = await this.request<AuthVerdict>(
      'POST',
      `/v1/providers/${encodeURIComponent(providerId)}/auth/poll`,
      { flow_id: flowId },
    );
    // A settled verdict changed the daemon's credentials; a pending one did not.
    if (verdict?.status !== 'pending') this.invalidateRouter();
    return verdict;
  }

  cancelProviderAuth(providerId: string, flowId: string): Promise<AuthVerdict> {
    return this.request<AuthVerdict>(
      'POST',
      `/v1/providers/${encodeURIComponent(providerId)}/auth/cancel`,
      { flow_id: flowId },
    );
  }

  /**
   * Re-probe ONE provider's auth state live (`GET /v1/providers/:id/status`).
   *
   * The fleet answer is cached for minutes; a status check is the user asking
   * "is this still signed in RIGHT NOW", so it bypasses that cache and folds
   * the fresh verdict back into the cached row — no full re-probe of every
   * provider, and no screen left painting the stale dot.
   */
  async providerStatus(providerId: string, signal?: AbortSignal): Promise<ProviderStatus> {
    const response = await this.request<{ status?: ProviderStatus }>(
      'GET',
      `/v1/providers/${encodeURIComponent(providerId)}/status`,
      undefined,
      signal,
    );
    const status = response.status;
    if (!status) throw new Error(`Provider status response is missing status for ${providerId}`);
    this.mergeCachedProvider(providerId, { status });
    return status;
  }

  /** Live quota report for one provider (`GET /v1/providers/:id/limits`). */
  async providerLimits(providerId: string, signal?: AbortSignal): Promise<ProviderLimits> {
    const response = await this.request<{ report?: ProviderLimits }>(
      'GET',
      `/v1/providers/${encodeURIComponent(providerId)}/limits`,
      undefined,
      signal,
    );
    const limits = response.report ?? {};
    this.mergeCachedProvider(providerId, { limits });
    for (const receive of providerLimitsListeners.get(this.base) ?? []) receive(providerId, limits);
    return limits;
  }

  /** Share live quota reads with mounted provider views on this gateway. */
  onProviderLimits(receive: (providerId: string, limits: ProviderLimits) => void): () => void {
    let listeners = providerLimitsListeners.get(this.base);
    if (!listeners) {
      listeners = new Set();
      providerLimitsListeners.set(this.base, listeners);
    }
    listeners.add(receive);
    return () => {
      listeners.delete(receive);
      if (!listeners.size) providerLimitsListeners.delete(this.base);
    };
  }

  private providerResetKey(providerId: string, accountId: string): string {
    return `vis.provider-reset:${JSON.stringify([this.base, providerId, accountId])}`;
  }

  hasPendingProviderReset(providerId: string, accountId: string): boolean {
    try {
      return !!localStorage.getItem(this.providerResetKey(providerId, accountId));
    } catch {
      return false;
    }
  }

  /** Persist BEFORE sending: an uncertain response must never become another spend. */
  async consumeProviderResetCredit(
    providerId: string,
    accountId: string,
  ): Promise<ProviderResetOutcome> {
    if (!providerId.trim() || !accountId.trim())
      throw new Error('Select an authenticated account first.');
    const key = this.providerResetKey(providerId, accountId);
    const pending = providerResetInflight.get(key);
    if (pending) return pending;
    let idempotencyKey: string;
    try {
      idempotencyKey = localStorage.getItem(key) || randomUuid();
      localStorage.setItem(key, idempotencyKey);
    } catch {
      throw new Error('Cannot safely save a reset attempt on this device. No reset was requested.');
    }
    const attempt = (async () => {
      try {
        const result = await this.request<{ outcome?: ProviderResetOutcome }>(
          'POST',
          `/v1/providers/${encodeURIComponent(providerId)}/reset-credits/consume`,
          { account_id: accountId, idempotency_key: idempotencyKey },
        );
        if (
          !result.outcome ||
          !['reset', 'nothing_to_reset', 'no_credit', 'already_redeemed'].includes(result.outcome)
        ) {
          throw new Error(
            'Reset could not be confirmed. Retry the same request to check its result.',
          );
        }
        localStorage.removeItem(key);
        return result.outcome;
      } finally {
        this.invalidateRouter();
        providerResetInflight.delete(key);
      }
    })();
    providerResetInflight.set(key, attempt);
    return attempt;
  }

  /** Keep the shared router cache honest after a single-provider re-probe. */
  private mergeCachedProvider(providerId: string, patch: Partial<RouterProvider>): void {
    const entry = routerCache.get(this.base);
    if (!entry) return;
    routerCache.set(this.base, {
      at: entry.at,
      rows: entry.rows.map((row) => (row.id === providerId ? { ...row, ...patch } : row)),
    });
  }

  async slashes(sid: string, signal?: AbortSignal): Promise<SlashCommand[]> {
    const response = await this.request<{ commands: SlashCommand[] }>(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}/slashes`,
      undefined,
      signal,
    );
    return response.commands ?? [];
  }

  // GET /v1/sessions/:sid/suggest?kind=file&q= — the SHARED fuzzy file index
  // (fff) behind the TUI `@` picker and the grep tool. Returns ranked
  // relative paths with size/age/git-status meta.
  async suggestFiles(sid: string, query: string, signal?: AbortSignal): Promise<FileSuggestion[]> {
    const rows = await this.request<FileSuggestion[]>(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}/suggest?kind=file&q=${encodeURIComponent(query)}`,
      undefined,
      signal,
    );
    return rows ?? [];
  }

  // POST /v1/sessions/:sid/fs/actions/open — open one workspace file in the
  // editor ON THE MACHINE THAT RUNS THE SESSION. A path in a transcript names a
  // file on that machine, so the press travels back instead of looking for a
  // tree this device does not have.
  async openPath(
    sid: string,
    path: string,
    signal?: AbortSignal,
  ): Promise<{ path: string; is_open: boolean }> {
    return await this.request<{ path: string; is_open: boolean }>(
      'POST',
      `/v1/sessions/${encodeURIComponent(sid)}/fs/actions/open`,
      { path },
      signal,
    );
  }

  // GET /v1/sessions/:sid/fs/file — the LINES of one workspace file, around the
  // line a pressed path named. The editor opens where the files are; this is what
  // the device doing the reading can show for itself, phone included. The gateway
  // answers one bounded window: clipped lines, a byte cap, and no binary file.
  async readPath(
    sid: string,
    path: string,
    line?: number,
    signal?: AbortSignal,
  ): Promise<FileWindow> {
    const at = line === undefined ? '' : `&line=${encodeURIComponent(String(line))}`;
    return await this.request<FileWindow>(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}/fs/file?path=${encodeURIComponent(path)}${at}`,
      undefined,
      signal,
    );
  }

  // ── Sessions ────────────────────────────────────────────────────
  //
  // The list, one session's meta, its transcript and its queued backlog are each
  // snapshotted per gateway. A screen reads its snapshot synchronously while
  // mounting (instant frame, no white flash) and these same calls refresh it
  // underneath — so navigation only ever changes what actually changed.

  /** Last session list seen for this gateway. */
  cachedSessions(): Session[] | null {
    const rows = readSnapshot<Session[]>(this.snapshotKey('sessions'));
    return rows ? this.withoutDeletedSessions(rows) : null;
  }

  /** Last meta row seen for ONE session. */
  cachedSession(sid: string): Session | null {
    return this.isSessionDeleted(sid) ? null : readSnapshot<Session>(this.snapshotKey('session', sid));
  }

  /**
   * The fullest row this device holds for one session: its own snapshot, else
   * its row in the list window.
   *
   * A lean payload is reconciled against THIS, not against the per-session
   * snapshot alone. On a cold start that snapshot does not exist yet, so opening
   * a session took the lean row as the whole truth and the facts only the list
   * carries (`LIST_ONLY_SESSION_KEYS`) were missing from the cached row until
   * the next list read landed.
   */
  private heldSessionRow(sid: string): Session | null {
    return this.cachedSession(sid) ?? this.cachedSessions()?.find((row) => row.id === sid) ?? null;
  }

  /** Last transcript seen for ONE session. Reading it renews its LRU position. */
  cachedTranscript(sid: string): TranscriptTurn[] | null {
    return this.isSessionDeleted(sid)
      ? null
      : readSnapshot<TranscriptTurn[]>(this.snapshotKey('transcript', sid));
  }

  /**
   * Warm one session's newest transcript page. Concurrent list polls and an opening
   * screen share the same flight; a newer row queues one re-check behind an older one.
   * `urgent` reads it ahead of queued polls, for a reader about to open the session.
   */
  private prefetchTranscript(row: Session, urgent = false): Promise<boolean> {
    const key = this.snapshotKey('transcript', row.id);
    const warming = transcriptPrefetches.get(key);
    if (warming) return warming.then(() => this.prefetchTranscript(row, urgent));

    const held = this.cachedSession(row.id);
    const merged = reconcileSession(
      held,
      row,
      readSnapshot<SessionGoal>(this.snapshotKey('goal', row.id)),
    );
    if (merged !== held) writeSnapshot(this.snapshotKey('session', row.id), merged);
    const stamp = transcriptPrefetchStamp(merged);
    // A running placeholder is provisional to an OPEN screen, which must read its
    // newest trace. It is not permission for the list poll to download that page forever.
    if (this.cachedTranscript(row.id) !== null && transcriptPrefetchStamps.get(key) === stamp)
      return Promise.resolve(true);

    const task = this.transcriptIfMoved(row.id, merged, undefined, urgent).then(
      () => {
        transcriptPrefetchStamps.set(key, stamp);
        return true;
      },
      () => false,
    );
    transcriptPrefetches.set(key, task);
    void task.then(() => {
      if (transcriptPrefetches.get(key) === task) transcriptPrefetches.delete(key);
    });
    return task;
  }

  /**
   * Read one session's newest transcript page because the reader is reaching for
   * its row: a pointer resting on it, or a press. The read goes ahead of queued
   * polls, and the screen that opens the session joins it through
   * `openingTranscript` instead of asking a second time.
   */
  warmTranscript(row: Session): void {
    void this.prefetchTranscript(row, true);
  }

  /** Prepare an unread row near the viewport and keep its answer until the row leaves. */
  retainTranscript(row: Session): () => void {
    const key = this.snapshotKey('transcript', row.id);
    retainedTranscripts.set(key, (retainedTranscripts.get(key) ?? 0) + 1);
    void this.prefetchTranscript(row, true);
    let retained = true;
    return () => {
      if (!retained) return;
      retained = false;
      const remaining = (retainedTranscripts.get(key) ?? 1) - 1;
      if (remaining > 0) retainedTranscripts.set(key, remaining);
      else retainedTranscripts.delete(key);
      trimSnapshotKind(key);
      trimSnapshotKind(this.snapshotKey('session', row.id));
      scheduleSnapshotFlush(snapshotStores);
    };
  }

  /** Reserve the cache for visible rows before spending spare slots on list prefetch. */
  private transcriptWarmRows(rows: readonly Session[], accepts: (row: Session) => boolean): Session[] {
    const prefix = `${this.snapshotKey('transcript')}\u0000`;
    const retained = new Set([...retainedTranscripts.keys()].filter((key) => key.startsWith(prefix)));
    let spare = Math.max(0, SESSION_CACHE_LIMIT - retained.size);
    const selected: Session[] = [];
    const seen = new Set<string>();
    for (const row of rows) {
      if (!row.id || seen.has(row.id) || !accepts(row)) continue;
      seen.add(row.id);
      if (!retained.has(this.snapshotKey('transcript', row.id))) {
        if (spare === 0) continue;
        spare -= 1;
      }
      selected.push(row);
    }
    return selected;
  }

  /** Pull active sessions into the rolling cache without displacing visible answers. */
  private prefetchActiveTranscripts(rows: readonly Session[]): void {
    for (const row of this.transcriptWarmRows(rows, sessionIsActive))
      void this.prefetchTranscript(row);
  }

  /**
   * Warm the newest page of each row that shows NEW, within the cache budget. Only a row
   * that settled after the reader's `previous` window holds the list back: the reader
   * already sees that window, so NEW never outruns its transcript there. A failed warm
   * keeps the previous window visible, and the next poll tries again. Other rows warm
   * behind the list, so a first read paints without waiting for every unread answer.
   */
  private async prefetchSettledTranscripts(
    previous: readonly Session[] | null,
    rows: readonly Session[],
  ): Promise<boolean> {
    const before = new Map(previous?.map((row) => [row.id, row]));
    const selected = this.transcriptWarmRows(rows, (row) => sessionHasNewAnswer(before.get(row.id), row));
    const held: Promise<boolean>[] = [];
    for (const row of selected) {
      const warming = this.prefetchTranscript(row);
      if (sessionSettled(before.get(row.id), row)) held.push(warming);
    }
    return (await Promise.all(held)).every(Boolean);
  }
  /** Last queued backlog seen for ONE session. */
  cachedQueuedTurns(sid: string): QueuedTurn[] | null {
    return readSnapshot<QueuedTurn[]>(this.snapshotKey('queued', sid));
  }

  /** Last queue hold paired with `cachedQueuedTurns`, including an explicit clear. */
  cachedQueuePaused(sid: string): QueuePausedInfo | null {
    return this.queuePaused.get(sid) ?? null;
  }

  /**
   * The running-turn bubble of ONE session, as it was last painted.
   *
   * MEMORY ONLY, on purpose: this is written on every streamed delta, and
   * `writeSnapshot` re-serialises the whole store to `localStorage`. It also has
   * no business surviving a cold start — a turn that was running when the
   * process died is re-adopted from the gateway, not from a stale cache. It
   * exists so that LEAVING and RE-ENTERING a session inside one process repaints
   * the in-flight answer instantly, instead of showing the previous turn's
   * ending until a replay or a refetch lands on top of it.
   *
   * `seq` is the gateway's per-session journal cursor of the newest event folded
   * into `turn`, so the reader can drop a replay it has already applied.
   */
  cachedRunningTurn<T>(sid: string): { turn: T; seq: number } | null {
    return readSnapshot<{ turn: T; seq: number }>(this.snapshotKey('running-turn', sid));
  }

  rememberRunningTurn(sid: string, turn: unknown, seq: number): void {
    if (this.isSessionDeleted(sid)) return;
    const key = this.snapshotKey('running-turn', sid);
    snapshots.delete(key);
    if (turn === null) return;
    snapshots.set(key, { turn, seq });
    trimSnapshotKind(key);
  }

  /**
   * The image bytes THIS device sent with one turn, by turn id.
   *
   * The live rail and the queue mirror ship attachment DESCRIPTORS, never pixels
   * (`attachment_previews`), so until the turn is persisted and refetched the
   * sender's own copy is the only thing that can paint the picture. It lives on
   * the CLIENT rather than in screen state because leaving the session unmounts
   * the screen: that is why images sent to a still-running turn came back empty
   * after stepping out of the session and back in.
   *
   * Memory only and bounded, like the running-turn bubble: base64 pixels have no
   * business in `localStorage`, and a turn this far back is settled anyway —
   * from then on the persisted row owns its images.
   */
  private readonly sentAttachments = new Map<string, GatewayAttachment[]>();
  private static readonly SENT_ATTACHMENT_CACHE = 8;

  rememberSentAttachments(sid: string, tid: string | undefined, sent: GatewayAttachment[]): void {
    if (!tid || !sent.length) return;
    const key = `${sid}\u0000${tid}`;
    // Re-insert so the newest turn is always last in iteration order.
    this.sentAttachments.delete(key);
    this.sentAttachments.set(key, sent);
    while (this.sentAttachments.size > GatewayClient.SENT_ATTACHMENT_CACHE) {
      const oldest = this.sentAttachments.keys().next();
      if (oldest.done) break;
      this.sentAttachments.delete(oldest.value);
    }
  }

  cachedSentAttachments(sid: string, tid: string | undefined): GatewayAttachment[] | undefined {
    if (!tid) return undefined;
    return this.sentAttachments.get(`${sid}\u0000${tid}`);
  }

  /**
   * The same bytes, from the GATEWAY — `GET /v1/sessions/:sid/turns/:tid/attachments`.
   *
   * `rememberSentAttachments` only ever covers the device that did the sending,
   * and only until that process dies. Restart the app (or open the session on
   * another device) while the turn is still running and the user bubble painted
   * its text with the pictures missing, because the live rail ships byte-free
   * chips and the persisted row does not exist yet. The gateway has held the
   * bytes the whole time, so ask it, and fold the answer into the same cache the
   * sender's own copy lives in.
   *
   * In-flight requests are shared, and the entry is dropped once settled: a turn
   * mid-hand-off can legitimately answer empty, and the next mount must be free
   * to ask again.
   */
  async fetchTurnAttachments(
    sid: string,
    tid: string | undefined,
    signal?: AbortSignal,
    refresh = false,
  ): Promise<GatewayAttachment[]> {
    if (!tid) return [];
    const cached = this.cachedSentAttachments(sid, tid);

    if (!refresh && cached?.length) return cached;
    const key = `${sid}\u0000${tid}`;
    const inflight = this.attachmentFetches.get(key);
    if (inflight) return inflight;
    const pending = (async () => {
      const metadataOnly = refresh && !!cached?.length;
      const path = `/v1/sessions/${encodeURIComponent(sid)}/turns/${encodeURIComponent(tid)}/attachments`;
      const res = await this.request<{ attachments?: GatewayAttachment[] }>(
        'GET',
        metadataOnly ? `${path}?transcription_only=true` : path,
        undefined,
        signal,
      );
      const received = res.attachments ?? [];
      const rows = metadataOnly
        ? cached.map((base, index) => ({
            ...base,
            ...received[index],
            base64: base.base64,
          }))
        : received.filter((row) => !!row?.base64);
      this.rememberSentAttachments(sid, tid, rows);
      return rows;
    })();
    this.attachmentFetches.set(key, pending);
    void pending.then(
      () => this.attachmentFetches.delete(key),
      () => this.attachmentFetches.delete(key),
    );
    return pending;
  }

  private readonly attachmentFetches = new Map<string, Promise<GatewayAttachment[]>>();

  /**
   * Drop ONE row from the cached backlog.
   *
   * A row leaves the queue on `turn.queued.drained` / `.deleted`, and the gateway
   * appends both with `:store? false` — they are LIVE-only frames that no replay
   * and no snapshot ever repeats. So a removal must also be written into the
   * cache the next mount seeds from, or re-entering the session paints a
   * "Queued" row for a turn that is already running.
   */
  forgetQueuedTurn(sid: string, tid: string): void {
    const key = this.snapshotKey('queued', sid);
    const rows = readSnapshot<QueuedTurn[]>(key);
    if (!rows) return;
    const next = rows.filter((row) => row.turnId !== tid);
    if (next.length !== rows.length) writeSnapshot(key, next);
  }

  /** Drop every snapshot of one session — it is gone or is being replaced. */
  forgetSession(sid: string): void {
    this.forgetProjectHeads(sid, this.cachedSession(sid)?.workspace?.root);
    const transcriptKey = this.snapshotKey('transcript', sid);
    snapshots.delete(this.snapshotKey('session', sid));
    dropSnapshot(transcriptKey);
    snapshots.delete(this.snapshotKey('queued', sid));
    snapshots.delete(this.snapshotKey('running-turn', sid));
    snapshots.delete(this.snapshotKey('model', sid));
    // A session nothing holds any more has no stream left to resume either.
    if (sessionCursors.delete(this.snapshotKey('cursor', sid))) scheduleCursorFlush();
    for (const key of Array.from(this.sentAttachments.keys())) {
      if (key.startsWith(`${sid}\u0000`)) this.sentAttachments.delete(key);
    }
    for (const key of Array.from(this.attachmentFetches.keys())) {
      if (key.startsWith(`${sid}\u0000`)) this.attachmentFetches.delete(key);
    }
    for (const key of Array.from(this.turnTraces.keys())) {
      if (key.startsWith(`${sid}\u0000`)) this.turnTraces.delete(key);
    }
    scheduleSnapshotFlush(snapshotStores);
  }

  /**
   * The session list — this machine's HEAD WINDOW, revalidated rather than
   * re-downloaded.
   *
   * What this device holds of a machine is its newest `SESSIONS_PAGE` rows and
   * nothing below them. It used to be every row: the walk cost one conditional round
   * trip per window — measured against a 1192-session store, 12 serial requests every
   * ten seconds per machine, eleven of them proving nothing had changed — and the
   * ~315 KB it drained in was only ever re-cut into a page of ten. Every number that
   * needed the whole list is answered BESIDE this window now: `total` and the
   * per-project counts in `overview`, and a project's own page from `listProjectPage`.
   * Nothing on this device asks for the
   * fleet any more.
   *
   * - **Conditional GET.** The window carries a weak `ETag`, so an unchanged list
   *   costs one 304 with an empty body: nothing transferred, nothing parsed, nothing
   *   reconciled, and the SAME array handed back, which React bails out on.
   * - **The device's overlay.** `dirty=` names the sessions holding unsent words in
   *   this device's composer — the one thing about its own list the gateway cannot
   *   see — and it is part of the key the validator is pinned under, so a changed
   *   overlay is never answered from a stale window.
   *
   * `total` rides INSIDE the validator (`server/sessions-etag`) together with the
   * overview, so an unchanged head is also the gateway's word that the count printed
   * under every project header is still true.
   */
  async listSessions(signal?: AbortSignal): Promise<Session[]> {
    // THE ONE FACT THE GATEWAY CANNOT KNOW (see `dirtySessionIds`).
    //
    // Which sessions this list holds, and in what order, is the gateway's own
    // answer now — except for words parked in THIS device's composer, which
    // exist nowhere else. They ride down as `dirty=`, so an untitled session
    // holding unsent work is kept and banded THERE instead of being hidden and
    // rescued back here. Hydration is awaited because a first read that forgot
    // the overlay would paint the list without those rows and move them in a
    // second later; it is one shared promise, and a silent storage bridge
    // answers it with nothing rather than hanging (see `lib/bridge`).
    await hydrateDraftMessages();
    const overlay = dirtySessionIds(this.base).join(',');
    const key = this.snapshotKey('sessions');
    // A VALIDATOR BELONGS TO THE QUESTION IT ANSWERED: an ETag issued for one
    // overlay cannot answer a list asked for with another. So the overlay is
    // part of the key every pin is held under, while the ROWS keep the plain
    // key — a cold start still paints what it has.
    const pinKey = overlay ? `${key}\u0000${overlay}` : key;
    const durablePin = overlay
      ? `${this.snapshotKey('sessions-pin')}\u0000${overlay}`
      : this.snapshotKey('sessions-pin');
    const cached = this.cachedSessions();
    // Rows saved by an earlier run only paint the cold start (see the warm below).
    const listed = listedSessions.has(key);
    let pinned = GatewayClient.sessionsValidators.get(pinKey);
    // A webview kill clears the in-memory pin but not the rows it described. Put the
    // durable head ETag back onto those exact rows, so the first cold-start request
    // can be a 304 instead of re-downloading the window.
    if (!pinned && cached?.length) {
      const persisted = readSnapshot<{ etag?: unknown; total?: unknown }>(durablePin);
      // The rows on disk have to BE the window that validator was issued for: the
      // head of a list of `total`, which is the whole of a short list and
      // `SESSIONS_PAGE` of a long one.
      if (
        typeof persisted?.etag === 'string' &&
        persisted.etag &&
        typeof persisted.total === 'number' &&
        cached.length === Math.min(SESSIONS_PAGE, persisted.total)
      ) {
        pinned = {
          full: cached,
          windows: new Map([
            [
              HEAD_CURSOR,
              {
                etag: persisted.etag,
                after: HEAD_CURSOR,
                rows: cached,
                total: persisted.total,
                overview: this.overview,
              },
            ],
          ]),
        };
        GatewayClient.sessionsValidators.set(pinKey, pinned);
      }
    }
    // Only ever ask conditionally when a 304 can actually be ANSWERED from the rows
    // that validator was issued for.
    const known =
      pinned && pinned.full === cached ? pinned.windows : new Map<string, SessionsWindow>();

    const fetchWindow = async (after: string): Promise<SessionsWindow> => {
      const pin = known.get(after);
      const res = await this.requestFull<{
        sessions?: Session[];
        total?: number;
        overview?: GatewayOverview;
      }>(
        'GET',
        `/v1/sessions?order=recent&limit=${SESSIONS_PAGE}${
          after ? `&after=${encodeURIComponent(after)}` : ''
        }${overlay ? `&dirty=${encodeURIComponent(overlay)}` : ''}`,
        undefined,
        signal,
        pin ? { 'If-None-Match': pin.etag } : undefined,
      );
      if (res.status === 304 && pin) return pin;
      const rows = res.data?.sessions ?? [];
      const overview = after === HEAD_CURSOR ? (res.data?.overview ?? null) : null;
      if (after === HEAD_CURSOR) {
        this.overview = overview;
        writeSnapshot(this.snapshotKey('projects-overview'), overview);
      }
      // Every row names the model it runs on, so opening any of them paints the
      // right chip on the FIRST frame instead of after a per-session round trip.
      this.seedSessionModels(rows);
      return {
        etag: res.etag ?? '',
        after,
        rows,
        total: res.data?.total ?? rows.length,
        overview,
      };
    };

    const head = await fetchWindow(HEAD_CURSOR);
    listedSessions.add(key);
    // The stable project totals ride BESIDE the window and are complete there,
    // whatever depth this device is holding.
    this.overview = head.overview;
    // Active work warms in the background. A row that FINISHED while this run watched
    // is different: wait for its newest page before returning it, so NEW never outruns
    // its transcript. Rows seen for the first time, and rows saved by an earlier run,
    // warm behind the list: a cold start that waited for them painted seconds late.
    const visible = head.rows;
    this.prefetchActiveTranscripts(visible);
    if (!(await this.prefetchSettledTranscripts(listed ? cached : null, visible)))
      return this.withoutDeletedSessions(cached ?? []);

    // AN UNCHANGED WINDOW IS THE SAME ARRAY, NOT AN EQUAL ONE.
    //
    // A 304 is answered from the pin, so `head` IS the pinned window and its rows are
    // the very objects the screen is already rendering. Handing them back is what lets
    // React bail out of the whole list: a poll that changed nothing re-renders no row.
    const headPin = known.get(HEAD_CURSOR);
    if (headPin && head === headPin) return this.withoutDeletedSessions(headPin.rows);

    const rows = reconcileRows(cached, this.withoutDeletedSessions(head.rows));
    writeSnapshot(key, rows);
    // Pin the window onto the RECONCILED rows, so a later 304 restores the identities
    // the screen is rendering instead of the raw wire copies.
    if (head.etag) {
      GatewayClient.sessionsValidators.set(pinKey, {
        full: rows,
        windows: new Map([[HEAD_CURSOR, { ...head, rows }]]),
      });
      writeSnapshot(durablePin, { etag: head.etag, total: head.total });
    } else {
      GatewayClient.sessionsValidators.delete(pinKey);
      writeSnapshot(durablePin, null);
    }
    return rows;
  }

  /**
   * ONE PROJECT'S PAGE, CUT BY WHOEVER OWNS THE ORDER.
   *
   * `root` names the project, `limit` is the page the SCREEN measured
   * (`useSessionsPerPage`) and `after` is the cursor of the row the page before it
   * ended on — the same keyset window the fleet walk uses
   * (`state/list-sessions-page`). Back come the rows, the project's own `total`,
   * and the cursor of the page after this one.
   *
   * The pager used to print arithmetic over rows this device had filtered and
   * re-ordered for itself: the gateway counted 1034 sessions in a project this
   * list painted 763 of, which put its last page 27 pages beyond the pager's, and
   * the reader saw the last page paint three rows and swap them for ten. The
   * device decides nothing about the list any more — `dirty=` is the one fact it
   * adds — so a header's count and the pages under it are one arithmetic again,
   * and no page needs the whole fleet downloaded first.
   *
   * A JUMP IS STILL ONE REQUEST. A cursor names a ROW, so a page can only be asked
   * for from a row the page before it ended on; a reader who taps a number they
   * have not walked to is served from the deepest cursor their group HOLDS, with a
   * `limit` spanning the gap, and paints the tail of that answer. The gateway
   * decorates only the window it cuts, so one wide window is what a walk of
   * conditional round trips would have cost.
   *
   * THE VALIDATORS BELONG TO THE READER. `pins` is the group's own store
   * (`ProjectWindows`): the window is pinned there under the question it answered —
   * this gateway, this project, this page size, this cursor, this device's overlay —
   * so a page the reader walks back to costs one 304 with no body and hands back the
   * SAME rows array. Prefetching the pages ahead writes into that store.
   * `persistHead` marks the visible first page, not a wider read for a page jump.
   * That bounded head survives unmount and restart, one snapshot per project.
   *
   * `archived` is the VIEW this read asks for: a reveal reads `'only'`, which is another
   * list — its own pages, its own validators, and never the head this device keeps of the
   * project's active first page.
   *
   * `bands` is the window over the project's GROUPS this read paints, the same one
   * `listSessionGroups` cut: `grouped` then holds those bands' rows alone. It belongs to
   * the question a window answered, so a page held for another page of bands is not an
   * answer to this one.
   *
   * `warmTranscripts` prepares recent answers for a visible page. Reads ahead and
   * administrative walks leave it off, so they cannot evict the visible transcript cache.
   */
  async listProjectPage(
    root: string,
    limit: number,
    after: string,
    pins: ProjectWindows,
    signal?: AbortSignal,
    persistHead = false,
    archived: ArchiveView = 'exclude',
    bands?: BandWindow,
    warmTranscripts = false,
  ): Promise<ProjectPage> {
    // The overlay rides down here too: a session holding words typed on THIS device
    // is in this device's list and in nobody else's, so a page cut without it is a
    // page short (see `dirtySessionIds`).
    await hydrateDraftMessages();
    const overlay = dirtySessionIds(this.base).join(',');
    const key = this.projectWindowKey(root, limit, after, archived, bands);
    // Only the window this reader already shows can hold its next answer back.
    const seen = pins.get(key);
    const pin = this.heldProjectWindow(root, limit, after, pins, archived, bands);
    const res = await this.requestFull<{
      sessions?: Session[];
      awaiting?: Session[];
      grouped?: Session[];
      total?: number;
      next_cursor?: string | null;
    }>(
      'GET',
      `/v1/sessions?order=recent&root=${encodeURIComponent(root)}&limit=${limit}${
        after ? `&after=${encodeURIComponent(after)}` : ''
      }${overlay ? `&dirty=${encodeURIComponent(overlay)}` : ''}${
        archived === 'exclude' ? '' : `&archived=${archived}`
      }&grouped=aside${bands ? `&group_limit=${bands.limit}&group_offset=${bands.offset}` : ''}`,
      undefined,
      signal,
      pin?.etag ? { 'If-None-Match': pin.etag } : undefined,
    );
    const remember = async (window: { etag: string; page: ProjectPage }): Promise<ProjectPage> => {
      window = { ...window, page: this.withoutDeletedProjectSessions(window.page) };
      if (warmTranscripts) {
        const visible = [...window.page.rows, ...window.page.awaiting, ...window.page.grouped];
        // Only a row that settled in front of this reader waits for its answer. A first
        // read, also one answered from the saved head of a cold start, publishes at once.
        const previous = seen ? [...seen.page.rows, ...seen.page.awaiting, ...seen.page.grouped] : null;
        this.prefetchActiveTranscripts(visible);
        if (!(await this.prefetchSettledTranscripts(previous, visible)) && seen)
          return this.withoutDeletedProjectSessions(seen.page);
      }
      // The saved head is the project's ACTIVE first page: a reveal is a look at another
      // list, and a cold start must not paint the archive in its place.
      if (persistHead && !after && archived === 'exclude' && limit <= MAX_PROJECT_HEAD_ROWS)
        writeSnapshot(this.snapshotKey('project-head', root), { key, ...window });
      if (window.etag) pins.set(key, window);
      else pins.delete(key);
      return window.page;
    };
    if (res.status === 304 && pin) return remember(pin);
    // A row the wire repeated keeps the object the group is already rendering, so a
    // page that only gained a title does not re-render every row on it.
    const rows = reconcileRows(pin?.page.rows ?? null, res.data?.sessions ?? []);
    const awaiting = reconcileRows(pin?.page.awaiting ?? null, res.data?.awaiting ?? []);
    // The shelves ride with the HEAD window alone, so a tail answer OMITS the key.
    // Absent means "unchanged here"; an empty array means the project really has no
    // bands. Collapsing the two wiped the shelves as soon as a reader paged.
    const shelved = res.data?.grouped;
    const grouped = shelved
      ? reconcileRows(pin?.page.grouped ?? null, shelved)
      : (pin?.page.grouped ?? []);
    // Every row names the model it runs on, so opening any of them paints the right
    // chip on the FIRST frame instead of after a per-session round trip.
    this.seedSessionModels(rows);
    this.seedSessionModels(awaiting);
    this.seedSessionModels(grouped);
    const page: ProjectPage = {
      rows,
      total: res.data?.total ?? rows.length,
      nextCursor: res.data?.next_cursor ?? '',
      awaiting,
      grouped,
    };
    return remember({ etag: res.etag ?? '', page });
  }

  /**
   * The page this reader ALREADY holds for that question, or `null`.
   *
   * A prefetched window is a real answer, not a promise of one: painting it the
   * instant a page is turned is what makes the turn cost no wait, and the
   * conditional read that follows either confirms it with a 304 — the same rows, the
   * same objects, no repaint — or replaces it with what actually changed.
   */
  heldProjectPage(
    root: string,
    limit: number,
    after: string,
    pins: ProjectWindows,
    archived: ArchiveView = 'exclude',
    bands?: BandWindow,
  ): ProjectPage | null {
    const held = this.heldProjectWindow(root, limit, after, pins, archived, bands)?.page;
    return held ? this.withoutDeletedProjectSessions(held) : null;
  }

  private heldProjectWindow(
    root: string,
    limit: number,
    after: string,
    pins: ProjectWindows,
    archived: ArchiveView = 'exclude',
    bands?: BandWindow,
  ): { etag: string; page: ProjectPage } | null {
    const key = this.projectWindowKey(root, limit, after, archived, bands);
    const pin = pins.get(key);
    if (pin) return pin;
    if (after) return null;
    const saved = readSnapshot<{ key: string; etag: string; page: ProjectPage }>(
      this.snapshotKey('project-head', root),
    );
    return saved?.key === key ? saved : null;
  }

  /** A local mutation must not reappear from a project's saved head after restart. */
  private forgetProjectHeads(sid: string, root?: string): void {
    const prefix = `${this.snapshotKey('project-head')}\u0000`;
    for (const [key, value] of snapshots) {
      if (!key.startsWith(prefix)) continue;
      const { page } = value as { page: ProjectPage };
      if (
        (root && key === this.snapshotKey('project-head', root)) ||
        page.rows.some((row) => row.id === sid) ||
        page.awaiting.some((row) => row.id === sid) ||
        (page.grouped ?? []).some((row) => row.id === sid)
      )
        snapshots.delete(key);
    }
  }

  /** The question a project window answered, as one string. */
  private projectWindowKey(
    root: string,
    limit: number,
    after: string,
    archived: ArchiveView,
    bands?: BandWindow,
  ): string {
    return [
      this.base,
      root,
      String(limit),
      after,
      archived,
      bands ? `${bands.limit}:${bands.offset}` : '',
      dirtySessionIds(this.base).join(','),
    ].join('\u0000');
  }
  /**
   * THE session search every surface asks: `GET /v1/sessions/actions/search`.
   *
   * A blank query answers the RECENTS, a query the sessions whose title or transcript
   * matched it. The gateway decides those rows and their order once, for every client,
   * and sends the rows IN the answer: a hit further down the paged list needs no
   * second read. Each matched row carries its `match` (the band it hit in plus a few
   * snippets), so the UI previews the conversation without fetching it. `dirty=` rides
   * along as on the list (see `listSessions`), so an untitled session holding unsent
   * words on this device still counts as recent work. A query also reads the archive
   * (`archived=include`): a session put away is still found by what was said in it,
   * while the recents stay the active work.
   */
  async searchSessions(
    query: string,
    signal?: AbortSignal,
    scope: { projectId?: string; root?: string; groupIds?: readonly string[]; limit?: number; after?: string } = {},
  ): Promise<SessionSearch> {
    await hydrateDraftMessages();
    const overlay = dirtySessionIds(this.base).join(',');
    const filters = new URLSearchParams();
    if (scope.projectId !== undefined) filters.set('project_id', scope.projectId);
    if (scope.root !== undefined) filters.set('root', scope.root);
    if (scope.groupIds !== undefined) filters.set('group_ids', scope.groupIds.join(','));
    if (scope.limit !== undefined) filters.set('limit', String(scope.limit));
    if (scope.after) filters.set('after', scope.after);
    const res = await this.request<{
      sessions?: (Session & { match?: RawSessionMatch | null })[];
      total?: number;
      next_cursor?: string | null;
      has_more?: boolean;
    }>(
      'GET',
      `/v1/sessions/actions/search?q=${encodeURIComponent(query.trim())}${
        overlay ? `&dirty=${encodeURIComponent(overlay)}` : ''
      }${query.trim() ? '&archived=include' : ''}${filters.size ? `&${filters}` : ''}`,
      undefined,
      signal,
    );
    const rows = (res.sessions ?? []).filter((row) => !this.isSessionDeleted(row.id));
    // Every row names the model it runs on, exactly as a list row does.
    this.seedSessionModels(rows);
    return {
      sessions: rows,
      matches: rows.flatMap((row) => (row.match ? [sessionMatch(row.id, row.match)] : [])),
      total: res.total ?? rows.length,
      nextCursor: res.next_cursor ?? null,
      hasMore: res.has_more === true,
    };
  }

  /** The complete project catalog, including archived and empty projects. */
  async listProjects(signal?: AbortSignal): Promise<Project[]> {
    const answer = await this.request<{ projects?: Project[] }>(
      'GET', '/v1/projects?archived=include', undefined, signal,
    );
    return answer.projects ?? [];
  }

  /**
   * Start a session. `groupId` starts it INSIDE that session group, so a reader who
   * asked for it on a group's band sees the new row there and not loose in the
   * project (the gateway files it as it mints the soul).
   */
  createSession(opts: {
    title?: string;
    channel?: string;
    root?: string;
    groupId?: string;
  }): Promise<Session> {
    return this.request<Session>('POST', '/v1/sessions', {
      title: opts.title,
      channel: opts.channel ?? 'web',
      root: opts.root,
      group_id: opts.groupId,
    });
  }

  /**
   * The FOLDERS under `path` on this machine — the browse behind "Switch project…".
   * `path` is optional (the machine's own home answers) and understands a leading
   * `~`, so the app never has to know where a machine keeps its home.
   */
  browse(path?: string, signal?: AbortSignal): Promise<BrowseListing> {
    const query = path ? `?path=${encodeURIComponent(path)}` : '';
    return this.request<BrowseListing>('GET', `/v1/fs${query}`, undefined, signal);
  }

  /** Create ONE folder inside `path`, and answer with the folder itself. */
  createDirectory(path: string, name: string): Promise<BrowseEntry> {
    return this.request<BrowseEntry>('POST', '/v1/fs/actions/mkdir', {
      path,
      name,
    });
  }

  /**
   * Fork `sid` into a NEW INDEPENDENT session. `throughTurnId` is the LAST turn
   * the fork keeps; omitted, it copies the whole conversation. The source session
   * is untouched, and the answer is the fork's own row, ready to open.
   */
  async forkSession(sid: string, throughTurnId?: string): Promise<Session> {
    const res = await this.request<{ session: Session }>(
      'POST',
      `/v1/sessions/${encodeURIComponent(sid)}/forks`,
      throughTurnId ? { through_turn_id: throughTurnId } : {},
    );
    return res.session;
  }

  /**
   * Every turn of `sid`, oldest first, named by the words that opened it. The rows
   * carry no answers, so a long session's turns can be listed without paging the
   * whole transcript.
   */
  async forkPoints(sid: string, signal?: AbortSignal): Promise<ForkPoint[]> {
    const res = await this.request<{ turns?: ForkPoint[] }>(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}/forks`,
      undefined,
      signal,
    );
    return res.turns ?? [];
  }

  async agents(sid: string, signal?: AbortSignal): Promise<Subagent[]> {
    return this.request('GET', `/v1/sessions/${encodeURIComponent(sid)}/agents`, undefined, signal);
  }

  async cancelAgent(sid: string, childId: string): Promise<void> {
    await this.request('POST', `/v1/sessions/${encodeURIComponent(sid)}/agents/cancel`, {
      session_id: childId,
    });
  }

  async session(sid: string, signal?: AbortSignal, includeQueued = false): Promise<Session> {
    const response = await this.request<
      Session & { queued_turns?: unknown; queue_paused?: unknown }
    >(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}${includeQueued ? '?include=queued' : ''}`,
      undefined,
      signal,
    );
    const { queued_turns: queuedTurns, queue_paused: queuePaused, ...row } = response;
    if (includeQueued && !Array.isArray(queuedTurns)) {
      throw new Error('Gateway response omitted queued_turns');
    }
    const merged = reconcileSession(
      this.heldSessionRow(sid),
      row as Session,
      readSnapshot<SessionGoal>(this.snapshotKey('goal', sid)),
    );
    writeSnapshot(this.snapshotKey('session', sid), merged);

    if (includeQueued) {
      this.storeQueuedTurns(sid, queuedTurns as SubmittedTurn[]);
      this.queuePaused.set(sid, queuePausedFromWire(queuePaused));
    }
    return merged;
  }

  /**
   * Whole-life usage rollup for ONE session. On-demand only: it is absent from
   * `listSessions` and snapshots, fetched when a row expands, and is `null` for
   * a session that has no turns yet. The gateway memoizes each decoded iteration.
   */
  async sessionUsage(sid: string, signal?: AbortSignal): Promise<SessionUsage | null> {
    const res = await this.request<{ usage: SessionUsage | null }>(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}/usage`,
      undefined,
      signal,
    );
    return res.usage ?? null;
  }

  async deleteSession(sid: string): Promise<unknown> {
    const result = await this.request('DELETE', `/v1/sessions/${encodeURIComponent(sid)}`);
    this.forgetDeletedSession(sid);
    return result;
  }

  /** Forget a confirmed local or remote deletion without issuing another DELETE. */
  forgetDeletedSession(sid: string): void {
    this.forgetSession(sid);
    GatewayClient.deletedSessions.add(this.snapshotKey('session', sid));
    clearDraftMessage(draftMessageKey(this.base, sid));
    void flushDraftMessages();
    // Keep every other row warm rather than replacing the list with a skeleton.
    const rows = this.cachedSessions();
    if (rows)
      writeSnapshot(
        this.snapshotKey('sessions'),
        rows.filter((row) => row.id !== sid),
      );
  }

  isSessionDeleted(sid: string): boolean {
    return GatewayClient.deletedSessions.has(this.snapshotKey('session', sid));
  }

  private withoutDeletedSessions(rows: Session[]): Session[] {
    return rows.some((row) => this.isSessionDeleted(row.id))
      ? rows.filter((row) => !this.isSessionDeleted(row.id))
      : rows;
  }

  private withoutDeletedProjectSessions(page: ProjectPage): ProjectPage {
    const rows = this.withoutDeletedSessions(page.rows);
    const awaiting = this.withoutDeletedSessions(page.awaiting);
    // A head snapshot written before group shelves existed carries no `grouped`,
    // and a restored page is handed straight to the list.
    const grouped = this.withoutDeletedSessions(page.grouped ?? []);
    if (rows === page.rows && awaiting === page.awaiting && grouped === page.grouped) return page;
    // `total` counts the WINDOW's own rows - the parked strip and the group
    // shelves stand beside it - so only a deleted row of this page moves it.
    const deleted = new Set(
      page.rows.filter((row) => this.isSessionDeleted(row.id)).map((row) => row.id),
    );
    return {
      ...page,
      rows,
      awaiting,
      grouped,
      total: Math.max(0, page.total - deleted.size),
    };
  }

  /** Add an empty workspace root to the gateway's project inventory, idempotently. */
  async ensureProject(root: string): Promise<void> {
    await this.request('POST', '/v1/projects/actions/ensure', { root });
  }

  /**
   * Rename a project, or move it and its sessions to another folder on the machine.
   * The gateway answers 400 for a folder that does not exist and 409 for a folder
   * another project already uses.
   */
  async updateProject(pid: string, change: { name?: string; workspace_root?: string }): Promise<void> {
    await this.request('PATCH', `/v1/projects/${encodeURIComponent(pid)}`, change);
  }

  /**
   * Delete a project AND every session in it.
   *
   * Plain `DELETE /v1/projects/:pid` only drops the row and scatters its members
   * to project-less, so the blast radius is explicit on the wire: `is_recursive`
   * is the destructive one, and it answers with the ids it deleted so the caches
   * can be pruned here instead of racing a re-read.
   */
  async deleteProject(pid: string): Promise<string[]> {
    const res = await this.request<{ deleted_session_ids?: string[] }>(
      'DELETE',
      `/v1/projects/${encodeURIComponent(pid)}?is_recursive=true`,
    );
    const ids = res?.deleted_session_ids ?? [];
    for (const sid of ids) this.forgetDeletedSession(sid);
    return ids;
  }

  /**
   * Write a row the gateway just echoed into BOTH snapshots, so the list and the
   * session header repaint from cache with what it says instead of the stale row.
   */
  private absorbSessionRow(sid: string, row: Session): Session {
    const merged = reconcileSession(
      this.heldSessionRow(sid),
      row,
      readSnapshot<SessionGoal>(this.snapshotKey('goal', sid)),
    );
    this.forgetProjectHeads(sid, merged.workspace?.root);
    writeSnapshot(this.snapshotKey('session', sid), merged);
    const rows = this.cachedSessions();
    if (rows) {
      writeSnapshot(
        this.snapshotKey('sessions'),
        rows.map((entry) => (entry.id === sid ? reconcileSession(entry, merged) : entry)),
      );
    }
    return merged;
  }

  /** Apply the gateway's current identity to the open session and its cached row. */
  noteSessionAgentName(sid: string, agentName: string): void {
    const previous = this.cachedSession(sid);
    if (previous) this.absorbSessionRow(sid, { ...previous, agent_name: agentName });
  }

  /** Apply a live goal without allowing replay or an older HTTP snapshot to rewind it. */
  noteSessionGoal(sid: string, raw: unknown): Session | null {
    const previous = this.cachedSession(sid);
    const goal = sessionGoalFromWire(raw);
    if (!goal) return previous;
    const key = this.snapshotKey('goal', sid);
    const held = readSnapshot<SessionGoal>(key);
    if (!held || goal.revision > held.revision) writeSnapshot(key, goal);
    if (!previous) return null;
    return this.absorbSessionRow(sid, { ...previous, goal });
  }

  /** Rename a session. The gateway echoes the updated meta row. */
  async renameSession(sid: string, title: string): Promise<Session> {
    return this.absorbSessionRow(
      sid,
      await this.request<Session>('PATCH', `/v1/sessions/${encodeURIComponent(sid)}`, { title }),
    );
  }

  /**
   * Star or unstar a session. The star is the GATEWAY's fact, not this device's:
   * the reply carries the `favorite_rank` it allocated, every other client of the
   * machine reads the same one, and nothing local is kept that could disagree.
   */
  async setSessionFavorite(sid: string, isFavorite: boolean): Promise<Session> {
    return this.absorbSessionRow(
      sid,
      await this.request<Session>('PATCH', `/v1/sessions/${encodeURIComponent(sid)}`, {
        is_favorite: isFavorite,
      }),
    );
  }

  /**
   * Put a session away, or take it back. The stamp is the GATEWAY's: the reply carries the
   * `archived_at` it wrote, both snapshots take it, and every other client of the machine
   * reads the same decision — nothing local is kept that could disagree.
   *
   * A session still holding work is refused with 409 `session-busy`; taking one back is never
   * refused.
   */
  async setSessionArchived(sid: string, archived: boolean): Promise<Session> {
    return this.absorbSessionRow(
      sid,
      await this.request<Session>('PATCH', `/v1/sessions/${encodeURIComponent(sid)}`, {
        archived,
      }),
    );
  }
  /**
   * The GROUPS one project is divided into, addressed by workspace ROOT.
   *
   * This app groups its list by root and never holds a project id, so the gateway
   * resolves one for it. A root nothing has been filed under yet simply has no
   * groups: a READ never creates a project.
   *
   * `archived` picks the view: a project's reveal reads `'only'` and gets the bands the
   * human put away, each still counting the sessions filed under it.
   *
   * `bands` cuts ONE page out of that wall, and the answer then carries `total` and
   * `has_more` for the pager over them. Without it the answer is every band.
   */
  async listSessionGroups(
    root: string,
    signal?: AbortSignal,
    archived: ArchiveView = 'exclude',
    bands?: BandWindow,
  ): Promise<SessionGroupPage> {
    const query = new URLSearchParams({ root });
    if (archived !== 'exclude') query.set('archived', archived);
    if (bands) {
      query.set('limit', String(bands.limit));
      query.set('offset', String(bands.offset));
    }
    const page = await this.request<SessionGroupPage>(
      'GET',
      `/v1/session-groups?${query.toString()}`,
      undefined,
      signal,
    );
    // The page of the wall a project OPENS on is saved beside its head, so the next cold
    // start names its bands before this read comes back (`heldSessionGroups`).
    if (archived === 'exclude' && (bands?.offset ?? 0) === 0 && Array.isArray(page?.groups))
      writeSnapshot(this.snapshotKey('project-groups', root), {
        key: this.groupWallKey(root, archived, bands),
        page,
      });
    return page;
  }

  /**
   * The page of a project's wall this reader was answered last time, or `null`.
   *
   * Only the active first page is kept, the one a project opens on. Painting it with the
   * held head (`heldProjectPage`) names the bands on the first frame of a cold start;
   * `listSessionGroups` then confirms or replaces them.
   */
  heldSessionGroups(
    root: string,
    archived: ArchiveView = 'exclude',
    bands?: BandWindow,
  ): SessionGroupPage | null {
    const saved = readSnapshot<{ key: string; page: SessionGroupPage }>(
      this.snapshotKey('project-groups', root),
    );
    return saved?.key === this.groupWallKey(root, archived, bands) ? saved.page : null;
  }

  /** The question a page of a project's wall answered, as one string. */
  private groupWallKey(root: string, archived: ArchiveView, bands?: BandWindow): string {
    return [this.base, root, archived, bands ? `${bands.limit}:${bands.offset}` : ''].join('\u0000');
  }

  /** A band changed on this device must not be painted from a wall saved before the change. */
  private forgetGroupWalls(owner: { root?: string; gid?: string | null; projectId?: string | null }): void {
    const prefix = `${this.snapshotKey('project-groups')}\u0000`;
    for (const [key, value] of snapshots) {
      if (!key.startsWith(prefix)) continue;
      const { page } = value as { page: SessionGroupPage };
      if (
        (owner.root && key === this.snapshotKey('project-groups', owner.root)) ||
        (owner.projectId && page.project_id === owner.projectId) ||
        (owner.gid && page.groups.some((group) => group.id === owner.gid))
      )
        snapshots.delete(key);
    }
  }

  /**
   * Open a group in that project. The project is created if this root has none yet,
   * so the first group a reader makes needs no separate step. A name already taken
   * in the project is a 409 (`group-exists`), which is the caller's to report.
   */
  async createSessionGroup(root: string, name: string, color?: string): Promise<SessionGroup> {
    const group = await this.request<SessionGroup>(
      'POST',
      '/v1/session-groups',
      color ? { root, name, color } : { root, name },
    );
    this.forgetGroupWalls({ root });
    return group;
  }

  /** Rename, recolour, reorder, archive or restore a group. */
  async updateSessionGroup(
    gid: string,
    fields: { name?: string; color?: string; position?: number; archived?: boolean },
  ): Promise<SessionGroup> {
    const group = await this.request<SessionGroup>(
      'PATCH',
      `/v1/session-groups/${encodeURIComponent(gid)}`,
      fields,
    );
    // A reorder moves the other bands of the project too.
    this.forgetGroupWalls({ gid, projectId: group?.project_id });
    return group;
  }

  /**
   * Drop a group and say what becomes of its members. `'detach'` (the default) leaves
   * every session in the project, ungrouped; `'with-sessions'` deletes them together
   * with the group — the second answer the delete dialog offers. The gateway names both
   * lists, so the rows this device holds lose the group they were filed under, or
   * disappear, without racing a re-read.
   */
  async deleteSessionGroup(
    gid: string,
    sessions: 'detach' | 'with-sessions' = 'detach',
  ): Promise<{ detached: string[]; deleted: string[] }> {
    const res = await this.request<{
      scattered_session_ids?: string[];
      deleted_session_ids?: string[];
    }>(
      'DELETE',
      `/v1/session-groups/${encodeURIComponent(gid)}${
        sessions === 'with-sessions' ? '?sessions=delete' : ''
      }`,
    );
    this.forgetGroupWalls({ gid });
    const detached = res?.scattered_session_ids ?? [];
    const deleted = res?.deleted_session_ids ?? [];
    for (const sid of detached) {
      const row = this.cachedSession(sid);
      if (row) this.absorbSessionRow(sid, { ...row, group_id: null });
    }
    for (const sid of deleted) this.forgetDeletedSession(sid);
    return { detached, deleted };
  }

  /**
   * File a session under a group, or `null` to leave it ungrouped inside its project.
   * The gateway echoes the refreshed row, so the band it moves to is painted from the
   * gateway's own answer instead of a guess this device made.
   */
  async assignSessionGroup(sid: string, gid: string | null): Promise<Session> {
    const row = await this.request<Session>('PUT', `/v1/sessions/${encodeURIComponent(sid)}/group`, {
      group_id: gid,
    });
    // Filing moves a session between bands, so the counts a saved wall paints are stale.
    this.forgetGroupWalls({ root: row?.workspace?.root, gid });
    return this.absorbSessionRow(sid, row);
  }

  /**
   * Tell the gateway how far the reader has got in this session. The mark is the
   * GATEWAY's, not this device's: the TUI, this app and another phone clear the
   * same badge, and it never moves backwards. Leaving `seenAnswers` out marks
   * every settled answer read.
   *
   * Housekeeping, like `forgetVoiceJob`: a failed mark is nothing to report,
   * because the next time this session is on screen it is marked again.
   */
  async markSessionRead(sid: string, seenAnswers?: number): Promise<void> {
    try {
      const mark = await this.request<{ is_unread?: boolean; seen_answers?: number }>(
        'PUT',
        `/v1/sessions/${encodeURIComponent(sid)}/read`,
        seenAnswers === undefined ? {} : { seen_answers: seenAnswers },
      );
      const previous = this.cachedSession(sid);
      if (!previous) return;
      // Paint the answer straight into the cached row, so a cold start from the
      // snapshot does not show a badge the gateway has already retired.
      const isUnread = mark?.is_unread === true;
      const behind = Number(previous.answer_count ?? 0) - Number(mark?.seen_answers ?? 0);
      this.absorbSessionRow(sid, {
        ...previous,
        is_unread: isUnread,
        unread_answers: isUnread ? Math.max(0, behind) : 0,
      });
    } catch {
      // The reader's position is sent again the next time the session is open.
    }
  }

  /**
   * Merge a freshly fetched slice onto the rows we already hold, BY TURN ID.
   * Windowed fetches overlap (the newest page re-covers turns we painted an hour
   * ago), so positional splicing would duplicate or reorder them; matching on id
   * keeps one row per turn and reuses the old object whenever the wire repeated
   * itself, which is what makes the memoised usage fold and `memo`'d rows hit.
   */
  private mergeTurns(
    previous: TranscriptTurn[] | null,
    incoming: TranscriptTurn[],
    where: 'tail' | 'head',
  ): TranscriptTurn[] {
    if (!previous?.length) return incoming;
    if (!incoming.length) return previous;
    const index = new Map(previous.map((turn, at) => [turn.turn_id, at]));
    const merged = previous.slice();
    const fresh: TranscriptTurn[] = [];
    let changed = false;
    for (const turn of incoming) {
      const at = index.get(turn.turn_id);
      if (at === undefined) {
        fresh.push(turn);
        changed = true;
        continue;
      }
      const kept = reconcileRow(merged[at], turn);
      if (kept !== merged[at]) changed = true;
      merged[at] = kept;
    }
    if (!fresh.length) return changed ? merged : previous;
    return where === 'head' ? fresh.concat(merged) : merged.concat(fresh);
  }

  /**
   * One windowed transcript request. `limit`/`offset` are sliced by the gateway
   * BEFORE it hydrates iterations and attachments, which is the whole saving: a
   * 247-turn session costs ~750 ms and 40 MB whole, ~50 ms and 4 MB for the
   * newest 30 — and the gateway caps a page in BYTES too, so it may answer with
   * FEWER rows than asked and a HIGHER `offset` than the one requested. Page
   * from the returned `offset`; it is the only cursor that is true. A gateway
   * too old to know the params answers with the full
   * transcript and no `total`, so we synthesise the window from what arrived and
   * everything below still works.
   */
  private async fetchTranscriptPage(
    sid: string,
    query: Record<string, number>,
    signal?: AbortSignal,
    urgent = false,
  ): Promise<TranscriptPage> {
    const search = new URLSearchParams();
    for (const [key, value] of Object.entries(query)) search.set(key, String(value));
    search.set('iteration_limit', '8');
    const suffix = search.toString();
    const response = await this.request<{
      turns?: TranscriptTurn[];
      total?: number;
      offset?: number;
      has_more?: boolean;
    }>(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}/transcript${suffix ? `?${suffix}` : ''}`,
      undefined,
      signal,
      urgent,
    );
    const turns = response.turns ?? [];
    const total = typeof response.total === 'number' ? response.total : turns.length;
    const offset =
      typeof response.offset === 'number' ? response.offset : Math.max(0, total - turns.length);
    return {
      turns,
      total,
      offset,
      hasMore: typeof response.has_more === 'boolean' ? response.has_more : offset > 0,
    };
  }

  /** Read one durable Activity window without adding it to the transcript cache. */
  async activityPage(
    sid: string,
    id: string,
    after = 0,
    q = '',
    signal?: AbortSignal,
  ): Promise<ActivityProjection> {
    const query = new URLSearchParams({ after: String(after), limit: '32' });
    if (q) query.set('q', q);
    const raw = await this.request<unknown>(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}/activity/${encodeURIComponent(id)}?${query}`,
      undefined,
      signal,
    );
    const page = activityProjectionFromWire(raw);
    if (!page?.history || page.history.id !== id || page.history.after !== after) {
      throw new Error('Activity response does not match the requested history window.');
    }
    return page;
  }

  /** Explicit whole-file export; unlike normal navigation this allocates a Blob. */
  async activityExport(sid: string, id: string, signal?: AbortSignal): Promise<Blob> {
    const path = `/v1/sessions/${encodeURIComponent(sid)}/activity/${encodeURIComponent(id)}/export`;
    const response = await fetch(`${this.base}${path}`, {
      headers: this.headers(),
      signal,
    });
    if (!response.ok)
      throw new GatewayError(response.status, `Activity export failed: HTTP ${response.status}`);
    const blob = await response.blob();
    const incomplete = '\n\nINCOMPLETE EXPORT: Activity changed. Reload and retry.\n';
    // The server can mark a failed stream after HTTP 200 has already been sent.
    // Inspect only its ASCII trailer, not a second copy of the entire export.
    if ((await blob.slice(-incomplete.length).text()) === incomplete)
      throw new Error('Activity changed. Reload and retry.');
    return blob;
  }

  /** How much of `sid`'s transcript we hold, and how much older history exists. */
  transcriptWindow(sid: string): TranscriptWindow {
    return (
      transcriptWindows.get(this.snapshotKey('transcript', sid)) ?? {
        offset: 0,
        total: this.cachedTranscript(sid)?.length ?? 0,
      }
    );
  }

  /**
   * The NEWEST page of a session's transcript, merged onto whatever we already
   * hold (so earlier pages the user pulled in stay loaded). `urgent` reads it
   * ahead of queued polls; see `awaitGatewaySlot`.
   */
  async transcript(
    sid: string,
    signal?: AbortSignal,
    limit: number = TRANSCRIPT_PAGE,
    urgent = false,
  ): Promise<TranscriptTurn[]> {
    if (signal?.aborted) throw new DOMException('Aborted', 'AbortError');
    const key = `${this.snapshotKey('transcript', sid)}:${limit}`;
    const pending = transcriptReads.get(key);
    if (pending) return pending;
    const read = this.readTranscript(sid, limit, urgent).finally(() => {
      if (transcriptReads.get(key) === read) transcriptReads.delete(key);
    });
    transcriptReads.set(key, read);
    return read;
  }

  private async readTranscript(
    sid: string,
    limit: number,
    urgent: boolean,
  ): Promise<TranscriptTurn[]> {
    const key = this.snapshotKey('transcript', sid);
    const page = await this.fetchTranscriptPage(sid, { limit }, undefined, urgent);
    const cached = this.cachedTranscript(sid);
    const held = transcriptWindows.get(key);
    const heldOffset = cached?.length ? (held?.offset ?? 0) : page.offset;
    // We hold rows [heldOffset, heldOffset + cached.length). A newest page that
    // starts BEYOND that runs past a GAP: the session grew by more than one page
    // since we last looked (app backgrounded while the TUI kept working), and
    // concatenating would paint turn 123 straight into turn 223 — a hole no
    // "load earlier" can reach, because it only ever walks back from turn 123.
    // Drop the stale rows and restart the window at this page instead.
    const adjoins = !cached?.length || page.offset <= heldOffset + cached.length;
    // Both sides are contiguous slices with a known offset, so split the page at
    // our oldest row instead of trusting "unseen id ⇒ newer": a page that reaches
    // FURTHER BACK than we hold (a deleted turn, a smaller earlier limit) would
    // otherwise append ancient turns to the BOTTOM of the transcript.
    const before = adjoins ? Math.max(0, Math.min(page.turns.length, heldOffset - page.offset)) : 0;
    const turns = adjoins
      ? this.mergeTurns(
          this.mergeTurns(cached, page.turns.slice(before), 'tail'),
          page.turns.slice(0, before),
          'head',
        )
      : page.turns;
    writeSnapshot(key, turns);
    // The window starts at the OLDEST row we hold, which may predate this page.
    transcriptWindows.set(key, {
      offset: adjoins ? Math.min(heldOffset, page.offset) : page.offset,
      total: page.total,
    });
    // Stamp with the freshest meta row we hold, so a caller that already knows
    // the session did not move can skip the next fetch entirely — but ONLY when
    // that row is describing THIS body. The row is a SEPARATE observation of the
    // session, and the settle path deliberately refreshes it while the transcript
    // page is still in flight: stamping a body that is one turn short with a row
    // that already counts that turn is how a transcript stopped revalidating
    // while it was missing the newest answer.
    const meta = this.cachedSession(sid);
    transcriptStamps.set(
      key,
      typeof meta?.turn_count === 'number' && meta.turn_count !== page.total
        ? ''
        : transcriptStamp(meta),
    );
    return turns;
  }

  /**
   * The newest transcript page for a screen that opens with nothing to paint. It
   * joins a read already in flight for this session — `warmTranscript` starts one
   * when the reader reaches for its row — so the click costs no second request.
   * Without one, it reads the page ahead of queued polls.
   */
  async openingTranscript(sid: string, signal?: AbortSignal): Promise<TranscriptTurn[]> {
    const warming = transcriptPrefetches.get(this.snapshotKey('transcript', sid));
    if (warming && (await warming)) {
      const rows = this.cachedTranscript(sid);
      if (rows) return rows;
    }
    return this.transcript(sid, signal, TRANSCRIPT_PAGE, true);
  }

  /**
   * Pull the page of history immediately BEFORE the oldest row we hold. Returns
   * `null` when the beginning is already loaded, so the caller can hide its
   * "load earlier" affordance without a round-trip.
   */
  async transcriptEarlier(
    sid: string,
    signal?: AbortSignal,
    limit: number = TRANSCRIPT_PAGE,
  ): Promise<TranscriptTurn[] | null> {
    const key = this.snapshotKey('transcript', sid);
    const window = this.transcriptWindow(sid);
    if (window.offset <= 0) return null;
    const offset = Math.max(0, window.offset - limit);
    const page = await this.fetchTranscriptPage(
      sid,
      { offset, limit: window.offset - offset },
      signal,
    );
    const turns = this.mergeTurns(this.cachedTranscript(sid), page.turns, 'head');
    writeSnapshot(key, turns);
    transcriptWindows.set(key, { offset: page.offset, total: page.total });
    return turns;
  }

  /**
   * EVERY artifact this session ever produced, in ONE byte-free request.
   *
   * The transcript arrives newest-page-first, so a gallery derived from the
   * rows we hold listed only what the reader had already paged back to. The
   * gateway indexes the whole session instead; the bytes stay lazy behind
   * `attachmentUrl`.
   */
  async sessionArtifacts(sid: string, signal?: AbortSignal): Promise<SessionArtifactRow[]> {
    const response = await this.request<{ artifacts?: SessionArtifactRow[] }>(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}/artifacts`,
      undefined,
      signal,
    );
    return response.artifacts ?? [];
  }

  /**
   * Revalidate the transcript against a session meta row and fetch ONLY when
   * that row says a turn was actually persisted. Returns `null` when the cached
   * rows are still current — the caller keeps its state, its scroll, and its
   * rendered markdown, and the body never crosses the wire. `urgent` reads the page
   * ahead of queued polls.
   */
  async transcriptIfMoved(
    sid: string,
    row: Session | null,
    signal?: AbortSignal,
    urgent = false,
  ): Promise<TranscriptTurn[] | null> {
    const key = this.snapshotKey('transcript', sid);
    const warming = transcriptPrefetches.get(key);
    if (warming) {
      const prepared = await warming;
      if (signal?.aborted) return null;
      const expected = row && transcriptPrefetchStamp(row);
      if (prepared && expected && transcriptPrefetchStamps.get(key) === expected)
        return this.cachedTranscript(sid);
    }
    const stamp = transcriptStamp(row);
    const cached = this.cachedTranscript(sid);
    // A cached transcript holding a 'running' row is PROVISIONAL: that row is a
    // placeholder the gateway persists while a turn is in flight, and it carries
    // no outcome. Never let the stamp short-circuit past one — the turn may have
    // finished, failed or been cancelled since, and the caller would keep
    // painting a spinner for work that is long over.
    const provisional = !!cached?.some((turn) => turn.status === 'running');
    // …and the rows we hold have to ACCOUNT for the row we are revalidating
    // against. `total` is the gateway's own count of this session's turns — the
    // same population `turn_count` counts — so a row that counts more turns than
    // the body holds is describing an answer the body is missing, whatever the
    // stamp says. Without this check one poisoned stamp (see `transcript`) made
    // every later revalidation answer "nothing moved" for the rest of the
    // session's life: the list showed an answer, and opening the session painted
    // the transcript from before it. The stamp is persisted, so this is also the
    // repair path for a snapshot written by an older build.
    const short =
      typeof row?.turn_count === 'number' && row.turn_count > this.transcriptWindow(sid).total;
    if (stamp && cached !== null && !provisional && !short && transcriptStamps.get(key) === stamp)
      return null;
    const turns = await this.transcript(sid, signal, TRANSCRIPT_PAGE, urgent);
    if (stamp) transcriptStamps.set(key, stamp);
    return turns;
  }

  async transcriptMd(sid: string, signal?: AbortSignal): Promise<string> {
    return this.requestBody(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}/transcript.md`,
      { signal },
      (response) => response.text(),
    );
  }

  /**
   * The banner this session would raise right now, worded by the GATEWAY: the same two lines
   * `gateway/push.clj` sends a phone. The desktop app has no push channel and raises its own
   * alerts (`lib/desktop-notify.ts`), so it reads what they say from here rather than keeping
   * a second wording of its own.
   */
  sessionAlert(
    sid: string,
    reason: 'answer' | 'question',
    signal?: AbortSignal,
  ): Promise<SessionAlert> {
    return this.request<SessionAlert>(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}/alert?reason=${reason}`,
      undefined,
      signal,
    );
  }

  /**
   * ONE produced artifact's retained source —
   * `GET /v1/sessions/:sid/iterations/:iid/attachments/:idx`, the endpoint the
   * `iteration.completed` / transcript descriptors index. `<img src>` cannot
   * carry the bearer header a token-gated gateway demands, so the bytes are
   * fetched WITH the auth headers and retained as both a Blob and an object URL.
   *
   * THREE TIERS, and the network is the LAST one: the source this document
   * already made; else the bytes this DEVICE already downloaded, from the
   * persistent store in `attachment-cache`; else, and only then, the gateway.
   * An artifact is immutable, so every consumer shares exactly one download.
   */
  private attachmentSource(
    sid: string,
    iterationId: string,
    index: number,
  ): Promise<AttachmentSource> {
    const key = GatewayClient.attachmentKey(sid, iterationId, index);
    const cached = this.attachmentSources.get(key);
    if (cached) {
      // Map insertion order IS the eviction order, so a source asked for AGAIN
      // has to re-insert or the artifacts back on screen stay first in line.
      this.attachmentSources.delete(key);
      this.attachmentSources.set(key, cached);
      return cached;
    }
    const endpoint = this.attachmentEndpoint(sid, iterationId, index);
    const pending = (async () => {
      const stored = await readCachedAttachment(endpoint);
      if (stored) {
        this.attachmentSizes.set(key, stored.size);
        return { blob: stored, url: URL.createObjectURL(stored) };
      }
      let blob: Blob | null = null;
      // The live descriptor can beat the durable attachment row by a few hundred
      // milliseconds. A 404 here means "not landed yet", not "this picture is
      // broken"; each retry is its own diagnostic request.
      const landingDelays = [60, 140, 300, 600];
      for (let attempt = 0; ; attempt += 1) {
        const diagnostic = startRequestDiagnostic(this.base, 'GET', endpoint, {
          transport: 'fetch',
          attempt: attempt + 1,
        });
        const deadline = new AbortController();
        const timer = window.setTimeout(() => deadline.abort(), REQUEST_TIMEOUT_MS);
        let status = 0;
        let failure: { cause: unknown } | undefined;
        try {
          const response = await raceAbort(
            fetch(endpoint, { headers: this.headers(), signal: deadline.signal }),
            deadline.signal,
          );
          status = response.status;
          if (response.ok) {
            blob = await raceAbort(response.blob(), deadline.signal);
          } else {
            const error = new GatewayError(response.status, `HTTP ${response.status}`);
            failure = { cause: error };
            if (response.status !== 404 || attempt >= landingDelays.length) throw error;
          }
        } catch (cause) {
          const error = deadline.signal.aborted
            ? new GatewayError(0, 'attachment download timed out')
            : cause instanceof GatewayError
              ? cause
              : new GatewayError(0, `network error: ${errorOfDiagnostic(cause)}`);
          failure = { cause: error };
          throw error;
        } finally {
          finishRequestDiagnostic(diagnostic, {
            status,
            failure,
            timedOut: deadline.signal.aborted,
          });
          window.clearTimeout(timer);
        }
        if (blob) break;
        await new Promise<void>((resolve) => window.setTimeout(resolve, landingDelays[attempt]));
      }
      this.attachmentSizes.set(key, blob.size);
      // Keeping it is best-effort and never blocks the source it just produced.
      void writeCachedAttachment(endpoint, blob);
      return { blob, url: URL.createObjectURL(blob) };
    })();
    pending.catch(() => this.attachmentSources.delete(key));
    this.attachmentSources.set(key, pending);
    // Twice: once for the COUNT bound, which is knowable immediately, and again
    // when the bytes have landed and the SIZE bound finally has numbers to add.
    this.evictAttachmentSources();
    void pending.then(
      () => this.evictAttachmentSources(),
      () => undefined,
    );
    return pending;
  }

  /** The retained object URL used by image, audio, video, PDF, and frame elements. */
  attachmentUrl(sid: string, iterationId: string, index: number): Promise<string> {
    return this.attachmentSource(sid, iterationId, index).then((source) => source.url);
  }

  /** The same retained download, read directly without fetching its object URL. */
  attachmentBlob(sid: string, iterationId: string, index: number): Promise<Blob> {
    return this.attachmentSource(sid, iterationId, index).then((source) => source.blob);
  }

  /** Where ONE artifact is served from — its identity in every tier of cache. */
  attachmentEndpoint(sid: string, iterationId: string, index: number): string {
    return `${this.base}/v1/sessions/${encodeURIComponent(sid)}/iterations/${encodeURIComponent(iterationId)}/attachments/${index}`;
  }

  /**
   * A HUMAN'S REVISION OF AN ARTIFACT — `POST
   * /v1/sessions/:sid/iterations/:iid/attachments`.
   *
   * The filename is the identity, so saving an annotated note under the name it
   * was read as is the NEXT VERSION of that note rather than a second file
   * beside it. The gateway answers with the descriptor the transcript and the
   * byte endpoint already speak, so the caller can open the revision through
   * the paths it already has.
   */
  async saveArtifactText(
    sid: string,
    iterationId: string,
    filename: string,
    mediaType: string,
    text: string,
  ): Promise<IterationAttachment> {
    return this.saveArtifactBytes(
      sid,
      iterationId,
      filename,
      mediaType,
      new TextEncoder().encode(text),
    );
  }

  /**
   * The same revision, for an artifact whose content is BYTES rather than text —
   * a drawn-on picture, a stamped PDF. Same filename, so the gateway files it as
   * the next version of that artifact rather than as a second file.
   */
  async saveArtifactBytes(
    sid: string,
    iterationId: string,
    filename: string,
    mediaType: string,
    bytes: Uint8Array,
  ): Promise<IterationAttachment> {
    let binary = '';
    for (const byte of bytes) binary += String.fromCharCode(byte);
    const filed = await this.request<IterationAttachment>(
      'POST',
      `/v1/sessions/${encodeURIComponent(sid)}/iterations/${encodeURIComponent(iterationId)}/attachments`,
      { filename, media_type: mediaType, base64: btoa(binary) },
    );
    // The route is scoped to one iteration, so that IS the cut's home: stamping
    // it here means the descriptor can be folded into the transcript and opened
    // through `attachmentUrl` without another round trip.
    const saved = { ...filed, iteration_id: iterationId };
    this.noteArtifactRevision(sid, saved);
    return saved;
  }

  /**
   * Hear about a revision saved into `sid` while this screen is mounted; the
   * returned function stops listening.
   *
   * The transcript handed to the watcher is the folded one, so a screen that
   * derives its artifacts from turns simply adopts it.
   */
  onArtifactRevision(sid: string, watcher: (turns: TranscriptTurn[]) => void): () => void {
    const key = this.snapshotKey('transcript', sid);
    const held = revisionWatchers.get(key) ?? new Set<typeof watcher>();
    held.add(watcher);
    revisionWatchers.set(key, held);
    return () => {
      const live = revisionWatchers.get(key);
      if (!live) return;
      live.delete(watcher);
      if (live.size === 0) revisionWatchers.delete(key);
    };
  }

  /**
   * File one saved cut into the transcript this client holds, then tell whoever
   * is painting it. No snapshot means nothing to correct — the next read of the
   * session brings the revision with it.
   */
  private noteArtifactRevision(sid: string, saved: IterationAttachment): void {
    const key = this.snapshotKey('transcript', sid);
    const held = readSnapshot<TranscriptTurn[]>(key);
    if (!held) return;
    const next = withSavedAttachment(held, saved);
    if (next === held) return;
    writeSnapshot(key, next);
    for (const watcher of revisionWatchers.get(key) ?? []) watcher(next);
  }

  private static attachmentKey(sid: string, iterationId: string, index: number): string {
    return `${sid}\u0000${iterationId}\u0000${index}`;
  }

  /**
   * Claim one artifact's object URL for as long as a tile is painting it; the
   * returned function gives the claim back.
   *
   * Leaving a session and coming back re-mounts the WHOLE transcript at once, so
   * every artifact is requested in the same tick. Without a claim the newest
   * fetches push the cache over its bound and revoke the URLs of the pictures
   * still decoding right next to them: those tiles fire `error`, re-request,
   * evict each other in turn, and after two rounds give up as `✗ name`. That is
   * the "my images are gone when I re-open the session" report — the bytes were
   * always on the gateway, the app revoked them from under itself.
   */
  retainAttachment(
    sid: string,
    iterationId: string,

    index: number,
  ): () => void {
    const key = GatewayClient.attachmentKey(sid, iterationId, index);
    this.attachmentHolds.set(key, (this.attachmentHolds.get(key) ?? 0) + 1);
    let released = false;
    return () => {
      if (released) return;
      released = true;
      const left = (this.attachmentHolds.get(key) ?? 1) - 1;
      if (left > 0) {
        this.attachmentHolds.set(key, left);
        return;
      }
      this.attachmentHolds.delete(key);
      // The screen just let go of this one: now the bound can be honoured.
      this.evictAttachmentSources();
    };
  }

  /**
   * Bring the object-URL tier back inside its budget — by SIZE and by NUMBER,
   * through `cacheVictims`, the same policy the persistent tier is held to.
   *
   * Every live entry pins full DECODED bytes for the lifetime of the document,
   * and a long session of figures is exactly the memory curve iOS answers by
   * killing the webview. A bound counted in entries alone could not tell 24
   * thumbnails from 24 clips, so what each artifact landed with is counted too.
   * Held keys are SKIPPED, never merely deferred: the tier may sit over its
   * bound while that many pictures are genuinely on screen, which is the honest
   * trade (a visible image beats a freed URL).
   *
   * Cheap, now that the bytes survive on disk: a revoked URL costs a decode when
   * that figure scrolls back into view, never another download.
   */
  private evictAttachmentSources(): void {
    const entries = Array.from(this.attachmentSources.keys()).map((key, at) => ({
      url: key,
      bytes: this.attachmentSizes.get(key) ?? 0,
      used: at,
      pinned: this.attachmentHolds.has(key),
    }));
    for (const key of cacheVictims(entries, ATTACHMENT_MEMORY_BUDGET)) {
      const stale = this.attachmentSources.get(key);
      this.attachmentSources.delete(key);
      this.attachmentSizes.delete(key);
      void stale?.then((source) => URL.revokeObjectURL(source.url)).catch(() => undefined);
    }
  }

  /**
   * The correlation id each session's last submission from THIS client carried.
   *
   * A session is shared, so the gateway refuses a tid-less cancel that cannot name
   * the turn it means (409 `:not-owner`): `cancel-current` proves ownership with
   * the very `idempotency_key` the submit sent, and nothing else.
   */
  private readonly submissionKeys = new Map<string, string>();

  async submitTurn(
    sid: string,
    request: string,
    options: {
      model?: string;
      displayRequest?: string;
      attachments?: GatewayAttachment[];
      extraBody?: Record<string, unknown>;
      turnFeatures?: Record<string, boolean>;
    } = {},
  ): Promise<SubmittedTurn> {
    const clientId = `companion:${randomUuid()}`;
    this.submissionKeys.set(sid, clientId);
    const attachments = await Promise.all(
      (options.attachments ?? []).map(async (attachment) => {
        const query = new URLSearchParams({
          filename: attachment.filename,
          media_type: attachment.media_type,
        });
        const uploaded = await this.request<{ upload_id: string; size: number }>(
          'POST',
          `/v1/sessions/${encodeURIComponent(sid)}/attachments?${query.toString()}`,
          attachmentPayloadBlob(attachment),
        );
        return {
          upload_id: uploaded.upload_id,
          filename: attachment.filename,
          media_type: attachment.media_type,
          size: uploaded.size,
          reference: attachment.reference,
        };
      }),
    );
    const path = `/v1/sessions/${encodeURIComponent(sid)}/turns`;
    const submitted = await this.request<SubmittedTurn>('POST', path, {
      request,
      display_request: options.displayRequest,
      model: options.model,
      attachments,
      extra_body: options.extraBody,
      turn_features: options.turnFeatures,
      idempotency_key: clientId,
    });
    for (const listener of turnSubmissionListeners) listener(sid);
    return submitted;
  }

  /** Stop a turn we know the id of — the addressed route, open to every channel. */
  cancelTurn(sid: string, tid: string): Promise<unknown> {
    return this.request(
      'POST',
      `/v1/sessions/${encodeURIComponent(sid)}/turns/${encodeURIComponent(tid)}/cancel`,
    );
  }

  /**
   * Stop the turn we submitted here without knowing its id yet (Stop pressed before
   * `turn.started` landed). It names itself with the submission's correlation id;
   * without one the gateway would have to guess, and refuses.
   */
  cancelCurrentTurn(sid: string): Promise<unknown> {
    return this.request('POST', `/v1/sessions/${encodeURIComponent(sid)}/cancel-current`, {
      idempotency_key: this.submissionKeys.get(sid),
    });
  }

  // ── Queue (shared server-side backlog, same as the TUI) ─────────
  // A busy-time submitTurn is enqueued by the gateway and mirrored to every
  // channel via turn.queued/.updated/.deleted/.drained. These edit that backlog.

  /** Edit a still-queued turn's prompt before it starts. */
  updateQueuedTurn(sid: string, tid: string, request: string): Promise<unknown> {
    return this.request(
      'PATCH',
      `/v1/sessions/${encodeURIComponent(sid)}/turns/${encodeURIComponent(tid)}`,
      { request },
    );
  }

  /**
   * `→` on one queued row: `next_iteration` delivers it into the running turn at
   * its next step, `turn_end` keeps it for the turn end. The gateway mirrors the
   * change to every channel with `turn.queued.updated`.
   */
  markQueuedTurn(sid: string, tid: string, deliver: QueuedTurnDeliver): Promise<unknown> {
    return this.request(
      'PATCH',
      `/v1/sessions/${encodeURIComponent(sid)}/turns/${encodeURIComponent(tid)}`,
      { deliver },
    );
  }

  /** `→ Send now` on the queue header: mark every markable queued row at once. */
  sendQueueNow(sid: string): Promise<unknown> {
    return this.request('POST', `/v1/sessions/${encodeURIComponent(sid)}/queue/send-now`);
  }

  /** Drop a queued turn before it ever runs. */
  deleteQueuedTurn(sid: string, tid: string): Promise<unknown> {
    return this.request(
      'DELETE',
      `/v1/sessions/${encodeURIComponent(sid)}/turns/${encodeURIComponent(tid)}`,
    );
  }

  /**
   * The session's queued backlog AS THE GATEWAY KNOWS IT. The tray never
   * invents rows, and SSE only carries the deltas that happen while we are
   * subscribed — so a session opened (or reloaded, or backgrounded by iOS)
   * while messages sit queued must read the backlog back from here. Same
   * source the TUI resumes from (`chat/resume-session`).
   *
   * `?status=queued` keeps this poll cheap: the current gateway returns only the
   * queued rows instead of the session's entire turn history and content.
   */
  private storeQueuedTurns(sid: string, turns: unknown[]): QueuedTurn[] {
    const fetched = turns
      .filter(
        (turn): turn is Record<string, unknown> =>
          turn !== null && typeof turn === 'object' && !Array.isArray(turn),
      )
      .sort((a, b) => Number(a.queued_at ?? 0) - Number(b.queued_at ?? 0))
      .map(queuedTurnFromWire)
      .filter((row) => row.turnId !== '');
    const rows = reconcileRows(this.cachedQueuedTurns(sid), fetched);
    writeSnapshot(this.snapshotKey('queued', sid), rows);
    return rows;
  }

  async queuedTurns(
    sid: string,
    signal?: AbortSignal,
  ): Promise<{ turns: QueuedTurn[]; paused: QueuePausedInfo | null }> {
    const response = await this.request<{
      turns: SubmittedTurn[];
      queue_paused?: Record<string, unknown> | null;
    }>('GET', `/v1/sessions/${encodeURIComponent(sid)}/turns?status=queued`, undefined, signal);
    const paused = queuePausedFromWire(response.queue_paused);
    this.queuePaused.set(sid, paused);
    return { turns: this.storeQueuedTurns(sid, response.turns), paused };
  }

  /**
   * The typed input requests this session is BLOCKED on right now.
   *
   * SSE carries `view.open` live, but a screen opened (or reloaded,
   * or woken by a push) while a run is already parked has to read the open
   * forms back from here — the same snapshot the TUI restores from.
   */
  async inputViews(sid: string, signal?: AbortSignal): Promise<HumanInputRequest[]> {
    const response = await this.request<{ requests: unknown[] }>(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}/views/input`,
      undefined,
      signal,
    );
    return inputViewsFromWire(response.requests);
  }

  /** Apply one closed operator action to either View kind. */
  viewAction(sid: string, viewId: string, action: ViewAction): Promise<ViewActionOutcome> {
    return this.request<ViewActionOutcome>(
      'POST',
      `/v1/sessions/${encodeURIComponent(sid)}/views/${encodeURIComponent(viewId)}/actions`,
      action,
    );
  }

  /**
   * The live views this session is SHOWING right now.
   *
   * SSE carries `view.open` and its patches, but a screen opened
   * (or woken by a push, or reconnected after a gap) while a run is already
   * halfway through a scan has to read the picture back from here — the same
   * snapshot the TUI pane restores from, already materialized by the engine.
   */
  async liveViews(sid: string, signal?: AbortSignal): Promise<LiveView[]> {
    const response = await this.request<{ views: unknown[] }>(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}/views/live`,
      undefined,
      signal,
    );
    return liveViewsFromWire(response.views);
  }

  /**
   * One page of a log node's RECORD — what scrolled off the window it shows.
   *
   * The record is a file the engine appends to, so this reads a RANGE of it
   * rather than the whole run: a phone must not have to hold 100 000 lines to
   * look at the twenty before the ones on screen.
   */
  liveViewLog(
    sid: string,
    viewId: string,
    nodeId: string,
    from: number,
    limit: number,
    search = '',
    signal?: AbortSignal,
  ): Promise<LiveLogPage> {
    // The node rides the query string: node ids are free text a surface chose and may
    // hold `/`, which the gateway refuses inside a path segment.
    const query = `?node=${encodeURIComponent(nodeId)}&from=${encodeURIComponent(from)}&limit=${encodeURIComponent(limit)}&query=${encodeURIComponent(search)}`;
    return this.request<LiveLogPage>(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}/views/live/${encodeURIComponent(viewId)}/log${query}`,
      undefined,
      signal,
    );
  }

  /**
   * Status of ONE turn as the gateway REGISTRY knows it — `null` when this
   * daemon's live registry has no such row.
   *
   * This is the transport-independent liveness probe: the running-turn bubble normally
   * settles on the terminal SSE frame, but a reconnect gap (or a backgrounded
   * tab whose stream was torn down mid-turn) can swallow that one frame, and
   * then the bubble streams forever for a turn the gateway finished minutes
   * ago. Asking the registry costs one direct map lookup and never hydrates history.
   *
   * A still-`running`/`queued` turn is REPORTED, never flattened to `null`: a
   * caller that cannot tell "still working" from "never heard of it" has to
   * assume the worst about every quiet moment. One `shell` or `python_execution`
   * call blocks its iteration for as long as the command runs and emits no
   * frame at all until it returns, so that assumption tore the SSE stream down
   * every few seconds for the whole length of a long tool call.
   */
  async turnStatus(
    sid: string,
    tid: string,
    signal?: AbortSignal,
  ): Promise<Pick<TranscriptTurn, 'status' | 'content'> | null> {
    try {
      const row = await this.request<Record<string, unknown>>(
        'GET',
        `/v1/sessions/${encodeURIComponent(sid)}/turns/${encodeURIComponent(tid)}`,
        undefined,
        signal,
      );
      const status = String(row.status ?? '');
      if (status === '') return null;
      return {
        status,
        content: Array.isArray(row.content)
          ? (row.content as TranscriptTurn['content'])
          : undefined,
      };
    } catch (error) {
      if (error instanceof GatewayError && error.status === 404) return null;
      throw error;
    }
  }

  /**
   * The complete steps of turns that this process already read, by turn id.
   *
   * The transcript sends only the newest steps of a long turn, and `TurnTrace`
   * reads the rest when the turn comes near the reader. Without this cache, each
   * visit painted the short list first and the turn grew a moment later, so the
   * view jumped. Memory only and bounded, like `sentAttachments`.
   */
  private readonly turnTraces = new Map<string, TranscriptIteration[]>();

  /** The complete steps of `turn`, or null when this process has not read all of them. */
  cachedTurnTrace(sid: string, turn: TranscriptTurn): TranscriptIteration[] | null {
    const key = `${sid}\u0000${turn.turn_id}`;
    const rows = this.turnTraces.get(key);
    // A trace that was read while the turn ran has fewer steps than the settled turn.
    const total =
      turn.iterations_total ?? (turn.iterations_offset ?? 0) + (turn.iterations?.length ?? 0);
    if (!rows || rows.length < total) return null;
    // Map order is the LRU order: re-insert to mark this trace as used.
    this.turnTraces.delete(key);
    this.turnTraces.set(key, rows);
    return rows;
  }

  /**
   * The iterations the gateway has ALREADY PERSISTED for ONE turn — the resume
   * source for a turn that is still running.
   *
   * The running-turn bubble is normally seeded by the `turn.started` frame, and every
   * later delta is dropped while it is null. That frame is emitted exactly once
   * and the hub subscribes LIVE-ONLY, so anyone who was not listening at that
   * instant never gets it: a cold open on a session that is already streaming,
   * and — the reported bug — an iOS webview whose WebContent process the OS
   * killed during a long background (Capacitor #7810/#7905), which reloads the
   * page mid-turn. The stream reconnects fine and then streams into nothing.
   *
   * This is the same trace the TUI resumes from, so the adopted bubble starts
   * with everything that happened while we were away instead of a blank one.
   * The rows also go to `cachedTurnTrace` for the next visit.
   */
  async turnTrace(sid: string, tid: string, signal?: AbortSignal): Promise<TranscriptIteration[]> {
    const response = await this.request<{ iterations?: unknown }>(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}/turns/${encodeURIComponent(tid)}/trace`,
      undefined,
      signal,
    );
    const rows = Array.isArray(response.iterations) ? (response.iterations as TranscriptIteration[]) : [];
    if (this.isSessionDeleted(sid)) return rows;
    const key = `${sid}\u0000${tid}`;
    this.turnTraces.delete(key);
    this.turnTraces.set(key, rows);
    while (this.turnTraces.size > SESSION_CACHE_LIMIT) {
      const oldest = this.turnTraces.keys().next();
      if (oldest.done) break;
      this.turnTraces.delete(oldest.value);
    }
    return rows;
  }

  /**
   * Resume a queue the gateway paused after a provider failure — retries the
   * held head immediately and clears the failure counter/circuit breaker.
   */
  resumeQueue(sid: string): Promise<unknown> {
    return this.request('POST', `/v1/sessions/${encodeURIComponent(sid)}/resume-queue`);
  }

  // ── SSE live stream ─────────────────────────────────────────────
  //
  // GET /v1/events?sids=<sid> streams `data: {json}\n\n` frames. We read the
  // response body as a stream and parse SSE frames by hand so it works in every
  // Capacitor webview (native EventSource can't attach the bearer header).

  /**
   * Multiplex many watched sessions over one SSE connection. Reconnects resume
   * each session independently from the cursor it carries.
   *
   * A NEGATIVE cursor is the gateway's REWIND request, not live-only delivery:
   * it resolves to the running turn's first frame, so that whole turn replays.
   * It therefore means "this device has no cursor" — and `resumeCursor` answers
   * it with the one this gateway last served, so a relaunch resumes in range.
   * When requested, fleet status shares this connection but never its cursors.
   */
  streamSessionEvents(
    cursors: Map<string, number>,
    onEvent: (event: SseEvent) => void,
    opts: {
      signal?: AbortSignal;
      includeFleet?: boolean;
      reason?: GatewayStreamReason;
      onOpen?: () => void;
      onError?: (error: unknown) => void;
      /** Fired once the retry loop has ENDED — the stream is no longer running. */
      onClosed?: () => void;
    } = {},
  ): () => void {
    const controller = new AbortController();
    // Released by `controller.abort()` when the subscription closes.
    const signal = opts.signal
      ? linkSignals([opts.signal, controller.signal]).signal
      : controller.signal;

    void (async () => {
      let retryMs = 400;
      let attemptNumber = 0;
      let reason: GatewayStreamReason = opts.reason ?? 'subscribe';
      while (!signal.aborted && cursors.size > 0) {
        // Per-attempt controller: the stall watchdog aborts only THIS
        // connection attempt, so the outer loop reconnects with up-to-date
        // cursors instead of dying with the caller's shared signal.
        const attempt = new AbortController();
        // Released by `attempt.abort()` when this attempt ends.
        const attemptSignal = linkSignals([signal, attempt.signal]).signal;
        const diagnostic = startRequestDiagnostic(this.base, 'GET', '/v1/events', {
          transport: 'sse',
          stream: 'sessions',
          attempt: ++attemptNumber,
          session_ids: [...cursors.keys()],
          reason,
        });
        const activity = trackSseActivity();
        let status = 0;
        let failure: { cause: unknown } | undefined;
        let stallExpired = false;
        let closed = false;
        let retryAfterMs = 0;
        let stallTimer: ReturnType<typeof setTimeout> | null = null;
        // One watchdog for both phases of the attempt: a short bound on the
        // connect, the heartbeat bound once frames are flowing. Either way the
        // abort hits only THIS attempt and the outer loop reconnects.
        const armStall = (ms: number) => {
          if (stallTimer) clearTimeout(stallTimer);
          stallTimer = setTimeout(() => {
            stallExpired = true;
            attempt.abort();
          }, ms);
        };
        try {
          armStall(SSE_CONNECT_TIMEOUT_MS);
          const spec = Array.from(
            cursors,
            ([sid, cursor]) => `${sid}:${this.resumeCursor(sid, cursor)}`,
          ).join(',');
          // An older gateway ignores scope=both and still serves sessions. Fleet
          // stays unready, so the list keeps its existing polling safety net.
          const scope = opts.includeFleet ? '&scope=both' : '';
          const response = await raceAbort(
            fetch(`${this.base}/v1/events?sids=${encodeURIComponent(spec)}${scope}`, {
              headers: this.headers({ Accept: 'text/event-stream' }),
              signal: attemptSignal,
            }),
            attemptSignal,
          );
          status = response.status;
          if (!response.ok || !response.body) {
            throw new GatewayError(response.status, `SSE HTTP ${response.status}`);
          }

          activity.opened();
          opts.onOpen?.();
          retryMs = 400;
          // Stall watchdog: the gateway sends a heartbeat every 15 s. If we
          // see nothing for 45 s the socket was silently frozen (iOS
          // backgrounding, dead NAT, half-open TCP) — abort this attempt so
          // the outer loop reconnects with the up-to-date cursor.
          armStall(SSE_STALL_TIMEOUT_MS);

          await raceAbort(
            readSseFrames(
              response.body,
              (json, frameName) => {
                // The session's own event LOG lives here. A transcription's
                // progress rides its own job stream under `VOICE_JOB_EVENT`, is
                // not an engine event, and never enters this reducer.
                if (frameName === VOICE_JOB_EVENT) return;
                try {
                  const event = JSON.parse(json) as SseEvent;
                  const sid =
                    typeof event.session_id === 'string'
                      ? event.session_id
                      : typeof event.sid === 'string'
                        ? event.sid
                        : '';
                  // Deliver FIRST, then advance the cursor: an event whose
                  // handler failed must replay on reconnect, never be skipped.
                  // Fleet frames use an independent sequence, even when they name
                  // a watched session. Only session frames own replay cursors.
                  onEvent(event);
                  if (event.scope !== 'fleet') {
                    if (
                      sid &&
                      cursors.has(sid) &&
                      event.type === 'subscription.ready' &&
                      typeof event.cursor === 'number'
                    ) {
                      cursors.set(sid, event.cursor);
                      this.rememberSessionCursor(sid, event.cursor);
                    } else if (sid && cursors.has(sid) && typeof event.seq === 'number') {
                      const advanced = Math.max(cursors.get(sid) ?? -1, event.seq);
                      cursors.set(sid, advanced);
                      this.rememberSessionCursor(sid, advanced);
                    }
                  }
                } catch {
                  // Ignore one malformed frame without ending sibling sessions.
                }
              },
              () => {
                activity.chunk();
                armStall(SSE_STALL_TIMEOUT_MS);
              },
              attemptSignal,
              activity.heartbeat,
            ),
            attemptSignal,
          );
          if (!signal.aborted) {
            closed = true;
            throw new GatewayError(0, 'event stream closed');
          }
        } catch (error) {
          failure = { cause: error };
          if (signal.aborted) break;
          opts.onError?.(error);
          // A 4xx is NOT a reason to abandon the app's only push channel. A
          // token refresh racing a request (401), a proxy's 403/404, or the
          // gateway's own 400 "no valid sids" right after it restarted used to
          // end this loop FOREVER: the open session screen then sat silent —
          // no stall watchdog fires, because there is no socket left to stall —
          // until the user backed out and re-entered. Every failure now backs
          // off and retries; the hub supervises what is left.
          retryAfterMs = retryMs;
          retryMs = Math.min(retryMs * 2, 5_000);
        } finally {
          finishRequestDiagnostic(diagnostic, {
            status,
            failure,
            signal,
            timedOut: stallExpired && !signal.aborted,
            ...(closed ? { outcome: 'closed' as const } : {}),
            stream: activity.snapshot(),
          });
          if (stallTimer) clearTimeout(stallTimer);
          attempt.abort();
        }
        reason = activity.retryReason(status, stallExpired, closed);
        if (retryAfterMs > 0) await abortableDelay(retryAfterMs, signal);
      }
      opts.onClosed?.();
    })();

    return () => controller.abort();
  }

  /**
   * Watch the WHOLE machine's session status over ONE connection.
   *
   * A list learns that a run started, parked on a human or ended by RE-READING its
   * window on a timer, and every active session invalidates that window's ETag — so
   * a phone paid the whole payload every few seconds to discover one boolean. This
   * stream carries the boolean instead: one small `session.status` frame per real
   * transition, for every session on the machine, visited or not.
   *
   * It has no cursor and no replay by design — the gateway holds no fleet ring — so
   * a gap is repaired by one cold windowed read. `onOpen` is where that read goes.
   */
  streamFleetStatus(
    onEvent: (event: SseEvent) => void,
    opts: {
      signal?: AbortSignal;
      reason?: GatewayStreamReason;
      onOpen?: () => void;
      onError?: (error: unknown) => void;
      /** Fired once the retry loop has ENDED — the stream is no longer running. */
      onClosed?: () => void;
    } = {},
  ): () => void {
    const controller = new AbortController();
    // Released by `controller.abort()` when the subscription closes.
    const signal = opts.signal
      ? linkSignals([opts.signal, controller.signal]).signal
      : controller.signal;

    void (async () => {
      let retryMs = 400;
      let attemptNumber = 0;
      let reason: GatewayStreamReason = opts.reason ?? 'subscribe';
      while (!signal.aborted) {
        // Per-attempt controller, exactly as the multiplexed stream: the stall
        // watchdog aborts only THIS attempt and the outer loop reconnects.
        const attempt = new AbortController();
        // Released by `attempt.abort()` when this attempt ends.
        const attemptSignal = linkSignals([signal, attempt.signal]).signal;
        const diagnostic = startRequestDiagnostic(this.base, 'GET', '/v1/events', {
          transport: 'sse',
          stream: 'fleet',
          attempt: ++attemptNumber,
          reason,
        });
        const activity = trackSseActivity();
        let status = 0;
        let failure: { cause: unknown } | undefined;
        let stallExpired = false;
        let closed = false;
        let retryAfterMs = 0;
        let stallTimer: ReturnType<typeof setTimeout> | null = null;
        const armStall = (ms: number) => {
          if (stallTimer) clearTimeout(stallTimer);
          stallTimer = setTimeout(() => {
            stallExpired = true;
            attempt.abort();
          }, ms);
        };
        try {
          armStall(SSE_CONNECT_TIMEOUT_MS);
          const response = await raceAbort(
            fetch(`${this.base}/v1/events?scope=fleet`, {
              headers: this.headers({ Accept: 'text/event-stream' }),
              signal: attemptSignal,
            }),
            attemptSignal,
          );
          status = response.status;
          if (!response.ok || !response.body) {
            throw new GatewayError(response.status, `SSE HTTP ${response.status}`);
          }

          activity.opened();
          opts.onOpen?.();
          retryMs = 400;
          armStall(SSE_STALL_TIMEOUT_MS);

          await raceAbort(
            readSseFrames(
              response.body,
              (json, frameName) => {
                // A transcription's progress rides its own job stream and is not a
                // fact about any session's status.
                if (frameName === VOICE_JOB_EVENT) return;
                try {
                  onEvent(JSON.parse(json) as SseEvent);
                } catch {
                  // One malformed frame must not end the list's only push channel.
                }
              },
              () => {
                activity.chunk();
                armStall(SSE_STALL_TIMEOUT_MS);
              },
              attemptSignal,
              activity.heartbeat,
            ),
            attemptSignal,
          );
          if (!signal.aborted) {
            closed = true;
            throw new GatewayError(0, 'fleet stream closed');
          }
        } catch (error) {
          failure = { cause: error };
          if (signal.aborted) break;
          opts.onError?.(error);
          // No status ends this loop: the alternative to a retry is a list that
          // goes back to re-reading its window forever.
          retryAfterMs = retryMs;
          retryMs = Math.min(retryMs * 2, 5_000);
        } finally {
          finishRequestDiagnostic(diagnostic, {
            status,
            failure,
            signal,
            timedOut: stallExpired && !signal.aborted,
            ...(closed ? { outcome: 'closed' as const } : {}),
            stream: activity.snapshot(),
          });
          if (stallTimer) clearTimeout(stallTimer);
          attempt.abort();
        }
        reason = activity.retryReason(status, stallExpired, closed);
        if (retryAfterMs > 0) await abortableDelay(retryAfterMs, signal);
      }
      opts.onClosed?.();
    })();

    return () => controller.abort();
  }
}

/**
 * End an await when its signal aborts even if WebKit leaves the underlying promise pending.
 *
 * Safari normally rejects `fetch` and `reader.read()` on abort. After an iPhone changes
 * network interfaces while suspended, however, either promise can remain pending forever.
 * Racing the signal itself makes our deadline real; aborting the transport is then only the
 * resource cleanup, not the mechanism on which reconnect liveness depends.
 */
function raceAbort<T>(work: PromiseLike<T> | T, signal: AbortSignal): Promise<T> {
  if (signal.aborted) {
    // The caller already STARTED `work` — an in-flight fetch or body read. Returning
    // without touching it leaves its own abort rejection with no handler, which Node
    // reports as an unhandled rejection and which fails a whole test run whose every
    // case passed. Swallow only that abandoned tail; the caller still sees the abort.
    void Promise.resolve(work).catch(() => undefined);
    return Promise.reject(signal.reason ?? new DOMException('Aborted', 'AbortError'));
  }
  return new Promise<T>((resolve, reject) => {
    let settled = false;
    const cleanup = () => signal.removeEventListener('abort', onAbort);
    const onAbort = () => {
      if (settled) return;
      settled = true;
      cleanup();
      reject(signal.reason ?? new DOMException('Aborted', 'AbortError'));
    };
    signal.addEventListener('abort', onAbort, { once: true });
    Promise.resolve(work).then(
      (value) => {
        if (settled) return;
        settled = true;
        cleanup();
        resolve(value);
      },
      (error: unknown) => {
        if (settled) return;
        settled = true;
        cleanup();
        reject(error);
      },
    );
  });
}

/** Wait `ms`, or less if `signal` aborts first; the signal keeps no listener after. */
export function abortableDelay(ms: number, signal: AbortSignal): Promise<void> {
  return new Promise((resolve) => {
    if (signal.aborted) {
      resolve();
      return;
    }
    const done = () => {
      window.clearTimeout(timer);
      signal.removeEventListener('abort', done);
      resolve();
    };
    const timer = window.setTimeout(done, ms);
    signal.addEventListener('abort', done, { once: true });
  });
}

/** A signal that follows several others, and the call that stops it following them. */
export interface LinkedSignal {
  signal: AbortSignal;
  release: () => void;
}

/**
 * A signal that aborts when any input aborts. A long-lived input (a caller's signal
 * across requests, a stream's across reconnects) must not keep a listener per call, so
 * call `release` once the work is over, unless it ended by aborting an input.
 * `AbortSignal.any` links weakly by itself. The fallback for older WebViews holds its
 * links until an input aborts or `release` runs, so no abort is lost to a collected
 * signal while its work still runs.
 */
export function linkSignals(signals: AbortSignal[]): LinkedSignal {
  if (typeof AbortSignal.any === 'function') {
    return { signal: AbortSignal.any(signals), release: () => {} };
  }
  const ctrl = new AbortController();
  const release = () => {
    for (const input of signals) input.removeEventListener('abort', onAbort);
  };
  function onAbort() {
    release();
    ctrl.abort();
  }
  if (signals.some((input) => input.aborted)) ctrl.abort();
  else for (const input of signals) input.addEventListener('abort', onAbort);
  return { signal: ctrl.signal, release };
}
