// Typed client for the vis gateway HTTP/SSE API. This is the companion's twin
// of src/com/blockether/vis/internal/gateway/client.clj — the SAME daemon the
// TUI and other channels drive, reached over LAN / Tailscale / cloudflared.
//
// Auth: any non-loopback (or --require-token) gateway demands a bearer token;
// we send it on every request. A 401 surfaces as GatewayError so the UI can
// prompt a re-pair.

import type { PushGateway } from "./relay";
import {
  ATTACHMENT_MEMORY_BUDGET,
  cacheVictims,
  readCachedAttachment,
  writeCachedAttachment,
} from "./attachment-cache";
import type {
  AuthFlow,
  AuthVerdict,
  ModelPref,
  QueuedAttachment,
  QueuedTurn,
  ProviderLimits,
  ProviderPreset,
  ProviderStatus,
  RouterProvider,
  GatewayAttachment,
  GatewayCapabilities,
  GatewayHealth,
  GatewayOverview,
  GatewayConn,
  FileSuggestion,
  GatewayStatus,
  IterationAttachment,
  SessionArtifactRow,
  Session,
  SessionUsage,
  SettingsResponse,
  SlashCommand,
  SseEvent,
  SubmittedTurn,
  Toggle,
  TranscriptIteration,
  TranscriptTurn,
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
  ForkPoint,
  WorkspaceDraft,
  BrowseEntry,
  BrowseListing,
} from "./types";
import { PROTOCOL_HEADERS } from "./compat";
import { withSavedAttachment } from "./artifacts";
import {
  humanInputRequestsFromWire,
  type HumanInputOutcome,
  type HumanInputRequest,
  type HumanInputValues,
} from "./human-input";
import {
  liveViewsFromWire,
  type LiveLogPage,
  type LiveView,
} from "./live-view";
import {
  flushSnapshots,
  hydrateSnapshots,
  installSnapshotFlushOnHide,
  scheduleSnapshotFlush,
  type SnapshotStores,
} from "./snapshot-store";

export class GatewayError extends Error {
  status: number;
  body: unknown;
  constructor(status: number, message: string, body?: unknown) {
    super(message);
    this.name = "GatewayError";
    this.status = status;
    this.body = body;
  }
}

// One transcript-search hit inside a session: which SIDE it landed on (the
// user's own request, the assistant's answer, or the reasoning aside it thought
// out loud), a short preview snippet, and when it happened. Several travel per
// session, best band first.
export interface SessionMatchHit {
  side: "request" | "reply" | "thinking";
  snippet: string;
  at: number | null;
}

// One matching session, tagged with WHERE the query hit plus up to a handful of
// preview snippets. Only those small windows travel — never the conversation.
// `requestSnippet`/`replySnippet` are the first hit of each side, kept for
// callers that want a single line.
//
// `rank` is the gateway's own relevance band — 0 the session's TITLE, 1 the
// user's own words, 2 the assistant's answer, 3 its thinking — and the array
// arrives in that order. Search relevance is decided once, on the server, for
// every client; this app paints that order rather than re-deriving one.
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
  session_id: string;
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
 * How long a `/v1/router` payload stays good. Assembling it costs the daemon a
 * live auth + limits probe per provider, so five minutes of reuse turns "open
 * the model picker" from a multi-second wait into an instant paint.
 */
export const ROUTER_TTL_MS = 5 * 60 * 1000;

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
    VOICE_TIMEOUT_FLOOR_MS +
    Math.ceil(bytes / VOICE_BYTES_PER_SECOND) * VOICE_TIMEOUT_PER_SECOND_MS
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
 * Mirror of `wire/voice-job-event` (src/com/blockether/vis/internal/gateway/wire.clj)
 * and of `features.voice.progress_event` in `GET /v1/capabilities`; a
 * cross-channel test pins the two spellings together. It exists because this
 * client reads TWO unrelated SSE resources: a session's ordered event LOG (`id:`
 * cursor, engine event types, replayed from that cursor, open for the session's
 * life) and ONE transcription job's state (no cursor, no replay, this single
 * name, ends on the terminal frame). Keying off the name is what keeps a job
 * frame out of the session reducer, and a session event out of the progress
 * notice.
 */
export const VOICE_JOB_EVENT = "voice.job";

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
): Promise<void> {
  const reader = body.getReader();
  const decoder = new TextDecoder();
  let buffer = "";
  for (;;) {
    const { value, done } = await reader.read();
    onChunk?.();
    if (done) break;
    buffer += decoder.decode(value, { stream: true }).replace(/\r\n/g, "\n");
    let boundary: number;
    while ((boundary = buffer.indexOf("\n\n")) >= 0) {
      const frame = buffer.slice(0, boundary);
      buffer = buffer.slice(boundary + 2);
      const lines = frame.split("\n").map((line) => line.trimStart());
      const named = lines.find((line) => line.startsWith("event:"));
      const event = named ? named.slice(6).trim() || null : null;
      for (const line of lines) {
        if (!line.startsWith("data:")) continue;
        const json = line.slice(5).trim();
        if (json) onData(json, event);
      }
    }
  }
}

/** The gateway's own `{error}` sentence, or the bare status when it sent none. */
function errorText(parsed: unknown, status: number): string {
  const error = (parsed as { error?: string | { message?: string } })?.error;
  const message =
    error instanceof Object ? error.message : (error as string | undefined);
  return message || `HTTP ${status}`;
}

/** Router rows per gateway base URL, shared by every screen and client instance. */
const routerCache = new Map<string, { at: number; rows: RouterProvider[] }>();

/** In-flight router reads per base URL, so concurrent opens cost one request. */
const routerInflight = new Map<string, Promise<RouterProvider[]>>();

/**
 * Last-known payload per gateway+resource, kept for the tab's lifetime so a
 * screen that REMOUNTS — switching tabs, backing out of a session, reopening
 * one — paints its previous frame immediately and revalidates underneath
 * instead of flashing an empty skeleton. Nothing here is ever served as truth:
 * every reader still fires the real request and reconciles the answer on top.
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
const revisionWatchers = new Map<
  string,
  Set<(turns: TranscriptTurn[]) => void>
>();

function transcriptStamp(row: Session | null | undefined): string {
  if (!row) return "";
  // Neither fact present = a payload that cannot express movement (an older
  // gateway's detail row). Return '' so callers FETCH instead of trusting a
  // constant stamp that both never invalidates and never detects a change.
  if (row.turn_count === undefined && row.modified_at === undefined) return "";
  return `${row.turn_count ?? ""}\u0000${row.modified_at ?? ""}`;
}

/** Bound the cache so hopping through many sessions cannot pin every transcript. */
const SNAPSHOT_LIMIT = 32;

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
installSnapshotFlushOnHide(snapshotStores);

/** Persist the caches NOW — used when the app is being torn down. */
export function persistGatewayCaches(): void {
  flushSnapshots(snapshotStores);
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
  for (const oldest of snapshots.keys()) {
    if (snapshots.size <= SNAPSHOT_LIMIT) break;
    snapshots.delete(oldest);
  }
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
  if (
    typeof a !== "object" ||
    typeof b !== "object" ||
    a === null ||
    b === null
  )
    return false;
  if (Array.isArray(a) || Array.isArray(b)) {
    if (!Array.isArray(a) || !Array.isArray(b) || a.length !== b.length)
      return false;
    return a.every((item, index) => sameJson(item, b[index]));
  }
  const left = a as Record<string, unknown>;
  const right = b as Record<string, unknown>;
  const keys = Object.keys(left);
  if (keys.length !== Object.keys(right).length) return false;
  return keys.every(
    (key) =>
      Object.prototype.hasOwnProperty.call(right, key) &&
      sameJson(left[key], right[key]),
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
 * The gateway hashes `[total limit root after rows awaiting]` into the validator, so a
 * 304 revalidates the whole record — the counts may be reused, not just the rows.
 */
type SessionsWindow = {
  etag: string;
  /** The cursor this window was ASKED for; `HEAD_CURSOR` is the top of the list. */
  after: string;
  rows: Session[];
  total: number;
  hasMore: boolean;
  /**
   * The cursor of this window's LAST row — what the next page is asked for. A cursor
   * names a ROW (`state/list-sessions-page`), which is why a walk cannot tear.
   */
  nextCursor: string;
  /**
   * The sessions this gateway says are PARKED on an unanswered human-input request,
   * complete and from OUTSIDE the window (`state/list-sessions-page`). It rides the
   * same validator as the rows, so a 304 says the demand is unchanged too.
   */
  awaiting: Session[];
};

/**
 * Session-list page size. Measured on a 448-session gateway: the whole list is
 * ~326 ms / 315 KB to build and send, one 100-row window ~71 ms / 69 KB. The
 * first window is already more rows than any screen can show, so the list paints
 * at the cost of the window and the remainder streams in behind it.
 */
const SESSIONS_PAGE = 100;

/** The first window of the list: the one page that is asked for with no cursor. */
const HEAD_CURSOR = "";

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

/**
 * One live queue delta a screen has already applied: the row as it was added, or
 * `null` for a row that LEFT the queue (`turn.queued.drained` / `.deleted`).
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
 * delta NEWER than it wins over the read. `forget` names the ids whose removal
 * the read has just written back into the cache, to be dropped there too.
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
      if (!byId.has(tid)) appended.push(delta.row);
    } else {
      byId.delete(tid);
      forget.push(tid);
    }
  }
  return {
    rows: [...rows.filter((row) => byId.has(row.turnId)), ...appended],
    forget,
  };
}

function normalizeBase(url: string): string {
  return url.replace(/\/+$/, "");
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
  const request = typeof row.request === "string" ? row.request : "";
  const preview =
    typeof row.request_preview === "string" ? row.request_preview : "";
  const rawAttachments = Array.isArray(row.attachment_previews)
    ? row.attachment_previews
    : [];
  const attachments: QueuedAttachment[] = rawAttachments.map((entry) => {
    const item = (entry ?? {}) as Record<string, unknown>;
    return {
      filename: typeof item.filename === "string" ? item.filename : "image",
      mediaType:
        typeof item.media_type === "string" ? item.media_type : "image",
      sizeLabel: typeof item.size_label === "string" ? item.size_label : "",
    };
  });
  return {
    turnId: String(row.turn_id ?? row.id ?? ""),
    request,
    preview: preview || request,
    attachments,
  };
}

export class GatewayClient {
  readonly base: string;
  private readonly token?: string;
  // (session, iteration, index) → the produced artifact's object URL. The row is
  // append-only and content-addressed by that triple, so one download per
  // picture — but BOUNDED, because every entry pins full decoded bytes.
  private readonly attachmentUrls = new Map<string, Promise<string>>();
  // What each of those rows actually COSTS, learned when its bytes land. A bound
  // counted in entries alone cannot tell 24 thumbnails from 24 video clips.
  private readonly attachmentSizes = new Map<string, number>();
  // How many MOUNTED tiles are painting each key right now. Eviction REVOKES an
  // object URL, and a revoked URL is a permanently broken `<img>` — so a picture
  // that is on screen must never be the one handed back to the collector.
  private readonly attachmentHolds = new Map<string, number>();
  // Last conditional-GET validators for the session LIST, per gateway: one per
  // page window keyed by the CURSOR it was asked for, plus the merged array they
  // were built from. Static because the screens build a throwaway client whenever
  // the connection object changes, and the snapshot cache it pairs with is
  // module-level too. Pinning by IDENTITY is what makes it safe: any other code path
  // that rewrites the sessions snapshot (a local delete, a rename) swaps that array,
  // every pin misses, and the next poll is an unconditional walk instead of 304s
  // restoring stale rows.
  private static readonly sessionsValidators = new Map<
    string,
    { full: Session[]; windows: Map<string, SessionsWindow> }
  >();

  /** Backing store of `parkedSessions`, refreshed by every list read. */
  private parked: Session[] = [];
  constructor(conn: GatewayConn) {
    this.base = normalizeBase(conn.url);
    this.token = conn.token;
  }

  /** Cache key for one of this gateway's snapshot-able payloads. */
  private snapshotKey(kind: string, sid?: string): string {
    return sid
      ? `${this.base}\u0000${kind}\u0000${sid}`
      : `${this.base}\u0000${kind}`;
  }

  private headers(extra?: HeadersInit): Headers {
    const h = new Headers(extra);
    if (this.token) h.set("Authorization", `Bearer ${this.token}`);
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
   */
  private async requestFull<T>(
    method: string,
    path: string,
    body?: unknown,
    signal?: AbortSignal,
    extraHeaders?: Record<string, string>,
  ): Promise<{
    status: number;
    data: T | undefined;
    etag: string | null;
    headers: Headers;
  }> {
    const headers = this.headers(extraHeaders);
    // A Blob is a RECORDING (or any raw upload) and travels as itself: it carries its
    // own media type and JSON-encoding it would destroy it.
    const isRaw = body instanceof Blob;
    if (body !== undefined && !isRaw)
      headers.set("Content-Type", "application/json");
    // Bound the whole exchange, not just the connect: a resumed request usually
    // parks on the BODY read, with its headers already delivered.
    const deadline = new AbortController();
    const timer = window.setTimeout(() => deadline.abort(), REQUEST_TIMEOUT_MS);
    // A caller that aborted (screen unmounted, session switched) is not a stall,
    // and must keep reporting itself as one.
    const stalled = () => deadline.signal.aborted && !signal?.aborted;
    const seconds = Math.round(REQUEST_TIMEOUT_MS / 1000);
    try {
      const attemptSignal = anySignal(
        signal ? [signal, deadline.signal] : [deadline.signal],
      );
      let res: Response;
      try {
        res = await fetch(this.base + path, {
          method,
          headers,
          body:
            body === undefined
              ? undefined
              : isRaw
                ? (body as Blob)
                : JSON.stringify(body),
          signal: attemptSignal,
        });
      } catch (e) {
        throw stalled()
          ? new GatewayError(0, `gateway did not answer within ${seconds}s`)
          : new GatewayError(0, `network error: ${(e as Error).message}`);
      }
      if (res.status === 304)
        return {
          status: 304,
          data: undefined,
          etag: res.headers.get("ETag"),
          headers: res.headers,
        };
      let text: string;
      try {
        text = await res.text();
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
        // The gateway writes its refusal as a SENTENCE (`{error: "no voice transcription
        // engine is registered - …"}`); reading only `error.message` turned every one of
        // them into "HTTP 501" on screen, which told the reader nothing they could act on.
        const problem = parsed as {
          error?: string | { message?: string };
        };
        const msg =
          (typeof problem?.error === "string"
            ? problem.error
            : problem?.error?.message) ?? `HTTP ${res.status}`;
        throw new GatewayError(res.status, msg, parsed);
      }
      return {
        status: res.status,
        data: parsed as T,
        etag: res.headers.get("ETag"),
        headers: res.headers,
      };
    } finally {
      window.clearTimeout(timer);
    }
  }

  private async request<T>(
    method: string,
    path: string,
    body?: unknown,
    signal?: AbortSignal,
  ): Promise<T> {
    return (await this.requestFull<T>(method, path, body, signal)).data as T;
  }

  // ── Health / status ─────────────────────────────────────────────
  status(signal?: AbortSignal): Promise<GatewayStatus> {
    return this.request<GatewayStatus>(
      "GET",
      "/v1/admin/status",
      undefined,
      signal,
    );
  }

  async ping(signal?: AbortSignal): Promise<boolean> {
    try {
      await this.request("GET", "/healthz", undefined, signal);
      return true;
    } catch (e) {
      // A token-gated gateway still answers /healthz; a 401 means "reachable
      // but unauthorized", which is a connection we should flag distinctly.
      if (e instanceof GatewayError && e.status === 401) throw e;
      return false;
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
    return this.request<GatewayHealth>("GET", "/healthz", undefined, signal);
  }

  /**
   * Last capabilities payload seen for THIS gateway — paint it, then revalidate.
   * Capabilities are a per-gateway fact (attachment limits, media types, whether
   * voice exists at all), so the answer from five seconds ago is still the right
   * first frame for a screen the user just re-entered.
   */
  cachedCapabilities(): GatewayCapabilities | null {
    return readSnapshot<GatewayCapabilities>(this.snapshotKey("capabilities"));
  }

  async capabilities(signal?: AbortSignal): Promise<GatewayCapabilities> {
    const response = await this.request<GatewayCapabilities>(
      "GET",
      "/v1/capabilities",
      undefined,
      signal,
    );
    writeSnapshot(this.snapshotKey("capabilities"), response);
    return response;
  }

  // ── Projects overview ───────────────────────────────────────────

  /**
   * Last projects overview seen for THIS gateway — paint it, then revalidate.
   *
   * This is the whole cure for the flicker: coming back to a gateway, the header
   * row is drawn from the numbers this device last saw for it, in the first
   * frame, instead of being re-tallied from session windows as they arrive.
   */
  cachedProjectsOverview(): GatewayOverview | null {
    return readSnapshot<GatewayOverview>(this.snapshotKey("projects-overview"));
  }

  /**
   * `GET /v1/projects/overview` — every project with its counts and the
   * gateway's totals, tallied by the process that already holds the facts.
   */
  async projectsOverview(signal?: AbortSignal): Promise<GatewayOverview> {
    const response = await this.request<GatewayOverview>(
      "GET",
      "/v1/projects/overview",
      undefined,
      signal,
    );
    writeSnapshot(this.snapshotKey("projects-overview"), response);
    return response;
  }

  // ── Native push devices ─────────────────────────────────────────
  /**
   * Last device list seen for THIS gateway. The notifications panel is opened over
   * and over on an answer that rarely changes, so it paints this and revalidates
   * instead of asking `Checking…` every time (see `lib/notify-verdict.ts`).
   */
  cachedDevices(): DevicesState | null {
    return readSnapshot(this.snapshotKey("devices"));
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
    const key = this.snapshotKey("devices");
    const held = readSnapshot<DevicesState>(key);
    if (held && Date.now() - (deviceReads.get(key) ?? 0) < DEVICES_FRESH_MS) {
      return held;
    }
    const flight = deviceFlights.get(key);
    if (flight) return flight;
    const reading = this.request<DevicesState>(
      "GET",
      "/v1/devices",
      undefined,
      signal,
    )
      .then((response) => {
        writeSnapshot(key, response);
        deviceReads.set(key, Date.now());
        return response;
      })
      .finally(() => {
        deviceFlights.delete(key);
      });
    deviceFlights.set(key, reading);
    return reading;
  }

  /**
   * Idempotent: re-registering the same token refreshes it, never duplicates.
   *
   * The answer names the row that was just written, so it is merged into the
   * held list instead of being re-read: the panel reloading after a press asks
   * this machine nothing.
   */
  async registerDevice(
    input: PushDeviceInput,
  ): Promise<{ device: PushDevice; push: PushStatus }> {
    const response = await this.request<{ device: PushDevice; push: PushStatus }>(
      "POST",
      "/v1/devices",
      input,
    );
    const key = this.snapshotKey("devices");
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
      "DELETE",
      `/v1/devices/${encodeURIComponent(token)}`,
    );
    deviceReads.delete(this.snapshotKey("devices"));
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
   * Whether the listening engine is ready, still downloading (with how far it has got), or
   * failed and why. `start` POSTs instead: prepare the engine, which begins the download.
   */
  voiceModel(start = false, signal?: AbortSignal): Promise<VoiceModelState> {
    return this.request<VoiceModelState>(
      start ? "POST" : "GET",
      "/v1/voice/model",
      undefined,
      signal,
    );
  }

  /** [[voiceModel]] for the speaking direction. */
  speechModel(start = false, signal?: AbortSignal): Promise<VoiceModelState> {
    return this.request<VoiceModelState>(
      start ? "POST" : "GET",
      "/v1/speech/model",
      undefined,
      signal,
    );
  }

  // ── Voices: what this MACHINE can speak with ────────────────────
  //
  // No session in these paths on purpose. A cloning voice is a stored recording, so it
  // belongs to the machine, and the screen that manages voices is settings — which is
  // reading a machine and not a session.

  /** Every voice the speaking engine can use, plus whether it can learn another one. */
  speechVoices(signal?: AbortSignal): Promise<SpeechVoices> {
    return this.request<SpeechVoices>(
      "GET",
      "/v1/speech/voices",
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
    signal?: AbortSignal,
  ): Promise<SpeechVoice> {
    const query = new URLSearchParams({ name: about.name });
    if (about.lang) query.set("lang", about.lang);
    if (about.text) query.set("text", about.text);
    const answer = await this.request<{ voice: SpeechVoice }>(
      "POST",
      `/v1/speech/voices?${query.toString()}`,
      clip,
      signal,
    );
    return answer.voice;
  }

  /** Take an imported voice back. 404 means the catalogue on screen is stale. */
  async forgetSpeechVoice(id: string, signal?: AbortSignal): Promise<void> {
    await this.request(
      "DELETE",
      `/v1/speech/voices/${encodeURIComponent(id)}`,
      undefined,
      signal,
    );
  }

  /**
   * Speak a line ON THE MACHINE and hand back the audio.
   *
   * Two answers, one call: a short line comes back AS the bytes in a single round
   * trip, a long one answers 202 with a job that is followed to its audio here. The
   * caller only ever wanted the sound, and where the threshold sits is the gateway's
   * to publish (`features.speech.inline_max_chars`), never this client's to guess.
   *
   * `fetch` directly rather than `request`: `request` reads every answer as text, and
   * a WAV is not text.
   */
  async speakText(
    sid: string,
    text: string,
    voice?: string | null,
    signal?: AbortSignal,
  ): Promise<Blob> {
    const base = `/v1/sessions/${encodeURIComponent(sid)}/speech`;
    const answer = await this.audioFetch("POST", base, {
      body: JSON.stringify(voice ? { text, voice } : { text }),
      contentType: "application/json",
      signal,
    });
    if (answer.status !== 202) return await answer.blob();
    const started = (await answer.json()) as SpeechJob;
    const finished = await this.awaitSpeechJob(sid, started, signal);
    const audio = await this.audioFetch(
      "GET",
      `${base}/jobs/${encodeURIComponent(finished.id)}/audio`,
      { signal },
    );
    const blob = await audio.blob();
    // The audio is ours now, so the machine may forget the job. A failure here costs
    // nothing - finished jobs expire on their own.
    void this.request(
      "DELETE",
      `${base}/jobs/${encodeURIComponent(finished.id)}`,
      undefined,
      signal,
    ).catch(() => undefined);
    return blob;
  }

  /** One request whose answer is BYTES: no text read, no JSON parse, errors still named. */
  private async audioFetch(
    method: string,
    path: string,
    options: { body?: BodyInit; contentType?: string; signal?: AbortSignal },
  ): Promise<Response> {
    const headers = this.headers();
    if (options.contentType) headers.set("Content-Type", options.contentType);
    let res: Response;
    try {
      res = await fetch(this.base + path, {
        method,
        headers,
        body: options.body,
        signal: options.signal,
      });
    } catch (e) {
      throw new GatewayError(0, `network error: ${(e as Error).message}`);
    }
    if (!res.ok) {
      const body = await res.text().catch(() => "");
      let message = `HTTP ${res.status}`;
      try {
        const parsed = JSON.parse(body) as {
          error?: string | { message?: string };
        };
        const named =
          typeof parsed.error === "string" ? parsed.error : parsed.error?.message;
        if (named) message = named;
      } catch {
        if (body) message = body;
      }
      throw new GatewayError(res.status, message);
    }
    return res;
  }

  /** Follow one synthesis job to its end, or say why it will never get there. */
  private async awaitSpeechJob(
    sid: string,
    job: SpeechJob,
    signal?: AbortSignal,
  ): Promise<SpeechJob> {
    const path = `/v1/sessions/${encodeURIComponent(sid)}/speech/jobs/${encodeURIComponent(job.id)}`;
    const deadline = Date.now() + SPEECH_JOB_TIMEOUT_MS;
    let latest = job;
    while (!latest.is_done) {
      if (Date.now() > deadline) {
        throw new GatewayError(0, "the machine did not finish speaking in time");
      }
      await new Promise((resolve) =>
        setTimeout(resolve, SPEECH_JOB_POLL_MS),
      );
      latest = await this.request<SpeechJob>("GET", path, undefined, signal);
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
  ): Promise<VoiceJob> {
    const budget = voiceTimeoutMs(wav.size);
    const seconds = Math.round(budget / 1000);
    return new Promise<VoiceJob>((resolve, reject) => {
      if (signal?.aborted) {
        reject(signal.reason ?? new DOMException("Aborted", "AbortError"));
        return;
      }
      const xhr = new XMLHttpRequest();
      const onAbort = () => xhr.abort();
      const done = () => signal?.removeEventListener("abort", onAbort);
      xhr.open(
        "POST",
        `${this.base}/v1/sessions/${encodeURIComponent(sid)}/voice`,
      );
      xhr.timeout = budget;
      this.headers({ "Content-Type": "audio/wav" }).forEach((value, key) =>
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
        done();
        let parsed: unknown;
        try {
          parsed = xhr.responseText ? JSON.parse(xhr.responseText) : undefined;
        } catch {
          parsed = xhr.responseText;
        }
        if (xhr.status >= 200 && xhr.status < 300) {
          onUploaded(100);
          resolve(parsed as VoiceJob);
          return;
        }
        reject(
          new GatewayError(xhr.status, errorText(parsed, xhr.status), parsed),
        );
      };
      xhr.onerror = () => {
        done();
        reject(new GatewayError(0, "network error: upload failed"));
      };
      xhr.ontimeout = () => {
        done();
        reject(
          new GatewayError(
            0,
            `transcription did not answer within ${seconds}s`,
          ),
        );
      };
      xhr.onabort = () => {
        done();
        reject(signal?.reason ?? new DOMException("Aborted", "AbortError"));
      };
      signal?.addEventListener("abort", onAbort, { once: true });
      xhr.send(wav);
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
    const watchdog = new AbortController();
    const streamSignal = signal
      ? anySignal([signal, watchdog.signal])
      : watchdog.signal;
    const seen: { job: VoiceJob | null } = { job: null };
    let stalled = false;
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
      const response = await fetch(
        `${this.base}/v1/sessions/${encodeURIComponent(sid)}/voice/jobs/${encodeURIComponent(jobId)}/events`,
        {
          headers: this.headers({ Accept: "text/event-stream" }),
          signal: streamSignal,
        },
      );
      if (!response.ok || !response.body) {
        let parsed: unknown;
        try {
          parsed = await response.json();
        } catch {
          parsed = undefined;
        }
        throw new GatewayError(
          response.status,
          errorText(parsed, response.status),
          parsed,
        );
      }
      await readSseFrames(
        response.body,
        (json, event) => {
          // This stream carries `voice.job` frames and nothing else. Any other
          // name (an intermediary's own notice, a future kind sharing the
          // connection, a session event from a misrouted URL) is not this job's
          // progress and must never be reported as it.
          if (event !== VOICE_JOB_EVENT) return;
          let job: VoiceJob;
          try {
            job = JSON.parse(json) as VoiceJob;
          } catch {
            return;
          }
          if (!job || typeof job !== "object" || !job.id) return;
          seen.job = job;
          onJob(job);
        },
        armStall,
      );
    } catch (error) {
      if (stalled && !signal?.aborted) {
        throw new GatewayError(
          0,
          `transcription stopped reporting for ${Math.round(VOICE_STALL_TIMEOUT_MS / 1000)}s`,
        );
      }
      throw error;
    } finally {
      if (timer) clearTimeout(timer);
      watchdog.abort();
    }
    const job = seen.job;
    if (!job?.is_done) {
      throw new GatewayError(0, "transcription ended before the transcript");
    }
    return job;
  }

  /** Drop a collected job. Finished jobs also expire on the gateway by themselves. */
  async forgetVoiceJob(sid: string, jobId: string): Promise<void> {
    try {
      await this.request(
        "DELETE",
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
    } = {},
  ): Promise<VoiceTranscript> {
    const { onProgress, signal } = opts;
    // A reporting callback is a UI detail; it can never fail a transcription.
    const report = (progress: VoiceProgress) => {
      try {
        onProgress?.(progress);
      } catch {
        /* ignored */
      }
    };
    report({ phase: "uploading", progress: 0 });
    const accepted = await this.uploadVoice(
      sid,
      wav,
      (percent) => report({ phase: "uploading", progress: percent }),
      signal,
    );
    report({
      phase: accepted.phase ?? "queued",
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
    if (job.phase === "failed" || job.error) {
      throw new GatewayError(0, job.error || "transcription failed");
    }
    return { text: job.text ?? "" };
  }

  // ── Settings (shared feature-toggle registry, same as TUI) ──────
  /** Last settings payload seen for this gateway — paint it, then revalidate. */
  cachedSettings(): SettingsResponse | null {
    return readSnapshot<SettingsResponse>(this.snapshotKey("settings"));
  }

  async settings(signal?: AbortSignal): Promise<SettingsResponse> {
    const response = await this.request<SettingsResponse>(
      "GET",
      "/v1/settings?channel=all",
      undefined,
      signal,
    );
    writeSnapshot(this.snapshotKey("settings"), response);
    return response;
  }

  /**
   * One toggle by id, exactly as it was last seen. The composer footer paints
   * this on its FIRST frame: without it the reasoning chip is simply absent
   * until a round trip lands, on every session open, for a value that changes
   * once in a blue moon.
   */
  cachedSetting(id: string): Toggle | null {
    return readSnapshot<Toggle>(this.snapshotKey("setting", id));
  }

  /**
   * One toggle by id. `/v1/settings` only lists what the settings sheet shows,
   * so screen-owned knobs (reasoning effort lives in the composer footer) are
   * read one at a time here. The answer is snapshotted for the seed above.
   */
  async setting(id: string, signal?: AbortSignal): Promise<Toggle> {
    const toggle = await this.request<Toggle>(
      "GET",
      `/v1/settings/${encodeURIComponent(id)}`,
      undefined,
      signal,
    );
    writeSnapshot(this.snapshotKey("setting", id), toggle);
    return toggle;
  }

  async setSetting(
    id: string,
    action: "toggle" | "cycle" | "value",
    value?: string,
  ): Promise<Toggle> {
    const updated = await this.request<Toggle>("POST", "/v1/settings", {
      id,
      action,
      value,
    });
    // The by-id seed the composer reads is the same fact, so keep it in step —
    // otherwise cycling reasoning effort here would repaint the OLD word on the
    // next open until the revalidation landed.
    writeSnapshot(this.snapshotKey("setting", id), updated);
    // Patch the one toggle that changed instead of dropping the snapshot, so
    // reopening the dialog paints the NEW value rather than a blank sheet.
    const cached = this.cachedSettings();
    if (cached) {
      writeSnapshot(this.snapshotKey("settings"), {
        ...cached,
        groups: (cached.groups ?? []).map((group) => ({
          ...group,
          toggles: group.toggles.map((toggle) =>
            toggle.id === updated.id ? updated : toggle,
          ),
        })),
      });
    }
    return updated;
  }

  // ── Gateway-owned MCP servers ───────────────────────────────────
  async mcpServers(signal?: AbortSignal): Promise<McpServer[]> {
    return (
      (
        await this.request<McpServersResponse>(
          "GET",
          "/v1/mcp/servers",
          undefined,
          signal,
        )
      ).servers ?? []
    );
  }

  async saveMcpServer(
    name: string,
    server: McpServerInput,
  ): Promise<McpServer> {
    return this.request<McpServer>("POST", "/v1/mcp/servers", { name, server });
  }

  async setMcpServerEnabled(
    name: string,
    enabled: boolean,
  ): Promise<McpServer> {
    return this.request<McpServer>(
      "POST",
      `/v1/mcp/servers/${encodeURIComponent(name)}/actions/enable`,
      { enabled },
    );
  }

  async deleteMcpServer(name: string): Promise<void> {
    await this.request("DELETE", `/v1/mcp/servers/${encodeURIComponent(name)}`);
  }

  // Kill/start are RUNTIME ops, not config edits: they work for hand-written
  // servers too, because stopping a runaway child process is not rewriting
  // somebody's `vis.yml`. A kill holds until `startMcpServer`.
  async killMcpServer(name: string): Promise<McpServer> {
    return this.request<McpServer>(
      "POST",
      `/v1/mcp/servers/${encodeURIComponent(name)}/actions/kill`,
    );
  }

  async startMcpServer(name: string): Promise<McpServer> {
    return this.request<McpServer>(
      "POST",
      `/v1/mcp/servers/${encodeURIComponent(name)}/actions/start`,
    );
  }

  // MCP OAuth, headless: the gateway mints the flow and keeps every secret. This
  // device only shows `url` and hands back whatever the browser landed on. When
  // that browser can reach the gateway's loopback listener the flow finishes by
  // itself and `mcpAuthPoll` reports it.
  async mcpAuthStart(name: string): Promise<McpAuthFlow> {
    return this.request<McpAuthFlow>(
      "POST",
      `/v1/mcp/servers/${encodeURIComponent(name)}/auth/start`,
    );
  }

  async mcpAuthComplete(
    name: string,
    flowId: string,
    input: string,
  ): Promise<McpAuthFlow> {
    return this.request<McpAuthFlow>(
      "POST",
      `/v1/mcp/servers/${encodeURIComponent(name)}/auth/complete`,
      { flow_id: flowId, input },
    );
  }

  async mcpAuthPoll(name: string, flowId: string): Promise<McpAuthFlow> {
    return this.request<McpAuthFlow>(
      "POST",
      `/v1/mcp/servers/${encodeURIComponent(name)}/auth/poll`,
      { flow_id: flowId },
    );
  }

  async mcpAuthCancel(name: string, flowId: string): Promise<void> {
    await this.request(
      "POST",
      `/v1/mcp/servers/${encodeURIComponent(name)}/auth/cancel`,
      { flow_id: flowId },
    );
  }

  async mcpAuthLogout(name: string): Promise<McpAuthStatus> {
    return this.request<McpAuthStatus>(
      "POST",
      `/v1/mcp/servers/${encodeURIComponent(name)}/auth/logout`,
    );
  }

  async testMcpServer(
    name: string,
    server: McpServerInput,
  ): Promise<McpTestResult> {
    return this.request<McpTestResult>("POST", "/v1/mcp/servers/actions/test", {
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

  /** Cached rows at ANY age — paint these first, then revalidate. */
  cachedRouter(): RouterProvider[] | null {
    return routerCache.get(this.base)?.rows ?? null;
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
  }

  /**
   * Warm the router cache in the background. Fire-and-forget: never throws,
   * never blocks a render, and collapses into any request already in flight.
   */
  prefetchRouter(): void {
    if (this.isRouterFresh()) return;
    void this.router().catch(() => undefined);
  }

  async router(
    signal?: AbortSignal,
    opts?: { force?: boolean },
  ): Promise<RouterProvider[]> {
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
      inflight = this.request<{ providers: RouterProvider[] }>(
        "GET",
        "/v1/router",
      )
        .then((response) => {
          const rows = response.providers;
          routerCache.set(key, { at: Date.now(), rows });
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
      "GET",
      "/v1/provider-presets",
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
  async addProvider(
    providerId: string,
    baseUrl?: string,
  ): Promise<RouterProvider[]> {
    const response = await this.request<{ providers: RouterProvider[] }>(
      "POST",
      "/v1/providers",
      { id: providerId, base_url: baseUrl },
    );
    this.invalidateRouter();
    return response.providers;
  }

  /**
   * Drop a provider AND its stored credential, and answer with the fleet that
   * remains.
   */
  async removeProvider(providerId: string): Promise<RouterProvider[]> {
    const response = await this.request<{ providers: RouterProvider[] }>(
      "DELETE",
      `/v1/providers/${encodeURIComponent(providerId)}`,
    );
    this.invalidateRouter();
    return response.providers;
  }

  async setDefaultModel(provider: string, model: string): Promise<void> {
    await this.request<{ default_provider: string; default_model: string }>(
      "PATCH",
      "/v1/router",
      {
        role: "primary",
        provider,
        model,
      },
    );
    this.invalidateRouter();
  }

  /**
   * Tag the FALLBACK provider+model: the router's second root, used when the
   * default one cannot serve the turn. The daemon REFUSES a fallback on the
   * default's own provider (400) — a fallback is only useful somewhere else.
   */
  async setFallbackModel(provider: string, model: string): Promise<void> {
    await this.request<{ fallback_provider: string; fallback_model: string }>(
      "PATCH",
      "/v1/router",
      {
        role: "fallback",
        provider,
        model,
      },
    );
    this.invalidateRouter();
  }

  /** Drop the fallback tag: a blank `provider` on the fallback role clears it. */
  async clearFallbackModel(): Promise<void> {
    await this.request<{ fallback_provider: string | null }>(
      "PATCH",
      "/v1/router",
      {
        role: "fallback",
        provider: "",
        model: "",
      },
    );
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
    const seeded = readSnapshot<ModelPref>(this.snapshotKey("model", sid));
    if (seeded) return seeded;
    return (
      this.cachedSessions()?.find((row) => row.id === sid)?.model_pref ?? null
    );
  }

  /**
   * Record each row's pin as the per-session seed. Rows without one only clear a
   * seed that exists, so an unpinned fleet does not fill the snapshot store with
   * nulls it would then have to persist.
   */
  private seedSessionModels(rows: Session[]): void {
    for (const row of rows) {
      if (!row?.id) continue;
      const key = this.snapshotKey("model", row.id);
      const pref = row.model_pref ?? null;
      if (pref || readSnapshot<ModelPref>(key)) writeSnapshot(key, pref);
    }
  }

  async sessionModel(
    sid: string,
    signal?: AbortSignal,
  ): Promise<ModelPref | null> {
    const response = await this.request<{ model?: ModelPref }>(
      "GET",
      `/v1/sessions/${encodeURIComponent(sid)}/model`,
      undefined,
      signal,
    );
    const pref = response.model ?? null;
    writeSnapshot(this.snapshotKey("model", sid), pref);
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
    writeSnapshot(this.snapshotKey("model", sid), next);
    return next;
  }

  /** The gateway default as last seen — same first-frame job as above. */
  cachedDefaultModel(): ModelPref | null {
    return readSnapshot<ModelPref>(this.snapshotKey("model-default"));
  }

  /**
   * The gateway's DEFAULT provider+model — what a session with no pin actually
   * runs on. `sessionModel` answers only the explicit pin (null for "default"),
   * so any surface that names the live model needs this fallback.
   *
   * It rides `/v1/router`, which is a real auth/limits probe per provider on a
   * cold daemon — seconds. Hence the snapshot: the chip names the model at once
   * and this answer only ever corrects it.
   */
  async defaultModel(signal?: AbortSignal): Promise<ModelPref | null> {
    const rows = await this.router(signal);
    const row =
      rows.find((p) => p.is_default && p.default_model) ??
      rows.find((p) => p.default_model);
    if (!row?.default_model) return null;
    const pref = { provider: row.id, model: row.default_model };
    writeSnapshot(this.snapshotKey("model-default"), pref);
    return pref;
  }

  async setSessionModel(
    sid: string,
    provider: string,
    model: string,
  ): Promise<ModelPref | null> {
    const response = await this.request<{ model?: ModelPref }>(
      "PATCH",
      `/v1/sessions/${encodeURIComponent(sid)}/model`,
      { provider, model },
    );
    const pref = response.model ?? null;
    writeSnapshot(this.snapshotKey("model", sid), pref);
    return pref;
  }

  /** Begin OAuth. `kind: 'device'` finishes by polling; `'pkce'` needs a paste-back. */
  startProviderAuth(providerId: string): Promise<AuthFlow> {
    return this.request<AuthFlow>(
      "POST",
      `/v1/providers/${encodeURIComponent(providerId)}/auth/start`,
    );
  }

  async completeProviderAuth(
    providerId: string,
    flowId: string,
    redirectUrl: string,
  ): Promise<AuthVerdict> {
    const verdict = await this.request<AuthVerdict>(
      "POST",
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
      "POST",
      `/v1/providers/${encodeURIComponent(providerId)}/auth/complete`,
      { flow_id: flowId, api_key: apiKey },
    );
    this.invalidateRouter();
    return verdict;
  }

  async pollProviderAuth(
    providerId: string,
    flowId: string,
  ): Promise<AuthVerdict> {
    const verdict = await this.request<AuthVerdict>(
      "POST",
      `/v1/providers/${encodeURIComponent(providerId)}/auth/poll`,
      { flow_id: flowId },
    );
    // A settled verdict changed the daemon's credentials; a pending one did not.
    if (verdict?.status !== "pending") this.invalidateRouter();
    return verdict;
  }

  cancelProviderAuth(providerId: string, flowId: string): Promise<AuthVerdict> {
    return this.request<AuthVerdict>(
      "POST",
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
  async providerStatus(
    providerId: string,
    signal?: AbortSignal,
  ): Promise<ProviderStatus> {
    const response = await this.request<{ status?: ProviderStatus }>(
      "GET",
      `/v1/providers/${encodeURIComponent(providerId)}/status`,
      undefined,
      signal,
    );
    const status = response.status ?? {};
    this.mergeCachedProvider(providerId, { status });
    return status;
  }

  /** Live quota report for one provider (`GET /v1/providers/:id/limits`). */
  async providerLimits(
    providerId: string,
    signal?: AbortSignal,
  ): Promise<ProviderLimits> {
    const response = await this.request<{ report?: ProviderLimits }>(
      "GET",
      `/v1/providers/${encodeURIComponent(providerId)}/limits`,
      undefined,
      signal,
    );
    const limits = response.report ?? {};
    this.mergeCachedProvider(providerId, { limits });
    return limits;
  }

  /** Keep the shared router cache honest after a single-provider re-probe. */
  private mergeCachedProvider(
    providerId: string,
    patch: Partial<RouterProvider>,
  ): void {
    const entry = routerCache.get(this.base);
    if (!entry) return;
    routerCache.set(this.base, {
      at: entry.at,
      rows: entry.rows.map((row) =>
        row.id === providerId ? { ...row, ...patch } : row,
      ),
    });
  }

  async slashes(sid: string, signal?: AbortSignal): Promise<SlashCommand[]> {
    const response = await this.request<{ commands: SlashCommand[] }>(
      "GET",
      `/v1/sessions/${encodeURIComponent(sid)}/slashes`,
      undefined,
      signal,
    );
    return response.commands ?? [];
  }

  // GET /v1/sessions/:sid/suggest?kind=file&q= — the SHARED fuzzy file index
  // (fff) behind the TUI `@` picker and the grep tool. Returns ranked
  // relative paths with size/age/git-status meta.
  async suggestFiles(
    sid: string,
    query: string,
    signal?: AbortSignal,
  ): Promise<FileSuggestion[]> {
    const rows = await this.request<FileSuggestion[]>(
      "GET",
      `/v1/sessions/${encodeURIComponent(sid)}/suggest?kind=file&q=${encodeURIComponent(query)}`,
      undefined,
      signal,
    );
    return rows ?? [];
  }

  // ── Sessions ────────────────────────────────────────────────────
  //
  // The list, one session's meta, its transcript and its queued backlog are each
  // snapshotted per gateway. A screen reads its snapshot synchronously while
  // mounting (instant frame, no white flash) and these same calls refresh it
  // underneath — so navigation only ever changes what actually changed.

  /** Last session list seen for this gateway. */
  cachedSessions(): Session[] | null {
    return readSnapshot<Session[]>(this.snapshotKey("sessions"));
  }

  /**
   * The sessions this gateway last reported PARKED on an unanswered human-input
   * request.
   *
   * The list is ordered by content time and nothing else, so a parked run sits
   * wherever it last spoke — in a long fleet, past the end of the window this device
   * has read. The gateway therefore answers those rows BESIDE the window
   * (`state/list-sessions-page`) and a screen pins them above the list, instead of an
   * ordering that lifted them into it and moved every row under the reader the moment
   * a turn asked for a human or was answered.
   */
  parkedSessions(): Session[] {
    return this.parked;
  }

  /** Last meta row seen for ONE session. */
  cachedSession(sid: string): Session | null {
    return readSnapshot<Session>(this.snapshotKey("session", sid));
  }

  /** Last transcript seen for ONE session. */
  cachedTranscript(sid: string): TranscriptTurn[] | null {
    return readSnapshot<TranscriptTurn[]>(this.snapshotKey("transcript", sid));
  }

  /** Last queued backlog seen for ONE session. */
  cachedQueuedTurns(sid: string): QueuedTurn[] | null {
    return readSnapshot<QueuedTurn[]>(this.snapshotKey("queued", sid));
  }

  /**
   * The live bubble of ONE session, as it was last painted.
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
  cachedLiveTurn<T>(sid: string): { turn: T; seq: number } | null {
    const cached = snapshots.get(this.snapshotKey("live", sid));
    return (cached as { turn: T; seq: number } | undefined) ?? null;
  }

  rememberLiveTurn(sid: string, turn: unknown, seq: number): void {
    const key = this.snapshotKey("live", sid);
    if (turn === null) snapshots.delete(key);
    else snapshots.set(key, { turn, seq });
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
   * Memory only and bounded, like the live bubble: base64 pixels have no
   * business in `localStorage`, and a turn this far back is settled anyway —
   * from then on the persisted row owns its images.
   */
  private readonly sentAttachments = new Map<string, GatewayAttachment[]>();
  private static readonly SENT_ATTACHMENT_CACHE = 8;

  rememberSentAttachments(
    sid: string,
    tid: string | undefined,
    sent: GatewayAttachment[],
  ): void {
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

  cachedSentAttachments(
    sid: string,
    tid: string | undefined,
  ): GatewayAttachment[] | undefined {
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
  ): Promise<GatewayAttachment[]> {
    if (!tid) return [];
    const cached = this.cachedSentAttachments(sid, tid);
    if (cached?.length) return cached;
    const key = `${sid}\u0000${tid}`;
    const inflight = this.attachmentFetches.get(key);
    if (inflight) return inflight;
    const pending = (async () => {
      const res = await this.request<{ attachments?: GatewayAttachment[] }>(
        "GET",
        `/v1/sessions/${encodeURIComponent(sid)}/turns/${encodeURIComponent(tid)}/attachments`,
        undefined,
        signal,
      );
      const rows = (res.attachments ?? []).filter((row) => !!row?.base64);
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

  private readonly attachmentFetches = new Map<
    string,
    Promise<GatewayAttachment[]>
  >();

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
    const key = this.snapshotKey("queued", sid);
    const rows = readSnapshot<QueuedTurn[]>(key);
    if (!rows) return;
    const next = rows.filter((row) => row.turnId !== tid);
    if (next.length !== rows.length) writeSnapshot(key, next);
  }

  /** Drop every snapshot of one session — it is gone or is being replaced. */
  forgetSession(sid: string): void {
    snapshots.delete(this.snapshotKey("session", sid));
    snapshots.delete(this.snapshotKey("transcript", sid));
    snapshots.delete(this.snapshotKey("queued", sid));
    snapshots.delete(this.snapshotKey("live", sid));
    snapshots.delete(this.snapshotKey("model", sid));
    for (const key of Array.from(this.sentAttachments.keys())) {
      if (key.startsWith(`${sid}\u0000`)) this.sentAttachments.delete(key);
    }
    for (const key of Array.from(this.attachmentFetches.keys())) {
      if (key.startsWith(`${sid}\u0000`)) this.attachmentFetches.delete(key);
    }
    transcriptStamps.delete(this.snapshotKey("transcript", sid));
    transcriptWindows.delete(this.snapshotKey("transcript", sid));
    scheduleSnapshotFlush(snapshotStores);
  }

  /**
   * The session list — paged by CURSOR, and revalidated rather than re-downloaded.
   *
   * This is the app's most frequent poll and by far its largest payload (~315 KB
   * across a few hundred sessions). Three things keep it cheap:
   *
   * - **Paging.** Only `SESSIONS_PAGE` rows are asked for at a time. The first
   *   window answers in ~71 ms with more rows than fit on any screen, is handed
   *   to `onPage` immediately, and the rest of the list drains behind it.
   * - **Conditional GETs.** Every window carries a weak `ETag`, so an unchanged
   *   window costs one 304 with an empty body: nothing transferred, nothing
   *   parsed, nothing reconciled.
   * - **Cursors, not offsets.** Each window is asked for with the cursor of the last
   *   row of the one before it, so the page after that row is the same page however
   *   much the fleet moved in between.
   *
   * The list is recency-ordered and `total` is part of the validator, so any
   * arrival, answer, deletion or rename moves the FIRST window. An unchanged head
   * therefore ends the poll after a single 304 — with the SAME array identity
   * handed back, which React bails out on, so the list does not even re-render.
   *
   * `onPage` only fires on a cold load: with rows already on screen, a partial
   * list would paint as the list briefly shrinking.
   */
  async listSessions(
    signal?: AbortSignal,
    onPage?: (rows: Session[]) => void,
  ): Promise<Session[]> {
    const key = this.snapshotKey("sessions");
    const cached = this.cachedSessions();
    let pinned = GatewayClient.sessionsValidators.get(key);
    // A webview kill clears the in-memory pin but not the rows it described. Put
    // the durable head ETag back onto those exact rows, so the first cold-start
    // request can be a 304 instead of re-downloading the complete list.
    if (!pinned && cached?.length) {
      const persisted = readSnapshot<{ etag?: unknown; total?: unknown }>(
        this.snapshotKey("sessions-pin"),
      );
      if (
        typeof persisted?.etag === "string" &&
        persisted.etag &&
        persisted.total === cached.length
      ) {
        pinned = {
          full: cached,
          windows: new Map([
            [
              HEAD_CURSOR,
              {
                etag: persisted.etag,
                after: HEAD_CURSOR,
                rows: cached.slice(0, SESSIONS_PAGE),
                total: persisted.total,
                // A remembered pin is only ever ANSWERED by a 304, and a 304 on the
                // head of a list this client holds in full returns below without
                // walking, so it never needs a cursor to continue from. Anything else
                // is a 200 that carries the gateway's own.
                hasMore: false,
                nextCursor: HEAD_CURSOR,
                // The remembered pin says nothing about who is waiting on a human: a
                // demand is a fact of the CURRENT answer, so it starts empty and the
                // first read fills it.
                awaiting: [],
              },
            ],
          ]),
        };
        GatewayClient.sessionsValidators.set(key, pinned);
      }
    }
    // Only ever ask conditionally when a 304 can actually be ANSWERED from the
    // rows those validators were issued for.
    const known =
      pinned && pinned.full === cached
        ? pinned.windows
        : new Map<string, SessionsWindow>();

    const fetchWindow = async (after: string): Promise<SessionsWindow> => {
      const pin = known.get(after);
      const res = await this.requestFull<{
        sessions?: Session[];
        total?: number;
        has_more?: boolean;
        next_cursor?: string | null;
        awaiting?: Session[];
      }>(
        "GET",
        `/v1/sessions?limit=${SESSIONS_PAGE}${
          after ? `&after=${encodeURIComponent(after)}` : ""
        }`,
        undefined,
        signal,
        pin ? { "If-None-Match": pin.etag } : undefined,
      );
      if (res.status === 304 && pin) return pin;
      const rows = res.data?.sessions ?? [];
      const awaiting = res.data?.awaiting ?? [];
      // Every row names the model it runs on, so opening any of them paints the
      // right chip on the FIRST frame instead of after a per-session round trip. A
      // parked row is one a reader opens FIRST, and it may not be in `rows` at all.
      this.seedSessionModels(rows.concat(awaiting));
      return {
        etag: res.etag ?? "",
        after,
        rows,
        total: res.data?.total ?? rows.length,
        hasMore: Boolean(res.data?.has_more),
        nextCursor: res.data?.next_cursor ?? HEAD_CURSOR,
        awaiting,
      };
    };

    const head = await fetchWindow(HEAD_CURSOR);
    // The demand is answered by the HEAD and is complete there, so it is known before
    // the walk below decides whether there is anything left to read.
    this.parked = head.awaiting;
    const headPin = known.get(HEAD_CURSOR);
    const holdsWholeList = cached !== null && cached.length === head.total;

    // THE HEAD DECIDES WHETHER THERE IS A WALK AT ALL.
    //
    // Only the FIRST window moves minute to minute: the list is ordered by content
    // time, so every arrival, answer and rename lands at the top. The windows below
    // it would each cost a conditional round trip to be told 304 — measured against a
    // 1192-session store, 12 serial requests every ten seconds per machine, eleven of
    // them proving nothing had changed. That cascade is what a reader sees the moment
    // the list is opened.
    //
    // `total` rides INSIDE the head's validator (`server/sessions-etag`), and that is
    // what makes one request enough: a session created or deleted anywhere changes the
    // count, and a session that gains content ranks to the very top, so it lands in
    // this window. An unchanged head over a list held in full is therefore proof that
    // nothing below it moved.
    //
    // What it does NOT prove is CONTENT below the head, and the trade is deliberate: a
    // session renamed without re-ranking keeps its cached title until the ordering next
    // moves. Everything else re-ranks or re-counts by construction — a turn, a
    // deletion, a new session — and a mutation made HERE swaps the snapshot array,
    // which misses every pin and walks for real.
    if (headPin && holdsWholeList && head.etag !== "" && head.rows === headPin.rows) {
      return cached;
    }

    // A HEAD THAT CHANGED STILL REJOINS THE LIST IT ALREADY HAS.
    //
    // A cursor names a ROW, so the head's last row is a JOIN: whatever moved above it,
    // the rows after it are the rows already held here. That is the common case by far
    // — a turn finishes, its session ranks to the top, and the head is the only window
    // that changed — and splicing at the join answers it in ONE request where an offset
    // could only re-walk the fleet. The promoted session is still in the cached tail at
    // the place it left, so ids are de-duplicated with the head winning; and the result
    // must come out at `total`, or something below the head moved after all and the
    // honest answer is the walk.
    if (
      cached?.length &&
      head.rows.length === Math.min(SESSIONS_PAGE, head.total)
    ) {
      const join = head.rows[head.rows.length - 1];
      const at = join ? cached.findIndex((row) => row.id === join.id) : -1;
      if (at >= 0) {
        const held = new Set(head.rows.map((row) => row.id));
        const spliced = head.rows.concat(
          cached.slice(at + 1).filter((row) => !held.has(row.id)),
        );
        if (spliced.length === head.total) {
          const rows = reconcileRows(cached, spliced);
          writeSnapshot(key, rows);
          // Re-pin the one window that was actually re-read, onto the reconciled rows.
          // The windows below keep the pins they already have: a pin is only ever
          // ANSWERED when its ETag still matches, which is the gateway's own word that
          // those rows are unchanged.
          const windows = new Map(known);
          windows.set(HEAD_CURSOR, { ...head, rows: rows.slice(0, head.rows.length) });
          GatewayClient.sessionsValidators.set(key, { full: rows, windows });
          writeSnapshot(this.snapshotKey("sessions-pin"), {
            etag: head.etag,
            total: head.total,
          });
          return rows;
        }
      }
    }

    const progressive = !cached || cached.length === 0;

    /**
     * ONE walk over the windows, each asked for with the CURSOR of the last row of
     * the one before it.
     *
     * Windows used to be addressed by their offset into the gateway's ordering, which
     * is recomputed per request: a session starting a turn jumped to the top and
     * shifted everything below it, so one row arrived twice (a duplicate React key)
     * and another dropped out entirely while `merged.length` still equalled `total`,
     * and nothing downstream could notice. Every window was stamped with the ordering
     * it was cut from only to make that detectable, and a torn walk was thrown away
     * and re-walked. A cursor names a ROW instead of a count, so the page after it is
     * the same page whatever moved: there is nothing left to stamp, detect, retry or
     * de-duplicate. A SHORT window (a session deleted between the ordering and the
     * decoration of its page) is no longer a special case either — the next cursor is
     * the last row that actually arrived.
     */
    const drain = async (first: SessionsWindow) => {
      const fetched: SessionsWindow[] = [];
      let merged: Session[] = [];
      let window = first;
      for (;;) {
        fetched.push(window);
        merged = merged.length ? merged.concat(window.rows) : window.rows;
        if (progressive && merged.length) onPage?.(merged);
        if (!window.hasMore || !window.nextCursor) break;
        window = await fetchWindow(window.nextCursor);
        // A window that comes back empty ends the walk rather than looping forever.
        if (!window.rows.length) break;
      }
      return { fetched, merged };
    };

    const pass = await drain(head);
    const rows = reconcileRows(cached, pass.merged);
    writeSnapshot(key, rows);
    // Re-pin each window onto the RECONCILED rows, so a later 304 restores the
    // identities the screen is already rendering instead of the raw wire copies.
    const windows = new Map<string, SessionsWindow>();
    let start = 0;
    for (const window of pass.fetched) {
      const slice = rows.slice(start, start + window.rows.length);
      start += window.rows.length;
      if (!window.etag) continue;
      windows.set(window.after, { ...window, rows: slice });
    }
    if (windows.size) {
      GatewayClient.sessionsValidators.set(key, { full: rows, windows });
      const pin = windows.get(HEAD_CURSOR);
      writeSnapshot(
        this.snapshotKey("sessions-pin"),
        pin ? { etag: pin.etag, total: pin.total } : null,
      );
    } else {
      GatewayClient.sessionsValidators.delete(key);
      writeSnapshot(this.snapshotKey("sessions-pin"), null);
    }
    return rows;
  }

  // GET /v1/sessions/actions/search?q= searches the transcript store AND the session
  // titles server-side. Each hit carries the gateway's own `rank` band plus a short
  // snippet of the matching text, so the UI previews the conversation and paints the
  // order it was given rather than deriving a second one here.
  async searchSessionMatches(
    query: string,
    signal?: AbortSignal,
  ): Promise<SessionMatch[]> {
    const q = query.trim();
    if (!q) return [];
    const res = await this.request<{ matches?: RawSessionMatch[] }>(
      "GET",
      `/v1/sessions/actions/search?q=${encodeURIComponent(q)}`,
      undefined,
      signal,
    );
    return (res.matches ?? []).map((m) => ({
      sessionId: m.session_id,
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
            h.side === "request"
              ? ("request" as const)
              : h.side === "thinking"
                ? ("thinking" as const)
                : ("reply" as const),
          snippet: h.snippet as string,
          at: h.at ?? null,
        })),
    }));
  }

  createSession(opts: {
    title?: string;
    channel?: string;
    root?: string;
  }): Promise<Session> {
    return this.request<Session>("POST", "/v1/sessions", {
      title: opts.title,
      channel: opts.channel ?? "web",
      root: opts.root,
    });
  }

  /**
   * The FOLDERS under `path` on this machine — the browse behind "Switch project…".
   * `path` is optional (the machine's own home answers) and understands a leading
   * `~`, so the app never has to know where a machine keeps its home.
   */
  browse(path?: string, signal?: AbortSignal): Promise<BrowseListing> {
    const query = path ? `?path=${encodeURIComponent(path)}` : "";
    return this.request<BrowseListing>(
      "GET",
      `/v1/fs${query}`,
      undefined,
      signal,
    );
  }

  /** Create ONE folder inside `path`, and answer with the folder itself. */
  createDirectory(path: string, name: string): Promise<BrowseEntry> {
    return this.request<BrowseEntry>("POST", "/v1/fs/actions/mkdir", {
      path,
      name,
    });
  }

  /**
   * Every DRAFT of the repo `sid` lives in, newest first — the same list the TUI's
   * `/draft list` prints. Repo-scoped, not session-scoped: drafts stashed by other
   * sessions are in here too, which is what makes a "resume a draft" picker possible.
   */
  async drafts(sid: string, signal?: AbortSignal): Promise<WorkspaceDraft[]> {
    const res = await this.request<{ drafts?: WorkspaceDraft[] }>(
      "GET",
      `/v1/sessions/${encodeURIComponent(sid)}/workspace/drafts`,
      undefined,
      signal,
    );
    return res.drafts ?? [];
  }

  /**
   * Fork `sid` into a fresh draft and enter it — the wire twin of `/draft new`
   * (a clone of the repo as it stands) and `/draft clean` (`clean: true`, seeded
   * from the committed HEAD so the user's uncommitted work stays in the repo).
   * The gateway rejects a blank label, and a clean draft in a repo without a
   * commit.
   */
  createDraft(sid: string, label: string, clean = false): Promise<unknown> {
    return this.request(
      "POST",
      `/v1/sessions/${encodeURIComponent(sid)}/workspace/drafts`,
      {
        label,
        clean,
      },
    );
  }

  /** Move `sid` INTO an existing draft — the wire twin of `/draft resume <label>`. */
  resumeDraft(sid: string, workspaceId: string): Promise<unknown> {
    return this.request(
      "POST",
      `/v1/sessions/${encodeURIComponent(sid)}/workspace/resume`,
      {
        workspace_id: workspaceId,
      },
    );
  }

  /**
   * Every turn of `sid` a fork can be cut at, oldest-first — `GET
   * /v1/sessions/:sid/forks`. Lean rows on purpose: the picker paints one line
   * per turn and must not pull a transcript to do it.
   */
  async forkPoints(sid: string, signal?: AbortSignal): Promise<ForkPoint[]> {
    const res = await this.request<{ turns?: ForkPoint[] }>(
      "GET",
      `/v1/sessions/${encodeURIComponent(sid)}/forks`,
      undefined,
      signal,
    );
    return res.turns ?? [];
  }

  /**
   * Fork `sid` into a NEW INDEPENDENT session — the wire twin of the TUI's fork
   * and fork-at-turn. `throughTurnId` is the LAST turn the fork keeps; omitted,
   * the fork carries the whole conversation. The source session is untouched,
   * and the answer is the fork's own row, ready to open.
   */
  async forkSession(sid: string, throughTurnId?: string): Promise<Session> {
    const res = await this.request<{ session: Session }>(
      "POST",
      `/v1/sessions/${encodeURIComponent(sid)}/forks`,
      throughTurnId ? { through_turn_id: throughTurnId } : {},
    );
    return res.session;
  }

  async session(
    sid: string,
    signal?: AbortSignal,
    includeQueued = false,
  ): Promise<Session> {
    const response = await this.request<Session & { queued_turns?: unknown }>(
      "GET",
      `/v1/sessions/${encodeURIComponent(sid)}${includeQueued ? "?include=queued" : ""}`,
      undefined,
      signal,
    );
    const { queued_turns: queuedTurns, ...row } = response;
    const merged = reconcileRow(this.cachedSession(sid), row as Session);
    writeSnapshot(this.snapshotKey("session", sid), merged);

    if (includeQueued) {
      if (Array.isArray(queuedTurns)) {
        this.storeQueuedTurns(sid, queuedTurns);
      } else {
        // Protocol-compatible older gateways ignore the additive `include`
        // query. Only they pay the legacy second request.
        try {
          await this.queuedTurns(sid, signal);
        } catch (error) {
          if (signal?.aborted) throw error;
          // Keep metadata usable when only the optional queue read failed,
          // matching the former pair of independent screen requests.
        }
      }
    }
    return merged;
  }

  /**
   * Whole-life usage rollup for ONE session. On-demand only: it is absent from
   * `listSessions` and snapshots, fetched when a row expands, and is `null` for
   * a session that has no turns yet. The gateway memoizes each decoded iteration.
   */
  async sessionUsage(
    sid: string,
    signal?: AbortSignal,
  ): Promise<SessionUsage | null> {
    const res = await this.request<{ usage: SessionUsage | null }>(
      "GET",
      `/v1/sessions/${encodeURIComponent(sid)}/usage`,
      undefined,
      signal,
    );
    return res.usage ?? null;
  }

  async deleteSession(sid: string): Promise<unknown> {
    const result = await this.request(
      "DELETE",
      `/v1/sessions/${encodeURIComponent(sid)}`,
    );
    this.forgetSession(sid);
    // Drop just the deleted row from the list snapshot; the list keeps painting
    // every other session instead of falling back to a skeleton.
    const rows = this.cachedSessions();
    if (rows)
      writeSnapshot(
        this.snapshotKey("sessions"),
        rows.filter((row) => row.id !== sid),
      );
    return result;
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
      "DELETE",
      `/v1/projects/${encodeURIComponent(pid)}?is_recursive=true`,
    );
    const ids = res?.deleted_session_ids ?? [];
    for (const sid of ids) this.forgetSession(sid);
    const rows = this.cachedSessions();
    if (rows) {
      const gone = new Set(ids);
      writeSnapshot(
        this.snapshotKey("sessions"),
        rows.filter((row) => !gone.has(row.id)),
      );
    }
    return ids;
  }

  /**
   * Write a row the gateway just echoed into BOTH snapshots, so the list and the
   * session header repaint from cache with what it says instead of the stale row.
   */
  private absorbSessionRow(sid: string, row: Session): Session {
    const merged = reconcileRow(this.cachedSession(sid), row);
    writeSnapshot(this.snapshotKey("session", sid), merged);
    const rows = this.cachedSessions();
    if (rows) {
      writeSnapshot(
        this.snapshotKey("sessions"),
        rows.map((entry) => (entry.id === sid ? reconcileRow(entry, row) : entry)),
      );
    }
    return merged;
  }

  /** Rename a session. The gateway echoes the updated meta row. */
  async renameSession(sid: string, title: string): Promise<Session> {
    return this.absorbSessionRow(
      sid,
      await this.request<Session>(
        "PATCH",
        `/v1/sessions/${encodeURIComponent(sid)}`,
        { title },
      ),
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
      await this.request<Session>(
        "PATCH",
        `/v1/sessions/${encodeURIComponent(sid)}`,
        { is_favorite: isFavorite },
      ),
    );
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
    where: "tail" | "head",
  ): TranscriptTurn[] {
    if (!previous?.length) return incoming;
    if (!incoming.length) return previous;
    const index = new Map(previous.map((turn, at) => [turn.id, at]));
    const merged = previous.slice();
    const fresh: TranscriptTurn[] = [];
    let changed = false;
    for (const turn of incoming) {
      const at = index.get(turn.id);
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
    return where === "head" ? fresh.concat(merged) : merged.concat(fresh);
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
  ): Promise<TranscriptPage> {
    const search = new URLSearchParams();
    for (const [key, value] of Object.entries(query))
      search.set(key, String(value));
    const suffix = search.toString();
    const response = await this.request<{
      turns?: TranscriptTurn[];
      total?: number;
      offset?: number;
      has_more?: boolean;
    }>(
      "GET",
      `/v1/sessions/${encodeURIComponent(sid)}/transcript${suffix ? `?${suffix}` : ""}`,
      undefined,
      signal,
    );
    const turns = response.turns ?? [];
    const total =
      typeof response.total === "number" ? response.total : turns.length;
    const offset =
      typeof response.offset === "number"
        ? response.offset
        : Math.max(0, total - turns.length);
    return {
      turns,
      total,
      offset,
      hasMore:
        typeof response.has_more === "boolean" ? response.has_more : offset > 0,
    };
  }

  /** How much of `sid`'s transcript we hold, and how much older history exists. */
  transcriptWindow(sid: string): TranscriptWindow {
    return (
      transcriptWindows.get(this.snapshotKey("transcript", sid)) ?? {
        offset: 0,
        total: this.cachedTranscript(sid)?.length ?? 0,
      }
    );
  }

  /**
   * The NEWEST page of a session's transcript, merged onto whatever we already
   * hold (so earlier pages the user pulled in stay loaded).
   */
  async transcript(
    sid: string,
    signal?: AbortSignal,
    limit: number = TRANSCRIPT_PAGE,
  ): Promise<TranscriptTurn[]> {
    const key = this.snapshotKey("transcript", sid);
    const page = await this.fetchTranscriptPage(sid, { limit }, signal);
    const cached = this.cachedTranscript(sid);
    const held = transcriptWindows.get(key);
    const heldOffset = cached?.length ? (held?.offset ?? 0) : page.offset;
    // We hold rows [heldOffset, heldOffset + cached.length). A newest page that
    // starts BEYOND that runs past a GAP: the session grew by more than one page
    // since we last looked (app backgrounded while the TUI kept working), and
    // concatenating would paint turn 123 straight into turn 223 — a hole no
    // "load earlier" can reach, because it only ever walks back from turn 123.
    // Drop the stale rows and restart the window at this page instead.
    const adjoins =
      !cached?.length || page.offset <= heldOffset + cached.length;
    // Both sides are contiguous slices with a known offset, so split the page at
    // our oldest row instead of trusting "unseen id ⇒ newer": a page that reaches
    // FURTHER BACK than we hold (a deleted turn, a smaller earlier limit) would
    // otherwise append ancient turns to the BOTTOM of the transcript.
    const before = adjoins
      ? Math.max(0, Math.min(page.turns.length, heldOffset - page.offset))
      : 0;
    const turns = adjoins
      ? this.mergeTurns(
          this.mergeTurns(cached, page.turns.slice(before), "tail"),
          page.turns.slice(0, before),
          "head",
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
      typeof meta?.turn_count === "number" && meta.turn_count !== page.total
        ? ""
        : transcriptStamp(meta),
    );
    return turns;
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
    const key = this.snapshotKey("transcript", sid);
    const window = this.transcriptWindow(sid);
    if (window.offset <= 0) return null;
    const offset = Math.max(0, window.offset - limit);
    const page = await this.fetchTranscriptPage(
      sid,
      { offset, limit: window.offset - offset },
      signal,
    );
    const turns = this.mergeTurns(
      this.cachedTranscript(sid),
      page.turns,
      "head",
    );
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
  async sessionArtifacts(
    sid: string,
    signal?: AbortSignal,
  ): Promise<SessionArtifactRow[]> {
    const response = await this.request<{ artifacts?: SessionArtifactRow[] }>(
      "GET",
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
   * rendered markdown, and the body never crosses the wire.
   */
  async transcriptIfMoved(
    sid: string,
    row: Session | null,
    signal?: AbortSignal,
  ): Promise<TranscriptTurn[] | null> {
    const key = this.snapshotKey("transcript", sid);
    const stamp = transcriptStamp(row);
    const cached = this.cachedTranscript(sid);
    // A cached transcript holding a 'running' row is PROVISIONAL: that row is a
    // placeholder the gateway persists while a turn is in flight, and it carries
    // no outcome. Never let the stamp short-circuit past one — the turn may have
    // finished, failed or been cancelled since, and the caller would keep
    // painting a spinner for work that is long over.
    const provisional = !!cached?.some((turn) => turn.status === "running");
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
      typeof row?.turn_count === "number" &&
      row.turn_count > this.transcriptWindow(sid).total;
    if (
      stamp &&
      cached?.length &&
      !provisional &&
      !short &&
      transcriptStamps.get(key) === stamp
    )
      return null;
    const turns = await this.transcript(sid, signal);
    if (stamp) transcriptStamps.set(key, stamp);
    return turns;
  }

  async transcriptMd(sid: string, signal?: AbortSignal): Promise<string> {
    let response: Response;
    try {
      response = await fetch(
        `${this.base}/v1/sessions/${encodeURIComponent(sid)}/transcript.md`,
        { headers: this.headers(), signal },
      );
    } catch (error) {
      throw new GatewayError(0, `network error: ${(error as Error).message}`);
    }
    const text = await response.text();
    if (!response.ok)
      throw new GatewayError(response.status, `HTTP ${response.status}`, text);
    return text;
  }

  /**
   * ONE produced artifact's bytes as a `blob:` URL —
   * `GET /v1/sessions/:sid/iterations/:iid/attachments/:idx`, the endpoint the
   * `iteration.completed` / transcript descriptors index. `<img src>` cannot
   * carry the bearer header a token-gated gateway demands, so the bytes are
   * fetched WITH the auth headers and handed back as an object URL.
   *
   * THREE TIERS, and the network is the LAST one: the object URL this document
   * already made; else the bytes this DEVICE already downloaded, from the
   * persistent store in `attachment-cache`; else, and only then, the gateway.
   * An artifact is immutable, so a re-entered session, a re-opened app and a
   * figure scrolled back to all cost zero bytes on the wire — only artifacts
   * this device has never seen are downloaded. A FAILED fetch is evicted so a
   * row that has not landed yet is retried by the next render.
   */
  attachmentUrl(
    sid: string,
    iterationId: string,
    index: number,
  ): Promise<string> {
    const key = GatewayClient.attachmentKey(sid, iterationId, index);
    const cached = this.attachmentUrls.get(key);
    if (cached) {
      // Map insertion order IS the eviction order, so a picture asked for AGAIN
      // (a re-entered session re-mounting its tiles) has to re-insert, or the
      // very artifacts back on screen stay first in line to be revoked.
      this.attachmentUrls.delete(key);
      this.attachmentUrls.set(key, cached);
      return cached;
    }
    const endpoint = this.attachmentEndpoint(sid, iterationId, index);
    const pending = (async () => {
      const stored = await readCachedAttachment(endpoint);
      if (stored) {
        this.attachmentSizes.set(key, stored.size);
        return URL.createObjectURL(stored);
      }
      let response: Response;
      try {
        response = await fetch(endpoint, { headers: this.headers() });
      } catch (error) {
        throw new GatewayError(0, `network error: ${(error as Error).message}`);
      }
      if (!response.ok)
        throw new GatewayError(response.status, `HTTP ${response.status}`);
      const blob = await response.blob();
      this.attachmentSizes.set(key, blob.size);
      // Keeping it is best-effort and never blocks the picture it just produced.
      void writeCachedAttachment(endpoint, blob);
      return URL.createObjectURL(blob);
    })();
    pending.catch(() => this.attachmentUrls.delete(key));
    this.attachmentUrls.set(key, pending);
    // Twice: once for the COUNT bound, which is knowable immediately, and again
    // when the bytes have landed and the SIZE bound finally has numbers to add.
    this.evictAttachmentUrls();
    void pending.then(
      () => this.evictAttachmentUrls(),
      () => undefined,
    );
    return pending;
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
    let binary = "";
    for (const byte of bytes) binary += String.fromCharCode(byte);
    const filed = await this.request<IterationAttachment>(
      "POST",
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
  onArtifactRevision(
    sid: string,
    watcher: (turns: TranscriptTurn[]) => void,
  ): () => void {
    const key = this.snapshotKey("transcript", sid);
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
    const key = this.snapshotKey("transcript", sid);
    const held = readSnapshot<TranscriptTurn[]>(key);
    if (!held) return;
    const next = withSavedAttachment(held, saved);
    if (next === held) return;
    writeSnapshot(key, next);
    for (const watcher of revisionWatchers.get(key) ?? []) watcher(next);
  }

  private static attachmentKey(
    sid: string,
    iterationId: string,
    index: number,
  ): string {
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
      this.evictAttachmentUrls();
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
  private evictAttachmentUrls(): void {
    const entries = Array.from(this.attachmentUrls.keys()).map((key, at) => ({
      url: key,
      bytes: this.attachmentSizes.get(key) ?? 0,
      used: at,
      pinned: this.attachmentHolds.has(key),
    }));
    for (const key of cacheVictims(entries, ATTACHMENT_MEMORY_BUDGET)) {
      const stale = this.attachmentUrls.get(key);
      this.attachmentUrls.delete(key);
      this.attachmentSizes.delete(key);
      void stale
        ?.then((url) => URL.revokeObjectURL(url))
        .catch(() => undefined);
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

  submitTurn(
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
    const clientId = `companion:${crypto.randomUUID()}`;
    this.submissionKeys.set(sid, clientId);
    return this.request<SubmittedTurn>(
      "POST",
      `/v1/sessions/${encodeURIComponent(sid)}/turns`,
      {
        request,
        display_request: options.displayRequest,
        model: options.model,
        attachments: options.attachments,
        extra_body: options.extraBody,
        turn_features: options.turnFeatures,
        idempotency_key: clientId,
      },
    );
  }

  /** Stop a turn we know the id of — the addressed route, open to every channel. */
  cancelTurn(sid: string, tid: string): Promise<unknown> {
    return this.request(
      "POST",
      `/v1/sessions/${encodeURIComponent(sid)}/turns/${encodeURIComponent(tid)}/cancel`,
    );
  }

  /**
   * Stop the turn we submitted here without knowing its id yet (Stop pressed before
   * `turn.started` landed). It names itself with the submission's correlation id;
   * without one the gateway would have to guess, and refuses.
   */
  cancelCurrentTurn(sid: string): Promise<unknown> {
    return this.request(
      "POST",
      `/v1/sessions/${encodeURIComponent(sid)}/cancel-current`,
      { idempotency_key: this.submissionKeys.get(sid) },
    );
  }

  // ── Queue (shared server-side backlog, same as the TUI) ─────────
  // A busy-time submitTurn is enqueued by the gateway and mirrored to every
  // channel via turn.queued/.updated/.deleted/.drained. These edit that backlog.

  /** Edit a still-queued turn's prompt before it starts. */
  updateQueuedTurn(
    sid: string,
    tid: string,
    request: string,
  ): Promise<unknown> {
    return this.request(
      "PATCH",
      `/v1/sessions/${encodeURIComponent(sid)}/turns/${encodeURIComponent(tid)}`,
      { request },
    );
  }

  /** Drop a queued turn before it ever runs. */
  deleteQueuedTurn(sid: string, tid: string): Promise<unknown> {
    return this.request(
      "DELETE",
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
          turn !== null && typeof turn === "object" && !Array.isArray(turn),
      )
      .sort((a, b) => Number(a.queued_at ?? 0) - Number(b.queued_at ?? 0))
      .map(queuedTurnFromWire)
      .filter((row) => row.turnId !== "");
    const rows = reconcileRows(this.cachedQueuedTurns(sid), fetched);
    writeSnapshot(this.snapshotKey("queued", sid), rows);
    return rows;
  }

  async queuedTurns(sid: string, signal?: AbortSignal): Promise<QueuedTurn[]> {
    const response = await this.request<{ turns: SubmittedTurn[] }>(
      "GET",
      `/v1/sessions/${encodeURIComponent(sid)}/turns?status=queued`,
      undefined,
      signal,
    );
    return this.storeQueuedTurns(sid, response.turns);
  }

  /**
   * The typed input requests this session is BLOCKED on right now.
   *
   * SSE carries `human_input.request` live, but a screen opened (or reloaded,
   * or woken by a push) while a run is already parked has to read the open
   * forms back from here — the same snapshot the TUI restores from.
   */
  async humanInputRequests(
    sid: string,
    signal?: AbortSignal,
  ): Promise<HumanInputRequest[]> {
    const response = await this.request<{ requests: unknown[] }>(
      "GET",
      `/v1/sessions/${encodeURIComponent(sid)}/human-input`,
      undefined,
      signal,
    );
    return humanInputRequestsFromWire(response.requests);
  }

  /**
   * Answer one open request. Validation is the ENGINE's: a rejected answer comes
   * back `is_accepted false` with per-field errors and the request stays open,
   * exactly as it does for the TUI dialog.
   */
  submitHumanInput(
    sid: string,
    requestId: string,
    values: HumanInputValues,
  ): Promise<HumanInputOutcome> {
    return this.request<HumanInputOutcome>(
      "POST",
      `/v1/sessions/${encodeURIComponent(sid)}/human-input/${encodeURIComponent(requestId)}/actions/submit`,
      { values },
    );
  }

  /** Dismiss one open request. The blocked extension resumes unanswered. */
  cancelHumanInput(
    sid: string,
    requestId: string,
  ): Promise<{ is_cancelled: boolean; request_id: string }> {
    return this.request<{ is_cancelled: boolean; request_id: string }>(
      "POST",
      `/v1/sessions/${encodeURIComponent(sid)}/human-input/${encodeURIComponent(requestId)}/actions/cancel`,
    );
  }

  /**
   * The live views this session is SHOWING right now.
   *
   * SSE carries `human_input.live.open` and its patches, but a screen opened
   * (or woken by a push, or reconnected after a gap) while a run is already
   * halfway through a scan has to read the picture back from here — the same
   * snapshot the TUI pane restores from, already materialized by the engine.
   */
  async liveViews(sid: string, signal?: AbortSignal): Promise<LiveView[]> {
    const response = await this.request<{ views: unknown[] }>(
      "GET",
      `/v1/sessions/${encodeURIComponent(sid)}/human-input/live`,
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
    signal?: AbortSignal,
  ): Promise<LiveLogPage> {
    const query = `?from=${encodeURIComponent(from)}&limit=${encodeURIComponent(limit)}`;
    return this.request<LiveLogPage>(
      "GET",
      `/v1/sessions/${encodeURIComponent(sid)}/human-input/live/${encodeURIComponent(viewId)}/log/${encodeURIComponent(nodeId)}${query}`,
      undefined,
      signal,
    );
  }

  /** Replace one focusable live table's selected row ids in shared engine state. */
  focusLiveView(
    sid: string,
    viewId: string,
    nodeId: string,
    itemIds: string[],
  ): Promise<{ focused_ids: string[]; node_id: string; view_id: string }> {
    return this.request<{ focused_ids: string[]; node_id: string; view_id: string }>(
      "POST",
      `/v1/sessions/${encodeURIComponent(sid)}/human-input/live/${encodeURIComponent(viewId)}/actions/focus`,
      { node_id: nodeId, item_ids: itemIds },
    );
  }

  /**
   * Ask one view to stop, with `note` — the comment the person leaves with the
   * stop, when they leave one. The extension SEES the interruption and ends the
   * view itself, so this answers whether the ask LANDED, never that the work is
   * over. A view is always stoppable: nothing is asked of the human by one, so
   * nothing is left unanswered by stopping it.
   */
  interruptLiveView(
    sid: string,
    viewId: string,
    note?: string,
  ): Promise<{ is_interrupted: boolean; view_id: string }> {
    return this.request<{ is_interrupted: boolean; view_id: string }>(
      "POST",
      `/v1/sessions/${encodeURIComponent(sid)}/human-input/live/${encodeURIComponent(viewId)}/actions/interrupt`,
      note ? { note } : undefined,
    );
  }

  /**
   * Status of ONE turn as the gateway REGISTRY knows it — `null` when this
   * daemon's live registry has no such row.
   *
   * This is the transport-independent liveness probe: the live bubble normally
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
  ): Promise<Pick<TranscriptTurn, "status" | "content"> | null> {
    try {
      const row = await this.request<Record<string, unknown>>(
        "GET",
        `/v1/sessions/${encodeURIComponent(sid)}/turns/${encodeURIComponent(tid)}`,
        undefined,
        signal,
      );
      const status = String(row.status ?? "");
      if (status === "") return null;
      return {
        status,
        content: Array.isArray(row.content)
          ? (row.content as TranscriptTurn["content"])
          : undefined,
      };
    } catch (error) {
      if (error instanceof GatewayError && error.status === 404) return null;
      throw error;
    }
  }

  /**
   * The iterations the gateway has ALREADY PERSISTED for ONE turn — the resume
   * source for a turn that is still running.
   *
   * The live bubble is normally seeded by the `turn.started` frame, and every
   * later delta is dropped while it is null. That frame is emitted exactly once
   * and the hub subscribes LIVE-ONLY, so anyone who was not listening at that
   * instant never gets it: a cold open on a session that is already streaming,
   * and — the reported bug — an iOS webview whose WebContent process the OS
   * killed during a long background (Capacitor #7810/#7905), which reloads the
   * page mid-turn. The stream reconnects fine and then streams into nothing.
   *
   * This is the same trace the TUI resumes from, so the adopted bubble starts
   * with everything that happened while we were away instead of a blank one.
   */
  async turnTrace(
    sid: string,
    tid: string,
    signal?: AbortSignal,
  ): Promise<TranscriptIteration[]> {
    const response = await this.request<{ iterations?: unknown }>(
      "GET",
      `/v1/sessions/${encodeURIComponent(sid)}/turns/${encodeURIComponent(tid)}/trace`,
      undefined,
      signal,
    );
    return Array.isArray(response.iterations)
      ? (response.iterations as TranscriptIteration[])
      : [];
  }

  /**
   * Resume a queue the gateway paused after a provider failure — retries the
   * held head immediately and clears the failure counter/circuit breaker.
   */
  resumeQueue(sid: string): Promise<unknown> {
    return this.request(
      "POST",
      `/v1/sessions/${encodeURIComponent(sid)}/resume-queue`,
    );
  }

  // ── SSE live stream ─────────────────────────────────────────────
  //
  // GET /v1/sessions/:sid/events streams `data: {json}\n\n` frames. We read the
  // response body as a stream and parse SSE frames by hand so it works in every
  // Capacitor webview (native EventSource can't attach the bearer header).

  /**
   * Multiplex many watched sessions over one SSE connection. A cursor of -1
   * requests live-only delivery; reconnects resume each session independently.
   */
  streamSessionEvents(
    cursors: Map<string, number>,
    onEvent: (event: SseEvent) => void,
    opts: {
      signal?: AbortSignal;
      onOpen?: () => void;
      onError?: (error: unknown) => void;
      /** Fired once the retry loop has ENDED — the stream is no longer running. */
      onClosed?: () => void;
    } = {},
  ): () => void {
    const controller = new AbortController();
    const signal = opts.signal
      ? anySignal([opts.signal, controller.signal])
      : controller.signal;

    void (async () => {
      let retryMs = 400;
      while (!signal.aborted && cursors.size > 0) {
        // Per-attempt controller: the stall watchdog aborts only THIS
        // connection attempt, so the outer loop reconnects with up-to-date
        // cursors instead of dying with the caller's shared signal.
        const attempt = new AbortController();
        const attemptSignal = anySignal([signal, attempt.signal]);
        let stallTimer: ReturnType<typeof setTimeout> | null = null;
        // One watchdog for both phases of the attempt: a short bound on the
        // connect, the heartbeat bound once frames are flowing. Either way the
        // abort hits only THIS attempt and the outer loop reconnects.
        const armStall = (ms: number) => {
          if (stallTimer) clearTimeout(stallTimer);
          stallTimer = setTimeout(() => attempt.abort(), ms);
        };
        try {
          armStall(SSE_CONNECT_TIMEOUT_MS);
          const spec = Array.from(
            cursors,
            ([sid, cursor]) => `${sid}:${cursor}`,
          ).join(",");
          const response = await fetch(
            `${this.base}/v1/events?sids=${encodeURIComponent(spec)}`,
            {
              headers: this.headers({ Accept: "text/event-stream" }),
              signal: attemptSignal,
            },
          );
          if (!response.ok || !response.body) {
            throw new GatewayError(
              response.status,
              `SSE HTTP ${response.status}`,
            );
          }

          opts.onOpen?.();
          retryMs = 400;
          // Stall watchdog: the gateway sends a heartbeat every 15 s. If we
          // see nothing for 45 s the socket was silently frozen (iOS
          // backgrounding, dead NAT, half-open TCP) — abort this attempt so
          // the outer loop reconnects with the up-to-date cursor.
          armStall(SSE_STALL_TIMEOUT_MS);

          await readSseFrames(
            response.body,
            (json, frameName) => {
              // The session's own event LOG lives here. A transcription's
              // progress rides its own job stream under `VOICE_JOB_EVENT`, is
              // not an engine event, and never enters this reducer.
              if (frameName === VOICE_JOB_EVENT) return;
              try {
                const event = JSON.parse(json) as SseEvent;
                const sid =
                  typeof event.session_id === "string"
                    ? event.session_id
                    : typeof event.sid === "string"
                      ? event.sid
                      : "";
                // Deliver FIRST, then advance the cursor: an event whose
                // handler failed must replay on reconnect, never be skipped.
                onEvent(event);
                if (
                  sid &&
                  event.type === "subscription.ready" &&
                  typeof event.cursor === "number"
                ) {
                  cursors.set(sid, event.cursor);
                } else if (sid && typeof event.seq === "number") {
                  cursors.set(sid, Math.max(cursors.get(sid) ?? -1, event.seq));
                }
              } catch {
                // Ignore one malformed frame without ending sibling sessions.
              }
            },
            () => armStall(SSE_STALL_TIMEOUT_MS),
          );
          if (!signal.aborted) throw new GatewayError(0, "event stream closed");
        } catch (error) {
          if (signal.aborted) break;
          opts.onError?.(error);
          // A 4xx is NOT a reason to abandon the app's only push channel. A
          // token refresh racing a request (401), a proxy's 403/404, or the
          // gateway's own 400 "no valid sids" right after it restarted used to
          // end this loop FOREVER: the open session screen then sat silent —
          // no stall watchdog fires, because there is no socket left to stall —
          // until the user backed out and re-entered. Every failure now backs
          // off and retries; the hub supervises what is left.
          await abortableDelay(retryMs, signal);
          retryMs = Math.min(retryMs * 2, 5_000);
        } finally {
          if (stallTimer) clearTimeout(stallTimer);
          attempt.abort();
        }
      }
      opts.onClosed?.();
    })();

    return () => controller.abort();
  }

  streamEvents(
    sid: string,
    onEvent: (event: SseEvent) => void,
    opts: {
      cursor?: number;
      signal?: AbortSignal;
      onOpen?: () => void;
      onError?: (error: unknown) => void;
    } = {},
  ): () => void {
    const controller = new AbortController();
    const signal = opts.signal
      ? anySignal([opts.signal, controller.signal])
      : controller.signal;

    void (async () => {
      let cursor = opts.cursor;
      let retryMs = 400;

      while (!signal.aborted) {
        // Per-attempt controller — the stall watchdog aborts only this attempt
        // so the loop reconnects from `cursor` instead of dying.
        const attempt = new AbortController();
        const attemptSignal = anySignal([signal, attempt.signal]);
        let stallTimer: ReturnType<typeof setTimeout> | null = null;
        const armStall = (ms: number) => {
          if (stallTimer) clearTimeout(stallTimer);
          stallTimer = setTimeout(() => attempt.abort(), ms);
        };
        try {
          armStall(SSE_CONNECT_TIMEOUT_MS);
          const query = cursor != null ? `?cursor=${cursor}` : "";
          const response = await fetch(
            `${this.base}/v1/sessions/${encodeURIComponent(sid)}/events${query}`,
            {
              headers: this.headers({ Accept: "text/event-stream" }),
              signal: attemptSignal,
            },
          );
          if (!response.ok || !response.body) {
            throw new GatewayError(
              response.status,
              `SSE HTTP ${response.status}`,
            );
          }

          opts.onOpen?.();
          retryMs = 400;
          // Stall watchdog — same heartbeat bound as the multiplexed variant.
          armStall(SSE_STALL_TIMEOUT_MS);

          await readSseFrames(
            response.body,
            (json, frameName) => {
              // Session log only: a `voice.job` frame belongs to its own stream.
              if (frameName === VOICE_JOB_EVENT) return;
              try {
                const event = JSON.parse(json) as SseEvent;
                // Deliver FIRST, then advance: an event whose handler
                // failed must replay on reconnect, never be skipped.
                onEvent(event);
                if (typeof event.seq === "number")
                  cursor = Math.max(cursor ?? 0, event.seq);
              } catch {
                // A malformed frame must not end an otherwise healthy stream.
              }
            },
            () => armStall(SSE_STALL_TIMEOUT_MS),
          );
          if (!signal.aborted) throw new GatewayError(0, "event stream closed");
        } catch (error) {
          if (signal.aborted) return;
          opts.onError?.(error);
          if (
            error instanceof GatewayError &&
            error.status >= 400 &&
            error.status < 500
          ) {
            return;
          }
          await abortableDelay(retryMs, signal);
          retryMs = Math.min(retryMs * 2, 5_000);
        } finally {
          if (stallTimer) clearTimeout(stallTimer);
          attempt.abort();
        }
      }
    })();

    return () => controller.abort();
  }
}

/** Combine several AbortSignals into one that aborts when any input aborts. */

function abortableDelay(ms: number, signal: AbortSignal): Promise<void> {
  return new Promise((resolve) => {
    if (signal.aborted) {
      resolve();
      return;
    }
    const timer = window.setTimeout(resolve, ms);
    signal.addEventListener(
      "abort",
      () => {
        window.clearTimeout(timer);
        resolve();
      },
      { once: true },
    );
  });
}

function anySignal(signals: AbortSignal[]): AbortSignal {
  const ctrl = new AbortController();
  for (const s of signals) {
    if (s.aborted) {
      ctrl.abort();
      break;
    }
    s.addEventListener("abort", () => ctrl.abort(), { once: true });
  }
  return ctrl.signal;
}
