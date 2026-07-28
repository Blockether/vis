// Typed client for the vis gateway HTTP/SSE API. This is the companion's twin
// of src/com/blockether/vis/internal/gateway/client.clj — the SAME daemon the
// TUI and other channels drive, reached over LAN / Tailscale / cloudflared.
//
// Auth: any non-loopback (or --require-token) gateway demands a bearer token;
// we send it on every request. A 401 surfaces as GatewayError so the UI can
// prompt a re-pair.

import type {
  AuthFlow,
  AuthVerdict,
  ModelPref,
  QueuedAttachment,
  QueuedTurn,
  ProviderLimits,
  ProviderStatus,
  RouterProvider,
  GatewayAttachment,
  GatewayCapabilities,
  GatewayHealth,
  GatewayConn,
  FileSuggestion,
  GatewayStatus,
  GatewayTheme,
  Session,
  SettingsResponse,
  SlashCommand,
  SseEvent,
  SubmittedTurn,
  Toggle,
  TranscriptTurn,
  PushDevice,
  PushDeviceInput,
  PushStatus,
  VoiceModelState,
  VoiceTranscript,
} from './types';
import { PROTOCOL_HEADERS } from './compat';

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

// One transcript-search hit inside a session: which SIDE it landed on (the
// user's own request vs. the assistant's reply), a short preview snippet, and
// when it happened. Several travel per session, newest first.
export interface SessionMatchHit {
  side: 'request' | 'reply';
  snippet: string;
  at: number | null;
}

// One matching session, tagged with WHERE the query hit plus up to a handful of
// preview snippets. Only those small windows travel — never the conversation.
// `requestSnippet`/`replySnippet` are the first hit of each side, kept for
// callers that want a single line.
export interface SessionMatch {
  sessionId: string;
  inRequest: boolean;
  inReply: boolean;
  requestSnippet: string | null;
  replySnippet: string | null;
  hits: SessionMatchHit[];
}

interface RawSessionMatch {
  session_id: string;
  is_in_request?: boolean;
  is_in_reply?: boolean;
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

function transcriptStamp(row: Session | null | undefined): string {
  if (!row) return '';
  // Neither fact present = a payload that cannot express movement (an older
  // gateway's detail row). Return '' so callers FETCH instead of trusting a
  // constant stamp that both never invalidates and never detects a change.
  if (row.turn_count === undefined && row.modified_at === undefined) return '';
  return `${row.turn_count ?? ''}\u0000${row.modified_at ?? ''}`;
}

/** Bound the cache so hopping through many sessions cannot pin every transcript. */
const SNAPSHOT_LIMIT = 32;

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

function normalizeBase(url: string): string {
  return url.replace(/\/+$/, '');
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
    turnId: String(row.turn_id ?? row.id ?? ''),
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
  private static readonly ATTACHMENT_URL_CACHE = 24;

  constructor(conn: GatewayConn) {
    this.base = normalizeBase(conn.url);
    this.token = conn.token;
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

  private async request<T>(
    method: string,
    path: string,
    body?: unknown,
    signal?: AbortSignal,
  ): Promise<T> {
    const headers = this.headers();
    if (body !== undefined) headers.set('Content-Type', 'application/json');
    let res: Response;
    try {
      res = await fetch(this.base + path, {
        method,
        headers,
        body: body === undefined ? undefined : JSON.stringify(body),
        signal,
      });
    } catch (e) {
      throw new GatewayError(0, `network error: ${(e as Error).message}`);
    }
    const text = await res.text();
    let parsed: unknown = undefined;
    if (text) {
      try {
        parsed = JSON.parse(text);
      } catch {
        parsed = text;
      }
    }
    if (!res.ok) {
      const msg =
        (parsed as { error?: { message?: string } })?.error?.message ??
        `HTTP ${res.status}`;
      throw new GatewayError(res.status, msg, parsed);
    }
    return parsed as T;
  }

  // ── Health / status ─────────────────────────────────────────────
  status(signal?: AbortSignal): Promise<GatewayStatus> {
    return this.request<GatewayStatus>('GET', '/v1/admin/status', undefined, signal);
  }

  async ping(signal?: AbortSignal): Promise<boolean> {
    try {
      await this.request('GET', '/healthz', undefined, signal);
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
    return this.request<GatewayHealth>('GET', '/healthz', undefined, signal);
  }

  capabilities(signal?: AbortSignal): Promise<GatewayCapabilities> {
    return this.request<GatewayCapabilities>('GET', '/v1/capabilities', undefined, signal);
  }

  // ── Native push devices ─────────────────────────────────────────
  /**
   * Devices this gateway will push to (tokens masked), plus whether it can
   * push at all — the app needs both to tell "push impossible here" apart from
   * "push possible, this phone just isn't registered".
   */
  devices(signal?: AbortSignal): Promise<{ devices: PushDevice[]; push: PushStatus }> {
    return this.request('GET', '/v1/devices', undefined, signal);
  }

  /** Idempotent: re-registering the same token refreshes it, never duplicates. */
  registerDevice(input: PushDeviceInput): Promise<{ device: PushDevice; push: PushStatus }> {
    return this.request('POST', '/v1/devices', input);
  }

  unregisterDevice(token: string): Promise<{ is_removed: boolean }> {
    return this.request('DELETE', `/v1/devices/${encodeURIComponent(token)}`);
  }

  voiceModel(sid: string, start = false, signal?: AbortSignal): Promise<VoiceModelState> {
    return this.request<VoiceModelState>(
      start ? 'POST' : 'GET',
      `/v1/sessions/${encodeURIComponent(sid)}/voice/model`,
      undefined,
      signal,
    );
  }

  async transcribeVoice(sid: string, wav: Blob, signal?: AbortSignal): Promise<VoiceTranscript> {
    let response: Response;
    try {
      response = await fetch(`${this.base}/v1/sessions/${encodeURIComponent(sid)}/voice`, {
        method: 'POST',
        headers: this.headers({ 'Content-Type': 'audio/wav' }),
        body: wav,
        signal,
      });
    } catch (cause) {
      throw new GatewayError(0, `network error: ${(cause as Error).message}`);
    }

    const text = await response.text();
    let parsed: unknown;
    try {
      parsed = text ? JSON.parse(text) : undefined;
    } catch {
      parsed = text;
    }
    if (!response.ok) {
      const message =
        (parsed as { error?: string | { message?: string } })?.error instanceof Object
          ? (parsed as { error: { message?: string } }).error.message
          : (parsed as { error?: string })?.error;
      throw new GatewayError(response.status, message || `HTTP ${response.status}`, parsed);
    }
    return parsed as VoiceTranscript;
  }

  // ── Settings (shared feature-toggle registry, same as TUI) ──────
  /** Last settings payload seen for this gateway — paint it, then revalidate. */
  cachedSettings(): SettingsResponse | null {
    return readSnapshot<SettingsResponse>(this.snapshotKey('settings'));
  }

  async settings(signal?: AbortSignal): Promise<SettingsResponse> {
    const response = await this.request<SettingsResponse>(
      'GET',
      '/v1/settings?channel=all',
      undefined,
      signal,
    );
    writeSnapshot(this.snapshotKey('settings'), response);
    return response;
  }

  async setSetting(
    id: string,
    action: 'toggle' | 'cycle' | 'value',
    value?: string,
  ): Promise<Toggle> {
    const updated = await this.request<Toggle>('POST', '/v1/settings', { id, action, value });
    // Patch the one toggle that changed instead of dropping the snapshot, so
    // reopening the dialog paints the NEW value rather than a blank sheet.
    const cached = this.cachedSettings();
    if (cached) {
      writeSnapshot(this.snapshotKey('settings'), {
        ...cached,
        groups: (cached.groups ?? []).map((group) => ({
          ...group,
          toggles: group.toggles.map((toggle) => (toggle.id === updated.id ? updated : toggle)),
        })),
      });
    }
    return updated;
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
      inflight = this.request<{ providers: RouterProvider[] }>('GET', '/v1/router')
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

  async setDefaultModel(provider: string, model: string): Promise<void> {
    await this.request<{ default_provider: string; default_model: string }>('PATCH', '/v1/router', {
      provider,
      model,
    });
    this.invalidateRouter();
  }

  async sessionModel(sid: string, signal?: AbortSignal): Promise<ModelPref | null> {
    const response = await this.request<{ model?: ModelPref }>(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}/model`,
      undefined,
      signal,
    );
    return response.model ?? null;
  }

  /**
   * The gateway's DEFAULT provider+model — what a session with no pin actually
   * runs on. `sessionModel` answers only the explicit pin (null for "default"),
   * so any surface that names the live model needs this fallback.
   */
  async defaultModel(signal?: AbortSignal): Promise<ModelPref | null> {
    const rows = await this.router(signal);
    const row = rows.find((p) => p.is_default && p.default_model) ?? rows.find((p) => p.default_model);
    if (!row?.default_model) return null;
    return { provider: row.id, model: row.default_model };
  }

  async setSessionModel(
    sid: string,
    provider: string,
    model: string,
  ): Promise<ModelPref | null> {
    const response = await this.request<{ model?: ModelPref }>(
      'PATCH',
      `/v1/sessions/${encodeURIComponent(sid)}/model`,
      { provider, model },
    );
    return response.model ?? null;
  }

  /** Begin OAuth. `kind: 'device'` finishes by polling; `'pkce'` needs a paste-back. */
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

  async logoutProvider(providerId: string): Promise<AuthVerdict> {
    const verdict = await this.request<AuthVerdict>(
      'POST',
      `/v1/providers/${encodeURIComponent(providerId)}/logout`,
    );
    this.invalidateRouter();
    return verdict;
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
    const status = response.status ?? {};
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
    return limits;
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

  // ── Theme (same persisted selection and palette as the TUI) ─────
  theme(signal?: AbortSignal): Promise<GatewayTheme> {
    return this.request<GatewayTheme>('GET', '/v1/theme', undefined, signal);
  }

  setTheme(id: string): Promise<GatewayTheme> {
    return this.request<GatewayTheme>('POST', '/v1/theme', { id });
  }

  async slashes(signal?: AbortSignal): Promise<SlashCommand[]> {
    const response = await this.request<{ commands: SlashCommand[] }>(
      'GET',
      '/v1/slashes',
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
      'GET',
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
    return readSnapshot<Session[]>(this.snapshotKey('sessions'));
  }

  /** Last meta row seen for ONE session. */
  cachedSession(sid: string): Session | null {
    return readSnapshot<Session>(this.snapshotKey('session', sid));
  }

  /** Last transcript seen for ONE session. */
  cachedTranscript(sid: string): TranscriptTurn[] | null {
    return readSnapshot<TranscriptTurn[]>(this.snapshotKey('transcript', sid));
  }

  /** Last queued backlog seen for ONE session. */
  cachedQueuedTurns(sid: string): QueuedTurn[] | null {
    return readSnapshot<QueuedTurn[]>(this.snapshotKey('queued', sid));
  }

  /** Drop every snapshot of one session — it is gone or is being replaced. */
  forgetSession(sid: string): void {
    snapshots.delete(this.snapshotKey('session', sid));
    snapshots.delete(this.snapshotKey('transcript', sid));
    snapshots.delete(this.snapshotKey('queued', sid));
    transcriptStamps.delete(this.snapshotKey('transcript', sid));
    transcriptWindows.delete(this.snapshotKey('transcript', sid));
  }

  async listSessions(signal?: AbortSignal): Promise<Session[]> {
    const res = await this.request<{ sessions: Session[] }>(
      'GET',
      '/v1/sessions',
      undefined,
      signal,
    );
    const rows = reconcileRows(this.cachedSessions(), res.sessions ?? []);
    writeSnapshot(this.snapshotKey('sessions'), rows);
    return rows;
  }

  // GET /v1/sessions/actions/search?q= matches user requests + LLM responses in the
  // transcript store server-side, returning only the matching session ids.
  async searchSessionIds(query: string, signal?: AbortSignal): Promise<string[]> {
    const q = query.trim();
    if (!q) return [];
    const res = await this.request<{ session_ids: string[] }>(
      'GET',
      `/v1/sessions/actions/search?q=${encodeURIComponent(q)}`,
      undefined,
      signal,
    );
    return res.session_ids ?? [];
  }

  // Like searchSessionIds, but each hit is tagged with WHERE the query landed —
  // the user's own request vs. the assistant's reply — plus a short snippet of
  // the matching text so the UI can preview the conversation, not just the id.
  async searchSessionMatches(
    query: string,
    signal?: AbortSignal,
  ): Promise<SessionMatch[]> {
    const q = query.trim();
    if (!q) return [];
    const res = await this.request<{ matches?: RawSessionMatch[] }>(
      'GET',
      `/v1/sessions/actions/search?q=${encodeURIComponent(q)}`,
      undefined,
      signal,
    );
    return (res.matches ?? []).map((m) => ({
      sessionId: m.session_id,
      inRequest: Boolean(m.is_in_request),
      inReply: Boolean(m.is_in_reply),
      requestSnippet: m.request_snippet ?? null,
      replySnippet: m.reply_snippet ?? null,
      hits: (m.hits ?? [])
        .filter((h) => Boolean(h.snippet?.trim()))
        .map((h) => ({
          side: h.side === 'request' ? ('request' as const) : ('reply' as const),
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
    return this.request<Session>('POST', '/v1/sessions', {
      title: opts.title,
      channel: opts.channel ?? 'web',
      root: opts.root,
    });
  }

  async session(sid: string, signal?: AbortSignal): Promise<Session> {
    const row = await this.request<Session>(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}`,
      undefined,
      signal,
    );
    const merged = reconcileRow(this.cachedSession(sid), row);
    writeSnapshot(this.snapshotKey('session', sid), merged);
    return merged;
  }

  async deleteSession(sid: string): Promise<unknown> {
    const result = await this.request('DELETE', `/v1/sessions/${encodeURIComponent(sid)}`);
    this.forgetSession(sid);
    // Drop just the deleted row from the list snapshot; the list keeps painting
    // every other session instead of falling back to a skeleton.
    const rows = this.cachedSessions();
    if (rows) writeSnapshot(this.snapshotKey('sessions'), rows.filter((row) => row.id !== sid));
    return result;
  }

  /**
   * Rename a session. The gateway echoes the updated meta row, which is written
   * back into BOTH snapshots so the list and the session header repaint from
   * cache with the new title instead of the stale one.
   */
  async renameSession(sid: string, title: string): Promise<Session> {
    const row = await this.request<Session>(
      'PATCH',
      `/v1/sessions/${encodeURIComponent(sid)}`,
      { title },
    );
    const merged = reconcileRow(this.cachedSession(sid), row);
    writeSnapshot(this.snapshotKey('session', sid), merged);
    const rows = this.cachedSessions();
    if (rows) {
      writeSnapshot(
        this.snapshotKey('sessions'),
        rows.map((entry) => (entry.id === sid ? reconcileRow(entry, row) : entry)),
      );
    }
    return merged;
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
  ): Promise<TranscriptPage> {
    const search = new URLSearchParams();
    for (const [key, value] of Object.entries(query)) search.set(key, String(value));
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
   * hold (so earlier pages the user pulled in stay loaded).
   */
  async transcript(
    sid: string,
    signal?: AbortSignal,
    limit: number = TRANSCRIPT_PAGE,
  ): Promise<TranscriptTurn[]> {
    const key = this.snapshotKey('transcript', sid);
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
    // the session did not move can skip the next fetch entirely.
    transcriptStamps.set(key, transcriptStamp(this.cachedSession(sid)));
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
    const key = this.snapshotKey('transcript', sid);
    const stamp = transcriptStamp(row);
    const cached = this.cachedTranscript(sid);
    // A cached transcript holding a 'running' row is PROVISIONAL: that row is a
    // placeholder the gateway persists while a turn is in flight, and it carries
    // no outcome. Never let the stamp short-circuit past one — the turn may have
    // finished, failed or been cancelled since, and the caller would keep
    // painting a spinner for work that is long over.
    const provisional = !!cached?.some((turn) => turn.status === 'running');
    if (stamp && cached?.length && !provisional && transcriptStamps.get(key) === stamp) return null;
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
    if (!response.ok) throw new GatewayError(response.status, `HTTP ${response.status}`, text);
    return text;
  }

  /**
   * ONE produced artifact's bytes as a `blob:` URL —
   * `GET /v1/sessions/:sid/iterations/:iid/attachments/:idx`, the endpoint the
   * `iteration.completed` / transcript descriptors index. `<img src>` cannot
   * carry the bearer header a token-gated gateway demands, so the bytes are
   * fetched WITH the auth headers and handed back as an object URL. Immutable
   * by construction, hence cached; a FAILED fetch is evicted so a row that has
   * not landed yet is retried by the next render.
   */
  attachmentUrl(sid: string, iterationId: string, index: number): Promise<string> {
    const key = `${sid}\u0000${iterationId}\u0000${index}`;
    const cached = this.attachmentUrls.get(key);
    if (cached) return cached;
    const pending = (async () => {
      const path = `/v1/sessions/${encodeURIComponent(sid)}/iterations/${encodeURIComponent(iterationId)}/attachments/${index}`;
      let response: Response;
      try {
        response = await fetch(this.base + path, { headers: this.headers() });
      } catch (error) {
        throw new GatewayError(0, `network error: ${(error as Error).message}`);
      }
      if (!response.ok) throw new GatewayError(response.status, `HTTP ${response.status}`);
      return URL.createObjectURL(await response.blob());
    })();
    pending.catch(() => this.attachmentUrls.delete(key));
    this.attachmentUrls.set(key, pending);
    // Every live entry pins its decoded bytes for the lifetime of the document.
    // A long session that produced many figures is exactly the memory curve iOS
    // answers by killing the webview, so the cache is bounded and the oldest
    // object URLs are handed back to the collector. A tile still showing an
    // evicted URL re-requests on its `error` handler, which repopulates it.
    while (this.attachmentUrls.size > GatewayClient.ATTACHMENT_URL_CACHE) {
      const oldest = this.attachmentUrls.keys().next();
      if (oldest.done) break;
      const stale = this.attachmentUrls.get(oldest.value);
      this.attachmentUrls.delete(oldest.value);
      void stale?.then((url) => URL.revokeObjectURL(url)).catch(() => undefined);
    }
    return pending;
  }

  submitTurn(
    sid: string,
    request: string,
    options: { model?: string; displayRequest?: string; attachments?: GatewayAttachment[] } = {},
  ): Promise<SubmittedTurn> {
    return this.request<SubmittedTurn>(
      'POST',
      `/v1/sessions/${encodeURIComponent(sid)}/turns`,
      {
        request,
        display_request: options.displayRequest,
        model: options.model,
        attachments: options.attachments,
      },
    );
  }

  cancelCurrentTurn(sid: string): Promise<unknown> {
    return this.request(
      'POST',
      `/v1/sessions/${encodeURIComponent(sid)}/cancel-current`,
    );
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
  async queuedTurns(sid: string, signal?: AbortSignal): Promise<QueuedTurn[]> {
    const response = await this.request<{ turns: SubmittedTurn[] }>(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}/turns?status=queued`,
      undefined,
      signal,
    );
    const fetched = response.turns
      .sort((a, b) => Number(a.queued_at ?? 0) - Number(b.queued_at ?? 0))
      .map((turn) => queuedTurnFromWire(turn as unknown as Record<string, unknown>))
      .filter((row) => row.turnId !== '');
    const rows = reconcileRows(this.cachedQueuedTurns(sid), fetched);
    writeSnapshot(this.snapshotKey('queued', sid), rows);
    return rows;
  }

  /**
   * Terminal status of ONE turn as the gateway REGISTRY knows it — `null` while
   * it is still running (or unknown to this session).
   *
   * This is the transport-independent liveness probe: the live bubble normally
   * settles on the terminal SSE frame, but a reconnect gap (or a backgrounded
   * tab whose stream was torn down mid-turn) can swallow that one frame, and
   * then the bubble streams forever for a turn the gateway finished minutes
   * ago. Asking the registry costs one cheap listing and never lies.
   */
  async turnStatus(sid: string, tid: string, signal?: AbortSignal): Promise<string | null> {
    const response = await this.request<{ turns?: Record<string, unknown>[] }>(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}/turns`,
      undefined,
      signal,
    );
    const row = (response.turns ?? []).find(
      (turn) => String(turn.turn_id ?? '') === tid,
    );
    if (!row) return null;
    const status = String(row.status ?? '');
    return status === '' || status === 'running' || status === 'queued' ? null : status;
  }


  /**
   * Resume a queue the gateway paused after a provider failure — retries the
   * held head immediately and clears the failure counter/circuit breaker.
   */
  resumeQueue(sid: string): Promise<unknown> {
    return this.request(
      'POST',
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
        try {
          const spec = Array.from(cursors, ([sid, cursor]) => `${sid}:${cursor}`).join(',');
          const response = await fetch(
            `${this.base}/v1/events?sids=${encodeURIComponent(spec)}`,
            { headers: this.headers({ Accept: 'text/event-stream' }), signal: attemptSignal },
          );
          if (!response.ok || !response.body) {
            throw new GatewayError(response.status, `SSE HTTP ${response.status}`);
          }

          opts.onOpen?.();
          retryMs = 400;
          const reader = response.body.getReader();
          const decoder = new TextDecoder();
          let buffer = '';

          // Stall watchdog: the gateway sends a heartbeat every 15 s. If we
          // see nothing for 45 s the socket was silently frozen (iOS
          // backgrounding, dead NAT, half-open TCP) — abort this attempt so
          // the outer loop reconnects with the up-to-date cursor.
          const armStall = () => {
            if (stallTimer) clearTimeout(stallTimer);
            stallTimer = setTimeout(() => attempt.abort(), 45_000);
          };
          armStall();

          for (;;) {
            const { value, done } = await reader.read();
            armStall();
            if (done) break;
            buffer += decoder.decode(value, { stream: true }).replace(/\r\n/g, '\n');
            let boundary: number;
            while ((boundary = buffer.indexOf('\n\n')) >= 0) {
              const frame = buffer.slice(0, boundary);
              buffer = buffer.slice(boundary + 2);
              for (const line of frame.split('\n')) {
                const trimmed = line.trimStart();
                if (!trimmed.startsWith('data:')) continue;
                const json = trimmed.slice(5).trim();
                if (!json) continue;
                try {
                  const event = JSON.parse(json) as SseEvent;
                  const sid = typeof event.session_id === 'string'
                    ? event.session_id
                    : typeof event.sid === 'string' ? event.sid : '';
                  // Deliver FIRST, then advance the cursor: an event whose
                  // handler failed must replay on reconnect, never be skipped.
                  onEvent(event);
                  if (sid && event.type === 'subscription.ready' && typeof event.cursor === 'number') {
                    cursors.set(sid, event.cursor);
                  } else if (sid && typeof event.seq === 'number') {
                    cursors.set(sid, Math.max(cursors.get(sid) ?? -1, event.seq));
                  }
                } catch {
                  // Ignore one malformed frame without ending sibling sessions.
                }
              }
            }
          }
          if (!signal.aborted) throw new GatewayError(0, 'event stream closed');
        } catch (error) {
          if (signal.aborted) return;
          opts.onError?.(error);
          if (
            error instanceof GatewayError
            && error.status >= 400
            && error.status < 500
          ) return;
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
        try {
          const query = cursor != null ? `?cursor=${cursor}` : '';
          const response = await fetch(
            `${this.base}/v1/sessions/${encodeURIComponent(sid)}/events${query}`,
            { headers: this.headers({ Accept: 'text/event-stream' }), signal: attemptSignal },
          );
          if (!response.ok || !response.body) {
            throw new GatewayError(response.status, `SSE HTTP ${response.status}`);
          }

          opts.onOpen?.();
          retryMs = 400;
          const reader = response.body.getReader();
          const decoder = new TextDecoder();
          let buffer = '';

          // Stall watchdog — same 45 s bound as the multiplexed variant.
          const armStall = () => {
            if (stallTimer) clearTimeout(stallTimer);
            stallTimer = setTimeout(() => attempt.abort(), 45_000);
          };
          armStall();

          for (;;) {
            const { value, done } = await reader.read();
            armStall();
            if (done) break;
            buffer += decoder.decode(value, { stream: true }).replace(/\r\n/g, '\n');
            let boundary: number;
            while ((boundary = buffer.indexOf('\n\n')) >= 0) {
              const frame = buffer.slice(0, boundary);
              buffer = buffer.slice(boundary + 2);
              for (const line of frame.split('\n')) {
                const trimmed = line.trimStart();
                if (!trimmed.startsWith('data:')) continue;
                const json = trimmed.slice(5).trim();
                if (!json) continue;
                try {
                  const event = JSON.parse(json) as SseEvent;
                  // Deliver FIRST, then advance: an event whose handler
                  // failed must replay on reconnect, never be skipped.
                  onEvent(event);
                  if (typeof event.seq === 'number') cursor = Math.max(cursor ?? 0, event.seq);
                } catch {
                  // A malformed frame must not end an otherwise healthy stream.
                }
              }
            }
          }
          if (!signal.aborted) throw new GatewayError(0, 'event stream closed');
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
      'abort',
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
    s.addEventListener('abort', () => ctrl.abort(), { once: true });
  }
  return ctrl.signal;
}
