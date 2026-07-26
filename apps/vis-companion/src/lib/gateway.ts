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
  QueuedTurn,
  RouterProvider,
  GatewayAttachment,
  GatewayCapabilities,
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
  VoiceModelState,
  VoiceTranscript,
} from './types';

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

function normalizeBase(url: string): string {
  return url.replace(/\/+$/, '');
}

export class GatewayClient {
  readonly base: string;
  private readonly token?: string;

  constructor(conn: GatewayConn) {
    this.base = normalizeBase(conn.url);
    this.token = conn.token;
  }

  private headers(extra?: HeadersInit): Headers {
    const h = new Headers(extra);
    if (this.token) h.set('Authorization', `Bearer ${this.token}`);
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
      const h = await this.request<{ id?: string }>('GET', '/healthz', undefined, signal);
      return h?.id ?? null;
    } catch {
      return null;
    }
  }

  capabilities(signal?: AbortSignal): Promise<GatewayCapabilities> {
    return this.request<GatewayCapabilities>('GET', '/v1/capabilities', undefined, signal);
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
  settings(signal?: AbortSignal): Promise<SettingsResponse> {
    return this.request<SettingsResponse>(
      'GET',
      '/v1/settings?channel=all',
      undefined,
      signal,
    );
  }

  setSetting(
    id: string,
    action: 'toggle' | 'cycle' | 'value',
    value?: string,
  ): Promise<Toggle> {
    return this.request<Toggle>('POST', '/v1/settings', { id, action, value });
  }

  // ── Router: providers, models, auth ─────────────────────────────
  // `/v1/router` is the WHOLE picker payload in one call — the same one the
  // TUI's router dialog renders. Auth is driven step-by-step over HTTP: the
  // daemon owns the PKCE verifier, the device code, and the credential file,
  // so no token ever reaches this device.
  async router(signal?: AbortSignal): Promise<RouterProvider[]> {
    const response = await this.request<{ providers?: RouterProvider[] }>(
      'GET',
      '/v1/router',
      undefined,
      signal,
    );
    return response.providers ?? [];
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

  completeProviderAuth(
    providerId: string,
    flowId: string,
    redirectUrl: string,
  ): Promise<AuthVerdict> {
    return this.request<AuthVerdict>(
      'POST',
      `/v1/providers/${encodeURIComponent(providerId)}/auth/complete`,
      { flow_id: flowId, redirect_url: redirectUrl },
    );
  }

  /** Finish an `api-key` flow: the DAEMON persists the key in its own config. */
  submitProviderKey(
    providerId: string,
    flowId: string,
    apiKey: string,
  ): Promise<AuthVerdict> {
    return this.request<AuthVerdict>(
      'POST',
      `/v1/providers/${encodeURIComponent(providerId)}/auth/complete`,
      { flow_id: flowId, api_key: apiKey },
    );
  }

  pollProviderAuth(providerId: string, flowId: string): Promise<AuthVerdict> {
    return this.request<AuthVerdict>(
      'POST',
      `/v1/providers/${encodeURIComponent(providerId)}/auth/poll`,
      { flow_id: flowId },
    );
  }

  cancelProviderAuth(providerId: string, flowId: string): Promise<AuthVerdict> {
    return this.request<AuthVerdict>(
      'POST',
      `/v1/providers/${encodeURIComponent(providerId)}/auth/cancel`,
      { flow_id: flowId },
    );
  }

  logoutProvider(providerId: string): Promise<AuthVerdict> {
    return this.request<AuthVerdict>(
      'POST',
      `/v1/providers/${encodeURIComponent(providerId)}/logout`,
    );
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
  async listSessions(signal?: AbortSignal): Promise<Session[]> {
    const res = await this.request<{ sessions: Session[] }>(
      'GET',
      '/v1/sessions',
      undefined,
      signal,
    );
    return res.sessions ?? [];
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

  session(sid: string, signal?: AbortSignal): Promise<Session> {
    return this.request<Session>(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}`,
      undefined,
      signal,
    );
  }

  deleteSession(sid: string): Promise<unknown> {
    return this.request('DELETE', `/v1/sessions/${encodeURIComponent(sid)}`);
  }

  async transcript(sid: string, signal?: AbortSignal): Promise<TranscriptTurn[]> {
    const response = await this.request<{ turns: TranscriptTurn[] }>(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}/transcript`,
      undefined,
      signal,
    );
    return response.turns ?? [];
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
   * source and same filter the TUI resumes from (`chat/resume-session`).
   */
  async queuedTurns(sid: string, signal?: AbortSignal): Promise<QueuedTurn[]> {
    const response = await this.request<{ turns?: SubmittedTurn[] }>(
      'GET',
      `/v1/sessions/${encodeURIComponent(sid)}/turns`,
      undefined,
      signal,
    );
    return (response.turns ?? [])
      .filter((turn) => String(turn.status ?? '') === 'queued')
      .sort((a, b) => Number(a.queued_at ?? 0) - Number(b.queued_at ?? 0))
      .map((turn) => ({
        turnId: String(turn.turn_id ?? turn.id ?? ''),
        request: typeof turn.request === 'string' ? turn.request : '',
      }))
      .filter((row) => row.turnId !== '');
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
