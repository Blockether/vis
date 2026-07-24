// Typed client for the vis gateway HTTP/SSE API. This is the companion's twin
// of src/com/blockether/vis/internal/gateway/client.clj — the SAME daemon the
// TUI and other channels drive, reached over LAN / Tailscale / cloudflared.
//
// Auth: any non-loopback (or --require-token) gateway demands a bearer token;
// we send it on every request. A 401 surfaces as GatewayError so the UI can
// prompt a re-pair.

import type {
  GatewayAttachment,
  GatewayCapabilities,
  GatewayConn,
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

// One transcript-search hit, tagged with WHERE the query matched (the user's
// own request vs. the assistant's reply) plus a short preview snippet of each
// matching side. Only the small window travels — never the whole conversation.
export interface SessionMatch {
  sessionId: string;
  inRequest: boolean;
  inReply: boolean;
  requestSnippet: string | null;
  replySnippet: string | null;
}

interface RawSessionMatch {
  session_id: string;
  is_in_request?: boolean;
  is_in_reply?: boolean;
  request_snippet?: string | null;
  reply_snippet?: string | null;
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

  // GET /v1/sessions/search?q= matches user requests + LLM responses in the
  // transcript store server-side, returning only the matching session ids.
  async searchSessionIds(query: string, signal?: AbortSignal): Promise<string[]> {
    const q = query.trim();
    if (!q) return [];
    const res = await this.request<{ session_ids: string[] }>(
      'GET',
      `/v1/sessions/search?q=${encodeURIComponent(q)}`,
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
      `/v1/sessions/search?q=${encodeURIComponent(q)}`,
      undefined,
      signal,
    );
    return (res.matches ?? []).map((m) => ({
      sessionId: m.session_id,
      inRequest: Boolean(m.is_in_request),
      inReply: Boolean(m.is_in_reply),
      requestSnippet: m.request_snippet ?? null,
      replySnippet: m.reply_snippet ?? null,
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
        try {
          const spec = Array.from(cursors, ([sid, cursor]) => `${sid}:${cursor}`).join(',');
          const response = await fetch(
            `${this.base}/v1/events?sids=${encodeURIComponent(spec)}`,
            { headers: this.headers({ Accept: 'text/event-stream' }), signal },
          );
          if (!response.ok || !response.body) {
            throw new GatewayError(response.status, `SSE HTTP ${response.status}`);
          }

          opts.onOpen?.();
          retryMs = 400;
          const reader = response.body.getReader();
          const decoder = new TextDecoder();
          let buffer = '';

          for (;;) {
            const { value, done } = await reader.read();
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
                  if (sid && event.type === 'subscription.ready' && typeof event.cursor === 'number') {
                    cursors.set(sid, event.cursor);
                  } else if (sid && typeof event.seq === 'number') {
                    cursors.set(sid, Math.max(cursors.get(sid) ?? -1, event.seq));
                  }
                  onEvent(event);
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
        try {
          const query = cursor != null ? `?cursor=${cursor}` : '';
          const response = await fetch(
            `${this.base}/v1/sessions/${encodeURIComponent(sid)}/events${query}`,
            { headers: this.headers({ Accept: 'text/event-stream' }), signal },
          );
          if (!response.ok || !response.body) {
            throw new GatewayError(response.status, `SSE HTTP ${response.status}`);
          }

          opts.onOpen?.();
          retryMs = 400;
          const reader = response.body.getReader();
          const decoder = new TextDecoder();
          let buffer = '';

          for (;;) {
            const { value, done } = await reader.read();
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
                  if (typeof event.seq === 'number') cursor = Math.max(cursor ?? 0, event.seq);
                  onEvent(event);
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
