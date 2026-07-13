import EventSource from "react-native-sse";

export type SessionSoul = {
  id: string;
  title?: string;
  status?: string;
  channel?: string;
  created_at?: number;
  updated_at?: number;
  /* epoch ms of the session's last activity — the wire's actual recency field */
  last_active_at?: number;
  /* project membership + owner. project_id is null when the session belongs to no
     project; project_name rides along so the drawer needs no join. project_position
     is the session's manual order within its project (movable). */
  project_id?: string | null;
  project_name?: string | null;
  project_position?: number;
  owner_id?: string;
};

/* A PROJECT (GET /v1/projects). Sessions carry project_id; a project
   carries a live session_count. `channel` null ⇒ a cross-channel project visible
   in every channel's view. */
export type GatewayProject = {
  id: string;
  name: string;
  color?: string | null;
  channel?: string | null;
  position?: number;
  session_count?: number;
  owner_id?: string;
  archived_at?: number | null;
};

export type TurnCost = {
  total_cost?: number;
  model?: string;
  provider?: string;
};

/* Persisted turn attachment, gateway-hydrated from the attachment store. */
export type TurnAttachment = {
  kind?: string;
  media_type?: string;
  filename?: string;
  size?: number;
  base64?: string;
};
export type TurnTokens = { input?: number; output?: number };

export type GatewayTurn = {
  id?: string;
  turn_id?: string;
  request?: string;
  answer?: string;
  answer_md?: string;
  status?: string;
  started_at?: number;
  completed_at?: number;
  duration_ms?: number;
  iteration_count?: number;
  cost?: TurnCost;
  tokens?: TurnTokens;
  attachments?: TurnAttachment[];
};

/* One persisted iteration form (a settled tool call) from GET
   /v1/sessions/:sid/turns/:tid/trace `iterations[].forms[]`. Same fields the
   live SSE block.started/block.output carry, but stored: `src` is the tool-call
   source (the live `code`), `result_render` the rendered body, `tag` the
   observation/mutation class. Snake_case, namespace-dropped, like GatewayEvent. */
export type TraceForm = {
  tool_name?: string;
  tool_color_role?: string;
  src?: string;
  result_summary?: string;
  result_render?: string;
  duration_ms?: number;
  tag?: string;
};

/* One persisted iteration of a turn: its position, streamed thinking, and the
   tool-call forms that ran in it. `wire/canonical`, so it round-trips the HTTP
   hop as an identity. */
export type TraceIteration = {
  position?: number;
  iteration?: number;
  status?: string;
  thinking?: string;
  forms?: TraceForm[];
};

/* One live gateway SSE event (GET /v1/sessions/:sid/events). The wire is flat
   JSON — keyword map keys arrive snake_case with the namespace dropped
   (`:vis/tool-name` → tool_name, `:tool-color-role` → tool_color_role), while
   keyword VALUES keep their namespace (a colour role rides as the string
   "tool-color/search"). Only the fields the companion reduces are typed. */
export type GatewayEvent = {
  seq?: number;
  type?: string;
  turn_id?: string;
  iteration?: number;
  /* block.started / block.output — one native-tool / python op-card */
  block_id?: number;
  code?: string;
  result_summary?: string;
  result_render?: string;
  stdout?: string;
  error?: string;
  silent?: boolean;
  duration_ms?: number;
  tool_name?: string;
  tool_color_role?: string;
  /* content.delta (prose tail) / reasoning.delta (thinking tail) */
  text?: string;
  thinking?: string;
  /* turn.completed / turn.failed */
  status?: string;
  answer_md?: string;
  /* context.updated — the session-view utilization the model reads (ctx fullness) */
  utilization?: CtxUtilization;
};

/* The gateway tags every SSE frame with an `event: <type>` line (wire/sse-frame),
   so a native EventSource dispatches by type — these are the app event types the
   live reducer folds. Any other type (turn.queued.*, …) is ignored, so it need
   not be listened for. */
export const APP_EVENT_TYPES = [
  "turn.started",
  "block.started",
  "block.output",
  "content.delta",
  "reasoning.delta",
  "iteration.completed",
  "iteration.error",
  "turn.completed",
  "turn.failed",
  "context.updated",
] as const;

export type GatewayEventType = (typeof APP_EVENT_TYPES)[number];

export type ProviderInfo = { id: string; doc?: string };

/* /v1/models `catalog` — configured providers WITH their model catalogs (the
   same source the web channel’s session model picker renders). */
export type ProviderModels = { id: string; label?: string; models: string[] };

export type SessionModel =
  | { provider?: string; model?: string }
  | string
  | null;

/* /v1/settings — the engine toggle registry (the same rows the web
   channel's Settings dialog renders via `toggles-for-channel`). */
export type ToggleRow = {
  id: string;
  label: string;
  description?: string;
  type: "boolean" | "enum";
  enabled?: boolean;
  value?: string;
  choices?: string[];
};
export type SettingsGroup = { id: string; title: string; toggles: ToggleRow[] };

/* Rides POST /v1/sessions/:sid/turns `attachments` — the engine's
   prepare-inline-attachments reads {base64, filename} and sniffs the MIME
   from magic bytes (images only). */
export type OutgoingAttachment = { base64: string; filename?: string };

/* /ui/session/:sid/voice/model — Parakeet model lifecycle */
export type VoiceModelState = {
  status: "ready" | "downloading" | "failed" | "absent" | "unavailable";
  progress?: number;
  error?: string;
};

/* GET /v1/sessions/:sid/context — the read-only ctx mirror the model sees as its
   bound `session` (state/context-snapshot). The full string-keyed session-view;
   the companion reads `session_utilization` — the SAME {saturation,
   headroom_tokens, last_request_tokens} the model reads and the desktop/TUI
   context rail shows. */
export type CtxUtilization = {
  saturation?: number;
  headroom_tokens?: number;
  last_request_tokens?: number;
};
export type ContextSnapshot = {
  session_utilization?: CtxUtilization;
  [k: string]: unknown;
};

/* GET /v1/sessions/:sid/suggest?kind=file&q= — the SHARED fuzzy suggestion
   service behind every composer @-picker; web + TUI render these SAME rows
   ({:name :size :age :status}). */
export type SuggestRow = {
  name: string;
  size?: number;
  age?: string;
  status?: string;
};

/* GET /v1/sessions/:sid/workspace — the same workspace state the web footer and
   TUI directory picker read. `filesystem_roots` are the session's extra roots.
   `draft?` keeps its `?` on the wire (wire-key munges only `-`->`_`). */
export type FilesystemRoot = {
  trunk?: string;
  clone?: string;
  fork_ms?: number;
};
export type WorkspaceInfo = {
  id?: string;
  "draft?"?: boolean;
  root?: string;
  repo_root?: string;
  label?: string;
  fork_ms?: number;
  filesystem_roots?: FilesystemRoot[];
};

/* GET /v1/providers/:id/limits `report` — normalized RPM/TPM quota envelope
   (provider-limits/provider-limits), the same report the TUI provider view shows.
   Shape varies per provider; only the stable envelope fields are typed. */
export type ProviderLimitsReport = {
  provider?: string;
  status?: string;
  message?: string;
  static?: Record<string, unknown>;
  [k: string]: unknown;
};

/* GET /v1/providers/:id/status — provider health/config the model picker reads. */
export type ProviderStatusReport = {
  configured?: boolean;
  [k: string]: unknown;
};

type ListSessionsResponse = { sessions?: SessionSoul[] };
type ListProjectsResponse = { projects?: GatewayProject[] };
type ListTurnsResponse = { turns?: GatewayTurn[] };
type ProvidersResponse = { providers?: ProviderInfo[] };
type CatalogResponse = { catalog?: ProviderModels[] };
type ModelResponse = { model?: SessionModel };
type WorkspaceResponse = { workspace?: WorkspaceInfo | null };

type ClientOptions = {
  gatewayUrl: string;
  token?: string;
};

const trimTrailingSlash = (value: string): string => value.replace(/\/+$/, "");

export class VisGatewayClient {
  private readonly gatewayUrl: string;
  private readonly token: string | undefined;

  constructor(options: ClientOptions) {
    this.gatewayUrl = trimTrailingSlash(options.gatewayUrl);
    this.token = options.token?.trim() || undefined;
  }

  private headers(extra?: Record<string, string>): Headers {
    const headers = new Headers(extra);
    headers.set("Accept", "application/json");
    if (this.token) headers.set("Authorization", `Bearer ${this.token}`);
    return headers;
  }

  private async request<T>(path: string, init: RequestInit = {}): Promise<T> {
    const headers = this.headers();
    if (init.body && !headers.has("Content-Type")) {
      headers.set("Content-Type", "application/json");
    }

    const response = await fetch(`${this.gatewayUrl}${path}`, {
      ...init,
      headers,
    });
    const text = await response.text();
    const body = text ? JSON.parse(text) : null;

    if (!response.ok) {
      /* The gateway wraps errors as {error: {type, message, …}} (server.clj
         error-response). Reach the nested string — reading body.error raw would
         stringify the whole object to the infamous "[object Object]". Tolerate a
         plain-string error too, and a bare {message}. */
      const errObj = body?.error;
      const message =
        (typeof errObj === "string" ? errObj : errObj?.message) ??
        body?.message ??
        `${response.status} ${response.statusText}`;
      throw new Error(String(message));
    }

    return body as T;
  }

  /* ── sessions ─────────────────────────────────────────────────── */

  async listSessions(): Promise<SessionSoul[]> {
    const body = await this.request<ListSessionsResponse>("/v1/sessions");
    return body.sessions ?? [];
  }

  async createSession(title = "React Native"): Promise<SessionSoul> {
    return this.request<SessionSoul>("/v1/sessions", {
      method: "POST",
      body: JSON.stringify({ channel: "react-native", title }),
    });
  }

  async deleteSession(sessionId: string): Promise<void> {
    await this.request<null>(`/v1/sessions/${encodeURIComponent(sessionId)}`, {
      method: "DELETE",
    });
  }

  async renameSession(sessionId: string, title: string): Promise<SessionSoul> {
    return this.request<SessionSoul>(
      `/v1/sessions/${encodeURIComponent(sessionId)}`,
      {
        method: "PATCH",
        body: JSON.stringify({ title }),
      },
    );
  }

  /* ── projects (cross-channel session grouping) ────────────────────── */

  async listProjects(): Promise<GatewayProject[]> {
    /* `channel=all` so the RN drawer sees every project (its own cross-channel
       ones plus tui/web-scoped), matching list-sessions' cross-channel view. */
    const body = await this.request<ListProjectsResponse>(
      "/v1/projects?channel=all",
    );
    return body.projects ?? [];
  }

  async createProject(name: string, color?: string): Promise<GatewayProject> {
    /* No channel ⇒ a cross-channel project, visible from web/tui too. */
    return this.request<GatewayProject>("/v1/projects", {
      method: "POST",
      body: JSON.stringify(color ? { name, color } : { name }),
    });
  }

  async updateProject(
    projectId: string,
    patch: { name?: string; color?: string; archived?: boolean },
  ): Promise<GatewayProject> {
    return this.request<GatewayProject>(
      `/v1/projects/${encodeURIComponent(projectId)}`,
      {
        method: "PATCH",
        body: JSON.stringify(patch),
      },
    );
  }

  async deleteProject(projectId: string): Promise<void> {
    await this.request<null>(`/v1/projects/${encodeURIComponent(projectId)}`, {
      method: "DELETE",
    });
  }

  /* Move a session into a project, or remove it (projectId = null). Returns the
     re-projected soul (carries the fresh project_id/project_name). */
  async assignProject(
    sessionId: string,
    projectId: string | null,
  ): Promise<SessionSoul> {
    return this.request<SessionSoul>(
      `/v1/sessions/${encodeURIComponent(sessionId)}`,
      {
        method: "PATCH",
        body: JSON.stringify({ project_id: projectId }),
      },
    );
  }

  /* Persist the manual order of a project's sessions (movable tabs). `order` is
     the full list of session ids in the desired order. */
  async reorderProjectSessions(
    projectId: string,
    order: string[],
  ): Promise<void> {
    await this.request<null>(
      `/v1/projects/${encodeURIComponent(projectId)}/sessions`,
      {
        method: "PATCH",
        body: JSON.stringify({ order }),
      },
    );
  }

  /* ── turns ────────────────────────────────────────────────────── */

  async listTurns(sessionId: string): Promise<GatewayTurn[]> {
    const body = await this.request<ListTurnsResponse>(
      `/v1/sessions/${encodeURIComponent(sessionId)}/turns`,
    );
    return body.turns ?? [];
  }

  async submitTurn(
    sessionId: string,
    request: string,
    attachments?: OutgoingAttachment[],
  ): Promise<GatewayTurn> {
    const idempotencyKey = `${Date.now()}-${Math.random().toString(36).slice(2)}`;
    const payload: Record<string, unknown> = {
      request,
      idempotency_key: idempotencyKey,
    };
    if (attachments?.length) {
      payload.attachments = attachments.map(({ base64, filename }) => ({
        base64,
        filename,
      }));
    }
    return this.request<GatewayTurn>(
      `/v1/sessions/${encodeURIComponent(sessionId)}/turns`,
      {
        method: "POST",
        body: JSON.stringify(payload),
      },
    );
  }

  async cancelTurn(sessionId: string, turnId: string): Promise<void> {
    await this.request<unknown>(
      `/v1/sessions/${encodeURIComponent(sessionId)}/turns/${encodeURIComponent(turnId)}/cancel`,
      { method: "POST" },
    );
  }

  /* The canonical settled trace of ONE turn: its iteration rows (each with the
     tool-call forms that ran). Rehydrates tool cards into scrollback AFTER a
     turn settles — the /turns list carries no trace, so without this the live
     cards vanish once the poll's answer_md takes over. A pre-trace gateway 404s;
     callers treat that as "no cards". */
  async turnTrace(
    sessionId: string,
    turnId: string,
  ): Promise<TraceIteration[]> {
    const body = await this.request<{ iterations?: TraceIteration[] }>(
      `/v1/sessions/${encodeURIComponent(sessionId)}/turns/${encodeURIComponent(turnId)}/trace`,
    );
    return body.iterations ?? [];
  }

  /* Live event stream over a NATIVE Server-Sent-Events connection
     (react-native-sse's EventSource). The library owns the socket, frame
     parsing, heartbeat handling, AND reconnection: on a drop it re-requests
     with a `Last-Event-ID` header, which the gateway honours (`sse-cursor`) to
     resume losslessly — so NO app-level retry is needed. The gateway tags every
     frame with an `event: <type>` line, so events dispatch by type; we bind the
     reducer to `message` (untyped frames) AND every app event type. The 8KB
     anti-buffer pad and `: ping` heartbeats are comment frames with no data, so
     they never reach a listener. The first connect uses `cursor=0` to replay the
     in-flight turn; reconnects resume from the last seen `id:`. Returns a
     close() that tears the connection down. */
  streamEvents(
    sessionId: string,
    handlers: {
      onEvent: (ev: GatewayEvent) => void;
      onError?: (err: unknown) => void;
      onOpen?: () => void;
    },
    cursor = 0,
  ): () => void {
    const url = `${this.gatewayUrl}/v1/sessions/${encodeURIComponent(
      sessionId,
    )}/events?cursor=${cursor}`;
    const es = new EventSource<GatewayEventType>(url, {
      headers: {
        Accept: "text/event-stream",
        ...(this.token ? { Authorization: `Bearer ${this.token}` } : {}),
      },
      /* reconnect after a short delay, resuming via Last-Event-ID */
      pollingInterval: 2000,
      /* the gateway heartbeats forever; never time the idle socket out */
      timeout: 0,
      /* lifecycle logs on-device (readyState / status / reconnects) so a stream
         that never opens is diagnosable instead of silent */
      debug: typeof __DEV__ !== "undefined" && __DEV__,
    });

    const dispatch = (data: string | null) => {
      if (!data) return; /* heartbeat / comment / anti-buffer pad */
      try {
        handlers.onEvent(JSON.parse(data) as GatewayEvent);
      } catch {
        /* a partial / unparseable frame — skip it */
      }
    };

    es.addEventListener("open", () => handlers.onOpen?.());
    es.addEventListener("message", (ev) => dispatch(ev.data));
    for (const t of APP_EVENT_TYPES) {
      es.addEventListener(t, (ev) => dispatch(ev.data));
    }
    es.addEventListener("error", (ev) => handlers.onError?.(ev));

    return () => {
      es.removeAllEventListeners();
      es.close();
    };
  }

  /* ── model routing ────────────────────────────────────────────── */

  async listProviders(): Promise<ProviderInfo[]> {
    const body = await this.request<ProvidersResponse>("/v1/models");
    return body.providers ?? [];
  }

  /* Provider groups with model names — the gateway's canonical catalog. */
  async providerCatalog(): Promise<ProviderModels[]> {
    const body = await this.request<CatalogResponse>("/v1/models");
    return body.catalog ?? [];
  }

  /* ── settings (canonical gateway API, shared by every channel) ─── */

  /* `scope` = a channel name to see only that channel's controls, or
     "all" (default) for the cross-channel view — every visible toggle. */
  async settingsGroups(scope: string = "all"): Promise<SettingsGroup[]> {
    const body = await this.request<{ groups?: SettingsGroup[] }>(
      `/v1/settings?channel=${encodeURIComponent(scope)}`,
    );
    return body.groups ?? [];
  }

  /* Flip a boolean / cycle an enum; resolves to the refreshed row. */
  async settingsMutate(
    id: string,
    action: "toggle" | "cycle",
  ): Promise<ToggleRow> {
    return this.request<ToggleRow>("/v1/settings", {
      method: "POST",
      body: JSON.stringify({ id, action }),
    });
  }

  async sessionModel(sessionId: string): Promise<SessionModel> {
    const body = await this.request<ModelResponse>(
      `/v1/sessions/${encodeURIComponent(sessionId)}/model`,
    );
    return body.model ?? null;
  }

  async setSessionModel(
    sessionId: string,
    provider: string,
    model: string,
  ): Promise<SessionModel> {
    const body = await this.request<ModelResponse>(
      `/v1/sessions/${encodeURIComponent(sessionId)}/model`,
      { method: "PATCH", body: JSON.stringify({ provider, model }) },
    );
    return body.model ?? null;
  }

  /* ── voice (served by the web channel: /ui/session/:sid/voice) ── */

  async voiceModelState(
    sessionId: string,
    startDownload = false,
  ): Promise<VoiceModelState> {
    const url = `${this.gatewayUrl}/ui/session/${encodeURIComponent(sessionId)}/voice/model`;
    const response = await fetch(url, {
      method: startDownload ? "POST" : "GET",
      headers: this.headers(),
    });
    const text = await response.text();
    try {
      return JSON.parse(text) as VoiceModelState;
    } catch {
      return {
        status: "unavailable",
        error: `voice endpoint answered ${response.status}`,
      };
    }
  }

  /* POST the recorded WAV; resolves to the transcribed text. */
  async transcribeVoice(sessionId: string, fileUri: string): Promise<string> {
    const blob = await (await fetch(fileUri)).blob();
    const url = `${this.gatewayUrl}/ui/session/${encodeURIComponent(sessionId)}/voice`;
    const response = await fetch(url, {
      method: "POST",
      headers: this.headers({ "Content-Type": "application/octet-stream" }),
      body: blob,
    });
    const body = (await response.json()) as {
      text?: string;
      error?: string;
      status?: string;
    };
    if (!response.ok) {
      throw new Error(
        body.error ??
          (response.status === 425
            ? `voice model ${body.status ?? "not ready"}`
            : `${response.status}`),
      );
    }
    return (body.text ?? "").trim();
  }

  /* ── context mirror (GET /v1/sessions/:sid/context) — the canonical ctx the model reads.
     The live `context.updated` SSE event carries the same utilization incrementally. ── */

  async sessionContext(sessionId: string): Promise<ContextSnapshot> {
    return this.request<ContextSnapshot>(
      `/v1/sessions/${encodeURIComponent(sessionId)}/context`,
    );
  }

  /* ── @-file suggestions (GET /v1/sessions/:sid/suggest) — same rows web/TUI render ── */

  async suggest(
    sessionId: string,
    q: string,
    kind = "file",
  ): Promise<SuggestRow[]> {
    const rows = await this.request<SuggestRow[]>(
      `/v1/sessions/${encodeURIComponent(sessionId)}/suggest?kind=${encodeURIComponent(
        kind,
      )}&q=${encodeURIComponent(q)}`,
    );
    return rows ?? [];
  }

  /* ── workspace + filesystem roots — same state (and mutations) the web footer + TUI picker use.
     add/remove/change all resolve to the refreshed workspace so the caller re-renders roots. ── */

  async sessionWorkspace(sessionId: string): Promise<WorkspaceInfo | null> {
    const body = await this.request<WorkspaceResponse>(
      `/v1/sessions/${encodeURIComponent(sessionId)}/workspace`,
    );
    return body.workspace ?? null;
  }

  async addRoot(
    sessionId: string,
    path: string,
  ): Promise<WorkspaceInfo | null> {
    const body = await this.request<WorkspaceResponse>(
      `/v1/sessions/${encodeURIComponent(sessionId)}/workspace/roots`,
      { method: "POST", body: JSON.stringify({ path }) },
    );
    return body.workspace ?? null;
  }

  async removeRoot(
    sessionId: string,
    path: string,
  ): Promise<WorkspaceInfo | null> {
    /* DELETE carries the path as a query param (the gateway reads either body or ?path). */
    const body = await this.request<WorkspaceResponse>(
      `/v1/sessions/${encodeURIComponent(sessionId)}/workspace/roots?path=${encodeURIComponent(
        path,
      )}`,
      { method: "DELETE" },
    );
    return body.workspace ?? null;
  }

  async changeRoot(
    sessionId: string,
    path: string,
  ): Promise<WorkspaceInfo | null> {
    const body = await this.request<WorkspaceResponse>(
      `/v1/sessions/${encodeURIComponent(sessionId)}/workspace/root`,
      { method: "PATCH", body: JSON.stringify({ path }) },
    );
    return body.workspace ?? null;
  }

  /* ── provider status / limits (GET /v1/providers/:id/{status,limits}) — the same canonical
     reports the TUI provider dialog reads; never throws the picker if absent (returns {}). ── */

  async providerStatus(providerId: string): Promise<ProviderStatusReport> {
    const body = await this.request<{ status?: ProviderStatusReport }>(
      `/v1/providers/${encodeURIComponent(providerId)}/status`,
    );
    return body.status ?? {};
  }

  async providerLimits(providerId: string): Promise<ProviderLimitsReport> {
    const body = await this.request<{ report?: ProviderLimitsReport }>(
      `/v1/providers/${encodeURIComponent(providerId)}/limits`,
    );
    return body.report ?? {};
  }
}
