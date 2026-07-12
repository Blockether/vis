export type SessionSoul = {
  id: string;
  title?: string;
  status?: string;
  channel?: string;
  created_at?: number;
  updated_at?: number;
  /* epoch ms of the session's last activity — the wire's actual recency field */
  last_active_at?: number;
};

export type TurnCost = { total_cost?: number; model?: string; provider?: string };

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

export type ProviderInfo = { id: string; doc?: string };

/* /v1/models `catalog` — configured providers WITH their model catalogs (the
   same source the web channel’s session model picker renders). */
export type ProviderModels = { id: string; label?: string; models: string[] };

export type SessionModel = { provider?: string; model?: string } | string | null;

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

type ListSessionsResponse = { sessions?: SessionSoul[] };
type ListTurnsResponse = { turns?: GatewayTurn[] };
type ProvidersResponse = { providers?: ProviderInfo[] };
type CatalogResponse = { catalog?: ProviderModels[] };
type ModelResponse = { model?: SessionModel };

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

    const response = await fetch(`${this.gatewayUrl}${path}`, { ...init, headers });
    const text = await response.text();
    const body = text ? JSON.parse(text) : null;

    if (!response.ok) {
      const message = body?.message ?? body?.error ?? `${response.status} ${response.statusText}`;
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
      body: JSON.stringify({ channel: "react-native", title })
    });
  }

  async deleteSession(sessionId: string): Promise<void> {
    await this.request<null>(`/v1/sessions/${encodeURIComponent(sessionId)}`, {
      method: "DELETE"
    });
  }

  async renameSession(sessionId: string, title: string): Promise<SessionSoul> {
    return this.request<SessionSoul>(`/v1/sessions/${encodeURIComponent(sessionId)}`, {
      method: "PATCH",
      body: JSON.stringify({ title })
    });
  }

  /* ── turns ────────────────────────────────────────────────────── */

  async listTurns(sessionId: string): Promise<GatewayTurn[]> {
    const body = await this.request<ListTurnsResponse>(
      `/v1/sessions/${encodeURIComponent(sessionId)}/turns`
    );
    return body.turns ?? [];
  }

  async submitTurn(
    sessionId: string,
    request: string,
    attachments?: OutgoingAttachment[]
  ): Promise<GatewayTurn> {
    const idempotencyKey = `${Date.now()}-${Math.random().toString(36).slice(2)}`;
    const payload: Record<string, unknown> = { request, idempotency_key: idempotencyKey };
    if (attachments?.length) {
      payload.attachments = attachments.map(({ base64, filename }) => ({ base64, filename }));
    }
    return this.request<GatewayTurn>(`/v1/sessions/${encodeURIComponent(sessionId)}/turns`, {
      method: "POST",
      body: JSON.stringify(payload)
    });
  }

  async cancelTurn(sessionId: string, turnId: string): Promise<void> {
    await this.request<unknown>(
      `/v1/sessions/${encodeURIComponent(sessionId)}/turns/${encodeURIComponent(turnId)}/cancel`,
      { method: "POST" }
    );
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
      `/v1/settings?channel=${encodeURIComponent(scope)}`
    );
    return body.groups ?? [];
  }

  /* Flip a boolean / cycle an enum; resolves to the refreshed row. */
  async settingsMutate(id: string, action: "toggle" | "cycle"): Promise<ToggleRow> {
    return this.request<ToggleRow>("/v1/settings", {
      method: "POST",
      body: JSON.stringify({ id, action }),
    });
  }

  async sessionModel(sessionId: string): Promise<SessionModel> {
    const body = await this.request<ModelResponse>(
      `/v1/sessions/${encodeURIComponent(sessionId)}/model`
    );
    return body.model ?? null;
  }

  async setSessionModel(sessionId: string, provider: string, model: string): Promise<SessionModel> {
    const body = await this.request<ModelResponse>(
      `/v1/sessions/${encodeURIComponent(sessionId)}/model`,
      { method: "PATCH", body: JSON.stringify({ provider, model }) }
    );
    return body.model ?? null;
  }

  /* ── voice (served by the web channel: /ui/session/:sid/voice) ── */

  async voiceModelState(sessionId: string, startDownload = false): Promise<VoiceModelState> {
    const url = `${this.gatewayUrl}/ui/session/${encodeURIComponent(sessionId)}/voice/model`;
    const response = await fetch(url, {
      method: startDownload ? "POST" : "GET",
      headers: this.headers()
    });
    const text = await response.text();
    try {
      return JSON.parse(text) as VoiceModelState;
    } catch {
      return { status: "unavailable", error: `voice endpoint answered ${response.status}` };
    }
  }

  /* POST the recorded WAV; resolves to the transcribed text. */
  async transcribeVoice(sessionId: string, fileUri: string): Promise<string> {
    const blob = await (await fetch(fileUri)).blob();
    const url = `${this.gatewayUrl}/ui/session/${encodeURIComponent(sessionId)}/voice`;
    const response = await fetch(url, {
      method: "POST",
      headers: this.headers({ "Content-Type": "application/octet-stream" }),
      body: blob
    });
    const body = (await response.json()) as { text?: string; error?: string; status?: string };
    if (!response.ok) {
      throw new Error(
        body.error ??
          (response.status === 425 ? `voice model ${body.status ?? "not ready"}` : `${response.status}`)
      );
    }
    return (body.text ?? "").trim();
  }
}