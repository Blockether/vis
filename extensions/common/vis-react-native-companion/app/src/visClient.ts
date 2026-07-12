export type SessionSoul = {
  id: string;
  title?: string;
  status?: string;
  channel?: string;
  created_at?: number;
  updated_at?: number;
};

export type GatewayTurn = {
  id?: string;
  turn_id?: string;
  request?: string;
  answer?: string;
  answer_md?: string;
  status?: string;
  started_at?: number;
  completed_at?: number;
};

type ListSessionsResponse = { sessions?: SessionSoul[] };
type ListTurnsResponse = { turns?: GatewayTurn[] };

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

  private async request<T>(path: string, init: RequestInit = {}): Promise<T> {
    const headers = new Headers(init.headers);
    headers.set("Accept", "application/json");
    if (init.body && !headers.has("Content-Type")) {
      headers.set("Content-Type", "application/json");
    }
    if (this.token) {
      headers.set("Authorization", `Bearer ${this.token}`);
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

  async listTurns(sessionId: string): Promise<GatewayTurn[]> {
    const body = await this.request<ListTurnsResponse>(`/v1/sessions/${encodeURIComponent(sessionId)}/turns`);
    return body.turns ?? [];
  }

  async submitTurn(sessionId: string, request: string): Promise<GatewayTurn> {
    const idempotencyKey = `${Date.now()}-${Math.random().toString(36).slice(2)}`;
    return this.request<GatewayTurn>(`/v1/sessions/${encodeURIComponent(sessionId)}/turns`, {
      method: "POST",
      body: JSON.stringify({ request, idempotency_key: idempotencyKey })
    });
  }
}
