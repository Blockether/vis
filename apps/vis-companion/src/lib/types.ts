// Wire shapes served by the vis gateway (see
// src/com/blockether/vis/internal/gateway/server.clj). Only the fields the
// companion reads are typed; unknown fields are preserved but ignored.

export interface GatewayConn {
  /** Base URL, e.g. http://100.64.0.10:7890 (LAN, Tailscale, or cloudflared). */
  url: string;
  /** Bearer token; required for any non-loopback / --require-token gateway. */
  token?: string;
  /** Human label shown in the connection list. */
  label?: string;
}

export interface Session {
  id: string;
  title?: string;
  channel?: string;
  project_id?: string | null;
  updated_at?: string;
  [k: string]: unknown;
}

export interface Project {
  id: string;
  name: string;
  color?: string;
  archived?: boolean;
  [k: string]: unknown;
}

export type ToggleType = 'boolean' | 'enum';

export interface Toggle {
  id: string;
  label: string;
  type: ToggleType;
  description?: string;
  enabled?: boolean;
  value?: string;
  choices?: string[];
}

export interface ToggleGroup {
  id: string;
  title: string;
  toggles: Toggle[];
}

export interface SettingsResponse {
  groups: ToggleGroup[];
}

export interface ThemeSummary {
  id: string;
  display_name: string;
  mode: 'light' | 'dark';
}

export interface GatewayTheme extends ThemeSummary {
  css_vars: Record<string, string>;
  themes: ThemeSummary[];
}

export interface GatewayStatus {
  pid?: number;
  url?: string;
  db?: string;
  clients?: number;
  auth?: string;
  [k: string]: unknown;
}

/** One SSE event as delivered by GET /v1/events?sids=… */

export type JsonValue =
  | string
  | number
  | boolean
  | null
  | JsonValue[]
  | { [key: string]: JsonValue };

export interface ContentBlock {
  id: string;
  type: 'prose' | 'code' | 'tool' | 'reasoning' | 'error' | 'attachment' | 'notice';
  markdown?: string;
  text?: string;
  language?: string;
  tool?: string;
  status?: 'pending' | 'running' | 'completed' | 'failed' | 'cancelled';
  input?: JsonValue;
  output?: JsonValue;
  error?: JsonValue;
  code?: string;
  message?: string;
  retryable?: boolean;
  visibility?: 'private' | 'visible';
  attachment_id?: string;
  name?: string;
  media_type?: string;
  [key: string]: unknown;
}

export interface TranscriptForm {
  scope?: string;
  tag?: string;
  src?: string;
  source?: string;
  code?: string;
  result?: JsonValue;
  result_render?: string;
  result_summary?: string;
  error?: JsonValue;
  stdout?: string;
  tool_name?: string;
  tool_color_role?: string;
  silent?: boolean;
  duration_ms?: number;
  [key: string]: unknown;
}

export interface TranscriptIteration {
  id?: string;
  position?: number;
  thinking?: string;
  assistant_prose?: string;
  code?: string;
  forms?: TranscriptForm[];
  duration_ms?: number;
  cost_usd?: number;
  [key: string]: unknown;
}

export interface TranscriptTurn {
  id: string;
  turn_id?: string;
  user_request?: string;
  request?: string;
  status?: string;
  prior_outcome?: string;
  content?: ContentBlock[];
  iterations?: TranscriptIteration[];
  model?: string;
  provider?: string;
  created_at?: number;
  completed_at?: number;
  duration_ms?: number;
  iteration_count?: number;
  total_cost?: number;
  cost?:
    | number
    | {
        total_cost?: number;
        model?: string;
        provider?: string;
        [key: string]: unknown;
      };
  [key: string]: unknown;
}

export interface SubmittedTurn {
  id?: string;
  turn_id?: string;
  request?: string;
  status?: string;
  started_at?: number;
  [key: string]: unknown;
}

export interface SseEvent {
  type: string;
  sid?: string;
  seq?: number;
  [k: string]: unknown;
}
