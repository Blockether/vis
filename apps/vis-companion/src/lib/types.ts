// Wire shapes served by the vis gateway (see
// src/com/blockether/vis/internal/gateway/server.clj). Only the fields the
// companion reads are typed; unknown fields are preserved but ignored.

export interface GatewayConn {
  /** Base URL, e.g. http://100.64.0.10:7890 (LAN, Tailscale, or cloudflared). */
  url: string;
  /** Bearer token; required for any non-loopback / --require-token gateway. */
  token?: string;
  /** Human label shown in the connection list. */
  /** Human label shown in the connection list. */
  label?: string;
  /**
   * Stable, opaque gateway instance id reported by `/healthz`. Captured on
   * connect and used to build clean shareable links (`#/s/<sid>?gw=<id>`)
   * instead of embedding the full gateway URL. Never a secret.
   */
  id?: string;
}

export interface Session {
  id: string;
  title?: string;
  channel?: string;
  model?: string;
  project_id?: string | null;
  project_name?: string | null;
  project_position?: number | null;
  status?: 'idle' | 'running' | 'suspended' | string;
  /** Canonical gateway liveness; older gateways are inferred from status. */
  live?: boolean;
  current_turn_id?: string | null;
  /** In-flight facts and same-response gateway clock for clock-safe attachment. */
  running_request?: string;
  running_started_at?: number;
  server_time_ms?: number;
  turn_count?: number;
  created_at?: string;
  modified_at?: string;
  last_active_at?: string;
  workspace?: {
    root?: string;
    repo_root?: string;
    label?: string;
    fork_ms?: number;
  } | null;
  [k: string]: unknown;
}

export interface SlashCommand {
  name: string;
  doc: string;
}

// One ranked file row from GET /v1/sessions/:sid/suggest?kind=file — the same
// rows the TUI `@` picker renders (relative path + size/age/git-status meta).
export interface FileSuggestion {
  name: string;
  size: string;
  age: string;
  status: string;
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
  /** Browser-ready CSS custom properties for THIS theme (present in the themes list). */
  css_vars?: Record<string, string>;
}

/** App-local theme preference. `gateway` follows the gateway's active theme;
 *  `light`/`dark` pin a mode; any other value pins a specific theme id. */
export type ThemePref = 'gateway' | 'light' | 'dark' | string;

export interface GatewayTheme extends ThemeSummary {
  css_vars: Record<string, string>;
  themes: ThemeSummary[];
}

export interface VoiceModelState {
  status: 'ready' | 'downloading' | 'failed' | 'absent' | 'unavailable';
  progress?: number;
  error?: string;
}

export interface GatewayAttachment {
  id?: string;
  source?: string;
  size?: number;
  filename: string;
  media_type: string;
  base64: string;
}

export interface GatewayCapabilities {
  version: number;
  features: {
    chat: { enabled: boolean };
    pastes?: {
      enabled: boolean;
      transport: 'display_request';
      format: 'vis-paste-v1';
      inline_max_chars: number;
      collapsed_by_default: boolean;
    };
    attachments: {
      enabled: boolean;
      transport: 'inline-base64';
      media_types: string[];
      max_files: number;
      max_file_bytes: number;
    };
    voice: {
      enabled: boolean;
      transport: 'audio/wav';
      transcription: 'gateway-local';
      model: VoiceModelState;
    };
  };
}

export interface VoiceTranscript {
  text: string;
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
  /** Gateway-formatted Python, produced by the same cached ruff formatter as the TUI. */
  display_code?: string;
  comment?: string;
  result?: JsonValue;
  result_render?: string;
  result_summary?: string;
  result_kind?: string;
  result_detail?: Record<string, JsonValue>;
  render_segments?: JsonValue[];
  cards?: TranscriptForm[];
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
  answer?: string;
  code?: string;
  forms?: TranscriptForm[];
  duration_ms?: number;
  cost_usd?: number;
  error?: JsonValue;
  llm_selected?: { provider?: string; model?: string };
  llm_actual?: { provider?: string; model?: string };
  is_llm_fallback?: boolean;
  llm_routing_trace?: Array<Record<string, JsonValue>>;
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
  attachments?: GatewayAttachment[];
  iterations?: TranscriptIteration[];
  model?: string;
  provider?: string;
  created_at?: number;
  completed_at?: number;
  duration_ms?: number;
  iteration_count?: number;
  input_tokens?: number;
  input_regular_tokens?: number;
  input_cache_write_tokens?: number;
  input_cache_read_tokens?: number;
  output_tokens?: number;
  output_reasoning_tokens?: number;
  total_cost?: number;
  tokens?: {
    input?: number;
    input_regular?: number;
    cache_created?: number;
    cached?: number;
    output?: number;
    reasoning?: number;
  };
  meta_summary?: string;
  meta_fallback_note?: string;
  llm_selected?: { provider?: string; model?: string };
  llm_actual?: { provider?: string; model?: string };
  is_llm_fallback?: boolean;
  llm_routing_trace?: Array<Record<string, JsonValue>>;
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
  session_id?: string;
  seq?: number;
  /** Gateway epoch sampled when this event was emitted. */
  ts?: number;
  [k: string]: unknown;
}
