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
  /**
   * Every other URL the SAME gateway answers on — from the pairing `alt=` /
   * `hosts` list and, later, from what the gateway advertises in
   * `/v1/capabilities`. Persisted: this is how the app can move itself to the
   * Tailscale address after being paired on the LAN, and how it fails over
   * when the current address stops answering. See `lib/endpoints.ts` for the
   * durability order.
   */
  alts?: string[];
  /**
   * The user picked this address by hand in gateway settings. Freezes the
   * automatic upgrade to a more durable address — an explicit choice outranks
   * the ranking.
   */
  pinned?: boolean;
}

export interface Session {
  id: string;
  title?: string;
  channel?: string;
  /** The session state's ROOT model — a bare name, no provider, not the pin. */
  model?: string;
  /**
   * The session's pinned provider+model, straight off the list row. Absent means
   * "runs on the router default"; carrying it here is what lets a client name the
   * model without a `GET /v1/sessions/:sid/model` per session.
   */
  model_pref?: ModelPref | null;
  project_id?: string | null;
  project_name?: string | null;
  project_position?: number | null;
  status?: "idle" | "running" | "suspended" | string;
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
    /**
     * True when `root` is a DRAFT clone (~/.vis/drafts/<repo>/<label>) rather
     * than a project root. A draft belongs to `repo_root`; `label` names the
     * draft, not the project, so neither may be used as a grouping key.
     */
    is_draft?: boolean;
  } | null;
  [k: string]: unknown;
}

/**
 * One DRAFT of a repo — a per-session clone parked at `~/.vis/drafts/<repo>/<label>`.
 * Straight off `GET /v1/sessions/:sid/workspace/drafts`, which lists every draft of
 * THAT session's repo (active or stashed), newest first. `is_current` marks the one
 * the queried session itself is sitting in.
 */
export interface WorkspaceDraft {
  workspace_id: string;
  label?: string | null;
  root?: string | null;
  repo_root?: string | null;
  /** When the clone was forked off trunk. Absent when none was recorded. */
  fork_ms?: number | null;
  is_current?: boolean;
}

/**
 * One FOLDER on a machine's own disk, straight off `GET /v1/fs`. Directories only:
 * a session runs IN a folder, so a file is never an answer to "which project".
 */
export interface BrowseEntry {
  name: string;
  path: string;
  /** Visible children — enough to tell a full project from an empty shell. */
  entry_count: number;
  /** It is a git working tree, so it is a project and not a place that holds them. */
  is_repo: boolean;
  /** The checked-out branch, when it has one. */
  branch?: string | null;
}

/** One directory of a machine's filesystem, as the browse sheet reads it. */
export interface BrowseListing {
  path: string;
  /** `null` at the filesystem root — the crumb trail simply stops. */
  parent: string | null;
  /** That machine's home, so the trail can show `~` instead of `/Users/someone`. */
  home: string;
  /** The listing was capped; what is shown is a prefix of what is there. */
  is_truncated: boolean;
  entries: BrowseEntry[];
}

/**
 * GET /v1/sessions/:sid/usage — the whole-life rollup for one session. Fetched
 * on demand only and deliberately absent from the session list; the gateway
 * memoizes each decoded iteration's tool-call tally.
 */
export interface SessionUsage {
  turn_count?: number;
  iteration_count?: number;
  tool_call_count?: number;
  fold_count?: number;
  top_tools?: Array<{ name: string; count: number }>;
  /** Tool calls that FAILED, noisiest tool first — `top_tools`' shape, errors only. */
  top_errors?: Array<{ name: string; count: number }>;
  error_count?: number;
  input_tokens?: number;
  input_regular_tokens?: number;
  input_cache_write_tokens?: number;
  input_cache_read_tokens?: number;
  output_tokens?: number;
  output_reasoning_tokens?: number;
  /** Cached input over TOTAL input, derived gateway-side so every client agrees. */
  cache_hit_rate?: number;
  cost_usd?: number;
  duration_ms?: number;
  first_turn_at?: number;
  last_turn_at?: number;
  provider?: string;
  model?: string;
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

export type ToggleType = "boolean" | "enum";

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

/** Sanitized MCP inventory served by one gateway. Secret values never travel here. */
export interface McpServer {
  name: string;
  transport: "stdio" | "streamable_http";
  enabled: boolean;
  is_connected: boolean;
  /** False when the server comes from a hand-written config tier: listed, but this API never rewrites it. */
  is_managed: boolean;
  tools: number;
  /** True while the server is force-stopped at runtime. Config is untouched; `start` releases it. */
  is_killed: boolean;
  /** HTTP servers only: whether the gateway already holds a usable OAuth token. */
  is_authorized?: boolean;
  command?: string;
  /** Non-secret rest of the spec, so a client can render an edit form without losing them. */
  args?: string[];
  cwd?: string;
  url?: string;
  timeout_ms?: number;
}

export interface McpServersResponse {
  servers: McpServer[];
}

/**
 * One headless MCP OAuth flow. The daemon keeps the PKCE verifier, the state
 * nonce, and the token; only these fields ever travel, so this device can drive
 * a sign-in for a gateway running somewhere else entirely.
 */
export interface McpAuthFlow {
  flow_id: string;
  server: string;
  kind: "pkce";
  url: string;
  redirect_uri?: string;
  expires_at_ms?: number;
  status: "pending" | "ok" | "error";
  error?: string;
}

/** Non-secret view of a server's persisted MCP OAuth tokens. */
export interface McpAuthStatus {
  server: string;
  is_authorized: boolean;
  is_expired: boolean;
  has_refresh_token: boolean;
  expires_at_ms?: number;
  scope?: string;
}

export interface McpServerInput {
  transport: "stdio" | "streamable_http";
  enabled?: boolean;
  command?: string;
  args?: string[];
  cwd?: string;
  env?: Record<string, string>;
  url?: string;
  headers?: Record<string, string>;
  timeout_ms?: number;
}

export interface McpTestResult {
  name: string;
  is_connected: boolean;
  tools: Array<{ name?: string; description?: string }>;
}

// ── Router: providers, models, OAuth ────────────────────────────────
// Wire keys are snake_case strings and boolean flags are `is_<foo>` — these
// mirror the gateway EDN (`:is-authenticated`) mechanically. Never rename a
// field on the way in.

export interface ProviderStatus {
  is_authenticated?: boolean;
  detail?: string;
  label?: string;
  /** Where the credential came from: `auth-file`, `config`, `env-var`, … */
  source?: string;
  account_type?: string;
  /** Milliseconds until the daemon's credential expires, when it knows. */
  expires_in_ms?: number;
  /** Probe failure (unreachable local provider, refused token, …). */
  error?: string;
}

/**
 * One quota window out of the daemon's limits REPORT — the provider's own
 * payload, passed through verbatim (`used`/`limit` are absolute, not percent).
 */
export interface ProviderLimitRow {
  id?: string;
  label?: string;
  used?: number;
  limit?: number;
  remaining?: number;
  is_unlimited?: boolean;
  window?: {
    kind?: string;
    unit?: string;
    size?: number;
    resets_at_ms?: number;
  };
  note?: string;
}

/**
 * The gateway's limits report for one provider, exactly as `/v1/router` and
 * `/v1/providers/:id/limits` emit it. Rows live under `dynamic.limits`.
 */
export interface ProviderLimits {
  provider_id?: string;
  status?: "ok" | "loading" | "error" | string;
  fetched_at_ms?: number;
  static?: Record<string, unknown>;
  dynamic?: { limits?: ProviderLimitRow[]; note?: string };
  error?: { message?: string };
}

export interface RouterProvider {
  id: string;
  label: string;
  base_url?: string;
  models: string[];
  is_default: boolean;
  default_model: string | null;
  /** The FALLBACK tag — always a different provider than the default one. */
  is_fallback: boolean;
  fallback_model: string | null;
  status?: ProviderStatus;
  limits?: ProviderLimits;
}

/**
 * One row of the "Add provider" picker: a provider this machine knows how to
 * configure and does NOT carry yet (`GET /v1/provider-presets`).
 *
 * `auth_kind` is the second step the client owes the user right after the add —
 * `oauth` starts a flow, `api-key` collects a key, `none` is a local runtime
 * that needs no credential at all — and `is_local` marks the presets whose
 * `base_url` the USER owns, because LM Studio and Ollama listen wherever that
 * machine put them.
 */
export interface ProviderPreset {
  id: string;
  label: string;
  auth_kind: "oauth" | "api-key" | "none" | "command";
  is_local: boolean;
  base_url?: string;
  api_style?: string;
  /** The catalog the provider ships with; added verbatim. */
  models: string[];
}

export interface ModelPref {
  provider?: string;
  model?: string;
}

/**
 * A live auth flow the daemon is holding open. `kind` decides the UX:
 * `device` shows `user_code` + `verification_uri` and finishes by polling;
 * `pkce` opens `url` and needs the final redirect URL pasted back;
 * `api-key` shows `instructions` and needs the key typed in.
 * The PKCE verifier, device code, and API key never live on this device.
 */
export interface AuthFlow {
  flow_id: string;
  provider_id: string;
  kind: "pkce" | "device" | "api-key";
  url?: string;
  user_code?: string;
  verification_uri?: string;
  interval_ms?: number;
  expires_at?: number;
  instructions?: string[];
}

export interface AuthVerdict {
  status: "ok" | "pending" | "error" | "cancelled" | "logged-out";
  message?: string;
}

export interface ThemeSummary {
  id: string;
  display_name: string;
  mode: "light" | "dark";
  /** Browser-ready CSS custom properties for THIS theme (present in the themes list). */
  css_vars?: Record<string, string>;
}

/** App-local appearance choice. It is never read from or written to a gateway. */
/** Id of the app-local palette selected from the paired gateways' theme catalogs. */
export type ThemePref = string;

export interface GatewayTheme extends ThemeSummary {
  css_vars: Record<string, string>;
  themes: ThemeSummary[];
}

export interface VoiceModelState {
  status: "ready" | "downloading" | "failed" | "absent" | "unavailable";
  progress?: number;
  /** What the 'downloading' status is actually doing right now. */
  phase?: "downloading" | "extracting";
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

/**
 * ONE artifact a tool call PRODUCED (a matplotlib figure, an `attach`ed
 * image/file), as the gateway's byte-free descriptor. It ships identically on
 * the live `iteration.completed` frame and on the persisted transcript, so a
 * produced image renders the same live and in history. The bytes are lazy:
 * `GatewayClient.attachmentUrl(sid, iteration_id, index)`.
 */
export interface IterationAttachment {
  index: number;
  iteration_id?: string;
  tool_call_id?: string;
  kind?: string;
  media_type?: string;
  filename?: string;
  /**
   * Which cut of THIS name it is, 1-based. Re-attaching a filename is the next
   * version of that artifact rather than a second artifact, so a gallery groups
   * by `filename` and shows the highest version with the rest behind it.
   */
  version?: number;
  size?: number;
}

/**
 * One row of `GET /v1/sessions/:sid/artifacts` — the byte-free descriptor of
 * [[IterationAttachment]] plus the 1-based `turn` that produced it, counted
 * from the start of the session. The endpoint answers the WHOLE session, so a
 * gallery no longer depends on how far back the reader has paged.
 */
export interface SessionArtifactRow extends IterationAttachment {
  turn?: number;
}

/**
 * The gateway's version contract, mirrored from
 * `com.blockether.vis.internal.gateway.protocol`. `protocol` is the wire
 * number it speaks; `min_client` / `min_gateway` are the oldest counterparts it
 * still serves; `version` is the human Vis release.
 */
export interface GatewayProtocol {
  protocol?: number;
  min_client?: number;
  min_gateway?: number;
  version?: string;
}

/** `GET /healthz` — always open, so it answers even for a rejected client. */
export interface GatewayHealth {
  status?: string;
  id?: string;
  protocol?: GatewayProtocol;
}

export interface GatewayCapabilities {
  version: number;
  protocol?: GatewayProtocol;
  /**
   * Every base URL this gateway is reachable at, most durable first (Tailscale
   * before LAN). Lets an already-paired app discover the tailnet address
   * without re-scanning a pairing QR. Absent on older gateways.
   */
  addresses?: string[];
  /** The gateway's own verdict on the caller that asked. */
  compatibility?: {
    is_compatible?: boolean;
    reason?: string;
    client?: string;
    client_protocol?: number;
    client_version?: string;
  };
  features: {
    chat: { enabled: boolean };
    pastes?: {
      enabled: boolean;
      transport: "display_request";
      format: "vis-paste-v1";
      inline_max_chars: number;
      collapsed_by_default: boolean;
    };
    attachments: {
      enabled: boolean;
      transport: "inline-base64";
      media_types: string[];
      /** The subset of `media_types` that is a clip, not a still. */
      video_media_types?: string[];
      max_files: number;
      max_file_bytes: number;
      /** Clips carry their own, much larger ceiling. */
      max_video_bytes?: number;
    };
    voice: {
      enabled: boolean;
      transport: "audio/wav";
      transcription: "gateway-local";
      /** Since the job API: POST answers 202 and progress arrives from the job. */
      is_async?: boolean;
      /** Every phase a job can report, in order. */
      phases?: VoicePhase[];
      /** How a job reports: the gateway PUSHES every step on the job's stream. */
      progress?: "sse";
      /**
       * The `event:` name every frame on that stream carries (`voice.job`). Told,
       * never guessed: it is what separates a job's progress from a session
       * event, and `VOICE_JOB_EVENT` in `./gateway` is this client's copy.
       */
      progress_event?: string;
      model: VoiceModelState;
      /** Every transcription engine this gateway can use. */
      engines?: VoiceEngine[];
      /** Id of the engine a recording is sent to unless one is named. */
      selected?: string | null;
    };
    push?: PushStatus;
  };
}

export interface VoiceTranscript {
  text: string;
}

/** One transcription engine as the gateway advertises it. */
export interface VoiceEngine {
  id: string;
  label?: string;
}

/**
 * Where a transcription IS.
 *
 * `uploading` is the CLIENT's own phase (bytes still in flight); everything from
 * `queued` on is the gateway's job reporting on itself.
 */
export type VoicePhase =
  | "uploading"
  | "queued"
  | "preparing"
  | "transcribing"
  | "done"
  | "failed";

/** A transcription job, as its own `/voice/jobs/:id/events` stream reports it. */
export interface VoiceJob {
  id: string;
  engine?: string;
  phase: VoicePhase;
  progress: number;
  is_done: boolean;
  created_at?: number;
  updated_at?: number;
  text?: string;
  error?: string;
}

/** One progress tick handed to a caller of `transcribeVoice`. */
export interface VoiceProgress {
  phase: VoicePhase;
  /** 0..100 within THIS phase. */
  progress: number;
  engine?: string;
}

export interface GatewayStatus {
  pid?: number;
  url?: string;
  db?: string;
  clients?: number;
  auth?: string;
  [k: string]: unknown;
}

/**
 * Whether THIS gateway can deliver a native push at all: it needs an APNs key,
 * key id, team id and topic. `missing` names what is absent, so the app can say
 * why instead of just failing to notify.
 */
export interface PushStatus {
  is_available: boolean;
  provider: "apns" | "fcm" | "apns+fcm" | "relay" | "web";
  /** APNs view, mirrored at the top level for older gateways. */
  environment?: "sandbox" | "production";
  topic?: string | null;
  missing?: string[];
  apns?: {
    is_available: boolean;
    environment?: "sandbox" | "production";
    topic?: string | null;
    key_source?: string | null;
    missing?: string[];
  };
  /** Firebase Cloud Messaging — the Android half. */
  fcm?: {
    is_available: boolean;
    project_id?: string | null;
    source?: string | null;
    missing?: string[];
  };
  /** This gateway's generated VAPID identity for browser push. */
  web_push?: {
    is_available: boolean;
    application_server_key?: string | null;
    subject?: string | null;
    source?: string | null;
    missing?: string[];
  };
  /** The native push relay, used only by native builds without local credentials. */
  relay?: {
    is_available: boolean;
    url?: string | null;
    source?: string | null;
    /** Named, but over plain http: configured and unusable, not absent. */
    is_insecure?: boolean;
  };
  devices: number;
}

/** One device registered with the gateway. The raw token never leaves it. */
export interface PushDevice {
  token_preview: string;
  platform?: string;
  environment?: "sandbox" | "production";
  client?: string;
  client_version?: string;
  label?: string;
  bundle_id?: string;
  /** Set when this device is woken through a relay, which is not a secret. */
  relay_url?: string;
  is_relayed?: boolean;
  registered_at?: number;
  last_seen?: number;
}

/**
 * Body of `POST /v1/devices`. Exactly one of `token` (this gateway pushes with
 * its own credentials) or `grant` (it pushes through the relay) is required.
 */
export interface PushDeviceInput {
  token?: string;
  grant?: string;
  /** The relay that sealed `grant`; it is the only one that can open it. */
  relay_url?: string;
  platform?: string;
  environment?: "sandbox" | "production";
  client?: string;
  client_version?: string;
  label?: string;
  bundle_id?: string;
}

/** APNs' verdict per device for `POST /v1/devices/actions/test`. */
export interface PushSendResult {
  token_preview: string;
  status: number;
  reason?: string;
  is_delivered: boolean;
}

/** One SSE event as delivered by GET /v1/events?sids=… */

export type JsonValue =
  string | number | boolean | null | JsonValue[] | { [key: string]: JsonValue };

export interface ContentBlock {
  id: string;
  type:
    "prose" | "code" | "tool" | "reasoning" | "error" | "attachment" | "notice";
  markdown?: string;
  text?: string;
  language?: string;
  tool?: string;
  status?: "pending" | "running" | "completed" | "failed" | "cancelled";
  input?: JsonValue;
  output?: JsonValue;
  error?: JsonValue;
  code?: string;
  message?: string;
  retryable?: boolean;
  visibility?: "private" | "visible";
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
  /** Highlighting language for `display_code`, authored by the tool that rendered it. */
  display_language?: string;
  comment?: string;
  result?: JsonValue;
  result_render?: string;
  result_summary?: string;
  /**
   * The op-card headline WHILE the call runs, authored by the tool's own
   * renderer (`shell`'s `$ npm test (running)`). Its own key, never
   * `result_summary`: a pending card must not read as an outcome.
   */
  pending_summary?: string;
  /**
   * The op-card BODY while the call runs, authored by the tool's own renderer
   * (`shell`'s `**COMMAND**` section): the same sections its finished body is
   * built from, minus the outcome. Its own key, never `result_render`.
   */
  pending_render?: string;
  result_kind?: string;
  result_detail?: Record<string, JsonValue>;
  render_segments?: JsonValue[];
  cards?: TranscriptForm[];
  error?: JsonValue;
  stdout?: string;
  tool_name?: string;
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
  /** Artifacts this iteration's tool calls produced (bytes fetched lazily). */
  attachments?: IterationAttachment[];
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
  /** Set when the gateway enqueued the turn; orders the queue tray. */
  queued_at?: number;
  [key: string]: unknown;
}

/** One image on a queued message — chip metadata only, never pixel bytes. */
export interface QueuedAttachment {
  filename: string;
  mediaType: string;
  sizeLabel: string;
}

/** A message enqueued behind the running turn, mirrored from the gateway. */
export interface QueuedTurn {
  turnId: string;
  /** Raw request text, exactly as authored — what an edit starts from. */
  request: string;
  /** What the row PAINTS: image paths already collapsed to `🖼 name.png`. */
  preview: string;
  attachments: QueuedAttachment[];
}

/** The gateway paused distinct queued requests after a turn failed. */
export interface QueuePausedInfo {
  reason: string;
  held: number;
}

export interface SseEvent {
  type: string;
  sid?: string;
  session_id?: string;
  seq?: number;
  /** Gateway epoch sampled when this event was emitted. */
  ts?: number;
  /**
   * `subscription.ready` only: the turn the daemon is running for this session at
   * the moment it accepted the (re)subscribe, and an explicit liveness flag so an
   * idle session is distinguishable from an older daemon that omits both. Lets a
   * reconnecting screen decide whether the turn it paints is still real without a
   * round trip — its own cursor cannot answer that (the replay ring is process
   * memory and dies with the daemon).
   */
  current_turn_id?: string | null;
  is_live?: boolean;
  [k: string]: unknown;
}
