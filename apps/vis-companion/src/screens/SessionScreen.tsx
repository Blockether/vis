import {
  useCallback,
  useEffect,
  useLayoutEffect,
  useMemo,
  useRef,
  useState,
  type ClipboardEvent as ReactClipboardEvent,
  type MouseEvent as ReactMouseEvent,
} from "react";
import {
  AssistantMessage,
  transcriptEnterClass,
  UserMessage,
} from "../components/ChatContent";
import { ArtifactsChip, ArtifactsSheet } from "../components/ArtifactsSheet";
import { collapseArtifactVersions, collectArtifacts } from "../lib/artifacts";
import { ExpandableImage } from "../components/ImageViewer";
import { Banner, Spinner } from "../components/ui";
import {
  ArrowDownIcon,
  CameraIcon,
  ChevronIcon,
  CloseIcon,
  ImageIcon,
  MicIcon,
  PlusIcon,
} from "../components/icons";
import { HumanInputPrompt } from "../components/HumanInputPrompt";
import { ProviderRouterDialog } from "./RouterScreen";
import {
  attachmentsFromFiles,
  capturePhotoAttachment,
  editedAttachment,
  isVideoMediaType,
  pickMediaAttachments,
  type PendingAttachment,
} from "../lib/attachments";
import { sheetDismissed } from "../lib/image-file";
import { AttachImageContext } from "../lib/attach-image";
import type { GatewayClient } from "../lib/gateway";
import { holdKeyboardAcrossSheet } from "../lib/keyboard";
import {
  mergeQueueBacklog,
  queuedTurnFromWire,
  type QueueDelta,
} from "../lib/gateway";
import {
  exactCost,
  formatCost,
  formatTokens,
  sessionUsage,
} from "../lib/usage";
import type { SessionSubscriptionHub } from "../lib/subscriptions";
import {
  collapsePastePlaceholders,
  createComposerPaste,
  expandPastePlaceholders,
  pasteSummary,
  shouldCollapsePaste,
  type ComposerPaste,
} from "../lib/paste";
import {
  draftMessageKey,
  flushDraftMessages,
  peekDraftMessage,
  readDraftMessage,
  watchDraftMessageExits,
  writeDraftMessage,
} from "../lib/draft-messages";
import {
  appendSharedText,
  hydratePendingShare,
  onSharedText,
  takePendingShare,
} from "../lib/share-intake";
import {
  clearPendingVoice,
  readPendingVoice,
  savePendingVoice,
} from "../lib/pending-voice";
import { readerOwnsScroll } from "../lib/reader-gesture";
import type {
  ContentBlock,
  IterationAttachment,
  GatewayCapabilities,
  FileSuggestion,
  QueuedTurn,
  QueuePausedInfo,
  Session,
  SlashCommand,
  SseEvent,
  SubmittedTurn,
  TranscriptForm,
  TranscriptIteration,
  TranscriptTurn,
  VoiceModelState,
  VoiceProgress,
  ModelPref,
  Toggle,
  GatewayAttachment,
} from "../lib/types";
import {
  startWavRecording,
  voiceProgressLabel,
  type WavRecording,
} from "../lib/voice";
import { onWake } from "../lib/wake";
import {
  applyScrollAnchor,
  isViewportRotating,
  onViewportRotation,
  scrollAnchorFor,
  type ScrollAnchor,
  shellViewportHeight,
  useSafeBottomStyle,
} from "../lib/viewport";
import {
  markSessionRead,
  visibleAnsweredTurnCount,
} from "../lib/unread";
import { App } from "@capacitor/app";
import { Capacitor } from "@capacitor/core";

import { workspaceRelativePath } from "../lib/path";
import { isDraftWorkspace } from "../lib/fleet";

interface LiveActivity {
  kind: string;
  iteration?: number;
  command?: string;
  operation?: string;
  label?: string;
}

interface LiveTurn {
  id?: string;
  request: string;
  answer: string;
  iterations: TranscriptIteration[];
  activity?: LiveActivity;
  startedAt: number;
  cancelling?: boolean;
  status: "running" | "completed" | "failed" | "cancelled";
  // Bytes of the images this device just sent. The gateway's live rail carries
  // none (persisted rows own them), so the bubble would otherwise be text-only.
  attachments?: GatewayAttachment[];
  // The terminal frame's OWN content (today: the gateway's error card on
  // `turn.failed`). The transcript refetch normally takes over and renders it,
  // but that is a network round-trip that can fail — and then the bubble had
  // nothing to show but the bare word "failed". The TUI's independent terminal
  // path paints this card without asking anyone; so does this one.
  content?: ContentBlock[];
}

const TERMINAL_EVENTS = new Set([
  "turn.completed",
  "turn.failed",
  "turn.cancelled",
]);
const LIVE_BODY_THROTTLE_MS = 150;

// A body frame may wait for stable rotation geometry, but lifecycle truth may
// not: a terminal must stop the spinner/settle immediately and a new start must
// replace any cached bubble without being painted as the previous turn.
function forcesLiveFlushDuringRotation(event: SseEvent): boolean {
  return TERMINAL_EVENTS.has(event.type) || event.type === "turn.started";
}

// A settle's transcript refetch only has to pick up the ONE row the finished
// turn just persisted, on top of a snapshot that already holds the tail. Four
// rows cover that with slack (a turn and its neighbours, a queued row that
// drained meanwhile) at a fraction of a full 24-turn page's bytes and hydration.
const SETTLE_TAIL_TURNS = 4;

// Backoff between settle's transcript reads while the finished turn's row is
// still being written. Escalating and early-exiting: the first retry lands long
// before the old flat 300 ms sleep did, and a slow write (or a fetch that
// threw) gets four chances across ~1.1 s instead of one, so the 5 s reconcile
// tick stays a backstop rather than the thing that swaps a finished bubble for
// its persisted row.
const SETTLE_RETRY_MS = [70, 150, 300, 600];

// STUCK-turn self-heal. The live bubble settles on the terminal SSE frame; if
// that single frame is lost (reconnect gap, a backgrounded tab, a stream torn
// down mid-turn) nothing else ever ends the turn and the bubble streams
// forever. After this much silence on a running turn, ask the gateway registry
// directly. Long enough that a healthy terminal frame always wins the race.
const TURN_LIVENESS_IDLE_MS = 10000;
const TURN_LIVENESS_PROBE_INTERVAL_MS = 5000;
// A working turn can be SILENT for a very long time: one `shell` or
// `python_execution` call blocks its iteration until the command returns, and
// nothing is emitted meanwhile. Quiet is therefore NOT evidence of a broken
// stream while the registry still confirms the turn — reconnecting on every
// quiet tick tore the multiplexed SSE connection down every 10s for the whole
// length of a long command, which flapped the header to "Reconnecting" and
// replaced the "Vis is running: …" ticker with a reconnect notice. Only silence
// past this bound is treated as a frozen transport. 30s sits just under the
// gateway's own 15s heartbeat doubled, so a genuinely frozen socket is caught
// within two missed beats instead of four, and then at most one reconnect per
// window.
const TURN_STREAM_STALL_MS = 30_000;
// …and never on a SINGLE verdict. One probe can be wrong in both directions (a
// heartbeat racing the read, a registry row not yet visible), and every wrong
// verdict costs a stream teardown the user sees. So a suspicious answer only
// arms the watchdog: it must be RE-CHECKED on the next probe and agree with
// itself before anything is torn down. Two agreeing probes 5s apart cost one
// extra tick of latency and remove every single-sample false positive.
const TURN_STALL_CONFIRMATIONS = 2;
const TURN_TERMINAL_STATUSES = new Set(["completed", "failed", "cancelled"]);
const INITIAL_VISIBLE_TURNS = 8;
// Assistant turns can contain thousands of syntax-highlighted nodes. Twenty-four
// such turns is still enough DOM to make keyboard resize and momentum scrolling
// hitch on a phone, even when React does not re-render them. Keep the mounted
// window deliberately small; older turns remain available through the explicit
// pager above the transcript.
const FIRST_PAINT_TURNS = 2;
// Do not spend the next frame after opening a session mounting a second screenful
// of markdown. A small ramp leaves input focus, keyboard animation, and scrolling
// a chance to paint between batches.
const HYDRATE_TURNS_PER_FRAME = 2;
// Last-resort reveal for the loading veil. The veil is dropped by whoever
// finishes first — the transcript read, or the scroll effect one animation
// frame after the first paint — and BOTH can be lost on mobile: a webview that
// is frozen at that moment runs no animation frame, and a request suspended
// with the app can settle never. The pending flag is consumed either way, so
// nothing re-arms the reveal and the session looks permanently stuck. Reveal
// unconditionally after this: a transcript that is still loading paints when it
// lands, and an unreachable gateway shows its error instead of a spinner.
const LOADING_VEIL_MAX_MS = 12_000;

// A transcript row is a PLACEHOLDER while its turn runs. The engine persists it
// at SUBMIT with `running` (`/transcript` ships the engine row verbatim) and the
// gateway overlay calls the same state `streaming` (`persisted-status->wire` in
// gateway/state.clj). Neither carries the answer, so neither may ever stand in
// for the live bubble the user is watching.
const IN_FLIGHT_ROW_STATUSES = new Set([
  "running",
  "streaming",
  "queued",
  "pending",
]);

/**
 * Did the gateway QUEUE this submission behind a running turn instead of starting
 * it? `POST /v1/sessions/:sid/turns` answers with the turn record itself
 * (`submit-turn!` in gateway/state.clj): an enqueued one carries `status:
 * "queued"` and a `queued_at` stamp, a started one `status: "running"` and
 * `started_at`. THAT answer — never the local `running` flag, which can lag the
 * gateway in both directions — decides who owns the message: the live rail or the
 * queue tray.
 */
function isQueuedSubmission(turn: SubmittedTurn): boolean {
  const status = String(turn.status ?? "");
  if (status) return status === "queued";
  return turn.queued_at != null && turn.started_at == null;
}

function isRunningRow(turn: TranscriptTurn): boolean {
  const status = String(turn.status ?? "");
  return status === "running" || status === "streaming";
}

function isSettledRow(turn: TranscriptTurn): boolean {
  return !IN_FLIGHT_ROW_STATUSES.has(String(turn.status ?? ""));
}

function rowId(turn: TranscriptTurn): string {
  return String(turn.id ?? turn.turn_id ?? "");
}

/**
 * The turn the TRANSCRIPT itself says is still in flight, if any.
 *
 * `live` / `current_turn_id` are read from the gateway's in-memory registry, and
 * that registry can be wrong in the one direction that hurts: the stall watchdog
 * and the cancel backstop clear `:current-turn` while the engine keeps
 * iterating, so `/v1/sessions/:sid` answers `idle`, `live: false`,
 * `current_turn_id: null` about a turn on its 430th iteration. Believing that
 * flag is how this screen sat on "Vis is waiting for an update" for two hours:
 * nothing was adopted, so `reduceLiveEvent` dropped every delta that WAS
 * arriving, while `/transcript` held the whole turn the entire time.
 *
 * The persisted row is the second witness and it does not go through the
 * registry: the row is written `running` when the turn is accepted and only
 * patched at the terminal frame, so an unsettled row means work. Its id is the
 * same turn id `/turns/:tid/trace` resumes from, which makes it enough to adopt
 * from. Only one turn runs per session, so the LAST unsettled row — scanning
 * back over queued rows, stopping at the first settled one — is that turn.
 */
function inFlightRow(
  turns: readonly TranscriptTurn[] | null,
): TranscriptTurn | null {
  if (!turns?.length) return null;
  for (let index = turns.length - 1; index >= 0; index -= 1) {
    const turn = turns[index];
    if (isSettledRow(turn)) return null;
    if (isRunningRow(turn)) return turn;
  }
  return null;
}

/**
 * Has the streamed live turn actually been REPLACED by a settled transcript row?
 *
 * Identity alone cannot answer this. The terminal SSE frame carries the GATEWAY
 * turn id (`turn-terminal-payload` is the lean `{turn_id, status}`), while the
 * transcript rows are the ENGINE's own rows under ids the engine mints inside
 * `send!` — "the gateway's `tid` is NOT the engine's persisted row id". So an
 * id test either never matches (the bubble lingers, then duplicates) or matches
 * the still-`running` placeholder — and dropping the bubble against THAT is how
 * a whole finished turn, with every iteration the user watched, vanished from
 * the screen: the placeholder carries no content and `visibleTurns` filters it.
 *
 * The reliable claim is "a SETTLED row exists that was not here before this turn
 * started". `before` is the id snapshot taken while no live bubble existed;
 * `created_at` (when present) keeps a "load earlier" page of ancient history
 * from passing as this turn's answer. Same invariant the TUI holds in
 * `:message-received`: never drop the live trace unless something at least as
 * complete replaces it.
 */
function liveTurnSettledInto(
  turns: TranscriptTurn[] | null,
  before: Set<string>,
  finishedId: string,
  startedAt?: number,
): boolean {
  if (!turns?.length) return false;
  return turns.some((turn) => {
    if (!isSettledRow(turn)) return false;
    const id = rowId(turn);
    if (finishedId && id === finishedId) return true;
    if (before.has(id)) return false;
    const created = turn.created_at;
    if (startedAt && typeof created === "number" && Number.isFinite(created)) {
      // One minute of slack: clock skew between the engine's row stamp and the
      // client's `Date.now()` must never disqualify this turn's own answer.
      return created >= startedAt - 60_000;
    }
    return true;
  });
}

function stringField(event: SseEvent, key: string): string {
  const value = event[key];
  return typeof value === "string" ? value : "";
}

function applyText(current: string, event: SseEvent): string {
  const cumulative = stringField(event, "cumulative");
  return cumulative || current + stringField(event, "text");
}

function eventIteration(event: SseEvent): number {
  const value = event.iteration;
  const parsed = typeof value === "number" ? value : Number(value);
  return Number.isFinite(parsed) ? parsed : 0;
}

function compactLabel(value: string, fallback: string): string {
  const label = value.split("\n", 1)[0].trim();
  if (!label) return fallback;
  return label.length > 64 ? `${label.slice(0, 61)}…` : label;
}

function commandPhase(request: string): string | null {
  const text = request.trim();
  if (text.startsWith("!&")) {
    return `Vis is starting: ${compactLabel(text.slice(2), "…")}`;
  }
  if (text.startsWith("!")) {
    return `Vis is running: ${compactLabel(text.slice(1), "…")}`;
  }
  if (text.startsWith("/")) {
    return `Vis is running: ${compactLabel(text.split(/\s+/, 1)[0], "command")}`;
  }
  return null;
}

function liveProgressPhase(
  turn: LiveTurn,
  connected: boolean,
  workspaceRoots: readonly (string | null | undefined)[],
): string {
  if (!connected) return "Reconnecting — checking turn status";
  if (turn.cancelling) return "Vis is cancelling";

  const last = turn.iterations.at(-1);
  const activity = turn.activity;
  const iteration = Math.max(
    turn.iterations.length,
    activity?.iteration == null ? 0 : activity.iteration,
  );

  if (last?.error != null) return "Vis is retrying";
  if (iteration === 0)
    return commandPhase(turn.request) ?? "Vis is waiting for an update";

  const suffix = `(iter ${iteration})`;
  switch (activity?.kind) {
    case "shell-run":
      return `Vis is running: ${compactLabel(activity.command ?? "", "…")}`;
    case "shell-bg":
      return `Vis is starting: ${compactLabel(activity.command ?? "", "…")}`;
    case "slash":
      return `Vis is running: ${compactLabel(activity.command ?? "", "command")}`;
    case "provider-call":
      return `Vis is calling the provider ${suffix}`;
    case "response-parse":
      return `Vis is parsing model response ${suffix}`;
    case "tool":
    case "tool-call": {
      const label = workspaceRelativePath(activity.label, workspaceRoots);
      return `Vis is running: ${activity.operation || "tool"}${label ? ` ${compactLabel(label, "")}` : ""} ${suffix}`;
    }
    default:
      break;
  }

  if (last?.thinking?.trim()) return `Vis is thinking ${suffix}`;
  if (last?.forms?.length) return `Vis is running code ${suffix}`;
  return `Vis is working ${suffix}`;
}

function updateLiveIteration(
  turn: LiveTurn,
  position: number,
  update: (iteration: TranscriptIteration) => TranscriptIteration,
): LiveTurn {
  const index = turn.iterations.findIndex(
    (iteration) => iteration.position === position,
  );
  if (index < 0) {
    return {
      ...turn,
      iterations: [...turn.iterations, update({ position, forms: [] })].sort(
        (a, b) => (a.position ?? 0) - (b.position ?? 0),
      ),
    };
  }

  const iterations = [...turn.iterations];
  iterations[index] = update(iterations[index]);
  return { ...turn, iterations };
}

function formFromEvent(event: SseEvent, running = false): TranscriptForm {
  const cards = Array.isArray(event.cards)
    ? (event.cards as TranscriptForm[])
    : undefined;
  return {
    block_id: stringField(event, "block_id"),
    scope: stringField(event, "scope") || undefined,
    code: stringField(event, "code") || undefined,
    display_code: stringField(event, "display_code") || undefined,
    display_language: stringField(event, "display_language") || undefined,
    comment: stringField(event, "comment") || undefined,
    tool_name: stringField(event, "tool_name") || undefined,
    tool_color_role: stringField(event, "tool_color_role") || undefined,
    result_summary:
      stringField(event, "result_summary") ||
      (running ? "Running…" : undefined),
    // The tool-authored headline for a call still in flight; `Running…` above stays
    // the sentinel the placeholder logic keys on, this is what the card SHOWS.
    pending_summary: stringField(event, "pending_summary") || undefined,
    // …and the body it paints under that headline while it runs.
    pending_render: stringField(event, "pending_render") || undefined,
    result_render: stringField(event, "result_render") || undefined,
    result_kind: stringField(event, "result_kind") || undefined,
    result: event.result as TranscriptForm["result"],
    error: event.error as TranscriptForm["error"],
    stdout: stringField(event, "stdout") || undefined,
    cards,
    silent: event.silent === true,
    duration_ms:
      typeof event.duration_ms === "number" ? event.duration_ms : undefined,
  };
}

function formIsRunningPlaceholder(form: TranscriptForm): boolean {
  return (
    form.result == null &&
    form.error == null &&
    form.duration_ms == null &&
    (!form.result_summary || form.result_summary === "Running…")
  );
}

function formHasOutcome(form: TranscriptForm): boolean {
  return (
    form.result != null ||
    form.error != null ||
    form.duration_ms != null ||
    (!!form.result_summary && form.result_summary !== "Running…")
  );
}

function upsertLiveForm(
  iteration: TranscriptIteration,
  next: TranscriptForm,
): TranscriptIteration {
  const forms = [...(iteration.forms ?? [])];
  const blockId = next.block_id;
  let index = forms.findIndex((form) => blockId && form.block_id === blockId);
  // Fallback: a completed form supersedes the still-running placeholder for the
  // same tool when block_id didn't line up (gateway replay / a started event
  // that shipped no block_id). Without this the 'X Running…' placeholder and the
  // finished card both render — the same op shown twice.
  if (index < 0 && formHasOutcome(next)) {
    index = forms.findIndex(
      (form) =>
        formIsRunningPlaceholder(form) &&
        (form.tool_name ?? "") === (next.tool_name ?? "") &&
        (form.scope ?? "") === (next.scope ?? ""),
    );
  }
  if (index < 0) forms.push(next);
  else {
    const defined = Object.fromEntries(
      Object.entries(next).filter(([, value]) => value !== undefined),
    ) as TranscriptForm;
    forms[index] = { ...forms[index], ...defined };
  }
  return { ...iteration, forms };
}

function reduceLiveEvent(
  turn: LiveTurn | null,
  event: SseEvent,
): LiveTurn | null {
  const type = event.type;
  if (type === "turn.started") {
    const startedId = stringField(event, "turn_id");
    return {
      id: startedId,
      request: stringField(event, "request"),
      answer: "",
      iterations: [],
      startedAt:
        typeof event.started_at === "number" ? event.started_at : Date.now(),
      status: "running",
      // `turn.started` for the turn we optimistically painted must not drop the
      // attachments we are already showing (the event has no bytes).
      attachments:
        turn && (!turn.id || turn.id === startedId)
          ? turn.attachments
          : undefined,
    };
  }
  if (!turn) return turn;
  // The terminal frame IS the end of the turn (same rule as the TUI). Anything that
  // arrives after it — a trailing `activity`, a late `block.output`, a replayed
  // progress frame — must never re-animate a settled bubble. Without this guard a
  // post-terminal frame put the ticker back up ("Vis is running …") and repainted
  // progress for work that had already finished.
  if (turn.status !== "running") return turn;

  if (type === "content.block.delta") {
    const field = stringField(event, "field");
    const blockId = stringField(event, "block_id");
    const position = eventIteration(event);
    if (field === "text") {
      const next = updateLiveIteration(turn, position, (iteration) => ({
        ...iteration,
        thinking: applyText(iteration.thinking ?? "", event),
      }));
      return { ...next, activity: undefined };
    }
    if (field === "markdown" && blockId.includes(":assistant-prose:")) {
      const next = updateLiveIteration(turn, position, (iteration) => ({
        ...iteration,
        assistant_prose: applyText(iteration.assistant_prose ?? "", event),
      }));
      // The model's prose streamed first as a live `:content` ticker (turn.answer)
      // and now lands as this iteration's canonical prose. Mirror the TUI
      // (progress.clj drops `:content-stream`): clear the live answer so the same
      // text isn't rendered twice — once above the tool and once below it.
      return { ...next, answer: "", activity: undefined };
    }
    if (field === "markdown") {
      return {
        ...turn,
        answer: applyText(turn.answer, event),
        activity: undefined,
      };
    }
    return turn;
  }

  if (type === "iteration.completed") {
    const position = eventIteration(event);
    // Byte-free descriptors for whatever the agent attached during this
    // iteration (`vis_attach`). The bytes come from the gateway's attachment
    // endpoint on demand; this is the only frame that announces them live, and
    // the transcript hydrates the identical shape for history.
    const attached = Array.isArray(event.attachments)
      ? (event.attachments as IterationAttachment[])
      : undefined;
    const next = updateLiveIteration(turn, position, (iteration) => ({
      ...iteration,
      thinking: stringField(event, "thinking") || iteration.thinking,
      assistant_prose:
        stringField(event, "assistant_prose") || iteration.assistant_prose,
      attachments: attached?.length ? attached : iteration.attachments,
      error: undefined,
    }));
    // If this iteration finalized any prose, the live `:content` ticker that fed
    // it has been promoted into the iteration — drop it so it isn't duplicated.
    const promoted = next.iterations.find(
      (i) => i.position === position,
    )?.assistant_prose;
    return {
      ...next,
      answer: promoted ? "" : turn.answer,
      activity: undefined,
    };
  }

  if (type === "block.preview") {
    const position = eventIteration(event);
    const form = formFromEvent(event, false);
    const next = updateLiveIteration(turn, position, (iteration) =>
      upsertLiveForm(iteration, form),
    );
    return { ...next, activity: undefined };
  }

  if (type === "block.started" || type === "block.output") {
    const position = eventIteration(event);
    const form = formFromEvent(event, type === "block.started");
    const next = updateLiveIteration(turn, position, (iteration) =>
      upsertLiveForm(iteration, form),
    );
    if (type === "block.output") return { ...next, activity: undefined };
    return {
      ...next,
      activity: {
        kind: form.tool_name ? "tool" : "code",
        iteration: position,
        operation: form.tool_name || form.scope,
      },
    };
  }

  if (type === "activity") {
    const kind = stringField(event, "activity");
    const rawIteration = event.iteration;
    const iteration =
      typeof rawIteration === "number"
        ? rawIteration
        : typeof rawIteration === "string" && rawIteration.trim()
          ? Number(rawIteration)
          : undefined;
    return {
      ...turn,
      activity: kind
        ? {
            kind,
            iteration: Number.isFinite(iteration) ? iteration : undefined,
            command: stringField(event, "cmd") || undefined,
            operation: stringField(event, "op") || undefined,
            label: stringField(event, "label") || undefined,
          }
        : undefined,
    };
  }

  if (type === "iteration.error" || type === "provider.retry") {
    const position = eventIteration(event);
    const next = updateLiveIteration(turn, position, (iteration) => ({
      ...iteration,
      error: (event.error_data ??
        event.error ??
        event.detail ??
        "retrying") as TranscriptIteration["error"],
    }));
    return { ...next, activity: undefined };
  }

  return turn;
}

function coalesceLiveEvents(events: SseEvent[]): SseEvent[] {
  const merged: SseEvent[] = [];
  for (const event of events) {
    const previous = merged.at(-1);
    const sameDelta =
      previous?.type === "content.block.delta" &&
      event.type === "content.block.delta" &&
      stringField(previous, "field") === stringField(event, "field") &&
      stringField(previous, "block_id") === stringField(event, "block_id") &&
      eventIteration(previous) === eventIteration(event);

    if (!previous || !sameDelta) {
      merged.push(event);
      continue;
    }

    const currentCumulative = stringField(event, "cumulative");
    const previousCumulative = stringField(previous, "cumulative");
    if (currentCumulative) {
      merged[merged.length - 1] = event;
    } else if (previousCumulative) {
      merged[merged.length - 1] = {
        ...previous,
        ...event,
        cumulative: previousCumulative + stringField(event, "text"),
        text: "",
      };
    } else {
      merged[merged.length - 1] = {
        ...previous,
        ...event,
        cumulative: "",
        text: stringField(previous, "text") + stringField(event, "text"),
      };
    }
  }
  return merged;
}

const FALLBACK_SLASHES: SlashCommand[] = [
  { name: "/help", doc: "Show the available slash commands." },
  {
    name: "/new-session",
    doc: "Create a new session. Optional text starts its first turn.",
  },
  { name: "/sessions", doc: "Return to the session list." },
  { name: "/rename", doc: "Rename this session's title." },
  {
    name: "/export",
    doc: "Export this session transcript to Markdown or HTML.",
  },
  {
    name: "/export-html",
    doc: "Export this session transcript as styled HTML.",
  },
  {
    name: "/cd",
    doc: "Show or change the session's filesystem root (the directory Vis works in).",
  },
  { name: "/draft new", doc: "Create an isolated draft workspace." },
  { name: "/draft apply", doc: "Apply the active draft workspace." },
  { name: "/draft abandon", doc: "Abandon the active draft workspace." },
  { name: "/draft list", doc: "List draft workspaces." },
  {
    name: "/reload",
    doc: "Reload extensions, skills, prompts, and context files.",
  },
];

function mergeSlashCommands(remote: SlashCommand[]): SlashCommand[] {
  const byName = new Map<string, SlashCommand>();
  for (const command of [...FALLBACK_SLASHES, ...remote])
    byName.set(command.name, command);
  return [...byName.values()].sort((a, b) => a.name.localeCompare(b.name));
}

// `@` file-mention trigger, mirroring the TUI (`file_suggest.clj` trigger-regex)
// VERBATIM: the `@` must begin a word (start of text or right after whitespace),
// and `@@` escapes to a literal `@`. `head` is the input text up to the caret.
const FILE_MENTION_REGEX = /(?:^|\s)@(?!@)(\S*)$/;

function fileMentionAt(head: string): { query: string; at: number } | null {
  const match = FILE_MENTION_REGEX.exec(head);
  if (!match) return null;
  const query = match[1] ?? "";
  return { query, at: head.length - query.length - 1 };
}

// Visible inline token inserted by the picker — quoted when the path has spaces,
// matching the TUI's `format-file-mention`.
function formatFileMention(path: string): string {
  return /\s/.test(path) ? `@"${path}"` : `@${path}`;
}

// Splice the picked `path` over the active `@token` at the caret, returning the
// new text and caret offset. Mirrors `file_suggest.clj` apply-mention.
function applyFileMention(
  text: string,
  caret: number,
  path: string,
): { text: string; caret: number } {
  const head = text.slice(0, caret);
  const mention = fileMentionAt(head);
  if (!mention) return { text, caret };
  const before = text.slice(0, mention.at);
  const after = text.slice(caret);
  const token = `${formatFileMention(path)} `;
  return { text: before + token + after, caret: before.length + token.length };
}

// Expand inline `@path` mentions into the SAME agent-facing read-this-file
// directive the TUI emits (`input.clj` file-mention->prompt-block), so the model
// knows the user attached a file. The visible transcript keeps the short `@path`
// token; only the outbound agent text carries the directive. `@@` stays literal.
const FILE_MENTION_EXPAND_REGEX =
  /(?<!\S)@(?:"([^"]+)"|([A-Za-z0-9][A-Za-z0-9._/-]*))/g;

function expandFileMentions(text: string): string {
  return text.replace(FILE_MENTION_EXPAND_REGEX, (_match, quoted, bare) => {
    const path = (quoted ?? bare) as string;
    return `[Attached File: ${path}]\nThe user attached this file. Read it (via the file/zipper tools) before answering.`;
  });
}

// Matches the veil's `duration-200`. Kept in JS because the veil has to stay
// MOUNTED for the length of its own fade-out (see the reveal effect below).
const VEIL_FADE_MS = 200;

// Mirrors the TUI's `paint-content-loading!`: a centered Braille spinner next
// to "Loading session…" while an existing session hydrates. New-session creation
// never mounts this — it opens straight to the empty transcript, matching the
// TUI (which suppresses the spinner for a still-building `:build-id` tab).
function LoadingSession() {
  return (
    <div
      className="flex min-h-[55vh] items-center justify-center font-mono text-body text-white"
      role="status"
      aria-label="Loading session"
    >
      <Spinner />
      <span>&nbsp;&nbsp;Loading session…</span>
    </div>
  );
}

/**
 * iOS takes the keyboard down the instant focus leaves the field, and a button
 * press IS a focus change: tapping a paste chip slid the whole shell down, then
 * straight back up as the editor's textarea autofocused — two full keyboard
 * animations for one tap, and the same again on the way out. Cancelling the
 * mousedown default keeps focus (and the keyboard) exactly where it is until the
 * next field claims it, so the flow is one still handover.
 */
function keepKeyboard(event: ReactMouseEvent<HTMLElement>) {
  event.preventDefault();
}

// The session id is the durable handle a user pastes into `vis-agent`/tools, so it is
// tap-to-copy rather than inert text — shown short with the full id on hover.
function CopyableId({ id, className }: { id: string; className: string }) {
  const [copied, setCopied] = useState(false);
  async function copy() {
    try {
      await navigator.clipboard.writeText(id);
      setCopied(true);
      window.setTimeout(() => setCopied(false), 1_200);
    } catch {
      // Clipboard access can be unavailable in an untrusted mobile webview.
    }
  }
  const short = id.length > 8 ? id.slice(0, 8) : id;
  return (
    <button
      type="button"
      onClick={copy}
      title={`Copy session id\n${id}`}
      aria-label="Copy session id"
      className={`group inline-flex h-6 min-w-0 items-center gap-1 border border-dialog-edge px-2 font-mono text-chip transition-[background-color,color,border-color] hover:bg-hover ${copied ? "border-ok text-ok" : "text-dialog-hint"} ${className}`}
    >
      <span
        aria-hidden="true"
        className="opacity-50 transition-opacity group-hover:opacity-100"
      >
        #
      </span>
      <span className="truncate">{copied ? "Copied" : short}</span>
    </button>
  );
}

/**
 * The live bubble to paint on the FIRST frame of a session, from memory.
 *
 * Re-entering a streaming session used to start with `liveTurn = null`: the
 * screen painted the previous turn's ending, and the in-flight answer only
 * reappeared once the hub replayed its buffer or `adoptRunningTurn` finished a
 * round trip. The bubble is cached on every delta (see the effect that calls
 * `rememberLiveTurn`), so re-entry can start exactly where it left off and take
 * only NEW frames on top.
 *
 * Two guards, because a wrong seed renders an answer twice: the remembered turn
 * must still be `running`, and the hub must not have seen a terminal frame for
 * this session while the screen was away (that answer is already a settled
 * transcript row).
 */
function seedLiveTurn(
  client: GatewayClient,
  subscriptions: SessionSubscriptionHub,
  sid: string,
): { turn: LiveTurn; seq: number } | null {
  const cached = client.cachedLiveTurn<LiveTurn>(sid);
  if (!cached || cached.turn.status !== "running") return null;
  if (subscriptions.hasEndedTurn(sid)) return null;
  return cached;
}

function PasteEditor({
  editingPaste,
  onDraftChange,
  onClose,
  onSave,
}: {
  editingPaste: { id: number; draft: string };
  onDraftChange: (draft: string) => void;
  onClose: () => void;
  onSave: () => void;
}) {
  // `--safe-bottom` rides in on this element instead of the document root: it
  // changes with every keyboard movement, and a root-scoped custom property is
  // a whole-document style recalculation (see `useSafeBottomStyle`).
  const safeBottomStyle = useSafeBottomStyle();
  // This overlay lives inside SessionScreen's positioned root, so it follows
  // the app shell without creating its own fixed WebKit layer.
  return (
    <div
      className="absolute inset-0 z-50 flex h-full items-stretch justify-center bg-ink/85 p-0 pl-[env(safe-area-inset-left)] pr-[env(safe-area-inset-right)] backdrop-blur-[2px] transition-opacity duration-200 starting:opacity-0 motion-reduce:transition-none sm:items-center sm:p-5"
      onMouseDown={(event) => {
        if (event.target === event.currentTarget) onClose();
      }}
    >
      <section
        className="flex h-full w-full max-w-3xl flex-col overflow-hidden border-dialog-edge bg-panel shadow-none transition-[opacity,transform,translate,scale,rotate] duration-200 starting:translate-y-6 starting:opacity-0 motion-reduce:transition-none sm:h-[70%] sm:max-h-[calc(100%-2rem)] sm:border sm:shadow-[8px_8px_0_var(--dialog-shadow)] sm:starting:translate-y-2"
        role="dialog"
        aria-modal="true"
        aria-labelledby="paste-editor-title"
        onKeyDown={(event) => {
          if (event.key === "Escape") {
            event.stopPropagation();
            onClose();
          } else if (
            event.key === "Enter" &&
            (event.metaKey || event.ctrlKey)
          ) {
            event.preventDefault();
            onSave();
          }
        }}
      >
        <header className="flex min-h-12 shrink-0 items-center bg-dialog-title pt-[env(safe-area-inset-top)] text-dialog-title-foreground sm:pt-0">
          <div className="min-w-0 flex-1 px-3 py-2 sm:px-4">
            <h2
              id="paste-editor-title"
              className="truncate font-mono text-body font-bold tracking-wide"
            >
              {`Pasted #${editingPaste.id}`}
            </h2>
            <p className="truncate font-mono text-meta text-dialog-title-foreground/70">
              {pasteSummary(editingPaste.id, editingPaste.draft)}
            </p>
          </div>
          <button
            type="button"
            className="grid min-h-10 min-w-10 place-items-center border-l border-dialog-title-foreground/20 text-dialog-title-foreground/70 transition-colors hover:bg-err/15 hover:text-err focus-visible:bg-err/15 focus-visible:text-err focus-visible:outline-none"
            onMouseDown={keepKeyboard}
            onClick={onClose}
            aria-label="Close paste editor"
          >
            <CloseIcon />
          </button>
        </header>

        <textarea
          // eslint-disable-next-line jsx-a11y/no-autofocus
          autoFocus
          value={editingPaste.draft}
          onChange={(event) => onDraftChange(event.target.value)}
          spellCheck={false}
          autoCapitalize="off"
          autoCorrect="off"
          className="min-h-0 flex-1 resize-none touch-pan-y overflow-y-auto overscroll-contain border-t border-dialog-edge bg-input p-3 font-mono text-body text-dialog-foreground outline-none sm:p-4"
          aria-label={`Content of pasted block ${editingPaste.id}`}
        />

        <footer
          style={safeBottomStyle}
          className="flex shrink-0 items-center justify-end gap-2 border-t border-dialog-edge bg-panel-2 px-3 py-2 pb-[max(0.5rem,var(--safe-bottom,env(safe-area-inset-bottom)))] font-mono text-meta text-dialog-hint sm:px-4"
        >
          <span className="mr-auto hidden truncate sm:block">
            Esc cancels · ⌘↵ saves
          </span>
          <button
            type="button"
            className="min-h-9 border border-dialog-edge px-3 text-ui text-dialog-hint transition-colors hover:bg-warn-surface hover:text-err focus-visible:outline-none"
            onMouseDown={keepKeyboard}
            onClick={onClose}
          >
            Cancel
          </button>
          <button
            type="button"
            className="min-h-9 border border-accent bg-accent px-3 text-ui text-accent-foreground transition-colors hover:bg-accent/85 focus-visible:outline-none"
            onMouseDown={keepKeyboard}
            onClick={onSave}
          >
            Save
          </button>
        </footer>
      </section>
    </div>
  );
}

export function SessionScreen({
  client,
  subscriptions,
  sid,
  onBack,
  onOpenSession,
  onManageProviders,
  fresh = false,
}: {
  client: GatewayClient;
  subscriptions: SessionSubscriptionHub;
  sid: string;
  onBack: () => void;
  onOpenSession: (sid: string, fresh?: boolean) => void;
  /** Open this gateway's settings, where provider accounts and OAuth live. */
  onManageProviders?: () => void;
  fresh?: boolean;
}) {
  // The device rotates freely here. A flip is survived rather than forbidden:
  // `lib/viewport.ts` holds a rotation window open for the whole animation, and
  // every measurement in this screen (the `ResizeObserver` below, the thinking
  // bands in `ChatContent`) simply stops running inside it — the intermediate
  // widths are transitional and every answer taken from them is thrown away.
  // Every screen-level snapshot is seeded from the client's cache: reopening a
  // session paints its last known transcript on the FIRST frame and revalidates
  // underneath, instead of holding the loading sheet over an empty view.
  const [session, setSession] = useState<Session | null>(() =>
    client.cachedSession(sid),
  );
  const [turns, setTurns] = useState<TranscriptTurn[]>(
    () => client.cachedTranscript(sid) ?? [],
  );
  // Whether the turns on screen were confirmed against the gateway during THIS
  // visit. Cached rows paint the first frame, but a cached 'running' row is a
  // placeholder with no outcome: rendered before confirmation it spins and
  // counts elapsed time for a turn that may already be cancelled or done.
  const [turnsFresh, setTurnsFresh] = useState(false);
  // The composer holds a DRAFT MESSAGE, not a keystroke buffer: unsent text
  // survives leaving for the session list, backgrounding, and killing the app.
  // (A "draft" in this system is an isolated agent workspace — different thing.)
  // The seed is synchronous off the in-memory mirror so a revisit paints what you
  // typed on the FIRST frame; the effect below covers the cold start, where
  // storage still has to be read.
  const draftMessageId = draftMessageKey(client.base, sid);
  // Dictated audio waiting for a transcript is filed under the SAME identity as
  // the draft message, because that is what it becomes: unsent words belonging
  // to this gateway's copy of this session.
  const voiceMailboxId = draftMessageId;
  // The composer footer carries `--safe-bottom` itself; see `useSafeBottomStyle`.
  const safeBottomStyle = useSafeBottomStyle();
  const [prompt, setPrompt] = useState(
    () => peekDraftMessage(draftMessageId).text,
  );
  const [draftMessageReady, setDraftMessageReady] = useState(false);
  // Same fact, readable SYNCHRONOUSLY: the effects below run in declaration
  // order inside ONE commit, so a session switch reaches the recording effect
  // while `draftMessageReady` still holds the previous render's `true`. The ref
  // flips with the switch itself, which is what keeps the old composer text from
  // being written under the new session's key.
  const draftMessageReadyRef = useRef(false);
  const [error, setError] = useState<string | null>(null);
  // The session's provider/model pick. Seeded from the client's snapshot so the
  // header chip names the model on the FIRST frame instead of reading "model"
  // until the gateway answers, then written through by the router dialog.
  const [modelPref, setModelPref] = useState<ModelPref | null>(() =>
    client.cachedSessionModel(sid),
  );
  // The gateway's default route, shown when this session pins nothing. Same
  // seed: resolving it costs a `/v1/router` probe on a cold daemon.
  const [defaultPref, setDefaultPref] = useState<ModelPref | null>(() =>
    client.cachedDefaultModel(),
  );
  const [routerOpen, setRouterOpen] = useState(false);
  const [loading, setLoading] = useState(!fresh);
  // The veil outlives `loading` by one transition so it can dissolve.
  const [veiled, setVeiled] = useState(!fresh);
  const [connected, setConnected] = useState(false);
  // The bubble this screen re-enters with, resolved ONCE at mount.
  const [liveSeed] = useState(() => seedLiveTurn(client, subscriptions, sid));
  const [running, setRunning] = useState(liveSeed !== null);
  const [liveTurn, setLiveTurn] = useState<LiveTurn | null>(
    liveSeed?.turn ?? null,
  );
  const [queued, setQueued] = useState<QueuedTurn[]>(
    () => client.cachedQueuedTurns(sid) ?? [],
  );
  // Turn ids with a queue mutation in flight. The gateway is the ONE writer of the
  // queue tray (rows appear on `turn.queued` and leave on `.updated`/`.deleted`/
  // `.drained`), so an edit or removal is NOT applied optimistically — it is
  // marked busy until the daemon's own event lands. Mirroring the intent locally
  // is exactly how a row could disappear while the gateway still ran it.
  const [queueBusy, setQueueBusy] = useState<ReadonlySet<string>>(
    () => new Set(),
  );
  const [editingQueued, setEditingQueued] = useState<{
    turnId: string;
    text: string;
  } | null>(null);
  // Queue truth arrives on TWO streams that can cross: live `turn.queued*` frames
  // and the `?status=queued` re-reads done on open and on every wake tick. The
  // removals exist only on the live stream (the gateway appends
  // `turn.queued.drained`/`.deleted` with `:store? false`), so a read that left
  // the gateway BEFORE the head drained answers with a row the drain frame has
  // already cleared here — and, landing later, puts it back for good. That is the
  // tray showing "Queued · 1" for the very message whose answer is streaming
  // above it. Every live delta is stamped, and a backlog read is trusted only for
  // rows whose last delta is OLDER than the read itself.
  const queueDeltasRef = useRef(new Map<string, QueueDelta>());
  const noteQueueDelta = useCallback(
    (tid: string, row: QueuedTurn | null) => {
      if (!tid) return;
      queueDeltasRef.current.set(tid, { at: Date.now(), row });
      if (!row) client.forgetQueuedTurn(sid, tid);
    },
    [client, sid],
  );
  const acceptQueueBacklog = useCallback(
    (rows: QueuedTurn[], readStartedAt: number) => {
      const merged = mergeQueueBacklog(
        rows,
        queueDeltasRef.current,
        readStartedAt,
      );
      // The read has just rewritten the snapshot with the stale rows in it.
      for (const tid of merged.forget) client.forgetQueuedTurn(sid, tid);
      setQueued(merged.rows);
    },
    [client, sid],
  );
  const [queuePaused, setQueuePaused] = useState<QueuePausedInfo | null>(null);
  // The pause banner is gateway state too: it clears on `queue.resumed`, never
  // because we asked. This only disables the button while the request is out.
  const [resumingQueue, setResumingQueue] = useState(false);
  const [showJump, setShowJump] = useState(false);
  const [visibleTurnCount, setVisibleTurnCount] = useState(
    INITIAL_VISIBLE_TURNS,
  );
  // How much of that window is actually mounted right now. Ramps to
  // `visibleTurnCount` off the critical path; never shrinks the window itself,
  // so the "load earlier" affordance and its counts stay stable while it fills.
  const [hydratedTurnCount, setHydratedTurnCount] = useState(FIRST_PAINT_TURNS);
  // Reading IS being here. While this transcript is on screen its FINISHED turns
  // count as read, so an answer that lands while you watch never raises a badge
  // in the session list — but one that lands with the screen backgrounded does.
  // Both halves must count answers only: `turn_count` includes the turn that is
  // running right now (the gateway persists it at submit), and marking that as
  // read would pre-read the answer before it exists.
  //
  // The terminal event settles the live bubble before its transcript row has been
  // persisted. That visible bubble is already read, so count it during that gap;
  // `settle` batches the persisted row and bubble removal into one render, which
  // keeps the same answer from being counted twice.
  // This can be a much larger held history than the eight mounted rows after the
  // reader pages upward. Keep the completed-turn scan off the composer render path:
  // keystrokes do not change `turns`, and the read marker only needs to move when
  // the transcript, its meta row, or the live bubble changes.
  const readTurns = useMemo(
    () => visibleAnsweredTurnCount(session, turns, liveTurn?.status),
    [session, turns, liveTurn],
  );
  useEffect(() => {
    if (document.visibilityState !== "hidden") markSessionRead(sid, readTurns);
    // Coming back to a screen that stayed mounted through a suspend is also a read.
    return onWake(() => markSessionRead(sid, readTurns));
  }, [sid, readTurns]);
  // Turns that exist on the gateway BEFORE the window we hold. The transcript is
  // fetched newest-page-first (a long session is tens of megabytes whole), so
  // "earlier" can mean rows we have but hide, or rows we have not read yet.
  const [earlierRemaining, setEarlierRemaining] = useState(
    () => client.transcriptWindow(sid).offset,
  );
  const [artifactsOpen, setArtifactsOpen] = useState(false);
  // Everything this session PRODUCED, flattened out of the turns that made it,
  // then collapsed so a NAME is one artifact with a version history rather than
  // one row per file written. The transcript paints each artifact where it was
  // made, which is right and is also why "show me that chart again" is otherwise
  // a scroll hunt.
  const artifacts = useMemo(
    () => collapseArtifactVersions(collectArtifacts(turns, earlierRemaining)),
    [turns, earlierRemaining],
  );
  const [loadingEarlier, setLoadingEarlier] = useState(false);
  const [slashCommands, setSlashCommands] =
    useState<SlashCommand[]>(FALLBACK_SLASHES);
  const [slashIndex, setSlashIndex] = useState(0);
  const [slashDismissed, setSlashDismissed] = useState(false);
  const [caret, setCaret] = useState(0);
  const [fileSuggestions, setFileSuggestions] = useState<FileSuggestion[]>([]);
  const [fileIndex, setFileIndex] = useState(0);
  const [fileDismissed, setFileDismissed] = useState(false);
  // Capabilities are a per-GATEWAY fact (attachment limits, media types, whether
  // voice exists at all) that the composer is built out of. Starting from `null`
  // meant every re-entry painted a composer whose `+` and microphone only
  // appeared once the round-trip landed — the icons visibly popping in on a
  // screen that otherwise restored instantly. Seed from the last payload this
  // gateway answered: same paint-then-revalidate rule as session/transcript.
  const [capabilities, setCapabilities] = useState<GatewayCapabilities | null>(
    () => client.cachedCapabilities(),
  );
  // Staged files are part of the unsent message, not a side effect of the screen:
  // they are seeded from the stored draft message on the FIRST frame, exactly like
  // its text, so leaving and reopening a session does not eat the picture.
  const [attachments, setAttachments] = useState<PendingAttachment[]>(() => [
    ...peekDraftMessage(draftMessageId).attachments,
  ]);
  // Native hides two distinct acts behind one composer control, so the plus has
  // to ask which one: the OS gallery sheet never opens a shutter.
  const [attachMenuOpen, setAttachMenuOpen] = useState(false);
  const [pastes, setPastes] = useState<Map<number, ComposerPaste>>(
    () =>
      new Map(
        peekDraftMessage(draftMessageId).pastes.map((paste) => [
          paste.id,
          paste,
        ]),
      ),
  );
  // A paste chip is a HANDLE on its payload, not a tombstone: tapping it opens the
  // content for editing (the same affordance other agent composers give a collapsed
  // paste). `draft` is the live textarea buffer — `pastes`/`prompt` only move on Save.
  const [editingPaste, setEditingPaste] = useState<{
    id: number;
    draft: string;
  } | null>(null);
  // The paste editor is absolute inside this screen's positioned root, so it
  // inherits the app shell's keyboard pin without a second viewport listener or
  // a lagging fixed WebKit layer.
  const [composerNotice, setComposerNotice] = useState<string | null>(null);
  // Seeded from the same cached capabilities as the composer above, so the mic
  // button is there on the first frame instead of arriving a round-trip late.
  const [voiceSupported, setVoiceSupported] = useState(
    () => client.cachedCapabilities()?.features.voice.enabled ?? false,
  );
  const [voiceModel, setVoiceModel] = useState<VoiceModelState | null>(
    () => client.cachedCapabilities()?.features.voice.model ?? null,
  );
  const [voicePhase, setVoicePhase] = useState<
    "idle" | "recording" | "transcribing"
  >("idle");
  // Where the transcription IS, straight from the gateway job. Null means the
  // engine has not reported yet (the bytes are still leaving this phone).
  const [voiceProgress, setVoiceProgress] = useState<VoiceProgress | null>(
    null,
  );
  const [voiceRequested, setVoiceRequested] = useState(false);
  const scrollRef = useRef<HTMLDivElement>(null);
  const transcriptRef = useRef<HTMLDivElement>(null);
  const composerRef = useRef<HTMLTextAreaElement>(null);
  const fileInputRef = useRef<HTMLInputElement>(null);
  const recordingRef = useRef<WavRecording | null>(null);
  const pasteCounterRef = useRef(peekDraftMessage(draftMessageId).counter);
  const resizeScrollFrameRef = useRef<number | null>(null);
  const disclosureScrollFrameRef = useRef<number | null>(null);
  const scrollMetricsFrameRef = useRef<number | null>(null);
  // A rotation has one terminal scroll write: keep observers and stale scroll
  // events from competing with that write until it has landed.
  const rotationRestorePendingRef = useRef(false);
  // The SSE effect installs its current queue drain here. Rotation invokes it one
  // paint before restoring the captured scroll anchor, so the restore targets the
  // final streamed transcript rather than the pre-rotation body.
  const flushLiveEventsBeforeRotationRestoreRef = useRef<(() => void) | null>(
    null,
  );
  // Last measured height of the scroller itself, so a box that shrinks under a
  // parked reader can hand the lost pixels back (see the ResizeObserver below).
  const viewportHeightRef = useRef<number | null>(null);
  // The shell (window / visual viewport) height, so a scroller resize can be told
  // apart from a keyboard or rotation: same shell, different scroller = the
  // composer autosized.
  const shellHeightRef = useRef<number | null>(null);
  // Dictation that was already captured but not yet turned into text. A
  // backgrounded webview loses the network mid-request, and dropping the blob
  // there would throw away a finished sentence.
  const pendingVoiceRef = useRef<Blob | null>(null);
  // Scroll ownership is explicit: a pin is one correction, never a timed fight with
  // the browser's layout or the reader's finger.
  const followingRef = useRef(true);
  // A wake after a real absence owes the reader the newest turn, not the pixel
  // they left on. The wake pins immediately, but the catch-up transcript lands
  // one round trip later and grows the scroller under them — so the intent has
  // to survive until that data arrives.
  const resumePinRef = useRef(false);
  const showJumpRef = useRef(false);
  const liveTurnRef = useRef<LiveTurn | null>(liveSeed?.turn ?? null);
  // Journal cursor of the newest event already folded into the live bubble. The
  // hub replays a still-streaming turn from its `turn.started` on every
  // (re)subscribe; when the bubble was seeded from cache those frames are
  // ALREADY in it, and re-applying them would reset it to empty and re-append
  // the same prose. Anything at or below this seq is dropped.
  const lastLiveSeqRef = useRef(liveSeed?.seq ?? -1);
  // The sid whose bubble the cache effect last wrote, so a session switch never
  // stores the outgoing bubble under the incoming id.
  const liveSidRef = useRef(sid);
  // turn id -> what THIS device authored for a message it queued behind the running
  // turn. A user cancel drops the whole pre-cancel backlog server-side
  // (`drop-cancelled-backlog!`), so without a local copy the text, its pastes and its
  // images would simply be gone. Keyed by turn id, so rows queued from the TUI or
  // another device are absent here and stay THEIR editor's business.
  const authoredQueueRef = useRef<
    Map<
      string,
      {
        request: string;
        attachments: PendingAttachment[];
        pastes: Map<number, ComposerPaste>;
      }
    >
  >(new Map());
  const rememberQueued = (
    turnId: string | undefined,
    authored: {
      request: string;
      attachments: PendingAttachment[];
      pastes: Map<number, ComposerPaste>;
    },
  ) => {
    if (!turnId) return;
    const map = authoredQueueRef.current;
    map.set(turnId, authored);
    // Bounded like `sentAttachmentsRef`: a backlog this long is not coming back.
    while (map.size > 16) {
      const oldest = map.keys().next();
      if (oldest.done) break;
      map.delete(oldest.value);
    }
  };
  // Turn ids whose text has already been pulled back into the composer by a cancel.
  // A user cancel drops the pre-cancel backlog server-side and broadcasts one
  // `turn.queued.deleted` (reason `cancelled`) per row, while `cancel()` on THIS
  // device also restores its own tray straight away — this set is what stops the
  // same words landing in the box twice.
  const restoredQueueRef = useRef<Set<string>>(new Set());
  // Stop means stop, but the words the user already wrote are theirs. Every row a
  // cancel dropped comes back into the composer draft — whoever queued it, and
  // whichever channel pressed stop — appended after whatever is already typed and
  // never auto-sent. The draft is persisted per session, so leaving the screen (or
  // the app) does not lose it. Same contract the TUI honours in `:sync-queued-turn`
  // / `:restore-pending-to-input`.
  const restoreCancelledQueued = useCallback(
    (turnId: string | undefined, request: string) => {
      if (!turnId) return;
      const done = restoredQueueRef.current;
      if (done.has(turnId)) return;
      done.add(turnId);
      while (done.size > 64) {
        const oldest = done.values().next();
        if (oldest.done) break;
        done.delete(oldest.value);
      }
      // What THIS device authored wins over the wire text: only the local copy still
      // has the pastes and the image bytes that never travel on the queue mirror.
      const authored = authoredQueueRef.current.get(turnId);
      authoredQueueRef.current.delete(turnId);
      const text = (authored?.request || request || "").trim();
      if (text) {
        setPrompt((current) =>
          [current.trimEnd(), text].filter(Boolean).join("\n\n"),
        );
      }
      if (authored?.pastes.size) {
        setPastes((current) => {
          const next = new Map(current);
          for (const [id, paste] of authored.pastes)
            if (!next.has(id)) next.set(id, paste);
          return next;
        });
      }
      if (authored?.attachments.length) {
        setAttachments((current) => {
          const seen = new Set(current.map((item) => item.id));
          return [
            ...current,
            ...authored.attachments.filter((item) => !seen.has(item.id)),
          ];
        });
      }
    },
    [],
  );
  // The bytes are kept on the CLIENT, not in this screen: leaving the session
  // unmounts `SessionScreen`, and a live turn whose images lived only in screen
  // state came back text-only until the persisted row landed on top.
  const rememberSent = (
    turnId: string | undefined,
    sent: GatewayAttachment[],
  ) => {
    client.rememberSentAttachments(sid, turnId, sent);
  };
  const runningRef = useRef(false);
  const turnsRef = useRef<TranscriptTurn[]>([]);
  // Ids of every transcript row that existed BEFORE the current live turn — the
  // baseline `liveTurnSettledInto` measures "a new settled row landed" against.
  // Frozen for as long as a live bubble is on screen (see the mirror effect).
  //
  // Seeded here rather than left empty: when the screen re-enters a streaming
  // session the bubble exists on the very first render, so the mirror effect
  // never gets a chance to take the baseline and every cached row would count as
  // "landed after this turn started".
  const [preLiveTurnIdsSeed] = useState(
    () =>
      new Set(
        (client.cachedTranscript(sid) ?? [])
          .filter((turn) => !isRunningRow(turn))
          .map(rowId),
      ),
  );
  const preLiveTurnIdsRef = useRef<Set<string>>(preLiveTurnIdsSeed);
  const cancelRef = useRef<() => void>(() => undefined);
  // Keep the loading overlay up until a freshly opened session has been
  // scrolled to its bottom, so persisted history never flashes at the top first.
  const initialScrollPendingRef = useRef(!fresh);
  // Mirror the latest render values for async callbacks. Written in an effect so
  // render itself stays pure.
  useEffect(() => {
    runningRef.current = running;
    turnsRef.current = turns;
    // No live bubble ⇒ whatever is on screen IS the baseline. Once one appears
    // this stops updating, so the snapshot describes the transcript as it was
    // when the turn started.
    //
    // STILL-RUNNING rows are excluded on purpose. The gateway persists a
    // `running` placeholder for a turn the moment it is accepted, so a refetch
    // that lands between the POST and `turn.started` puts THIS turn's row into
    // the baseline. `liveTurnSettledInto` then rejects that very row as "already
    // there" once it settles, the live bubble is never retired, and the answer
    // renders twice — settled row plus bubble — until the screen is remounted.
    if (!liveTurnRef.current) {
      preLiveTurnIdsRef.current = new Set(
        turns.filter((turn) => !isRunningRow(turn)).map(rowId),
      );
    }
    cancelRef.current = () => void cancel();
  });

  // Remember the live bubble so LEAVING and RE-ENTERING this session repaints it
  // instantly instead of showing the previous turn's ending until a replay or a
  // refetch lands. Memory only (see `rememberLiveTurn`), keyed with the journal
  // cursor already folded in so the replay that follows resubscription can be
  // dropped rather than applied twice.
  //
  // The guard matters on a session switch: this effect runs with the NEW sid
  // while `liveTurn` still holds the OUTGOING session's bubble — the reset effect
  // below is what re-points both, and it sets `liveSidRef` when it does.
  useEffect(() => {
    if (liveSidRef.current !== sid) return;
    client.rememberLiveTurn(sid, liveTurn, lastLiveSeqRef.current);
  }, [client, sid, liveTurn]);

  // Switching session identity resets this screen's whole view state. React's
  // alternative is remounting via `key`, which would also tear down the live SSE
  // subscription mid-stream, so the reset stays explicit here.
  useEffect(() => {
    void recordingRef.current?.cancel();
    recordingRef.current = null;
    pendingVoiceRef.current = null;
    setTurns(client.cachedTranscript(sid) ?? []);
    setTurnsFresh(false);
    setEarlierRemaining(client.transcriptWindow(sid).offset);
    setLoadingEarlier(false);
    // Re-entering a session that is STILL streaming keeps its bubble: dropping it
    // here is what made the reader land on the previous turn's ending and wait for
    // a replay before the in-flight answer came back.
    const seed = seedLiveTurn(client, subscriptions, sid);
    setLiveTurn(seed?.turn ?? null);
    setRunning(seed !== null);
    setQueued(client.cachedQueuedTurns(sid) ?? []);
    setQueueBusy(new Set());
    setQueuePaused(null);
    liveTurnRef.current = seed?.turn ?? null;
    lastLiveSeqRef.current = seed?.seq ?? -1;
    liveSidRef.current = sid;
    preLiveTurnIdsRef.current = new Set(
      (client.cachedTranscript(sid) ?? [])
        .filter((turn) => !isRunningRow(turn))
        .map(rowId),
    );
    setSession(client.cachedSession(sid));
    setAttachments([...peekDraftMessage(draftMessageId).attachments]);
    setPastes(new Map());
    setEditingPaste(null);
    pasteCounterRef.current = 0;
    // The composer belongs to ONE session. Everything else here is reset per sid;
    // leaving the prompt behind meant the text typed for the previous session
    // stayed in the box, got re-recorded under THIS session's draft-message key,
    // and then sent to the wrong session.
    draftMessageReadyRef.current = false;
    setDraftMessageReady(false);
    setPrompt(peekDraftMessage(draftMessageId).text);
    setComposerNotice(null);
    setVoicePhase("idle");
    setVoiceRequested(false);
    setLoading(!fresh);
    setVeiled(!fresh);
    setVisibleTurnCount(INITIAL_VISIBLE_TURNS);
    setHydratedTurnCount(FIRST_PAINT_TURNS);
    followingRef.current = true;
    initialScrollPendingRef.current = !fresh;
    showJumpRef.current = false;
    setRouterOpen(false);
    // Switching sessions swaps the pin, so paint the NEW session's last known
    // one rather than blanking the chip back to the placeholder word.
    setModelPref(client.cachedSessionModel(sid));
  }, [sid, fresh, draftMessageId]);

  // Reasoning effort is the other per-turn dial you change mid-session, so it
  // rides the composer footer right next to the model chip — it is the ONLY
  // control for it (the model picker deliberately has none). The gateway keeps
  // it out of `/v1/settings` (`:settings? false`) because each channel owns its
  // own control, hence the by-id read.
  const [reasoning, setReasoning] = useState<Toggle | null>(() =>
    client.cachedSetting("reasoning_level"),
  );
  const [reasoningBusy, setReasoningBusy] = useState(false);
  // The level the user just asked for, shown until the gateway confirms it.
  const [pendingLevel, setPendingLevel] = useState<string | null>(null);

  useEffect(() => {
    const controller = new AbortController();
    void client
      .setting("reasoning_level", controller.signal)
      .then((toggle) => setReasoning(toggle))
      .catch(() => {
        // Optional knob: a gateway without it simply paints no chip.
      });
    return () => controller.abort();
  }, [client]);

  // One tap = next choice. Cycling beats a popover for a two-to-four value
  // enum, and the gateway owns the order (`cycle` action) — but it also HANDS
  // that order over in `choices`, so the next word is known locally and the
  // chip can show it on the tap instead of after the round-trip.
  function nextReasoningLevel(toggle: Toggle): string | null {
    const choices = toggle.choices ?? [];
    if (choices.length < 2) return null;
    const at = toggle.value ? choices.indexOf(toggle.value) : -1;
    return choices[(at + 1) % choices.length] ?? null;
  }

  async function cycleReasoning() {
    if (!reasoning || reasoningBusy) return;
    // Optimistic: the guess is only ever a guess, and the gateway's answer
    // below overwrites it — a disagreement re-keys the word and simply plays
    // the swap a second time.
    setPendingLevel(nextReasoningLevel(reasoning));
    setReasoningBusy(true);
    try {
      setReasoning(await client.setSetting(reasoning.id, "cycle"));
    } catch (e) {
      setComposerNotice((e as Error).message);
    } finally {
      setPendingLevel(null);
      setReasoningBusy(false);
    }
  }

  // What the chip SAYS: the optimistic pick while the write is in flight, the
  // gateway's own value the rest of the time. Never empty — that is the whole
  // point of the swap.
  const reasoningLevel = pendingLevel ?? reasoning?.value ?? "default";

  // The header chip shows whatever model this session actually runs on, so read
  // the gateway's answer rather than assuming the global default.
  useEffect(() => {
    let live = true;
    void client
      .sessionModel(sid)
      .then((pref) => {
        if (live) setModelPref(pref);
      })
      .catch(() => {
        /* A missing pick is not an error worth interrupting the transcript for. */
      });
    return () => {
      live = false;
    };
  }, [client, sid]);

  // An unpinned session runs on the gateway default, so the chip names THAT
  // model rather than the placeholder word. Re-read when the picker closes: the
  // default may have just been changed from it.
  useEffect(() => {
    let live = true;
    void client
      .defaultModel()
      .then((pref) => {
        if (live) setDefaultPref(pref);
      })
      .catch(() => {
        /* Without a readable default the chip simply falls back to the pin. */
      });
    return () => {
      live = false;
    };
  }, [client, routerOpen]);

  const markQueueBusy = useCallback((turnId: string, busy: boolean) => {
    setQueueBusy((current) => {
      const next = new Set(current);
      if (busy) {
        next.add(turnId);
      } else {
        next.delete(turnId);
      }
      return next;
    });
  }, []);

  const scrollToEnd = useCallback((behavior: ScrollBehavior = "auto") => {
    const viewport = scrollRef.current;
    if (!viewport) return;
    viewport.scrollTo({ top: viewport.scrollHeight, behavior });
    followingRef.current = true;
    if (showJumpRef.current) {
      showJumpRef.current = false;
      setShowJump(false);
    }
  }, []);

  // Opening a session must LAND on the latest turn. The opening layout effect waits
  // until the mounted window is complete, so this is one correction against the
  // complete initial DOM rather than a visible sequence of guesses as rows hydrate.
  const pinToEnd = useCallback(() => {
    scrollToEnd("auto");
  }, [scrollToEnd]);

  // A real gesture never fights the reader; the handlers remain attached to claim the
  // gesture before any future correction can be introduced.
  const releasePin = useCallback(() => {}, []);

  // Rotation reflows the whole transcript: every wrapped line changes width, so
  // the pixel `scrollTop` you were reading at stops pointing at the same turn
  // and the reader lands somewhere else entirely. The browser's own scroll
  // anchoring cannot rescue it — this scroller runs `[overflow-anchor:none]` so
  // that loading earlier turns stays stable — so anchor it ourselves: remember
  // the turn at the top edge before the flip and put it back after, or simply
  // stay pinned to the bottom when that is where the reader was.
  const scrollAnchorRef = useRef<ScrollAnchor | null>(null);
  // Where the corrector below last left `scrollTop`.
  const correctedTopRef = useRef(-1);

  const captureScrollAnchor = useCallback(() => {
    const viewport = scrollRef.current;
    const transcript = transcriptRef.current;
    if (!viewport || !transcript) return;
    // A scroller sitting exactly where the corrector left it means nothing the
    // reader did moved it: every scroll event since was the echo of a
    // correction, and re-reading the fold there re-anchors onto history that
    // landed above the reader in the same frame — React commits the next chunk
    // from a task, before the frame this would run in, so that push is never
    // billed. Measured, it leaked 14 477 px of a 33 425 px "↑ Load earlier".
    if (
      scrollAnchorRef.current?.el.isConnected &&
      Math.abs(viewport.scrollTop - correctedTopRef.current) < 1
    ) {
      return;
    }
    // Following the live turn needs no anchor: the bottom IS the anchor.
    scrollAnchorRef.current = followingRef.current
      ? null
      : scrollAnchorFor(viewport, transcript);
  }, []);

  const restoreScrollAnchor = useCallback(() => {
    const viewport = scrollRef.current;
    if (!viewport) return;
    if (applyScrollAnchor(viewport, scrollAnchorRef.current)) return;
    if (followingRef.current) scrollToEnd("auto");
  }, [scrollToEnd]);

  // THE one owner of "the reader keeps their line". Rows land above the fold from
  // three directions — history prepends, the backfill that refills the render
  // window, and traces ramping their segments — and each corrector that
  // compensated its own growth billed the same frame again: measured, a
  // 39 730 px "↑ Load earlier" walked the scroller 59 910 px. A content-box
  // observer sees every one of those mutations in the frame it lands, before
  // paint, and re-seating an anchored child is idempotent — whoever ran first
  // fixed the frame and the next entry measures zero.
  useEffect(() => {
    const viewport = scrollRef.current;
    const transcript = transcriptRef.current;
    if (!viewport || !transcript || typeof ResizeObserver === "undefined")
      return;
    let frame: number | null = null;
    // `captureScrollAnchor` already ignores the echo of our own corrections.
    // Rotation is its own transaction and owns the anchor for its duration.
    const busy = () =>
      isViewportRotating() || rotationRestorePendingRef.current;
    const recapture = () => {
      frame = null;
      if (!busy()) captureScrollAnchor();
    };
    const handleViewportScroll = () => {
      if (frame === null) frame = window.requestAnimationFrame(recapture);
    };
    const observer = new ResizeObserver(() => {
      if (busy()) return;
      // The end is its own anchor, and a hand on the glass owns the scroller: in
      // The end is its own anchor, and a hand on the glass owns the scroller: in
      // both cases the reader's line is wherever they just put it, so re-read it.
      if (followingRef.current || readerOwnsScroll()) {
        captureScrollAnchor();
        return;
      }
      // Otherwise hold the line the reader chose. `captureScrollAnchor` refuses
      // to re-read the fold while the scroller sits where this left it, so the
      // anchor survives the whole growth window and every pixel that lands
      // above it is billed exactly once: measured, a 33 417 px "↑ Load earlier"
      // moved the scroller 33 416 px and the reader's turn 1 px.
      if (!applyScrollAnchor(viewport, scrollAnchorRef.current))
        captureScrollAnchor();
      correctedTopRef.current = viewport.scrollTop;
    });
    observer.observe(transcript);
    viewport.addEventListener("scroll", handleViewportScroll, {
      passive: true,
    });
    recapture();
    return () => {
      observer.disconnect();
      viewport.removeEventListener("scroll", handleViewportScroll);
      if (frame !== null) window.cancelAnimationFrame(frame);
    };
  }, [captureScrollAnchor]);

  // Rotation is one transaction: snapshot before intermediate reflows, then wait
  // two paint frames after the final viewport measurement before restoring once.
  // `settle` is intentionally ignored; it only says the OS geometry is still in
  // motion and replaying an anchor there makes the transcript jump.
  useEffect(() => {
    let firstFrame: number | null = null;
    let finalFrame: number | null = null;
    const stop = onViewportRotation((phase) => {
      if (phase === "start") {
        rotationRestorePendingRef.current = true;
        captureScrollAnchor();
        return;
      }
      if (phase !== "end") return;
      if (firstFrame !== null) window.cancelAnimationFrame(firstFrame);
      if (finalFrame !== null) window.cancelAnimationFrame(finalFrame);
      firstFrame = window.requestAnimationFrame(() => {
        firstFrame = null;
        // Fold any body frames intentionally held during the animation while
        // there is still a full paint left before the one terminal scroll write.
        flushLiveEventsBeforeRotationRestoreRef.current?.();
        finalFrame = window.requestAnimationFrame(() => {
          finalFrame = null;
          restoreScrollAnchor();
          rotationRestorePendingRef.current = false;
        });
      });
    });
    return () => {
      stop();
      if (firstFrame !== null) window.cancelAnimationFrame(firstFrame);
      if (finalFrame !== null) window.cancelAnimationFrame(finalFrame);
      rotationRestorePendingRef.current = false;
    };
  }, [captureScrollAnchor, restoreScrollAnchor]);

  // Pass the session's meta `row` and the transcript is re-read ONLY when that
  // row says a turn was persisted since the copy already on screen — a long
  // session's transcript is tens of megabytes, so a blind re-read is the most
  // expensive thing this screen can do. Called with no row it always refetches.
  const loadTranscript = useCallback(
    async (row?: Session | null) => {
      try {
        const next =
          row === undefined
            ? await client.transcript(sid)
            : await client.transcriptIfMoved(sid, row);
        if (!next) {
          // Unchanged: the gateway just told us the cache IS its current answer.
          setTurnsFresh(true);
          // Unchanged: keep the painted turns, their object identities, and the
          // scroll position exactly as they are.
          if (!turnsRef.current.length) {
            initialScrollPendingRef.current = false;
            setLoading(false);
          }
          return turnsRef.current;
        }
        setTurns(next);
        setTurnsFresh(true);
        setEarlierRemaining(client.transcriptWindow(sid).offset);
        setError(null);
        // With turns present, the scroll effect drops the overlay only after it
        // pins the viewport to the bottom; an empty transcript has nothing to
        // scroll, so reveal it immediately.
        if (!next.length) {
          initialScrollPendingRef.current = false;
          setLoading(false);
        }
        return next;
      } catch (cause) {
        setError((cause as Error).message);
        initialScrollPendingRef.current = false;
        setLoading(false);
        return null;
      }
    },
    [client, sid],
  );

  // ADOPT a turn that was ALREADY RUNNING before this screen started listening.
  //
  // The live bubble is seeded by exactly one frame, `turn.started`, and
  // `reduceLiveEvent` drops every delta while it is null. The hub subscribes
  // LIVE-ONLY, so that frame is never replayed: anyone who was not listening at
  // the instant it was emitted gets a perfectly healthy stream pouring into
  // nothing.
  //
  // That is the "left the app mid-stream, came back, it never resumed" bug. On a
  // long background iOS kills the WKWebView WebContent process (Capacitor
  // ionic-team/capacitor#7810, #7905); the webview reloads the page from scratch
  // on return, so React state — including the live bubble — is gone, while the
  // gateway happily keeps working. Reconnecting the socket cannot fix it, and
  // neither can any transport-level retry: the seed frame is in the past.
  // Opening a session that is already streaming from another client is the same
  // hole, cold.
  //
  // So ask for the state instead of waiting for an event that will not come:
  // the session row says what is running, and the turn trace returns the
  // iterations the gateway has already persisted for it — the same source the
  // TUI resumes from. The bubble comes back with the work done while we were
  // away, and every subsequent delta now has somewhere to land.
  const adoptRunningTurn = useCallback(
    async (
      row: Session | null,
      signal?: AbortSignal,
      rows?: readonly TranscriptTurn[] | null,
    ) => {
      if (!row) return;
      const gatewayLive =
        row.live !== undefined ? row.live : row.status === "running";
      const claimed = row.current_turn_id ?? "";
      // The registry answers FIRST — exact, free, no round trip. But when it says
      // nothing is running, that is not taken as "nothing is running": the
      // transcript is asked instead (see `inFlightRow`). A registry that dropped
      // `:current-turn` under a live turn is precisely the state where the seed
      // frame can never arrive, so refusing to adopt there is what pins the
      // screen to the placeholder, counting minutes, for the rest of the turn.
      const running = inFlightRow(rows ?? turnsRef.current);
      // Identity stays the REGISTRY's id whenever it has one: the terminal SSE
      // frame carries that id and `ownsTerminal` settles this very bubble by
      // matching it. Only a registry that has LOST the turn hands identity to the
      // row — there is no other id left, and the reconcile tick's coverage check
      // retires it when the row finally settles.
      const tid =
        gatewayLive && claimed !== "" ? claimed : running ? rowId(running) : "";
      if (tid === "") return;
      // Either witness is enough, and neither cost a round trip: a turn IS running.
      // Claiming it here rather than after `turnTrace` matters — until `running`
      // is set, `turnsSettled` treats the persisted `running` placeholder as a
      // finished row, so opening a streaming session painted that row with no
      // "Vis is running …" ticker for the whole length of the trace fetch.
      setRunning(true);
      // A bubble that is still streaming OWNS the turn: it holds deltas newer
      // than any persisted trace, so replacing it would visibly rewind it. Only
      // a missing (or already settled) bubble is adopted into.
      const held = liveTurnRef.current;
      if (held && (held.status === "running" || held.id === tid)) return;
      // WHICH id the trace is read under is not the same question as which id this
      // bubble carries. `/turns/:tid/trace` resolves the ENGINE's turn id — the
      // persisted row's — and answers 200 with ZERO iterations for the gateway
      // registry's `current_turn_id` (measured against a live daemon: registry id
      // -> 0, row id -> every iteration so far). Reading by the registry id is why
      // a resumed session adopted an EMPTY bubble and only filled from the next
      // delta onwards, losing everything the turn had already done.
      //
      // With the row in hand there is nothing left to ask: `/transcript` inlines
      // the running row's `iterations`, byte-for-byte what `/turns/<row id>/trace`
      // answers for that id (both measured on a live daemon). The endpoint stays
      // for the two cases that have no row — a registry claim the transcript has
      // not caught up with, and a daemon too old to inline. An EMPTY inline trace
      // is an answer, not a miss (a row whose first iteration has not persisted
      // yet), so a missing key is the only thing that may fall through to it.
      const inlineTrace = running?.iterations;
      let iterations: TranscriptIteration[];
      if (inlineTrace != null) {
        iterations = inlineTrace;
      } else {
        try {
          iterations = await client.turnTrace(
            sid,
            running ? rowId(running) : tid,
            signal,
          );
        } catch {
          // Older gateway, or a flaky link. The next reconcile tick retries.
          return;
        }
      }
      if (signal?.aborted) return;
      const now = liveTurnRef.current;
      if (now && (now.status === "running" || now.id === tid)) return;
      // `running_started_at` is the GATEWAY's clock; the bubble's elapsed timer
      // reads the device's. Rebase through `server_time_ms` (shipped in the same
      // response for exactly this) so a phone minutes off UTC does not show a
      // turn that started in the future or an hour ago.
      const startedAt =
        row.running_started_at != null && row.server_time_ms != null
          ? Date.now() -
            Math.max(0, row.server_time_ms - row.running_started_at)
          : // Adopted off the transcript there is no `running_started_at` to rebase
            // — the row's own stamp is the only start there is, and without it a
            // turn that began two hours ago would restart its clock at 0s.
            typeof running?.created_at === "number" &&
              Number.isFinite(running.created_at)
            ? running.created_at -
              (row.server_time_ms != null ? row.server_time_ms - Date.now() : 0)
            : Date.now();
      const adopted: LiveTurn = {
        id: tid,
        request:
          row.running_request ??
          running?.user_request ??
          running?.request ??
          "",
        answer: "",
        iterations,
        startedAt,
        status: "running",
      };
      liveTurnRef.current = adopted;
      setLiveTurn(adopted);
      setRunning(true);
    },
    [client, sid],
  );

  useEffect(() => {
    const controller = new AbortController();
    // Meta FIRST: it is a tiny payload whose stamp decides whether the transcript
    // has to be read back at all. Re-entering a session that has not moved then
    // costs one small request and no re-render, instead of refetching, reparsing
    // and re-rendering the whole history every time you walk back into it.
    const backlogReadAt = Date.now();
    void (async () => {
      // A COLD open holds no cached rows, so the meta row cannot save the
      // transcript read — it can only delay it by a whole round trip on a link
      // that is often a phone on someone else's network. Ask for both at once
      // there. A re-entry keeps the meta-first order, where the stamp is exactly
      // what makes the transcript read free.
      const cold = !client.cachedTranscript(sid)?.length;
      const body = cold ? loadTranscript() : null;
      let row: Session | null = null;
      try {
        row = await client.session(sid, controller.signal, true);
        setSession(row);
        acceptQueueBacklog(client.cachedQueuedTurns(sid) ?? [], backlogReadAt);
      } catch {
        /* Unreachable gateway: fall through, the transcript read reports it. */
      }
      if (controller.signal.aborted) return;
      const rows = await (body ?? loadTranscript(row));
      if (controller.signal.aborted) return;
      // AFTER the transcript: the persisted 'running' placeholder must already be
      // on screen when the bubble takes it over, or the two swap places and the
      // turn flickers.
      await adoptRunningTurn(row, controller.signal, rows);
    })();
    return () => controller.abort();
  }, [client, sid, loadTranscript, acceptQueueBacklog, adoptRunningTurn]);

  // Reconcile against the gateway's authoritative liveness. A streamed live turn
  // is only cleared by a terminal SSE event, but that event can be missed — the
  // turn finished in the TUI/another client before we subscribed, the ring
  // rewound past the completion, or the socket dropped mid-turn. Left alone the
  // bubble spins "working" forever while the session is actually idle. Poll the
  // session every few seconds; when the gateway says it is no longer live but we
  // still show a running turn, reload the transcript and drop the live bubble.
  useEffect(() => {
    let cancelled = false;
    // A request still in flight when the OS suspends the webview never settles
    // and never rejects. A plain boolean latch would then block every later
    // tick for the life of the app — the "restart it before I see anything"
    // bug. Stamp the start instead and treat an older one as lost.
    const STALE_RECONCILE_MS = 20_000;
    let inflightSince: number | null = null;
    const reconcileOnce = async () => {
      if (document.visibilityState === "hidden") return;
      // Liveness is sampled HERE but only acted on after two more round-trips
      // (transcript + queue) below. On wake we resync the stream and reconcile at
      // exactly the moment the gateway drains a queued row, so `turn.started` can
      // land INSIDE that window: the verdict in hand then says "idle" about a
      // session that has since started a turn. Snapshot the work that exists NOW —
      // anything newer than this read is not what `gatewayLive` described, and
      // clearing it killed the live turn the drained queue row had just started.
      // `reduceLiveEvent` drops every delta while the live turn is null, so the
      // answer never streamed: the queue emptied and the bubble stayed blank until
      // the whole turn persisted.
      const liveBefore = liveTurnRef.current;
      const liveIdBefore = liveBefore?.id ?? "";
      const runningBefore = runningRef.current;
      let next: Session;
      try {
        next = await client.session(sid);
      } catch {
        return;
      }
      if (cancelled) return;
      setSession(next);
      const gatewayLive =
        next.live !== undefined ? next.live : next.status === "running";
      // Safety net: on wake we ALWAYS refetch the transcript and check whether
      // the streamed live turn has already been persisted while we were
      // backgrounded. iOS/Android suspend fetch-body streams silently, so the
      // terminal event that would have cleared the live bubble may have been
      // dropped. If the persisted turn now exists, drop the live bubble; if
      // the gateway is idle but we still show work, do the same.
      const liveId = liveTurnRef.current?.id ?? "";
      const liveStartedAt = liveTurnRef.current?.startedAt;
      let nextTurns: TranscriptTurn[] | null = null;
      try {
        // Gated on the row this tick just read: an idle session costs one tiny
        // request per tick instead of re-reading its whole transcript.
        nextTurns = await client.transcriptIfMoved(sid, next);
      } catch {
        nextTurns = null;
      }
      if (cancelled) return;
      // The ENGINE's witness, taken from the rows this tick just read: a persisted
      // `running` row outlives any registry hiccup (see `inFlightRow`). Every
      // verdict below that used to trust `gatewayLive` alone now needs both to
      // agree before it retires work the user is watching.
      const persistedRunning = inFlightRow(nextTurns ?? turnsRef.current);
      // Decided and applied BEFORE the queue round-trip below: with the clear
      // sitting after another await, the settled transcript row and the live
      // bubble both painted for that window (the same turn twice).
      const covered = liveTurnSettledInto(
        nextTurns,
        preLiveTurnIdsRef.current,
        liveId,
        liveStartedAt,
      );
      if (nextTurns) {
        setTurns(nextTurns);
        setTurnsFresh(true);
        // Only the very bubble this coverage verdict is about — a turn started
        // since the read must keep streaming.
        //
        // And never against the turn the GATEWAY says it is running right now: the
        // transcript verdict is a heuristic (an id that never matches plus a 60 s
        // created_at window), and right after a queued row drains, the PREVIOUS
        // turn's freshly persisted row falls inside that window. One tick then
        // retired a live bubble mid-stream. The registry row is not a heuristic.
        // The persisted row counts too: while it is still `running` the turn is
        // demonstrably alive, whatever the registry currently believes.
        const stillRunningThis =
          liveId !== "" &&
          ((gatewayLive && (next.current_turn_id ?? "") === liveId) ||
            (persistedRunning !== null && rowId(persistedRunning) === liveId));
        if (
          covered &&
          !stillRunningThis &&
          (liveTurnRef.current?.id ?? "") === liveId
        ) {
          setRunning(false);
          setLiveTurn(null);
          liveTurnRef.current = null;
        }
        // Turns that arrived while the app was away are exactly why the reader
        // came back. Pin AFTER they render, or the wake's pin lands on the old
        // height and the new tail sits below the fold.
        if (resumePinRef.current) requestAnimationFrame(() => pinToEnd());
      }
      // Same reconcile for the queue: a `turn.queued`/`.deleted` frame dropped
      // by a suspended stream would otherwise leave the tray lying until the
      // row drained. Gateway truth wins outright — we never merge in a local
      // guess.
      try {
        const backlogReadAt = Date.now();
        const backlog = await client.queuedTurns(sid);
        if (cancelled) return;
        acceptQueueBacklog(backlog, backlogReadAt);
      } catch {
        /* Keep the last known backlog; the next tick retries. */
      }
      const live = liveTurnRef.current;
      const showsWork =
        (live !== null &&
          liveBefore !== null &&
          (live.id ?? "") === liveIdBefore) ||
        (runningRef.current && runningBefore);
      // `persistedRunning` vetoes this: "idle" from a registry that lost the turn
      // is not idleness, and freezing the ticker there is the same wrong answer in
      // a quieter costume.
      if (!covered && !gatewayLive && !persistedRunning && showsWork) {
        setRunning(false);
        // The gateway is idle but its answer is NOT in the transcript yet (the
        // engine row lags, or the read failed). Stop the ticker — never delete
        // what the user watched. Deleting here is what made a whole turn, and
        // every event streamed into it, disappear the moment it ended; the TUI
        // keeps its snapshotted trace for exactly this case
        // (`:message-received`, channel_tui/state.clj). The next covered read
        // swaps in the persisted row.
        setLiveTurn((turn) => {
          if (!turn || turn.status !== "running") return turn;
          const settledTurn: LiveTurn = {
            ...turn,
            status: "completed",
            activity: undefined,
            cancelling: false,
          };
          liveTurnRef.current = settledTurn;
          return settledTurn;
        });
      }
      if (cancelled) return;
      // Gateway idle, engine still writing: nothing will be PUSHED into the bubble
      // either — the hub's liveness comes from the same registry — so repair it
      // from the row this tick ALREADY read. `/transcript` inlines the running
      // row's `iterations`, so this costs no request at all; pulling
      // `/turns/:tid/trace` on top of it would re-download the entire turn every
      // few seconds, for as long as a stall that can run for hours.
      if (!gatewayLive && persistedRunning) {
        const painted = liveTurnRef.current;
        // Two mints, one turn: a bubble seeded by `turn.started` carries the
        // GATEWAY's id, this row carries the ENGINE's, and the frozen bubble in
        // the incident is exactly one of those. Letting that mismatch veto the
        // repair would leave the only case this branch exists for unhandled, so
        // the submitted text stands as the second witness when the ids differ.
        const rowRequest = (
          persistedRunning.user_request ??
          persistedRunning.request ??
          ""
        ).trim();
        const traced = persistedRunning.iterations;
        // Never shrink: deltas that did land are newer than any persisted row, and
        // a row read before them is no evidence that they never came.
        const grown: LiveTurn | null =
          painted?.status === "running" &&
          (painted.id === rowId(persistedRunning) ||
            (rowRequest !== "" && painted.request.trim() === rowRequest)) &&
          traced != null &&
          traced.length > painted.iterations.length
            ? { ...painted, iterations: traced }
            : null;
        if (grown) {
          liveTurnRef.current = grown;
          setLiveTurn(grown);
        }
      }
      // Last, against the row this tick read: the gateway is running a turn we
      // are not painting. That is every way the seed frame can be missed — a
      // webview the OS reloaded while backgrounded, a turn another client
      // started, a stream that reconnected past it. Free when the transcript
      // carries the row; it costs a request only when the registry claims a turn
      // the transcript has not caught up with.
      await adoptRunningTurn(next, undefined, nextTurns ?? turnsRef.current);
      // Consumed either way: with no new turns the wake's own pin was enough.
      resumePinRef.current = false;
    };
    // Never stack: each reconcile is two sequential round-trips (session +
    // transcript), so on a slow gateway a fixed 5s tick would overlap and pile
    // requests up. One in flight at a time — a skipped tick self-heals 5s later.
    const reconcile = async () => {
      if (
        inflightSince !== null &&
        Date.now() - inflightSince < STALE_RECONCILE_MS
      )
        return;
      inflightSince = Date.now();
      try {
        await reconcileOnce();
      } finally {
        inflightSince = null;
      }
    };
    const timer = window.setInterval(() => void reconcile(), 5000);
    // Every wake signal, DOM *and* native resume (a Capacitor iOS webview can
    // resume without firing a single DOM event). Force the multiplexed SSE
    // stream to reconnect so a silently frozen background socket does not sit
    // there forever, and drop a latch a suspended request left behind.
    // Coming back from a glance at a notification, the place you were reading IS
    // the answer. Coming back to a session you left hours ago, it is noise: the
    // work moved on and the last thing you saw is buried. So a long absence
    // returns to the end of the conversation, a short one does not.
    const RESUME_AT_END_AFTER_MS = 60_000;
    const stopWake = onWake(({ awayMs }) => {
      inflightSince = null;
      subscriptions.resync();
      if (awayMs >= RESUME_AT_END_AFTER_MS) {
        resumePinRef.current = true;
        pinToEnd();
      }
      void reconcile();
    });
    // Coming back from an outage, the frame that ended this turn may simply be
    // gone: the gateway's replay ring is process memory, so a cursor the server
    // accepts still replays nothing after a daemon restart. The server closes
    // that hole itself — every (re)subscribe opens with `subscription.ready`,
    // which names the turn the daemon is running for this session RIGHT NOW.
    //
    // So this is a verdict, not a timer: if the daemon's turn matches the one
    // being painted, the bubble is genuinely live and nothing is fetched. If it
    // disagrees — a different turn, or none at all — the gap is proven and one
    // reconcile settles it, instead of waiting out the 5 s tick (or up to
    // STALE_RECONCILE_MS, when a request that died with the old socket still
    // holds the latch). The TUI's `:sync-gateway-ready`, from the other side.
    //
    // `replay: false`: the buffered backlog is for reopening a screen mid-turn;
    // this listener only ever wants live control frames.
    // The ready frame is only as rare as the transport is stable: a gateway in a
    // reconnect-backoff loop emits one per attempt, and this handler drops the
    // in-flight latch, so with no floor of its own a flapping socket would drive
    // reconciles as fast as it can reconnect. Throttled on its OWN stamp (the TUI
    // does the same with `:gateway-resynced-at-ms`) at the poll's own period, so
    // the worst case degrades to the 5 s tick instead of a request storm.
    const READY_RECONCILE_MIN_MS = 5000;
    let lastReadyReconcileAt = 0;
    const stopReady = subscriptions.subscribeSession(
      sid,
      (event) => {
        if (event.type !== "subscription.ready") return;
        const live = liveTurnRef.current;
        const painted = live?.status === "running" ? live.id : "";
        const running =
          typeof event.current_turn_id === "string"
            ? event.current_turn_id
            : "";
        // An older daemon omits the state entirely; then the frame degrades to
        // "reconcile on every reconnect", which is merely one extra read.
        if (typeof event.is_live === "boolean" && running === painted) return;
        const now = Date.now();
        if (now - lastReadyReconcileAt < READY_RECONCILE_MIN_MS) return;
        lastReadyReconcileAt = now;
        // A read issued over the socket that just died cannot answer for the gap,
        // so its silence must not suppress the one reconcile that can.
        inflightSince = null;
        void reconcile();
      },
      { replay: false },
    );
    return () => {
      cancelled = true;
      window.clearInterval(timer);
      stopWake();
      stopReady();
    };
  }, [
    client,
    sid,
    loadTranscript,
    subscriptions,
    pinToEnd,
    acceptQueueBacklog,
    adoptRunningTurn,
  ]);

  const refreshSlashCommands = useCallback(
    (signal?: AbortSignal) => {
      return client
        .slashes(sid, signal)
        .then((commands) => setSlashCommands(mergeSlashCommands(commands)))
        .catch(() => {
          if (signal?.aborted) return;
          setSlashCommands(mergeSlashCommands([]));
        });
    },
    [client, sid],
  );

  // The palette is derived on the gateway and MOVES at runtime: `/reload`
  // rescans extensions, skills, agents, harness commands and prompt templates,
  // so a skill dropped into `.agents/skills` mid-session has to show up without
  // restarting the app. Fetch it on mount, on every wake, and again the moment
  // the composer enters slash mode — that is exactly when a stale list shows.
  useEffect(() => {
    const controller = new AbortController();
    void refreshSlashCommands(controller.signal);
    const stopWake = onWake(() => void refreshSlashCommands());
    return () => {
      controller.abort();
      stopWake();
    };
  }, [refreshSlashCommands]);

  const slashMode = prompt.trimStart().startsWith("/");

  useEffect(() => {
    if (slashMode) void refreshSlashCommands();
  }, [slashMode, refreshSlashCommands]);

  // Revalidated on every RECONNECT, not just on mount: a probe that lost the race
  // with a gateway that was down/asleep used to pin `voiceSupported` false for the
  // whole life of the screen — the mic button silently vanished from the composer
  // and only a full app restart brought it back.
  useEffect(() => {
    const controller = new AbortController();
    let active = true;
    void (async () => {
      try {
        const next = await client.capabilities(controller.signal);
        if (!active) return;
        setCapabilities(next);
        setVoiceSupported(next.features.voice.enabled);
        setVoiceModel(next.features.voice.model);
      } catch {
        try {
          const model = await client.voiceModel(sid, false, controller.signal);
          if (!active) return;
          // An ANSWER of `unavailable` is the gateway saying it has no voice
          // extension — that one is authoritative and does hide the mic.
          setVoiceSupported(model.status !== "unavailable");
          setVoiceModel(model);
        } catch {
          if (!active) return;
          // Both probes failed to ARRIVE: that is a transport verdict about the
          // network, not about the gateway's features. Keep the last answer this
          // gateway actually gave instead of inventing "voice is gone".
          const cached = client.cachedCapabilities()?.features.voice;
          setVoiceSupported(cached?.enabled ?? false);
          setVoiceModel(cached?.model ?? { status: "unavailable" });
        }
      }
    })();
    return () => {
      active = false;
      controller.abort();
    };
  }, [client, sid, connected]);

  useEffect(() => {
    if (!voiceSupported || voiceModel?.status !== "downloading") return;
    let inflight = false;
    const timer = window.setInterval(() => {
      // Same anti-stacking rule as the reconcile poll: one request in flight,
      // and nothing at all while the app is backgrounded.
      if (inflight || document.visibilityState === "hidden") return;
      inflight = true;
      void client
        .voiceModel(sid)
        .then(setVoiceModel)
        .catch(() => undefined)
        .finally(() => {
          inflight = false;
        });
    }, 2000);
    return () => window.clearInterval(timer);
  }, [client, sid, voiceModel?.status, voiceSupported]);

  // A dictation of any length overflows the 80px composer, and a PROGRAMMATIC
  // value change never scrolls a textarea to its caret (only real user input
  // does) — so the box keeps showing the FIRST lines of what was just said while
  // the end, the part you are about to keep typing after, sits below the fold.
  // Park the caret at the end and scroll there ourselves, one frame later so the
  // autosize effect has already committed the new height.
  //
  // It does NOT focus. Taking focus after a dictation raises the on-screen
  // keyboard on iOS/Android, which is a decision only the reader can make: a
  // transcript that came out right is meant to be SENT, not edited, and the
  // keyboard then covers half the transcript for nothing. Whatever had focus
  // when the words landed keeps it — so a composer that was already focused
  // (desktop, or a keyboard the user deliberately kept up) stays focused and
  // the caret still lands at the end.
  const revealComposerEnd = useCallback(() => {
    requestAnimationFrame(() => {
      const textarea = composerRef.current;
      if (!textarea) return;
      const end = textarea.value.length;
      textarea.setSelectionRange(end, end);
      setCaret(end);
      textarea.scrollTop = textarea.scrollHeight;
    });
  }, []);

  // Turn captured audio into composer text. Kept apart from the mic button
  // because the button is no longer the only thing that ends a recording.
  const transcribeVoice = useCallback(
    async (wav: Blob) => {
      // Durable BEFORE the request, never only after it fails: iOS tears the
      // webview down mid-flight (reclaim, crash, reload) and audio that lives in
      // this closure alone dies with it. Only a transcript empties the outbox.
      pendingVoiceRef.current = wav;
      void savePendingVoice(voiceMailboxId, wav);
      try {
        const transcript = await client.transcribeVoice(sid, wav, {
          onProgress: setVoiceProgress,
        });
        pendingVoiceRef.current = null;
        void clearPendingVoice(voiceMailboxId);
        const text = transcript.text.trim();
        // A transcript that comes back empty is a REAL outcome (a muted or
        // hijacked mic records perfect silence), so it has to say so: dropping it
        // silently is what makes the button feel dead.
        if (text) {
          setPrompt(
            (current) =>
              `${current.trimEnd()}${current.trim() ? " " : ""}${text}`,
          );
          // Show the TAIL of the dictation, not its opening line.
          revealComposerEnd();
        } else {
          setComposerNotice("No speech recognised — nothing was captured.");
        }
      } catch (cause) {
        // Offline, asleep, or a gateway that never answered. The words are NOT
        // lost: they sit in the outbox and drain on the next wake — and `online`
        // is one of the signals that fires a wake (lib/wake.ts).
        const message = (cause as Error).message;
        const unreachable =
          message.startsWith("network error") ||
          message.includes("did not answer") ||
          message.includes("stopped sending");
        setComposerNotice(
          unreachable
            ? "Saved what you said — it transcribes as soon as the gateway is reachable."
            : message,
        );
      } finally {
        setVoicePhase("idle");
        setVoiceProgress(null);
      }
    },
    [client, sid, revealComposerEnd, voiceMailboxId],
  );

  // End dictation and transcribe what WAS captured. Every path that takes the
  // microphone away lands here, not just the mic button: iOS suspends the
  // webview the moment the app leaves the foreground, so `onaudioprocess` stops
  // firing while the composer still reads "Listening…" and the recorder is never
  // stopped — the words spoken up to that point are simply lost. Finishing here
  // puts them in the composer draft, which is persisted, so they survive even a
  // webview the OS kills outright.
  const finishVoice = useCallback(
    async (options?: { notice?: string }) => {
      const recording = recordingRef.current;
      if (!recording) return;
      recordingRef.current = null;
      setVoicePhase("transcribing");
      if (options?.notice) setComposerNotice(options.notice);
      let wav: Blob;
      try {
        wav = await recording.stop();
      } catch (cause) {
        setComposerNotice((cause as Error).message);
        setVoicePhase("idle");
        return;
      }
      await transcribeVoice(wav);
    },
    [transcribeVoice],
  );

  const finishVoiceRef = useRef(finishVoice);
  useEffect(() => {
    finishVoiceRef.current = finishVoice;
  });

  // Leaving the foreground is NOT the end of a dictation. It takes TWO things to
  // keep capture alive on iOS, and only both together: the `audio` entry in
  // `UIBackgroundModes` (ios/App/App/Info.plist), which stops WebKit from muting
  // the microphone track, and the `play-and-record` audio session every
  // dictation claims (src/lib/voice.ts), which stops WebKit from interrupting
  // the AudioContext — and with it `onaudioprocess` — the instant the app
  // backgrounds. With both in place capture survives backgrounding, the screen
  // locking, and the phone going to sleep. So we let it run.
  //
  // Two nets remain. `pagehide` means the page itself is going away (web build,
  // or a webview the OS tears down) — nothing survives that, so settle what was
  // said into the composer draft, which IS persisted. And on the way back to the
  // foreground we ask the recorder whether capture actually survived: if the OS
  // took the mic anyway (a call, another app, an older iOS), finish here, in the
  // foreground, where the transcription request can complete.
  useEffect(() => {
    if (voicePhase !== "recording") return;
    const finish = (notice: string) => {
      void finishVoiceRef.current({ notice });
    };
    const onPageHide = () => {
      finish(
        "Dictation ended when the app closed — transcribing what was said.",
      );
    };
    window.addEventListener("pagehide", onPageHide);
    let removed = false;
    let sub: { remove: () => void } | null = null;
    try {
      void App.addListener("appStateChange", ({ isActive }) => {
        if (!isActive) return;
        if (recordingRef.current && !recordingRef.current.isCapturing()) {
          finish(
            "Dictation stopped while the app was away — transcribing what was said.",
          );
        }
      })
        .then((handle) => {
          if (removed) handle.remove();
          else sub = handle;
        })
        .catch(() => undefined);
    } catch {
      /* plugin unavailable */
    }
    return () => {
      window.removeEventListener("pagehide", onPageHide);
      removed = true;
      sub?.remove();
    };
  }, [voicePhase]);

  // Audio the gateway has not turned into text yet — because the webview was
  // backgrounded mid-request, because the phone had no link, or because the app
  // was killed between the two. Drain it whenever the app is live again.
  const voicePhaseRef = useRef(voicePhase);
  useEffect(() => {
    voicePhaseRef.current = voicePhase;
  });
  const retryPendingVoice = useCallback(async () => {
    if (voicePhaseRef.current !== "idle") return;
    // The ref is only a fast path: after a cold start it is empty and the
    // outbox is the sole record that anything was ever said.
    const wav =
      pendingVoiceRef.current ?? (await readPendingVoice(voiceMailboxId));
    if (!wav || voicePhaseRef.current !== "idle") return;
    setVoicePhase("transcribing");
    await transcribeVoice(wav);
  }, [transcribeVoice, voiceMailboxId]);
  useEffect(() => onWake(() => void retryPendingVoice()), [retryPendingVoice]);
  // Cold start / session switch: adopt whatever this session still owes.
  useEffect(() => {
    void retryPendingVoice();
  }, [retryPendingVoice]);

  useEffect(
    () => () => {
      void recordingRef.current?.cancel();
      recordingRef.current = null;
    },
    [],
  );

  useEffect(() => {
    async function settle(event: SseEvent) {
      const type = event.type;
      // WHICH turn this terminal is about, decided BEFORE any await. The gateway
      // drains the queue the instant a turn ends, so `turn.started` for the next
      // (auto-sent) row can land inside every gap below — and this function used
      // to settle whatever bubble it happened to find: it stamped the NEW turn
      // `completed` (so `reduceLiveEvent`'s "a settled bubble never re-animates"
      // guard then dropped every one of its deltas) and deleted it outright as
      // soon as the PREVIOUS turn showed up in the transcript. The queued turn ran
      // to completion into a bubble nobody was painting: an empty "Vis" until the
      // whole thing persisted. This is the same rule the reconcile tick already
      // applies before it clears a bubble.
      const finishedId =
        stringField(event, "turn_id") || liveTurnRef.current?.id || "";
      // An id-less bubble is OUR optimistic one, i.e. this very turn. Anything
      // started since carries the gateway's own id, so it can never be mistaken
      // for the turn that just ended.
      const ownsTerminal = (turn: LiveTurn | null) =>
        !!turn && ((turn.id ?? "") === "" || turn.id === finishedId);
      const finishedStartedAt = ownsTerminal(liveTurnRef.current)
        ? liveTurnRef.current?.startedAt
        : undefined;
      if (!liveTurnRef.current || ownsTerminal(liveTurnRef.current))
        setRunning(false);
      // Settle the live bubble ITSELF, synchronously. The transcript refetch below
      // is a network round-trip and may fail outright, and until it lands the
      // bubble still reads `status: 'running'` — spinner up, "Vis is thinking",
      // the last thinking band alive — for a turn the gateway already finished.
      // The terminal frame IS the end of the turn; that claim needs no transcript.
      // Mirrors the TUI's independent terminal path.
      setLiveTurn((turn) => {
        if (!turn || turn.status !== "running" || !ownsTerminal(turn))
          return turn;
        const failedBlocks =
          type === "turn.failed" && Array.isArray(event.content)
            ? (event.content as ContentBlock[])
            : undefined;
        const next: LiveTurn = {
          ...turn,
          status:
            type === "turn.failed"
              ? "failed"
              : type === "turn.cancelled"
                ? "cancelled"
                : "completed",
          activity: undefined,
          cancelling: false,
          content: failedBlocks?.length ? failedBlocks : turn.content,
        };
        liveTurnRef.current = next;
        return next;
      });
      // Keep the streamed live turn on screen until the finished turn is
      // actually persisted in the transcript, otherwise it vanishes for a frame
      // (the persisted row lags the terminal event) and the view jumps.
      // (`finishedId` / `finishedStartedAt` were captured above, before the awaits.)
      // Fetch the finished transcript WITHOUT touching state, then apply the
      // turns and drop the live bubble in ONE synchronous (React-batched) update.
      // The persisted finished turn carries the engine's row id, not the live
      // turn's gateway id, so `visibleTurns` can't filter it out — if `setTurns`
      // rendered before `setLiveTurn(null)`, both would show for a frame (dup).
      let next: TranscriptTurn[] | null = null;
      // Refresh the meta row alongside the transcript: the transcript read stamps
      // itself with the freshest row it can see, so the next reconcile tick knows
      // this copy is current and skips its own read.
      void client
        .session(sid)
        .then(setSession)
        .catch(() => undefined);
      // How big a page this settle needs. A full page is 24 turns of hydrated
      // iterations — hundreds of kilobytes to megabytes on a phone link — and a
      // settle only has to bring ONE freshly persisted row into view. When the
      // snapshot we hold already reaches the tail of the session, the newest few
      // rows adjoin it and `GatewayClient.transcript` merges them onto what is
      // already rendered. Anything else (cold cache, a window that lags behind
      // because turns ran while the app was away) takes the full page: a short
      // page that does NOT adjoin would REPLACE the transcript with those few
      // rows.
      const settleLimit = (): number | undefined => {
        const cached = client.cachedTranscript(sid);
        if (!cached?.length) return undefined;
        const held = client.transcriptWindow(sid);
        return held.offset + cached.length >= held.total
          ? SETTLE_TAIL_TURNS
          : undefined;
      };
      try {
        next = await client.transcript(sid, undefined, settleLimit());
      } catch {
        next = null;
      }
      const covers = (turns: TranscriptTurn[] | null) =>
        liveTurnSettledInto(
          turns,
          preLiveTurnIdsRef.current,
          finishedId,
          finishedStartedAt,
        );
      // The persisted row lags the terminal frame by however long the engine's
      // write takes. Poll for it on a short escalating backoff instead of
      // sleeping one flat 300 ms and asking once: the common case (the row is
      // already there a few tens of ms later) swaps the bubble for the real row
      // ~4x sooner, and a slow write now gets four chances over a longer window
      // instead of one, so the bubble is handed over on THIS path rather than by
      // a reconcile tick up to 5 s later.
      //
      // A THROWN fetch keeps `next` null, and that must NOT end the loop: a
      // single offline blip on the first read used to abort settle outright and
      // leave the finished turn missing from the transcript until a reconcile
      // tick. Only an answer that actually covers this turn stops the polling.
      for (const wait of SETTLE_RETRY_MS) {
        if (next && covers(next)) break;
        await new Promise((resolve) => window.setTimeout(resolve, wait));
        try {
          next = await client.transcript(sid, undefined, settleLimit());
        } catch {
          /* keep the earlier snapshot */
        }
      }
      if (next) {
        setTurns(next);
        // This read WAS confirmed against the gateway this visit, so a running
        // row in it is real and may paint. Without the flag `visibleTurns`
        // filtered the placeholder row out while the live bubble was being
        // dropped below — the turn left the screen entirely.
        setTurnsFresh(true);
        setError(null);
        setLoading(false);
        // Drop the live bubble ONLY against a settled row that carries the
        // answer. Against the still-`running` placeholder (or an id that never
        // matches) this deleted the finished turn outright.
        // …and only while the bubble on screen is still THIS turn's: a queued row
        // that drained during the fetch owns the rail now.
        if (covers(next) && ownsTerminal(liveTurnRef.current)) {
          setLiveTurn(null);
          liveTurnRef.current = null;
        } else if (covers(next)) {
          // A queued row drained into the rail while we were fetching. THIS turn's
          // answer is now persisted, so it is old news for the bubble that is
          // streaming — fold it into the baseline. The baseline is frozen for as
          // long as ANY bubble is up (see the mirror effect), so without this the
          // reconcile tick reads "the live turn has landed" off the PREVIOUS turn's
          // row (`liveTurnSettledInto`'s 60 s created_at slack accepts it) and
          // deletes a bubble that is still streaming — the same blank "Vis", one
          // tick later.
          preLiveTurnIdsRef.current = new Set([
            ...preLiveTurnIdsRef.current,
            ...next.filter((turn) => !isRunningRow(turn)).map(rowId),
          ]);
        }
      }
      if (type === "turn.failed") {
        // The settled turn carries the gateway's OWN error card (rate limit, auth,
        // transport) and the transcript refresh above already renders it. A second
        // banner reading "The turn failed." is duplicate noise on top of it.
        const blocks = event.content;
        if (!Array.isArray(blocks) || blocks.length === 0) {
          setError(
            stringField(event, "message") ||
              stringField(event, "error") ||
              "The turn failed.",
          );
        }
      }
    }

    // Match the TUI's 150 ms live-body throttle. One reducer pass and one React
    // state update replace hundreds of token-level updates during fast streams.
    const eventQueue: SseEvent[] = [];
    // Highest seq ENQUEUED, which runs ahead of the highest seq APPLIED for as
    // long as a flush is pending. Dedup has to key off this one (an event sitting
    // in the queue must not be taken again on a resubscribe replay), while the
    // cache cursor may only ever advance with content that is actually folded in.
    let enqueuedSeq = lastLiveSeqRef.current;
    let timerId: number | null = null;
    const flushEvents = () => {
      if (timerId !== null) window.clearTimeout(timerId);
      timerId = null;
      // Streaming Markdown repeatedly reflows the transcript. During an OS
      // rotation those intermediate widths are throwaway geometry, and every
      // commit both burns the frame budget and moves the live bottom underneath
      // the frozen scroll transaction. Keep body frames queued until the final
      // viewport is known; lifecycle frames still punch through immediately.
      if (
        isViewportRotating() &&
        !eventQueue.some(forcesLiveFlushDuringRotation)
      )
        return;
      const drained = eventQueue.splice(0);
      // Advance the cached cursor HERE, not on arrival: unmounting drops the
      // pending queue, and a cursor that had already counted those frames would
      // make the next visit filter them out of a bubble that never got them —
      // a hole in the answer that only the terminal frame could fill.
      for (const event of drained) {
        if (
          typeof event.seq === "number" &&
          event.seq > lastLiveSeqRef.current
        ) {
          lastLiveSeqRef.current = event.seq;
        }
      }
      const batch = coalesceLiveEvents(drained);
      if (!batch.length) return;

      // Queue-mirror + pause control frames (channel-agnostic, same events the
      // TUI consumes). Not live-turn events, so handle them outside the reducer.
      for (const event of batch) {
        const tid = stringField(event, "turn_id");
        switch (event.type) {
          case "turn.queued": {
            const row = queuedTurnFromWire(
              event as unknown as Record<string, unknown>,
            );
            noteQueueDelta(tid, row);
            setQueued((current) =>
              current.some((item) => item.turnId === tid)
                ? current
                : [...current, row],
            );
            break;
          }
          case "turn.queued.updated":
            setQueued((current) =>
              current.map((item) =>
                item.turnId === tid
                  ? {
                      ...item,
                      ...queuedTurnFromWire(
                        event as unknown as Record<string, unknown>,
                      ),
                    }
                  : item,
              ),
            );
            break;
          case "turn.queued.deleted":
            noteQueueDelta(tid, null);
            setQueued((current) =>
              current.filter((item) => item.turnId !== tid),
            );
            // Reason `cancelled` = the gateway dropped this row WITH a user stop
            // (`drop-cancelled-backlog!`), so the text has nowhere else to live.
            // A plain delete (the user removed the row) carries no reason and
            // restores nothing.
            if (stringField(event, "reason") === "cancelled") {
              restoreCancelledQueued(tid, stringField(event, "request"));
            }
            break;
          case "turn.queued.drained":
            noteQueueDelta(tid, null);
            setQueued((current) =>
              current.filter((item) => item.turnId !== tid),
            );
            break;
          case "queue.paused":
            setQueuePaused({
              reason: stringField(event, "reason") || "turn_failed",
              held: Number(event.held ?? 0),
            });
            break;
          case "queue.resumed":
            setQueuePaused(null);
            break;
          // Someone ELSE repointed this session — the TUI picker, another
          // device, an embedded caller. The gateway broadcasts the new pin, and
          // it is the single writer of that fact, so follow the frame instead of
          // trusting this screen's last local pick until a reopen (the TUI
          // projects the same event onto its footer chip). Blank provider AND
          // model means the override was cleared.
          case "session.model_updated":
            setModelPref(
              client.noteSessionModel(sid, {
                provider: stringField(event, "provider"),
                model: stringField(event, "model"),
              }),
            );
            break;
          default:
            break;
        }
      }

      // The composer follows the batch's LAST lifecycle frame, and it is decided
      // AFTER `settle` below: a terminal and the `turn.started` of the queued row
      // it drained can share one throttle window, and settle's `setRunning(false)`
      // must not be the final word when a new turn is already running.
      setLiveTurn((turn) => {
        const reduced = batch.reduce(reduceLiveEvent, turn);
        liveTurnRef.current = reduced;
        return reduced;
      });

      let terminal: SseEvent | undefined;
      for (let index = batch.length - 1; index >= 0; index -= 1) {
        if (TERMINAL_EVENTS.has(batch[index].type)) {
          terminal = batch[index];
          break;
        }
      }
      if (terminal) void settle(terminal);
      let lifecycle: SseEvent | undefined;
      for (let index = batch.length - 1; index >= 0; index -= 1) {
        const type = batch[index].type;
        if (TERMINAL_EVENTS.has(type) || type === "turn.started") {
          lifecycle = batch[index];
          break;
        }
      }
      if (lifecycle?.type === "turn.started") setRunning(true);
    };

    flushLiveEventsBeforeRotationRestoreRef.current = flushEvents;

    // Liveness watchdog — the transport-independent twin of the TUI's
    // `:turn-liveness-tick`. A terminal registry verdict uses the SAME `settle`
    // path as a real frame. If the turn is still live, the stream has probably
    // been frozen (notably by WKWebView), so reconnect and replay it instead.
    let lastEventAt = Date.now();
    let lastStallResyncAt = 0;
    // Consecutive probes that agreed on a suspicious verdict. Reset by ANY
    // reassuring answer, so only a persistent fault ever reaches a reconnect.
    let stallStrikes = 0;
    let unknownStrikes = 0;
    let probing = false;
    const livenessTimer = window.setInterval(() => {
      const live = liveTurnRef.current;
      if (probing || !live || live.status !== "running" || !live.id) return;
      const quietSince = Math.max(lastEventAt, live.startedAt ?? 0);
      const silentFor = Date.now() - quietSince;
      if (silentFor < TURN_LIVENESS_IDLE_MS) {
        // Frames are flowing: whatever the last probe suspected is disproved.
        stallStrikes = 0;
        unknownStrikes = 0;
        return;
      }
      probing = true;
      void client
        .turnStatus(sid, live.id)
        .then((turn) => {
          const current = liveTurnRef.current;
          if (
            !current ||
            current.status !== "running" ||
            current.id !== live.id
          )
            return;
          if (turn && !TURN_TERMINAL_STATUSES.has(String(turn.status ?? ""))) {
            // The gateway CONFIRMS the turn is still working, so the transport is
            // not the suspect: a long tool call is simply quiet. Leave the stream
            // alone until the silence outlasts the stall bound — and even then,
            // only once a SECOND probe has re-checked and still sees no frames.
            unknownStrikes = 0;
            if (silentFor < TURN_STREAM_STALL_MS) {
              stallStrikes = 0;
              return;
            }
            stallStrikes += 1;
            if (stallStrikes < TURN_STALL_CONFIRMATIONS) return;
            if (Date.now() - lastStallResyncAt < TURN_STREAM_STALL_MS) return;
            lastStallResyncAt = Date.now();
            stallStrikes = 0;
            subscriptions.resync();
            return;
          }
          if (!turn) {
            // "No such turn" is the one answer that cannot be trusted on sight:
            // a row can be momentarily unreadable (a restarting gateway, a proxy
            // hiccup) while the turn is perfectly alive. Re-check before acting.
            stallStrikes = 0;
            unknownStrikes += 1;
            if (unknownStrikes < TURN_STALL_CONFIRMATIONS) return;
            unknownStrikes = 0;
            lastEventAt = Date.now();
            subscriptions.resync();
            return;
          }
          stallStrikes = 0;
          unknownStrikes = 0;
          const type =
            turn.status === "failed"
              ? "turn.failed"
              : turn.status === "cancelled"
                ? "turn.cancelled"
                : "turn.completed";
          // A lost terminal SSE frame must settle with the registry's canonical
          // error card, not the partial streamed body that preceded the failure.
          return settle({
            type,
            turn_id: live.id,
            ...(type === "turn.failed" && turn.content?.length
              ? { content: turn.content }
              : {}),
          } as unknown as SseEvent);
        })
        .catch(() => undefined)
        .finally(() => {
          probing = false;
        });
    }, TURN_LIVENESS_PROBE_INTERVAL_MS);

    const unsubscribeConnection =
      subscriptions.subscribeConnection(setConnected);
    const unsubscribeEvents = subscriptions.subscribeSession(sid, (event) => {
      // The subscribe handshake is a control frame, not transcript. It must not
      // reach the reducer, and above all must not pass for traffic: the liveness
      // watchdog below measures SILENCE, and a reconnect is exactly when a frozen
      // stream has to stay visibly silent. The `subscription.ready` listener above
      // is the one that acts on it.
      if (event.type === "subscription.ready") return;
      lastEventAt = Date.now();
      // The hub replays a still-streaming turn from its `turn.started` on every
      // (re)subscribe. When the bubble was seeded from the in-memory cache those
      // frames are already folded in, and re-applying them would blank the
      // bubble (`turn.started` resets it) and re-append the same prose. `seq` is
      // the gateway's per-session journal cursor and is monotonic across stored
      // AND live-only frames, so anything at or below what we hold is a repeat.
      if (lastLiveSeqRef.current > enqueuedSeq)
        enqueuedSeq = lastLiveSeqRef.current;
      if (typeof event.seq === "number") {
        if (event.seq <= enqueuedSeq) return;
        enqueuedSeq = event.seq;
      }
      eventQueue.push(event);
      const forceFlush = forcesLiveFlushDuringRotation(event);
      if (forceFlush && timerId !== null) {
        window.clearTimeout(timerId);
        timerId = null;
      }
      if (timerId !== null || (isViewportRotating() && !forceFlush)) return;
      // `turn.started` also flushes immediately: on re-entry it is the frame
      // that replaces a cached bubble whose turn has since been superseded, and
      // holding it for a throttle window paints the previous answer twice.
      const delay = forceFlush ? 0 : LIVE_BODY_THROTTLE_MS;
      timerId = window.setTimeout(flushEvents, delay);
    });

    return () => {
      if (flushLiveEventsBeforeRotationRestoreRef.current === flushEvents) {
        flushLiveEventsBeforeRotationRestoreRef.current = null;
      }
      window.clearInterval(livenessTimer);
      if (timerId !== null) window.clearTimeout(timerId);
      eventQueue.length = 0;
      unsubscribeEvents();
      unsubscribeConnection();
      setConnected(false);
    };
  }, [
    client,
    loadTranscript,
    sid,
    subscriptions,
    noteQueueDelta,
    restoreCancelledQueued,
  ]);

  useLayoutEffect(() => {
    if (
      initialScrollPendingRef.current &&
      turns.length &&
      hydratedTurnCount >= Math.min(FIRST_PAINT_TURNS, turns.length)
    ) {
      initialScrollPendingRef.current = false;
      pinToEnd();
      // The first paint window is the whole opening contract. Reveal one frame
      // after it is bottom-pinned; older turns continue hydrating above the
      // viewport instead of keeping an already-readable transcript behind a veil.
      requestAnimationFrame(() => setLoading(false));
      return;
    }
    // Not while the reader is dragging. `followingRef` is a measurement from the
    // last frame, and this effect runs once per backfilled chunk — i.e. every
    // frame — so catching up here would undo the drag AND re-assert following on
    // the way out, teaching the next chunk to do it again. Stand down and let
    // `handleScroll` say where the gesture actually left them.
    if (followingRef.current && !readerOwnsScroll()) scrollToEnd("auto");
  }, [
    turns,
    visibleTurnCount,
    hydratedTurnCount,
    liveTurn?.id,
    scrollToEnd,
    pinToEnd,
  ]);

  // Fill the render window back up to `visibleTurnCount`, a chunk per frame,
  // once the first paint is out. Rows land ABOVE the viewport, so a reader at
  // the bottom sees nothing; a reader who scrolled up is held by the scroller's
  // one anchor observer.
  useEffect(() => {
    if (hydratedTurnCount >= visibleTurnCount) return;
    let frame: number | null = window.requestAnimationFrame(() => {
      frame = null;
      setHydratedTurnCount((count) => count + HYDRATE_TURNS_PER_FRAME);
    });
    return () => {
      if (frame !== null) window.cancelAnimationFrame(frame);
    };
  }, [hydratedTurnCount, visibleTurnCount]);

  // The veil must DISSOLVE, not vanish. Unmounting it the instant the transcript
  // is ready swaps a full-bleed `bg-ink` sheet for the whole transcript inside a
  // single frame — and reopening a *cached* session is exactly that worst case:
  // the turns are already painted, so the veil is only up for a frame or two and
  // its removal reads as a jump rather than a load. Holding it mounted at
  // `opacity-0` for one transition lets the transcript cross-fade in underneath.
  useEffect(() => {
    if (loading) {
      setVeiled(true);
      return;
    }
    if (!veiled) return;
    const timer = window.setTimeout(() => setVeiled(false), VEIL_FADE_MS);
    return () => window.clearTimeout(timer);
  }, [loading, veiled]);

  // The veil can never outlive the watchdog (see LOADING_VEIL_MAX_MS).
  useEffect(() => {
    if (!loading) return;
    const timer = window.setTimeout(
      () => setLoading(false),
      LOADING_VEIL_MAX_MS,
    );
    return () => window.clearTimeout(timer);
  }, [loading]);

  // Deferred Markdown, fonts, and content-visibility can change the transcript's
  // measured height after React commits. Keep a newly opened/followed session at
  // its actual bottom as those measurements settle.
  //
  // The SCROLLER itself must be observed too, not just its content. Focusing the
  // composer raises the keyboard, which shrinks the shell and therefore the
  // scroller's `clientHeight` while the transcript's own height never changes —
  // so a content-only observer stays silent, `scrollTop` is left where it was,
  // and the bottom of the conversation slides under the keyboard. That is the
  // "I tapped the input and got scrolled up" jump: nothing scrolled, the window
  // shrank around a reader who was pinned to the end.
  useEffect(() => {
    const transcript = transcriptRef.current;
    const viewport = scrollRef.current;
    if (!transcript || typeof ResizeObserver === "undefined") return;
    viewportHeightRef.current = viewport?.clientHeight ?? null;
    shellHeightRef.current = shellViewportHeight();

    const observer = new ResizeObserver(() => {
      // A composer that grows or shrinks a line takes those pixels from the
      // scroller's BOTTOM edge only: every line already on screen keeps its exact
      // y, so the still-looking thing to do is NOTHING. Touching `scrollTop` at
      // all — re-anchoring a mid-history reader or re-following the end — is what
      // slides the answer bubble up and down as the input stretches while you
      // type. A shell-height change is the other story: the keyboard really does
      // eat the bottom of the conversation, so that case keeps its compensation.
      // A rotation resizes both observed boxes several times before the layout
      // settles. Those measurements belong to different geometries, so this
      // keyboard-only compensation must not issue a competing scroll write. The
      // rotation transaction restores its snapshot once after the final paint.
      if (isViewportRotating() || rotationRestorePendingRef.current) {
        const box = scrollRef.current;
        if (box) viewportHeightRef.current = box.clientHeight;
        shellHeightRef.current = shellViewportHeight();
        return;
      }
      const box = scrollRef.current;
      let composerOnly = false;
      if (box) {
        const previous = viewportHeightRef.current;
        const height = box.clientHeight;
        viewportHeightRef.current = height;
        const shell = shellViewportHeight();
        const shellMoved =
          shellHeightRef.current === null ||
          Math.abs(shell - shellHeightRef.current) > 1;
        shellHeightRef.current = shell;
        if (previous !== null && previous !== height) {
          // ...unless the reader is parked at the END. Those pixels come off the
          // scroller's bottom edge, so the last streamed line slides under the
          // grown composer and simply stays there: following is silently broken
          // until the next chunk snaps the view back by the whole accumulated
          // gap. Re-pin in the frame it grew — one line, not a leap.
          if (!shellMoved) composerOnly = !followingRef.current;
          else if (!followingRef.current) {
            const limit = Math.max(0, box.scrollHeight - height);
            box.scrollTop = Math.max(
              0,
              Math.min(limit, box.scrollTop + (previous - height)),
            );
          }
        }
      }
      if (composerOnly || !followingRef.current || readerOwnsScroll()) return;
      if (resizeScrollFrameRef.current !== null) return;
      resizeScrollFrameRef.current = window.requestAnimationFrame(() => {
        resizeScrollFrameRef.current = null;
        // Re-checked here, not just above: the finger can land in the frame this
        // catch-up was waiting for.
        if (followingRef.current && !readerOwnsScroll()) scrollToEnd("auto");
      });
    });
    observer.observe(transcript);
    if (viewport) observer.observe(viewport);

    return () => {
      observer.disconnect();
      if (resizeScrollFrameRef.current !== null) {
        window.cancelAnimationFrame(resizeScrollFrameRef.current);
        resizeScrollFrameRef.current = null;
      }
      if (disclosureScrollFrameRef.current !== null) {
        window.cancelAnimationFrame(disclosureScrollFrameRef.current);
        disclosureScrollFrameRef.current = null;
      }
      if (scrollMetricsFrameRef.current !== null) {
        window.cancelAnimationFrame(scrollMetricsFrameRef.current);
        scrollMetricsFrameRef.current = null;
      }
    };
  }, [scrollToEnd, sid]);

  // Autosize the composer WITHOUT thrashing layout. The naive pattern
  // (`height='auto'` then read `scrollHeight` on every keystroke) invalidates
  // the footer → section → chat scroller and forces a synchronous reflow of
  // the ENTIRE transcript per keypress — that was the typing lag. With the box
  // height left untouched, reading `scrollHeight` only lays out the textarea's
  // own content, so the common case (no height change) costs nothing upstream.
  const promptLengthRef = useRef(0);
  useEffect(() => {
    const textarea = composerRef.current;
    if (!textarea) return;
    const shrunk = prompt.length < promptLengthRef.current;
    promptLengthRef.current = prompt.length;
    // Sending clears the composer outright. Measuring `scrollHeight` on that
    // commit forces a synchronous layout of the whole footer → section →
    // transcript chain in the very frame that mounts the optimistic bubble —
    // the hitch you feel on send. An empty box has exactly one height, the
    // class's own, so drop the inline override and measure nothing.
    if (!prompt) {
      if (textarea.style.height) textarea.style.height = "";
      return;
    }
    // 80px is `max-h-20`, the class's own ceiling: measuring past it only wrote
    // a height the stylesheet clamps away, every keystroke, forever.
    const needed = Math.min(textarea.scrollHeight, 80);
    if (needed > textarea.clientHeight + 1) {
      // Content wrapped past the current box — grow (one cheap targeted write).
      textarea.style.height = `${needed}px`;
    } else if (shrunk && textarea.style.height) {
      // Text got shorter while grown: remeasure from natural height so the box
      // shrinks back. Only this rare path pays the full reset + reflow.
      //
      // `height: auto` collapses the composer to ONE row for the duration of that
      // measurement, and the transcript scroller grows by the whole difference in
      // the same synchronous layout. A reader pinned to the end is past the new
      // (smaller) maximum, so the browser CLAMPS `scrollTop` down — and the clamp
      // is not undone when the height is written back a statement later. Deleting
      // a single character therefore walked the transcript down by up to a full
      // composer's worth of pixels. Undo the transient clamp here: same scroller
      // geometry before and after means the only thing that moved was the browser's
      // clamp.
      const box = scrollRef.current;
      const parkedTop = box ? box.scrollTop : 0;
      const parkedHeight = box ? box.clientHeight : 0;
      textarea.style.height = "auto";
      textarea.style.height = `${Math.min(textarea.scrollHeight, 80)}px`;
      if (
        box &&
        box.clientHeight === parkedHeight &&
        box.scrollTop !== parkedTop
      ) {
        box.scrollTop = parkedTop;
      }
    }
  }, [prompt]);

  // Cold start: read the stored draft message and adopt it only while the
  // composer is still untouched, so typing that raced the read is never
  // overwritten. Until it resolves nothing is recorded — writing the empty
  // initial composer first would erase the very message we are about to restore.
  useEffect(() => {
    let cancelled = false;
    draftMessageReadyRef.current = false;
    setDraftMessageReady(false);
    watchDraftMessageExits();
    void readDraftMessage(draftMessageId).then((message) => {
      if (cancelled) return;
      if (message.text || message.attachments.length > 0) {
        setPrompt((current) => current || message.text);
        setPastes((current) =>
          current.size
            ? current
            : new Map(message.pastes.map((paste) => [paste.id, paste])),
        );
        setAttachments((current) =>
          current.length ? current : [...message.attachments],
        );
        pasteCounterRef.current = Math.max(
          pasteCounterRef.current,
          message.counter,
        );
      }
      draftMessageReadyRef.current = true;
      setDraftMessageReady(true);
    });
    // Leaving the screen is one of the moments the message must be on disk.
    return () => {
      cancelled = true;
      void flushDraftMessages();
    };
  }, [draftMessageId]);

  // Record every change. Sending clears the composer, which clears the message.
  useEffect(() => {
    if (!draftMessageReady || !draftMessageReadyRef.current) return;
    writeDraftMessage(draftMessageId, {
      text: prompt,
      pastes: pastes.values(),
      attachments,
      counter: pasteCounterRef.current,
    });
  }, [draftMessageReady, draftMessageId, prompt, pastes, attachments]);

  // Take what the system share sheet, an Android SEND or a Shortcuts run
  // dropped on us. AFTER the draft message hydrates: the restore above adopts
  // stored text only while the composer is untouched, so a share pasted first
  // would be thrown away by it — and the recorder above only persists once
  // `draftMessageReady` is set, which is what puts the share on disk too.
  //
  // APPENDS, never replaces: dumping five links in a row is the point, and a
  // half-written prompt must survive the interruption. The store hands the
  // payload over exactly once, so a re-render or a session switch cannot paste
  // the same link twice.
  useEffect(() => {
    if (!draftMessageReady) return;
    let cancelled = false;
    const drain = () => {
      if (cancelled) return;
      const share = takePendingShare();
      if (!share) return;
      setPrompt((current) => appendSharedText(current, share));
    };
    // Warm: already parked, or dropped while this screen is open.
    drain();
    const stop = onSharedText(drain);
    // Cold: the share outlived the webview and is still coming off storage.
    void hydratePendingShare().then(drain);
    return () => {
      cancelled = true;
      stop();
    };
  }, [draftMessageReady, draftMessageId]);

  async function addAttachments() {
    const limits = capabilities?.features.attachments;
    const maximum = limits?.max_files ?? 8;
    const remaining = maximum - attachments.length;
    if (remaining <= 0) {
      setComposerNotice(`You can attach up to ${maximum} files`);
      return;
    }

    // iOS/Android's file input hands freshly captured photos to JS as HEIC or
    // with no MIME type. The native picker transcodes them before this boundary,
    // so the gateway receives one of its supported image formats.
    if (!Capacitor.isNativePlatform()) {
      fileInputRef.current?.click();
      return;
    }

    // Take the keyboard down HERE, before the sheet: the composer keeps DOM focus
    // across a native sheet, so on cancel nothing puts the keyboard back, and on
    // delivery iOS raises it again itself. Blur first, refocus once it settles.
    const restoreKeyboard = holdKeyboardAcrossSheet(composerRef.current);
    try {
      const result = await pickMediaAttachments({
        maxFiles: remaining,
        maxFileBytes: limits?.max_file_bytes ?? 25 * 1024 * 1024,
        maxVideoBytes: limits?.max_video_bytes,
        mediaTypes: limits?.media_types,
      });
      setAttachments((current) =>
        [...current, ...result.attachments].slice(0, maximum),
      );
      setComposerNotice(
        result.rejected.length ? result.rejected.join(" · ") : null,
      );
    } catch (cause) {
      setComposerNotice(
        sheetDismissed(cause) ? "No files selected." : (cause as Error).message,
      );
    } finally {
      restoreKeyboard();
    }
  }

  // Taking a picture is a DIFFERENT act from picking one: the OS gallery sheet
  // never opens the camera, so this is the composer's shutter. Native only —
  // on web the file input already exposes whatever capture the browser has.
  async function takePhoto() {
    const limits = capabilities?.features.attachments;
    const maximum = limits?.max_files ?? 8;
    if (attachments.length >= maximum) {
      setComposerNotice(`You can attach up to ${maximum} files`);
      return;
    }

    // The camera sheet closes the keyboard the same way the picker does.
    const restoreKeyboard = holdKeyboardAcrossSheet(composerRef.current);
    try {
      const result = await capturePhotoAttachment({
        maxFileBytes: limits?.max_file_bytes ?? 25 * 1024 * 1024,
        mediaTypes: limits?.media_types,
      });
      setAttachments((current) =>
        [...current, ...result.attachments].slice(0, maximum),
      );
      setComposerNotice(
        result.rejected.length ? result.rejected.join(" · ") : null,
      );
    } catch (cause) {
      // A cancelled shutter is a decision, not a failure.
      setComposerNotice(
        sheetDismissed(cause) ? "No photo taken." : (cause as Error).message,
      );
    } finally {
      restoreKeyboard();
    }
  }

  async function onFilesPicked(fileList: FileList | null) {
    const input = fileInputRef.current;
    if (input) input.value = "";
    const files = fileList ? Array.from(fileList) : [];
    if (!files.length) return;

    const limits = capabilities?.features.attachments;
    const maximum = limits?.max_files ?? 8;
    const remaining = maximum - attachments.length;
    if (remaining <= 0) {
      setComposerNotice(`You can attach up to ${maximum} files`);
      return;
    }
    try {
      const result = await attachmentsFromFiles(files, {
        maxFiles: remaining,
        maxFileBytes: limits?.max_file_bytes ?? 25 * 1024 * 1024,
        maxVideoBytes: limits?.max_video_bytes,
        mediaTypes: limits?.media_types,
      });
      setAttachments((current) =>
        [...current, ...result.attachments].slice(0, maximum),
      );
      setComposerNotice(
        result.rejected.length ? result.rejected.join(" · ") : null,
      );
    } catch (cause) {
      setComposerNotice((cause as Error).message);
    }
  }
  function removeAttachment(id: string) {
    setAttachments((current) =>
      current.filter((attachment) => attachment.id !== id),
    );
    setComposerNotice(null);
  }

  function removePaste(id: number) {
    const paste = pastes.get(id);
    if (!paste) return;
    setPrompt((current) =>
      current.replace(paste.token, "").replace(/ {2,}/g, " "),
    );
    setPastes((current) => {
      const next = new Map(current);
      next.delete(id);
      return next;
    });
    if (editingPaste?.id === id) setEditingPaste(null);
  }

  function openPasteEditor(id: number) {
    const paste = pastes.get(id);
    if (!paste) return;
    setEditingPaste({ id, draft: paste.content });
  }

  function closePasteEditor() {
    // Focus must travel INPUT -> INPUT. Unmounting the dialog first drops focus
    // to <body>, iOS takes the keyboard down with its full animation, and the
    // deferred composer focus drags it straight back up — the shell bounces
    // twice for a plain cancel. Focus the composer while the dialog is STILL
    // mounted and the keyboard is handed over without a frame of "nothing is
    // focused"; `preventScroll` keeps the transcript where it is.
    composerRef.current?.focus({ preventScroll: true });
    setEditingPaste(null);
  }

  function savePasteEdit() {
    const editing = editingPaste;
    if (!editing) return;
    const previous = pastes.get(editing.id);
    if (!previous) {
      closePasteEditor();
      return;
    }
    const content = editing.draft;
    if (!content.trim()) {
      removePaste(editing.id);
      closePasteEditor();
      return;
    }
    // The token carries the line/byte count, so an edit rewrites it. Swap the OLD
    // token for the new one in place — literal split/join, never a pattern, so a
    // token containing regex metacharacters can never corrupt the prompt.
    if (!shouldCollapsePaste(content)) {
      // Edited down to something that reads fine inline: drop the chip and let the
      // text live in the composer, exactly as if it had been typed.
      setPrompt((current) => current.split(previous.token).join(content));
      setPastes((current) => {
        const next = new Map(current);
        next.delete(editing.id);
        return next;
      });
      closePasteEditor();
      return;
    }
    const updated = createComposerPaste(editing.id, content);
    setPrompt((current) => current.split(previous.token).join(updated.token));
    setPastes((current) => new Map(current).set(updated.id, updated));
    closePasteEditor();
  }

  async function addPastedMedia(files: File[]) {
    const limits = capabilities?.features.attachments;
    const maximum = limits?.max_files ?? 8;
    const remaining = maximum - attachments.length;
    if (remaining <= 0) {
      setComposerNotice(`You can attach up to ${maximum} files`);
      return;
    }
    try {
      const result = await attachmentsFromFiles(files, {
        maxFiles: remaining,
        maxFileBytes: limits?.max_file_bytes ?? 25 * 1024 * 1024,
        maxVideoBytes: limits?.max_video_bytes,
        mediaTypes: limits?.media_types,
      });
      setAttachments((current) =>
        [...current, ...result.attachments].slice(0, maximum),
      );
      setComposerNotice(
        result.rejected.length ? result.rejected.join(" · ") : null,
      );
    } catch (cause) {
      setComposerNotice((cause as Error).message);
    }
  }

  // A page of a DOCUMENT arrives here as fresh PNG bytes named after the page it
  // came from (`report-p3.png`). It becomes a NEW pending attachment rather than
  // replacing anything: `vis_attach` keeps the PDF or the HTML page itself off
  // the wire, so the captured — and possibly drawn-on — picture is the only
  // thing that can carry its content to the model, and the name is what says
  // which page that was. A refusal THROWS because the human is still standing in
  // the viewer: the message belongs there, not only in the composer behind it.
  const attachCapturedImage = useCallback(
    async (image: Blob, filename: string) => {
      const limits = capabilities?.features.attachments;
      const maximum = limits?.max_files ?? 8;
      if (attachments.length >= maximum)
        throw new Error(`You can attach up to ${maximum} files`);
      const result = await attachmentsFromFiles(
        [new File([image], filename, { type: image.type || "image/png" })],
        {
          maxFiles: 1,
          maxFileBytes: limits?.max_file_bytes ?? 25 * 1024 * 1024,
          maxVideoBytes: limits?.max_video_bytes,
          mediaTypes: limits?.media_types,
        },
      );
      const attached = result.attachments[0];
      if (!attached)
        throw new Error(
          result.rejected[0] ?? "This page could not be attached",
        );
      setAttachments((current) => [...current, attached].slice(0, maximum));
      setComposerNotice(`${attached.filename} is attached to your message`);
    },
    [attachments.length, capabilities],
  );

  // A picture that has not been sent yet is still EDITABLE: the viewer flattens
  // the annotations into fresh bytes and they go back into the same composer
  // slot, so "paste a screenshot, circle the bug, send" stays one gesture chain
  // and the model never receives the un-annotated copy alongside it.
  async function applyAttachmentEdit(id: string, edited: Blob) {
    const target = attachments.find((entry) => entry.id === id);
    if (!target) return;
    const limits = capabilities?.features.attachments;
    try {
      const next = await editedAttachment(target, edited, {
        maxFileBytes: limits?.max_file_bytes,
        maxVideoBytes: limits?.max_video_bytes,
        mediaTypes: limits?.media_types,
      });
      setAttachments((current) =>
        current.map((entry) => (entry.id === id ? next : entry)),
      );
      setComposerNotice(null);
    } catch (cause) {
      setComposerNotice(`${target.filename}: ${(cause as Error).message}`);
    }
  }

  function handlePaste(event: ReactClipboardEvent<HTMLTextAreaElement>) {
    // Media paste (screenshots, copied pictures, a clip) — works on web and in
    // the iOS/Android WKWebView, which surface pasted media as clipboard files.
    // The gateway's own list is the authority; this only keeps text out.
    const mediaFiles = Array.from(event.clipboardData.files).filter(
      (file) => file.type.startsWith("image/") || isVideoMediaType(file.type),
    );
    if (mediaFiles.length) {
      event.preventDefault();
      void addPastedMedia(mediaFiles);
      return;
    }

    const content = event.clipboardData
      .getData("text/plain")
      .replace(/\r\n?/g, "\n");
    if (!content || !shouldCollapsePaste(content)) return;
    event.preventDefault();

    const id = ++pasteCounterRef.current;
    const paste = createComposerPaste(id, content);
    const input = event.currentTarget;
    const start = input.selectionStart ?? prompt.length;
    const end = input.selectionEnd ?? start;
    const nextPrompt = `${prompt.slice(0, start)}${paste.token}${prompt.slice(end)}`;
    setPastes((current) => new Map(current).set(id, paste));
    setPrompt(nextPrompt);
    window.requestAnimationFrame(() => {
      const caret = start + paste.token.length;
      composerRef.current?.setSelectionRange(caret, caret);
    });
  }

  async function toggleVoice() {
    setVoiceRequested(true);
    setComposerNotice(null);

    if (recordingRef.current) {
      await finishVoice();
      return;
    }

    if (voiceModel?.status === "downloading") {
      setComposerNotice(
        "Voice model is still downloading — dictation starts when it lands.",
      );
      return;
    }

    // The microphone is acquired FIRST, inside the click's own user-gesture
    // window. iOS/WKWebView only honours getUserMedia + AudioContext.resume()
    // there; probing the model first put a network round trip in between, and
    // the context then came up suspended — onaudioprocess never fired, the WAV
    // was silence, and the empty transcript looked like a dead button. That
    // round trip is slowest exactly while a turn streams, which is when this
    // was reported.
    let recording: WavRecording | null = null;
    try {
      recording = await startWavRecording({
        // The mic can die without ending the turn: suspension, a call, another
        // app. Close the sentence on the spot instead of leaving a recorder
        // nobody will ever stop.
        onInterrupted: (reason) =>
          void finishVoiceRef.current({ notice: reason }),
      });
      // A gateway we cannot REACH is not a reason to refuse dictation: capture,
      // resampling and WAV encoding are entirely local, and the result queues in
      // the voice outbox until the link is back. Only a gateway that ANSWERS
      // "not ready" can stop a recording.
      let model = voiceModel;
      if (model?.status !== "ready") {
        let reachable = true;
        try {
          model = await client.voiceModel(sid, true);
          setVoiceModel(model);
        } catch {
          reachable = false;
        }
        if (!reachable) {
          setComposerNotice(
            "Gateway unreachable — recording anyway; it transcribes once it answers.",
          );
        } else if (model && model.status !== "ready") {
          await recording.cancel();
          setComposerNotice(
            model.status === "downloading"
              ? "Downloading the voice model — dictation starts when it lands."
              : model.status === "failed"
                ? `Voice model failed${model.error ? ` · ${model.error}` : ""}`
                : "Voice model is not ready yet.",
          );
          return;
        }
      }
      recordingRef.current = recording;
      setVoicePhase("recording");
    } catch (cause) {
      await recording?.cancel().catch(() => {});
      setVoicePhase("idle");
      setComposerNotice((cause as Error).message);
    }
  }

  async function send() {
    const authoredRequest = prompt.trim();
    const request =
      expandFileMentions(expandPastePlaceholders(authoredRequest, pastes)) ||
      (attachments.length ? "Please inspect the attached image(s)." : "");
    const displayRequest =
      collapsePastePlaceholders(authoredRequest, pastes) || request;
    if (!request || voicePhase !== "idle") return;

    const [command = "", ...argParts] = authoredRequest.split(/\s+/);
    const args = argParts.join(" ");

    if (command === "/help") {
      setPrompt("/");
      setSlashDismissed(false);
      setSlashIndex(0);
      return;
    }

    if (command === "/sessions") {
      setPrompt("");
      onBack();
      return;
    }

    if (command === "/new-session") {
      setPrompt("");
      setError(null);
      setRunning(true);
      try {
        const created = await client.createSession({ channel: "web" });
        if (command === "/new-session" && args)
          await client.submitTurn(created.id, args);
        onOpenSession(created.id, true);
      } catch (cause) {
        setPrompt(request);
        setError((cause as Error).message);
      } finally {
        setRunning(false);
      }
      return;
    }

    // A turn is already running: the gateway enqueues this behind it and mirrors
    // it back as `turn.queued`, which fills the tray. Keep the composer live.
    //
    // `liveTurn` is part of the test, not just `running`: a turn whose stream we
    // joined late (started from the TUI, adopted on wake, replayed without a
    // `turn.started` frame) streams into the live bubble with `running` still
    // false. Testing only `running` then took the FRESH-submit path — it painted
    // an optimistic bubble OVER the answer the user was watching (the live view
    // "reset") while the gateway queued that very message, so the same text also
    // appeared in the tray.
    if (running || liveTurn || queued.length) {
      const pendingAttachments = attachments;
      const pendingPastes = pastes;
      setPrompt("");
      setAttachments([]);
      setPastes(new Map());
      setComposerNotice(null);
      setSlashDismissed(false);
      setError(null);
      try {
        const sent: GatewayAttachment[] = pendingAttachments.map(
          ({ filename, media_type, base64 }) => ({
            filename,
            media_type,
            base64,
          }),
        );
        const submitted = await client.submitTurn(sid, request, {
          displayRequest,
          attachments: sent,
        });
        const queuedId = submitted.turn_id ?? submitted.id;
        rememberSent(queuedId, sent);
        // Keep the AUTHORED shape (raw text, pastes, image bytes) so a cancel can
        // hand it straight back to the composer instead of losing it with the
        // backlog the gateway drops.
        rememberQueued(queuedId, {
          request: authoredRequest,
          attachments: pendingAttachments,
          pastes: pendingPastes,
        });
        // The gateway was IDLE after all: a lingering bubble (kept on purpose when
        // a finished answer has not reached the transcript yet) made this look
        // busy, and the turn STARTED instead of queueing. Paint the live rail now
        // instead of waiting for `turn.started` to come back around.
        if (!isQueuedSubmission(submitted)) {
          setRunning(true);
          setLiveTurn({
            id: queuedId,
            request: displayRequest,
            answer: "",
            iterations: [],
            startedAt: Date.now(),
            status: "running",
            attachments: sent.length ? sent : undefined,
          });
          // This bubble goes into the TRANSCRIPT, so ride it down on the settle
          // schedule: a single scroll measures the height before the deferred
          // segments land, and its own late scroll event then clears
          // `followingRef`. A QUEUED submission needs none of this — its tray row
          // renders in the footer, above the composer, always in view.
          pinToEnd();
        }
      } catch (cause) {
        setPrompt(authoredRequest);
        setPastes(pendingPastes);
        setAttachments((current) =>
          current.length ? current : pendingAttachments,
        );
        setError((cause as Error).message);
        requestAnimationFrame(() => composerRef.current?.focus());
      }
      return;
    }

    const pendingAttachments = attachments;
    const pendingPastes = pastes;
    // The rail as it stood BEFORE the optimistic bubble. If the gateway answers
    // "queued", this submission never owned the rail and whatever was streaming
    // must come back rather than stay overwritten.
    const previousLive = liveTurn;
    setPrompt("");
    setAttachments([]);
    setPastes(new Map());
    setComposerNotice(null);
    setSlashDismissed(false);
    setError(null);
    setRunning(true);
    const sent: GatewayAttachment[] = pendingAttachments.map(
      ({ filename, media_type, base64 }) => ({
        filename,
        media_type,
        base64,
      }),
    );
    setLiveTurn({
      request: displayRequest,
      answer: "",
      iterations: [],
      startedAt: Date.now(),
      status: "running",
      attachments: sent.length ? sent : undefined,
    });
    // The optimistic bubble has just been added to the transcript: same settle
    // schedule, for the same reason one frame is not enough.
    pinToEnd();

    try {
      const submitted = await client.submitTurn(sid, request, {
        displayRequest,
        attachments: sent,
      });
      const submittedId = submitted.turn_id ?? submitted.id;
      rememberSent(submittedId, sent);
      if (isQueuedSubmission(submitted)) {
        // The gateway QUEUED us: a turn started between our liveness read and this
        // POST (another channel, or a queued row draining). A queued message is
        // the tray's alone, so the optimistic bubble goes — left up it showed the
        // message as SENT while the same text sat in the tray, on top of a rail it
        // had already taken from the turn actually running.
        rememberQueued(submittedId, {
          request: authoredRequest,
          attachments: pendingAttachments,
          pastes: pendingPastes,
        });
        setLiveTurn((turn) => {
          // Only OUR optimistic bubble is reverted — it is the one with no id. A
          // `turn.started` that landed while the POST was in flight has already
          // replaced it and owns the rail.
          const next = turn && !turn.id ? previousLive : turn;
          liveTurnRef.current = next;
          return next;
        });
      } else {
        setLiveTurn((turn) => (turn ? { ...turn, id: submittedId } : turn));
      }
    } catch (cause) {
      setRunning(false);
      setLiveTurn(null);
      setPrompt(authoredRequest);
      setPastes(pendingPastes);
      setAttachments((current) =>
        current.length ? current : pendingAttachments,
      );
      setError((cause as Error).message);
      requestAnimationFrame(() => composerRef.current?.focus());
    }
  }

  // Cancel is "stop", not "stop and then run the rest": the gateway terminally drops
  // every turn queued BEFORE a user cancel and mirrors each drop back as a
  // `turn.queued.deleted` with reason `cancelled`. `restoreCancelledQueued` is the
  // ONE place that puts those words back in the composer; the local pass below only
  // gets there first (and carries this device's pastes and image bytes).

  async function cancel() {
    // One stop is one stop. A second press (button re-tap, Escape) while the
    // request is in flight would re-announce a state the transcript is already
    // showing as "Vis is cancelling".
    if (liveTurnRef.current?.cancelling) return;
    // Snapshot BEFORE the request: every row queued from here on is the opposite
    // intent ("stop that, run THIS"), survives the drop server-side and drains on
    // its own — stealing it into the composer would send it twice.
    const backlog = queued;
    setLiveTurn((turn) => {
      const next = turn
        ? { ...turn, cancelling: true, activity: undefined }
        : turn;
      liveTurnRef.current = next;
      return next;
    });
    try {
      await client.cancelCurrentTurn(sid);
      for (const row of backlog)
        restoreCancelledQueued(row.turnId, row.request);
      requestAnimationFrame(() => composerRef.current?.focus());
    } catch (cause) {
      // The stop never landed, so the affordance has to come back.
      setLiveTurn((turn) => {
        const next = turn ? { ...turn, cancelling: false } : turn;
        liveTurnRef.current = next;
        return next;
      });
      setError((cause as Error).message);
    }
  }

  function handleDisclosureClick(event: ReactMouseEvent<HTMLDivElement>) {
    const target = event.target;
    const viewport = scrollRef.current;
    if (!(target instanceof Element) || !viewport) return;

    const disclosure = target.closest("summary, [data-disclosure-toggle]");
    if (!disclosure || !viewport.contains(disclosure)) return;

    const anchorTop = disclosure.getBoundingClientRect().top;
    followingRef.current = false;
    if (disclosureScrollFrameRef.current !== null) {
      window.cancelAnimationFrame(disclosureScrollFrameRef.current);
    }

    const preserveAnchor = () => {
      const activeViewport = scrollRef.current;
      if (!activeViewport || !disclosure.isConnected) return;
      const shift = disclosure.getBoundingClientRect().top - anchorTop;
      if (Math.abs(shift) > 0.5) activeViewport.scrollTop += shift;
    };

    disclosureScrollFrameRef.current = window.requestAnimationFrame(() => {
      preserveAnchor();
      disclosureScrollFrameRef.current = window.requestAnimationFrame(() => {
        disclosureScrollFrameRef.current = null;
        preserveAnchor();
        const activeViewport = scrollRef.current;
        if (!activeViewport) return;
        const distance =
          activeViewport.scrollHeight -
          activeViewport.scrollTop -
          activeViewport.clientHeight;
        const following = distance < 64;
        followingRef.current = following;
        if (showJumpRef.current !== !following) {
          showJumpRef.current = !following;
          setShowJump(!following);
        }
      });
    });
  }

  // Tapping the composer must not move the conversation. The keyboard slides up
  // over ~300ms and the shell shrinks with it, so the scroller loses height in
  // several steps while the transcript keeps its own — the reader who was parked
  // at the end ends up looking at the middle. Worse, the first shrink can be
  // observed as a large distance-to-bottom and clear `followingRef`, which then
  // vetoes the catch-up. Re-pin instead, on the same settle schedule the session
  // opens with, but ONLY for a reader who was already at the bottom: someone
  // reading history and tapping reply keeps their place.
  function handleComposerFocus() {
    if (followingRef.current) pinToEnd();
  }

  function handleScroll() {
    // Scroll fires faster than the screen can paint. Batch its geometry reads and
    // anchor scan into one frame to avoid forcing repeated transcript layouts.
    if (scrollMetricsFrameRef.current !== null) return;
    scrollMetricsFrameRef.current = window.requestAnimationFrame(() => {
      scrollMetricsFrameRef.current = null;
      const viewport = scrollRef.current;
      if (!viewport) return;
      // Rotation owns the scroll position through its terminal anchor restore.
      // A stale scroll event from a reflow must not change following or overwrite
      // the snapshot while that transaction is pending.
      if (isViewportRotating() || rotationRestorePendingRef.current) return;
      // The one opening correction has already happened; subsequent scroll events
      // belong to the reader and are measured normally.
      const distance =
        viewport.scrollHeight - viewport.scrollTop - viewport.clientHeight;
      const following = distance < 64;
      followingRef.current = following;
      if (showJumpRef.current !== !following) {
        showJumpRef.current = !following;
        setShowJump(!following);
      }
      // Keep the rotation anchor fresh: iOS can deliver the orientation signal
      // AFTER the reflow, and by then the top-most turn is already unreadable.
      // Scrolls during a rotation are the reflow's own, never the reader's.
      if (!isViewportRotating()) captureScrollAnchor();
    });
  }

  useEffect(() => {
    if (!running) return;
    const onKeyDown = (event: KeyboardEvent) => {
      if (event.key !== "Escape") return;
      event.preventDefault();
      cancelRef.current();
    };
    window.addEventListener("keydown", onKeyDown);
    return () => window.removeEventListener("keydown", onKeyDown);
  }, [running]);

  const slashText = prompt.trimStart();
  const slashOpen =
    !slashDismissed &&
    slashText.startsWith("/") &&
    !slashText.startsWith("//") &&
    !slashText.includes("\n");
  const slashQuery = slashText.toLowerCase();
  const slashMatches = slashOpen
    ? slashCommands.filter((command) =>
        command.name.toLowerCase().startsWith(slashQuery),
      )
    : [];
  const selectedSlash =
    slashMatches[Math.min(slashIndex, Math.max(0, slashMatches.length - 1))];

  function completeSlash(command: SlashCommand) {
    const noArgs = new Set(["/help", "/sessions"]);
    const completed = command.name + (noArgs.has(command.name) ? "" : " ");
    setPrompt(completed);
    setSlashIndex(0);
    setSlashDismissed(noArgs.has(command.name));
    requestAnimationFrame(() => {
      const element = composerRef.current;
      if (!element) return;
      element.focus();
      // Park the caret at the end of the completed command. Mirrors completeFile:
      // without this, iOS keeps the native selection where "/" was (inside the
      // freshly written word), so the virtual keyboard fires autocorrect and
      // inserts at the wrong spot when a command is tapped rather than Enter-ed.
      element.setSelectionRange(completed.length, completed.length);
      setCaret(completed.length);
    });
  }

  // `@` file-mention picker — the SAME fuzzy index the TUI composer uses,
  // served by GET /v1/sessions/:sid/suggest. The trigger smarts live here (never
  // the gateway), so a literal `@@` is never endangered.
  const caretPos = Math.min(caret, prompt.length);
  const fileMention = !slashOpen
    ? fileMentionAt(prompt.slice(0, caretPos))
    : null;
  const fileOpen = fileMention !== null && !fileDismissed;
  const fileQuery = fileMention?.query ?? "";
  const fileMatches = fileOpen ? fileSuggestions : [];
  const selectedFile =
    fileMatches[Math.min(fileIndex, Math.max(0, fileMatches.length - 1))];

  useEffect(() => {
    // `fileMatches` above already renders nothing while the menu is closed, so
    // this effect never has to clear suggestion state.
    if (!fileOpen) return;
    const controller = new AbortController();
    const timer = window.setTimeout(() => {
      void client
        .suggestFiles(sid, fileQuery, controller.signal)
        .then((rows) => setFileSuggestions(rows))
        .catch(() => {
          /* keep the last rows on a transient failure */
        });
    }, 90);
    return () => {
      controller.abort();
      window.clearTimeout(timer);
    };
  }, [client, sid, fileOpen, fileQuery]);

  function completeFile(path: string) {
    const spliced = applyFileMention(prompt, caretPos, path);
    setPrompt(spliced.text);
    setFileIndex(0);
    setFileDismissed(true);
    requestAnimationFrame(() => {
      const element = composerRef.current;
      if (!element) return;
      element.focus();
      element.setSelectionRange(spliced.caret, spliced.caret);
      setCaret(spliced.caret);
    });
  }

  const activePastes = Array.from(pastes.values()).filter((paste) =>
    prompt.includes(paste.token),
  );
  const title = session?.title?.trim() || "Chat";
  // A draft is a per-session clone of the project — an isolated agent
  // workspace, NOT the unsent composer text (that is a "draft message"). The
  // header names it so the operator always knows the session is parked in a
  // draft and not on the project itself; nothing renders here otherwise.
  const draftName =
    session && isDraftWorkspace(session)
      ? session.workspace?.label?.trim() ?? ""
      : "";
  // Cumulative session usage — the SAME fold the TUI footer runs over its message
  // vector (`footer/session-usage`), so both surfaces read one number. Memoized on
  // the transcript identity: it only moves when a turn lands, never per keystroke.
  const usage = useMemo(() => sessionUsage(turns), [turns]);
  const usageTokens = formatTokens(usage);
  const usageCost = formatCost(usage.cost);
  const usageTitle = [
    `${usage.input.toLocaleString()} input`,
    `${usage.output.toLocaleString()} output`,
    usage.cached > 0 ? `${usage.cached.toLocaleString()} cached` : null,
    usage.cost > 0 ? exactCost(usage.cost) : null,
    `${usage.turns} turn${usage.turns === 1 ? "" : "s"}`,
  ]
    .filter((part): part is string => Boolean(part))
    .join(" · ");
  const visibleStart = Math.max(0, turns.length - visibleTurnCount);
  // What is mounted this frame: the window, clamped by the hydration ramp.
  const renderStart = Math.max(visibleStart, turns.length - hydratedTurnCount);
  // Everything older than the first bubble on screen, wherever it lives.
  const earlierTotal = visibleStart + earlierRemaining;
  const liveTurnId = liveTurn?.id;
  // While a live turn streams, drop the transcript's own copy of that same turn
  // (a running turn is persisted as a bare 'running' row) so it isn't rendered
  // twice — the live bubble owns it until `settle` confirms the finished row.
  const visibleTurns = useMemo(
    () =>
      turns.slice(renderStart).filter((turn) => {
        // A persisted 'running' row is a placeholder, not a result. Painted from
        // the cache — reopening a session you already left — it resurrects the
        // working spinner and its elapsed clock for a turn that has since been
        // cancelled or finished, until the refetch lands seconds later. Only a
        // transcript confirmed against the gateway this visit may show one.
        if (isRunningRow(turn) && !turnsFresh) return false;
        if (!liveTurn) return true;
        const id = turn.id ?? turn.turn_id;
        // Same turn by id — the live bubble owns it.
        if (liveTurnId && id === liveTurnId) return false;
        // The persisted 'running' row is the very turn being streamed live, even
        // when its id can't be matched (e.g. turn.started replayed without a
        // turn_id). Only one turn runs per session, so drop it to avoid a dup.
        if (isRunningRow(turn)) return false;
        return true;
      }),
    [turns, renderStart, liveTurn, liveTurnId, turnsFresh],
  );
  // Memoized rows keep their element IDENTITY across composer keystrokes
  // (prompt/caret state), so React bails out of the whole transcript subtree
  // instead of re-reconciling every turn wrapper on each keypress — that
  // reconciliation was what made `/` and `@` completion typing lag.
  // A persisted turn row may still read `status: 'running'` — the gateway writes
  // that row at submit and the transcript we hold can predate the terminal frame.
  // Rendered naively it keeps a "Vis is thinking…" ticker (and its elapsed clock)
  // alive under a turn that ended. Once this session is no longer live, nothing
  // in the transcript is running: settle every row. Same rule as the TUI, which
  // treats the terminal frame — not the persisted status — as the end of a turn.
  // A live bubble that ALREADY settled (terminal frame in, persisted row not yet
  // fetched) is not work in flight: treating it as such kept every persisted
  // 'running' row spinning with its elapsed clock until the refetch landed.
  const turnsSettled = !running && (!liveTurn || liveTurn.status !== "running");
  // What the composer may advertise as in-flight. `running` on its own is a latch:
  // set optimistically at submit, cleared by the terminal frame or the 5s
  // reconcile. Pairing it with the bubble's own status means a settled turn can
  // never leave a spinner and a growing elapsed counter in the footer.
  const activeWork = running && (!liveTurn || liveTurn.status === "running");
  // Only ONE turn can be in flight per session, and when a live bubble exists it
  // owns it. With a queue draining, the gateway starts the next turn while the
  // persisted row of the PREVIOUS one still reads `running` — rendered naively
  // that finished row kept its live ticker ("Vis is thinking (iter N)") alive
  // right above its own model/usage footer, so the ticker looked like it had
  // flown into the finished turn's meta line. A row is therefore live-eligible
  // only when it is the LAST visible row and no live bubble is painting.
  // Kept as a boolean (not `liveTurn`) so streaming frames don't re-run the memo.
  const hasLiveBubble = liveTurn != null;
  const turnRows = useMemo(
    () =>
      visibleTurns.map((turn, index) => {
        const request = turn.user_request ?? turn.request ?? "";
        // No content-visibility deferral: the DOM is already bounded to
        // INITIAL_VISIBLE_TURNS by pagination, and deferred turns rendered
        // white placeholder bands (plus a synchronous render hitch) when
        // fast-scrolling up into them on iOS — the 480px intrinsic-size guess
        // never matched the real height, so the scroll position shifted too.
        return (
          <div
            className={index === 0 ? "" : "mt-10"}
            key={turn.id ?? turn.turn_id}
          >
            {(request || (turn.attachments?.length ?? 0) > 0) && (
              <UserMessage attachments={turn.attachments}>
                {request}
              </UserMessage>
            )}
            <AssistantMessage
              turn={turn}
              settled={
                turnsSettled || hasLiveBubble || index < visibleTurns.length - 1
              }
              client={client}
              sid={sid}
            />
          </div>
        );
      }),
    [visibleTurns, turnsSettled, hasLiveBubble, client, sid],
  );
  // The sender's own copy of the pictures dies with the process. Ask the gateway
  // for the bytes of a live turn that has none in hand — a restarted app, or a
  // second device, has no other source until the turn lands and is refetched.
  const [fetchedLiveAttachments, setFetchedLiveAttachments] = useState<{
    id: string;
    rows: GatewayAttachment[];
  } | null>(null);
  const liveTurnAttachments = liveTurn?.attachments;
  useEffect(() => {
    if (!liveTurnId || liveTurnAttachments?.length) return;
    if (client.cachedSentAttachments(sid, liveTurnId)?.length) return;
    const controller = new AbortController();
    let cancelled = false;
    void client
      .fetchTurnAttachments(sid, liveTurnId, controller.signal)
      .then((rows) => {
        if (!cancelled && rows.length)
          setFetchedLiveAttachments({ id: liveTurnId, rows });
      })
      .catch(() => {});
    return () => {
      cancelled = true;
      controller.abort();
    };
  }, [client, sid, liveTurnId, liveTurnAttachments]);

  const liveRow = useMemo(() => {
    if (!liveTurn) return null;
    // A screenshot just sent lives only in this device's memory until the turn
    // is persisted: the live rail and the queue tray ship no attachment bytes.
    const liveAttachments =
      liveTurn.attachments ??
      client.cachedSentAttachments(sid, liveTurn.id) ??
      (fetchedLiveAttachments?.id === liveTurn.id
        ? fetchedLiveAttachments?.rows
        : undefined);
    return (
      <div
        className={`${turns.length ? "mt-10 " : ""}${transcriptEnterClass}`}
        data-live="true"
      >
        {(liveTurn.request || (liveAttachments?.length ?? 0) > 0) && (
          <UserMessage attachments={liveAttachments}>
            {liveTurn.request}
          </UserMessage>
        )}
        <AssistantMessage
          turn={{
            id: liveTurn.id ?? "live",
            request: liveTurn.request,
            status: liveTurn.status,
            iterations: liveTurn.iterations,
            content:
              liveTurn.content ??
              (liveTurn.answer
                ? [
                    {
                      id: "live-answer",
                      type: "prose",
                      markdown: liveTurn.answer,
                    },
                  ]
                : []),
          }}
          streaming={liveTurn.status === "running"}
          activity={liveProgressPhase(liveTurn, connected, [
            session?.workspace?.root,
            session?.workspace?.repo_root,
          ])}
          startedAt={liveTurn.startedAt}
          client={client}
          sid={sid}
        />
      </div>
    );
  }, [
    liveTurn,
    turns.length,
    client,
    sid,
    connected,
    fetchedLiveAttachments,
    session?.workspace?.root,
    session?.workspace?.repo_root,
  ]);
  // Rows are about to land ABOVE the viewport. Stopping the follow is all this
  // has to do: the anchor observer holds the reader's line for every mutation.
  const anchorPrepend = () => {
    followingRef.current = false;
  };

  // "Earlier" has two sources: rows already fetched but hidden by the render
  // window, and history still on the gateway. Reveal local rows first — that is
  // free — and only page the daemon once they run out.
  const loadEarlierTurns = () => {
    if (visibleStart > 0) {
      anchorPrepend();
      setVisibleTurnCount((count) =>
        Math.min(turns.length, count + INITIAL_VISIBLE_TURNS),
      );
      return;
    }
    if (loadingEarlier || earlierRemaining <= 0) return;
    setLoadingEarlier(true);
    const held = turns.length;
    void client
      .transcriptEarlier(sid)
      .then((older) => {
        if (!older) return;
        anchorPrepend();
        // Keep every bubble that was on screen, plus the page that just landed.
        setVisibleTurnCount(
          (count) => count + Math.max(0, older.length - held),
        );
        setTurns(older);
        setEarlierRemaining(client.transcriptWindow(sid).offset);
      })
      .catch((cause: unknown) => setError((cause as Error).message))
      .finally(() => setLoadingEarlier(false));
  };

  return (
    <AttachImageContext.Provider value={attachCapturedImage}>
      <section className="relative flex h-full min-h-0 flex-col overflow-hidden bg-ink transition-[opacity,transform,translate,scale,rotate] duration-200 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none">
        {/* A run BLOCKED on the operator (`vis.request_human_input`) parks until it
         is answered. The prompt portals its own overlay, so it sits here purely
         to be mounted for this session — the TUI shows the same form. */}
        <HumanInputPrompt
          client={client}
          subscriptions={subscriptions}
          sid={sid}
        />
        {/* In landscape on a notched phone the horizontal safe-area insets are the
         ones that bite (the notch on one side, the rounded corner on the other),
         and this header used to pad only the TOP — so the back button and the
         share/id cluster ran under the bezel and off the visible screen. The
         insets live on the two edge children, not on the header, so the dark
         back-button block and the panel background still reach the physical
         edge; only their CONTENT is pushed into the safe area. */}
        <header className="z-10 flex min-h-13 shrink-0 items-stretch gap-0 border-b border-dialog-edge bg-panel-2 pt-[env(safe-area-inset-top)]">
          <button
            type="button"
            className="grid w-[calc(2.75rem+env(safe-area-inset-left))] shrink-0 place-items-center border-r border-dialog-edge bg-dialog-title pl-[env(safe-area-inset-left)] font-mono text-subhead font-bold text-dialog-title-foreground transition-[background-color,transform,translate,scale,rotate] duration-150 active:scale-[0.96] hover:bg-accent-2 focus-visible:outline-none focus-visible:bg-accent-2 motion-reduce:transition-none mouse:w-[calc(2.5rem+env(safe-area-inset-left))]"
            onClick={onBack}
            aria-label="Back to sessions"
          >
            <ChevronIcon className="size-4" aria-hidden />
          </button>
          <div className="min-w-0 flex-1 self-center px-3 py-1.5">
            <h1 className="truncate font-mono text-body font-bold text-white">
              {title}
            </h1>
            <div className="flex min-w-0 items-center gap-1.5 font-mono text-meta text-dialog-hint">
              <span
                className={`size-1.5 shrink-0 ${connected ? "bg-ok" : "animate-pulse bg-turn-edge motion-reduce:animate-none"}`}
              />
              <span className="shrink-0">{connected ? "Connected" : "Reconnecting"}</span>
              {draftName !== "" && (
                <>
                  <span className="shrink-0 opacity-40" aria-hidden="true">·</span>
                  <span
                    className="inline-flex min-w-0 items-center truncate font-bold uppercase tracking-[0.08em] text-warn-strong"
                    title={session?.workspace?.root}
                  >
                    draft {draftName || ""}
                  </span>
                </>
              )}
            </div>
          </div>
          <div className="flex shrink-0 items-center gap-1 self-center pl-1 pr-[max(0.5rem,env(safe-area-inset-right))] sm:pr-[max(0.75rem,env(safe-area-inset-right))]">
            <CopyableId
              id={sid}
              className="hidden max-w-[9rem] sm:inline-flex"
            />
            <ArtifactsChip
              count={artifacts.length}
              open={artifactsOpen}
              onToggle={() => setArtifactsOpen((was) => !was)}
            />
          </div>
        </header>

        {routerOpen && (
          <ProviderRouterDialog
            client={client}
            sid={sid}
            onClose={() => setRouterOpen(false)}
            onPicked={setModelPref}
            onManageProviders={onManageProviders}
          />
        )}

        {editingPaste && (
          <PasteEditor
            editingPaste={editingPaste}
            onDraftChange={(draft) =>
              setEditingPaste({ id: editingPaste.id, draft })
            }
            onClose={closePasteEditor}
            onSave={savePasteEdit}
          />
        )}

        <div className="relative flex min-h-0 flex-1 flex-col">
          {/* Layered over the transcript it indexes, INSIDE the same box: the
          artifacts are the session's own output, not another screen. */}
          {artifactsOpen && (
            <ArtifactsSheet
              client={client}
              sid={sid}
              artifacts={artifacts}
              onClose={() => setArtifactsOpen(false)}
            />
          )}
          {/* The scroller is deliberately NOT a live region. role="log" implies
          aria-live="polite", and WebKit answers that by keeping an AXLiveRegionNode
          set for the subtree and re-diffing it on every mutation: with a whole
          transcript inside, a streaming turn pegged the WebContent process at ~100%
          CPU indefinitely (sampled on device: 3505 of 3609 main-thread samples in
          -[UIKitWebAccessibilityObjectWrapper _performLiveRegionUpdate] ->
          -[AXLiveRegionNode isEqual:]). Streaming is announced instead by the small
          sr-only role="status" node each turn renders, which is what a screen reader
          actually wants: one short phase message, not the entire log re-scanned. */}
          <div
            ref={scrollRef}
            className="min-h-0 flex-1 overflow-x-hidden overflow-y-auto overscroll-contain scroll-pb-8 bg-ink [overflow-anchor:none]"
            onClickCapture={handleDisclosureClick}
            onScroll={handleScroll}
            onPointerDown={releasePin}
            onWheel={releasePin}
            onTouchMove={releasePin}
            role="region"
            aria-label="Transcript"
          >
            <div
              ref={transcriptRef}
              className={`mx-auto min-h-full w-full max-w-3xl pl-[max(0.875rem,env(safe-area-inset-left))] pr-[max(0.875rem,env(safe-area-inset-right))] pt-4 sm:pl-[max(1.5rem,env(safe-area-inset-left))] sm:pr-[max(1.5rem,env(safe-area-inset-right))] sm:pt-6 ${
                !turns.length && !liveTurn
                  ? "flex flex-col pb-4 sm:pb-6"
                  : "pb-10"
              }`}
            >
              {error && <Banner kind="err">{error}</Banner>}

              <>
                {/* Keep the new-session cue fluid: it shares the scroller's available
              height, including when a software keyboard changes it. The mark is
              intentionally unframed so the brand feels like part of the canvas,
              not a small dialog inside it. */}
                {!turns.length && !liveTurn ? (
                  <div className="flex min-h-0 flex-1 flex-col items-center justify-center px-3 text-center transition-[opacity,transform,translate,scale,rotate] duration-300 starting:translate-y-2 starting:opacity-0 motion-reduce:transition-none">
                    <img
                      src="/vis-logo.png"
                      alt=""
                      className="w-14 max-w-full object-contain sm:w-16"
                      aria-hidden="true"
                    />
                    <div className="mt-4 max-w-md">
                      <h2 className="text-head font-semibold text-dialog-foreground">
                        What would you like to work on?
                      </h2>
                      <p className="mt-1 text-body text-dialog-hint">
                        Describe a task, ask a question, or add a screenshot to
                        get started.
                      </p>
                    </div>
                  </div>
                ) : null}

                {earlierTotal > 0 && (
                  // Not anchorable: this row is pinned above the history it loads, so
                  // its own top never moves and anchoring on it would hold nothing
                  // while 40 000 px lands underneath it.
                  <div className="mb-5 flex justify-center" data-anchor="skip">
                    <button
                      type="button"
                      className="border border-dialog-edge bg-panel px-3 py-1.5 font-mono text-chip font-bold text-dialog-hint transition-colors hover:border-accent hover:text-dialog-hint-key"
                      onClick={loadEarlierTurns}
                      disabled={loadingEarlier}
                    >
                      {loadingEarlier
                        ? "Loading earlier…"
                        : visibleStart > 0
                          ? `↑ Load ${Math.min(INITIAL_VISIBLE_TURNS, earlierTotal)} earlier · ${earlierTotal} remaining`
                          : `↑ Load earlier · ${earlierTotal} remaining`}
                    </button>
                  </div>
                )}

                {turnRows}

                {liveRow}
              </>
            </div>
          </div>
          {veiled && (
            <div
              aria-hidden={!loading}
              className={`absolute inset-0 z-10 flex items-center justify-center bg-ink transition-opacity duration-200 motion-reduce:transition-none ${
                loading ? "opacity-100" : "pointer-events-none opacity-0"
              }`}
            >
              <LoadingSession />
            </div>
          )}
        </div>

        {/* `sm:pt-2`, never `sm:py-2`: the `sm` variant is emitted after the base
          rules, so a shorthand there would drop `--safe-bottom` — and `sm` is
          exactly where a phone lands when it is turned on its side, home
          indicator included. */}
        <footer
          style={safeBottomStyle}
          className="relative z-10 shrink-0 border-t border-dialog-edge bg-ink pl-[max(0.875rem,env(safe-area-inset-left))] pb-[calc(0.5rem+var(--safe-bottom,env(safe-area-inset-bottom)))] pr-[max(0.875rem,env(safe-area-inset-right))] pt-1.5 sm:pl-[max(1.5rem,env(safe-area-inset-left),calc((100%_-_46rem)/2))] sm:pr-[max(1.5rem,env(safe-area-inset-right),calc((100%_-_46rem)/2))] sm:pt-2"
        >
          {/* Anchored to the footer's top edge, so it always clears the queue
            tray and composer no matter how tall they grow. Hidden while a
            completion list occupies the same strip. */}
          {showJump && !fileMatches.length && !slashMatches.length && (
            <button
              type="button"
              className="absolute bottom-full left-1/2 z-20 mb-2 inline-flex -translate-x-1/2 items-center gap-1.5 border border-dialog-edge bg-button px-3 py-1.5 font-mono text-meta font-bold text-button-foreground shadow-[4px_4px_0_var(--dialog-shadow)] transition-[opacity,transform,translate,scale,rotate,background-color] duration-150 starting:translate-y-2 starting:opacity-0 active:scale-[0.97] motion-reduce:transition-none"
              onClick={() => scrollToEnd("smooth")}
            >
              <ArrowDownIcon />
              Latest
            </button>
          )}

          {fileMatches.length > 0 && (
            <div
              id="file-mention-list"
              role="listbox"
              aria-label="File mentions"
              className="absolute bottom-full left-[max(0.5rem,env(safe-area-inset-left))] right-[max(0.5rem,env(safe-area-inset-right))] mb-1.5 max-h-[min(20rem,55dvh)] overflow-y-auto border border-dialog-edge bg-panel shadow-[6px_6px_0_var(--dialog-shadow)] transition-[opacity,transform,translate,scale,rotate] duration-150 starting:translate-y-2 starting:opacity-0 motion-reduce:transition-none sm:left-[max(1.5rem,env(safe-area-inset-left),calc((100%_-_46rem)/2))] sm:right-[max(1.5rem,env(safe-area-inset-right),calc((100%_-_46rem)/2))] sm:shadow-[8px_8px_0_var(--dialog-shadow)]"
            >
              <div className="bg-dialog-title px-3 py-2 font-mono text-meta font-bold text-dialog-title-foreground">
                Attach a file
              </div>
              {fileMatches.map((file, index) => (
                <button
                  key={file.name}
                  type="button"
                  role="option"
                  aria-selected={index === fileIndex}
                  className={`grid min-h-9 w-full grid-cols-[1fr_auto] items-center gap-3 border-t border-dialog-edge px-3 py-1.5 text-left transition-colors ${
                    index === fileIndex
                      ? "bg-accent text-accent-foreground"
                      : "text-dialog-foreground hover:bg-hover"
                  }`}
                  onPointerDown={(event) => event.preventDefault()}
                  onClick={() => completeFile(file.name)}
                >
                  <code className="truncate font-mono text-ui font-semibold text-accent-ink">
                    {file.name}
                  </code>
                  <span className="shrink-0 font-mono text-chip text-dialog-hint">
                    {[
                      file.size,
                      file.age,
                      file.status && file.status !== "clean" ? file.status : "",
                    ]
                      .filter(Boolean)
                      .join(" · ")}
                  </span>
                </button>
              ))}
            </div>
          )}

          {slashMatches.length > 0 && (
            <div
              id="slash-command-list"
              role="listbox"
              aria-label="Slash commands"
              className="absolute bottom-full left-[max(0.5rem,env(safe-area-inset-left))] right-[max(0.5rem,env(safe-area-inset-right))] mb-1.5 max-h-[min(20rem,55dvh)] overflow-y-auto border border-dialog-edge bg-panel shadow-[6px_6px_0_var(--dialog-shadow)] transition-[opacity,transform,translate,scale,rotate] duration-150 starting:translate-y-2 starting:opacity-0 motion-reduce:transition-none sm:left-[max(1.5rem,env(safe-area-inset-left),calc((100%_-_46rem)/2))] sm:right-[max(1.5rem,env(safe-area-inset-right),calc((100%_-_46rem)/2))] sm:shadow-[8px_8px_0_var(--dialog-shadow)]"
            >
              <div className="bg-dialog-title px-3 py-2 font-mono text-meta font-bold text-dialog-title-foreground">
                Slash commands
              </div>
              {slashMatches.map((command, index) => (
                <button
                  key={command.name}
                  type="button"
                  role="option"
                  aria-selected={index === slashIndex}
                  className={`grid min-h-9 w-full grid-cols-[7.5rem_1fr] items-start gap-3 border-t border-dialog-edge px-3 py-1.5 text-left transition-colors sm:grid-cols-[10rem_1fr] ${
                    index === slashIndex
                      ? "bg-accent text-accent-foreground"
                      : "text-dialog-foreground hover:bg-hover"
                  }`}
                  onPointerDown={(event) => event.preventDefault()}
                  onClick={() => completeSlash(command)}
                >
                  <code className="break-words font-mono text-ui font-semibold text-accent-ink">
                    {command.name}
                  </code>
                  <span className="line-clamp-2 text-meta text-dialog-hint">
                    {command.doc}
                  </span>
                </button>
              ))}
            </div>
          )}

          {queuePaused && (
            <div className="mb-1.5 flex flex-wrap items-center gap-x-2 gap-y-1 border border-warn-strong bg-warn-surface px-2.5 py-1.5 font-mono text-meta text-warn-strong">
              <span
                className="size-1.5 shrink-0 bg-warn-strong"
                aria-hidden="true"
              />
              <span className="font-bold text-warn-strong">Queue paused</span>
              <span className="min-w-0 flex-1 truncate">
                {queuePaused.held} held · {queuePaused.reason.replace(/_/g, " ")}
              </span>
              <button
                type="button"
                disabled={resumingQueue}
                className="shrink-0 border border-warn-strong px-2 py-0.5 font-bold text-warn-strong transition-colors hover:bg-warn-strong hover:text-ink disabled:cursor-not-allowed disabled:opacity-50 disabled:hover:bg-transparent disabled:hover:text-warn-strong"
                onClick={() => {
                  setResumingQueue(true);
                  void client
                    .resumeQueue(sid)
                    .catch((cause) => setError((cause as Error).message))
                    .finally(() => setResumingQueue(false));
                }}
              >
                {resumingQueue ? "Continuing…" : "Continue queue"}
              </button>
            </div>
          )}

          {queued.length > 0 && (
            <div className="mb-1.5 border border-dialog-edge bg-panel">
              <div className="flex items-center gap-1.5 border-b border-dialog-edge bg-dialog-title px-2.5 py-1 font-mono text-meta font-bold text-dialog-title-foreground">
                <span aria-hidden="true">┌</span>
                Queued · {queued.length}
              </div>
              {queued.map((item, index) => {
                const editing = editingQueued?.turnId === item.turnId;
                const busy = queueBusy.has(item.turnId);
                return (
                  <div
                    key={item.turnId}
                    className={`flex items-center gap-2 border-t border-dialog-edge px-2.5 py-1 first:border-t-0 transition-[opacity,transform,translate,scale,rotate] duration-150 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none${busy ? " opacity-50" : ""}`}
                  >
                    <span className="shrink-0 font-mono text-meta font-bold text-accent-ink">
                      #{index + 1}
                    </span>
                    {editing ? (
                      <input
                        autoFocus
                        value={editingQueued.text}
                        onChange={(event) =>
                          setEditingQueued({
                            turnId: item.turnId,
                            text: event.target.value,
                          })
                        }
                        onKeyDown={(event) => {
                          if (event.key === "Enter") {
                            event.preventDefault();
                            const text = editingQueued.text.trim();
                            if (text && text !== item.request) {
                              // The gateway owns the row: it is rewritten here only when
                              // the daemon confirms with `turn.queued.updated`.
                              markQueueBusy(item.turnId, true);
                              void client
                                .updateQueuedTurn(sid, item.turnId, text)
                                .catch((cause) =>
                                  setError((cause as Error).message),
                                )
                                .finally(() =>
                                  markQueueBusy(item.turnId, false),
                                );
                            }
                            setEditingQueued(null);
                          } else if (event.key === "Escape") {
                            event.preventDefault();
                            setEditingQueued(null);
                          }
                        }}
                        onBlur={() => setEditingQueued(null)}
                        className="min-w-0 flex-1 border border-accent bg-input px-1 py-0.5 font-mono text-ui text-dialog-foreground outline-none"
                        aria-label={`Edit queued message ${index + 1}`}
                      />
                    ) : (
                      <button
                        type="button"
                        disabled={busy}
                        onClick={() =>
                          setEditingQueued({
                            turnId: item.turnId,
                            text: item.request,
                          })
                        }
                        className="flex min-w-0 flex-1 items-center gap-1 text-left font-mono text-ui text-dialog-foreground transition-colors hover:text-accent-ink disabled:cursor-not-allowed"
                        title="Tap to edit"
                      >
                        {/* Image chips first: a queued screenshot reads as its filename,
                        never as the raw /var/folders path the OS pasted. */}
                        {item.attachments.map((attachment) => (
                          <span
                            key={attachment.filename}
                            className="inline-flex shrink-0 items-center gap-1 border border-dialog-edge bg-input px-1 text-chip text-dialog-hint"
                            title={`${attachment.filename}${attachment.sizeLabel ? ` · ${attachment.sizeLabel}` : ""}`}
                          >
                            <span className="max-w-[7rem] truncate">
                              {attachment.filename}
                            </span>
                          </span>
                        ))}
                        <span className="min-w-0 flex-1 truncate">
                          {item.preview ||
                            (item.attachments.length ? "" : "(empty)")}
                        </span>
                      </button>
                    )}
                    <button
                      type="button"
                      disabled={busy}
                      className="grid size-6 shrink-0 place-items-center text-dialog-hint transition-colors hover:bg-warn-surface hover:text-err disabled:cursor-not-allowed disabled:hover:bg-transparent disabled:hover:text-dialog-hint"
                      onClick={() => {
                        setEditingQueued((current) =>
                          current?.turnId === item.turnId ? null : current,
                        );
                        // Removal is the gateway's to make: the row leaves the tray on
                        // `turn.queued.deleted`. A rejected delete (already started)
                        // therefore keeps showing the truth instead of hiding a turn
                        // that still runs.
                        markQueueBusy(item.turnId, true);
                        void client
                          .deleteQueuedTurn(sid, item.turnId)
                          .catch((cause) => setError((cause as Error).message))
                          .finally(() => markQueueBusy(item.turnId, false));
                      }}
                      aria-label={`Remove queued message ${index + 1}`}
                    >
                      <CloseIcon className="size-3" />
                    </button>
                  </div>
                );
              })}
            </div>
          )}

          <div className="relative border border-dialog-edge bg-input shadow-[3px_3px_0_var(--dialog-shadow)] transition-colors focus-within:border-accent">
            {activePastes.length > 0 && (
              <div className="flex gap-1 overflow-x-auto overscroll-x-contain border-b border-dialog-edge px-1.5 py-1 [scrollbar-width:thin]">
                {activePastes.map((paste) => (
                  <span
                    key={paste.id}
                    className="inline-flex min-h-7 shrink-0 items-center border border-code-edge bg-code font-mono text-chip text-accent-ink"
                  >
                    <button
                      type="button"
                      className="max-w-56 truncate px-2 text-left underline decoration-dotted underline-offset-2 transition-colors hover:bg-hover focus-visible:bg-hover focus-visible:outline-none"
                      onMouseDown={keepKeyboard}
                      onClick={() => openPasteEditor(paste.id)}
                      aria-label={`Edit pasted block ${paste.id}`}
                      title="Edit this paste"
                    >
                      {paste.token}
                    </button>
                    <button
                      type="button"
                      className="grid min-h-7 w-7 place-items-center border-l border-code-edge text-dialog-hint transition-colors hover:bg-warn-surface hover:text-err"
                      onMouseDown={keepKeyboard}
                      onClick={() => removePaste(paste.id)}
                      aria-label={`Remove pasted block ${paste.id}`}
                    >
                      <CloseIcon className="size-3" />
                    </button>
                  </span>
                ))}
              </div>
            )}
            {attachments.length > 0 && (
              <div className="flex gap-1.5 overflow-x-auto overscroll-x-contain border-b border-dialog-edge px-1.5 py-1.5 [scrollbar-width:thin]">
                {attachments.map((attachment) => (
                  <div
                    key={attachment.id}
                    className="group relative flex min-w-0 max-w-40 shrink-0 items-center gap-1.5 border border-dialog-edge bg-panel pr-6 transition-[opacity,transform,translate,scale,rotate] duration-150 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none"
                  >
                    {isVideoMediaType(attachment.media_type) ? (
                      <video
                        src={attachment.previewUrl}
                        className="size-8 shrink-0 object-cover"
                        muted
                        playsInline
                        preload="metadata"
                      />
                    ) : (
                      <ExpandableImage
                        src={attachment.previewUrl}
                        alt={attachment.filename}
                        loading="eager"
                        className="size-8 shrink-0 object-cover"
                        frameClassName="min-w-0 flex-1"
                        onApply={(edited) =>
                          applyAttachmentEdit(attachment.id, edited)
                        }
                      >
                        <span className="truncate font-mono text-chip text-dialog-hint-key">
                          {attachment.filename}
                        </span>
                      </ExpandableImage>
                    )}
                    <button
                      type="button"
                      onMouseDown={keepKeyboard}
                      className="absolute inset-y-0 right-0 grid w-6 place-items-center text-body text-dialog-hint transition-colors hover:bg-warn-surface hover:text-err"
                      onClick={() => removeAttachment(attachment.id)}
                      aria-label={`Remove ${attachment.filename}`}
                    >
                      <CloseIcon className="size-3" />
                    </button>
                  </div>
                ))}
              </div>
            )}

            {(composerNotice ||
              voicePhase !== "idle" ||
              voiceModel?.status === "downloading" ||
              (voiceRequested && voiceModel?.status !== "ready")) && (
              <div className="pointer-events-none absolute bottom-full left-0 mb-1 flex max-w-full items-center gap-1.5 border border-dialog-edge bg-panel px-2 py-1 font-mono text-chip text-dialog-hint shadow-[3px_3px_0_var(--dialog-shadow)] transition-[opacity,transform,translate,scale,rotate] duration-150 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none">
                {voicePhase === "recording" ? (
                  <>
                    <span className="size-1.5 animate-pulse bg-err motion-reduce:animate-none" />{" "}
                    Listening · tap the microphone to finish
                  </>
                ) : voicePhase === "transcribing" ? (
                  <>
                    <span className="size-1.5 animate-pulse bg-accent motion-reduce:animate-none" />{" "}
                    {voiceProgressLabel(voiceProgress)}
                  </>
                ) : composerNotice ? (
                  composerNotice
                ) : voiceModel?.status === "downloading" ? (
                  <>
                    {voiceModel.phase === "extracting"
                      ? "Unpacking voice model"
                      : "Downloading voice model"}
                    {voiceModel.progress == null
                      ? "…"
                      : ` · ${Math.round(voiceModel.progress)}%`}
                  </>
                ) : voiceModel?.status === "failed" ? (
                  <>
                    Voice model failed
                    {voiceModel.error ? ` · ${voiceModel.error}` : ""}
                  </>
                ) : voiceModel?.status === "absent" ? (
                  <>Tap the microphone to install the local voice model</>
                ) : null}
              </div>
            )}

            <div className="flex items-end gap-1 p-1">
              <input
                ref={fileInputRef}
                type="file"
                accept={(
                  capabilities?.features.attachments.media_types ?? [
                    "image/*",
                    "video/*",
                  ]
                ).join(",")}
                multiple
                className="hidden"
                onChange={(event) => void onFilesPicked(event.target.files)}
              />

              {/* ONE attachment button. The gallery sheet has no shutter, so "take
                a photo" still needs its own path — without it the only way to
                attach what you are LOOKING at is to leave, open the camera app,
                come back and hunt for the file. It lives in this menu instead of
                a second icon crowding the composer. Web keeps the direct file
                dialog: its input already exposes whatever capture the browser
                has. */}
              <div
                className="relative shrink-0"
                onKeyDown={(event) => {
                  if (event.key === "Escape" && attachMenuOpen) {
                    event.stopPropagation();
                    setAttachMenuOpen(false);
                  }
                }}
              >
                {attachMenuOpen && (
                  <>
                    {/* Tapping anywhere else is a dismissal, not a mis-tap. */}
                    <button
                      type="button"
                      aria-label="Close attachment menu"
                      className="fixed inset-0 z-20 cursor-default"
                      onMouseDown={keepKeyboard}
                      onClick={() => setAttachMenuOpen(false)}
                    />
                    <div
                      role="menu"
                      aria-label="Attach"
                      className="absolute bottom-full left-0 z-30 mb-1.5 w-max min-w-40 border border-dialog-edge bg-panel shadow-[6px_6px_0_var(--dialog-shadow)] transition-[opacity,transform,translate,scale,rotate] duration-150 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none"
                    >
                      <button
                        type="button"
                        role="menuitem"
                        className="flex min-h-9 w-full items-center gap-2 px-3 py-1.5 text-left font-mono text-meta text-dialog-foreground transition-colors hover:bg-hover"
                        onMouseDown={keepKeyboard}
                        onClick={() => {
                          setAttachMenuOpen(false);
                          void takePhoto();
                        }}
                      >
                        <CameraIcon />
                        Take a photo
                      </button>
                      <button
                        type="button"
                        role="menuitem"
                        className="flex min-h-9 w-full items-center gap-2 border-t border-dialog-edge px-3 py-1.5 text-left font-mono text-meta text-dialog-foreground transition-colors hover:bg-hover"
                        onMouseDown={keepKeyboard}
                        onClick={() => {
                          setAttachMenuOpen(false);
                          void addAttachments();
                        }}
                      >
                        <ImageIcon />
                        Photos or videos
                      </button>
                    </div>
                  </>
                )}

                <button
                  type="button"
                  onMouseDown={keepKeyboard}
                  className="grid h-8 w-7 shrink-0 place-items-center text-dialog-hint transition-[background-color,color,transform,translate,scale,rotate] duration-150 hover:bg-hover hover:text-dialog-hint-key active:scale-[0.94] disabled:text-muted motion-reduce:transition-none mouse:h-7 mouse:w-6"
                  onClick={() => {
                    if (!Capacitor.isNativePlatform()) {
                      void addAttachments();
                      return;
                    }
                    setAttachMenuOpen((open) => !open);
                  }}
                  disabled={
                    attachments.length >=
                    (capabilities?.features.attachments.max_files ?? 8)
                  }
                  aria-haspopup={
                    Capacitor.isNativePlatform() ? "menu" : undefined
                  }
                  aria-expanded={
                    Capacitor.isNativePlatform() ? attachMenuOpen : undefined
                  }
                  aria-label={
                    Capacitor.isNativePlatform()
                      ? "Attach a photo or video"
                      : "Choose photos or videos"
                  }
                  title={
                    Capacitor.isNativePlatform()
                      ? "Attach a photo or video"
                      : "Choose photos or videos"
                  }
                >
                  <PlusIcon
                    className={`size-3.5 transition-transform duration-150 motion-reduce:transition-none ${attachMenuOpen ? "rotate-45" : ""}`}
                  />
                </button>
              </div>

              {voiceSupported && (
                <button
                  type="button"
                  onMouseDown={keepKeyboard}
                  className={`grid h-8 w-7 shrink-0 place-items-center transition-[background-color,color,transform,translate,scale,rotate] duration-150 active:scale-[0.94] disabled:text-muted motion-reduce:transition-none mouse:h-7 mouse:w-6 ${
                    voicePhase === "recording"
                      ? "animate-pulse bg-warn-surface text-err motion-reduce:animate-none"
                      : "text-dialog-hint hover:bg-hover hover:text-dialog-hint-key"
                  }`}
                  onClick={() => void toggleVoice()}
                  disabled={
                    voicePhase === "transcribing" ||
                    voiceModel?.status === "downloading"
                  }
                  aria-label={
                    voicePhase === "recording"
                      ? "Finish dictation"
                      : "Dictate message"
                  }
                  title={
                    voicePhase === "recording"
                      ? "Finish dictation"
                      : "Dictate message"
                  }
                >
                  <MicIcon />
                </button>
              )}

              <textarea
                ref={composerRef}
                rows={1}
                value={prompt}
                disabled={voicePhase === "recording"}
                placeholder={
                  voicePhase === "recording"
                    ? "Listening…"
                    : running
                      ? "Message Vis — queues behind the running turn"
                      : "Message Vis or type / or @"
                }
                aria-label="Message Vis"
                // Both completion menus are anchored to this textarea and are mutually
                // exclusive (`fileMention` is only computed while the slash menu is shut),
                // so the announced popup must name whichever one is actually open.
                aria-controls={
                  fileMatches.length
                    ? "file-mention-list"
                    : slashMatches.length
                      ? "slash-command-list"
                      : undefined
                }
                aria-expanded={
                  slashMatches.length > 0 || fileMatches.length > 0
                }
                className="h-8 min-h-8 max-h-20 min-w-0 flex-1 resize-none overflow-y-auto border-0 bg-transparent px-1 py-2 text-ui text-dialog-foreground outline-none placeholder:text-dialog-hint disabled:text-cancelled-foreground mouse:h-7 mouse:min-h-7 mouse:py-1.5 mouse:text-meta"
                onPaste={handlePaste}
                onFocus={handleComposerFocus}
                onSelect={(event) =>
                  setCaret(
                    (event.target as HTMLTextAreaElement).selectionStart ?? 0,
                  )
                }
                onChange={(event) => {
                  setPrompt(event.target.value);
                  setCaret(
                    event.target.selectionStart ?? event.target.value.length,
                  );
                  setSlashIndex(0);
                  setSlashDismissed(false);
                  setFileIndex(0);
                  setFileDismissed(false);
                }}
                onKeyDown={(event) => {
                  if (fileMatches.length) {
                    if (event.key === "ArrowDown" || event.key === "ArrowUp") {
                      event.preventDefault();
                      const delta = event.key === "ArrowDown" ? 1 : -1;
                      setFileIndex(
                        (current) =>
                          (current + delta + fileMatches.length) %
                          fileMatches.length,
                      );
                      return;
                    }
                    if (
                      (event.key === "Tab" || event.key === "Enter") &&
                      selectedFile
                    ) {
                      event.preventDefault();
                      completeFile(selectedFile.name);
                      return;
                    }
                    if (event.key === "Escape") {
                      event.preventDefault();
                      setFileDismissed(true);
                      return;
                    }
                  }
                  if (
                    slashMatches.length &&
                    (event.key === "ArrowDown" || event.key === "ArrowUp")
                  ) {
                    event.preventDefault();
                    const delta = event.key === "ArrowDown" ? 1 : -1;
                    setSlashIndex(
                      (current) =>
                        (current + delta + slashMatches.length) %
                        slashMatches.length,
                    );
                    return;
                  }
                  if (
                    slashMatches.length &&
                    event.key === "Tab" &&
                    selectedSlash
                  ) {
                    event.preventDefault();
                    completeSlash(selectedSlash);
                    return;
                  }
                  if (slashMatches.length && event.key === "Escape") {
                    event.preventDefault();
                    setSlashDismissed(true);
                    return;
                  }
                  if (
                    event.key === "Enter" &&
                    !event.shiftKey &&
                    !event.nativeEvent.isComposing
                  ) {
                    event.preventDefault();
                    if (
                      selectedSlash &&
                      slashText.toLowerCase() !==
                        selectedSlash.name.toLowerCase()
                    ) {
                      completeSlash(selectedSlash);
                    } else {
                      void send();
                    }
                  }
                }}
              />

              {/* The action rail keeps a CONSTANT footprint. Send and stop used to
                mount and unmount independently, so starting a turn, typing during
                one, or a turn simply ending resized the textarea under the caret.
                Both squares are always laid out; only the stop button's contents
                come and go.

                The stop affordance still retires the moment the cancel is
                accepted: the live bubble then carries the single "Vis is
                cancelling" line, and the finished turn carries "Cancelled by
                user." — one state at a time, never a button offering to cancel a
                cancel. */}
              <div className="grid size-8 shrink-0 place-items-center mouse:size-7">
                {activeWork && !liveTurn?.cancelling && (
                  <button
                    type="button"
                    onMouseDown={keepKeyboard}
                    className="grid size-full place-items-center border border-err bg-cancelled transition-[background-color,opacity,transform,translate,scale,rotate] duration-150 hover:bg-warn-surface active:scale-[0.94] starting:scale-90 starting:opacity-0 motion-reduce:transition-none"
                    onClick={cancel}
                    aria-label="Stop response"
                  >
                    <span className="size-1.5 bg-err" />
                  </button>
                )}
              </div>
              <button
                type="button"
                onMouseDown={keepKeyboard}
                className="grid size-8 shrink-0 place-items-center border border-dialog-edge bg-dialog-title text-ui font-bold text-dialog-title-foreground transition-[background-color,color,transform,translate,scale,rotate] duration-150 hover:bg-accent-2 active:scale-[0.94] disabled:scale-100 disabled:bg-button disabled:text-dialog-hint motion-reduce:transition-none mouse:size-7"
                onClick={send}
                disabled={
                  (!prompt.trim() && !attachments.length) ||
                  voicePhase !== "idle"
                }
                aria-label={running ? "Queue message" : "Send message"}
                title={running ? "Queue behind the running turn" : "Send"}
              >
                {"↑"}
              </button>
            </div>
          </div>

          {/* Composer strip, in the TUI footer's own reading order: the router chip
            sits LEFT directly under the input, cumulative session usage (tokens,
            then cost) rides the RIGHT edge. The chip truncates first so the
            numbers survive a narrow phone.

            ONE type step for the whole strip: `font-mono text-chip`, semibold,
            uppercase, the same 0.08em tracking — model, level and usage are the
            same sentence, so they must not read as three different fonts. The
            divider is a 10px hairline centred between the two words, not a
            full-height `border-l` rule: a border on the button grew with its
            padding and towered over 11px text. Tone does the separating: the
            model is the loud one, its level the quiet one, the cost the only
            accent. */}
          <div className="flex w-full items-center gap-2.5 pt-1">
            <button
              type="button"
              className="min-w-0 shrink truncate px-1 py-1 text-left font-mono text-chip font-semibold uppercase tracking-[0.08em] text-dialog-hint-key underline decoration-dialog-edge decoration-1 underline-offset-4 transition-colors duration-150 hover:text-accent-ink hover:decoration-accent focus-visible:text-accent-ink focus-visible:outline-none motion-reduce:transition-none"
              onClick={() => setRouterOpen(true)}
              aria-label="Change provider and model"
              title={
                (modelPref?.model ?? defaultPref?.model)
                  ? `${modelPref?.provider ?? defaultPref?.provider ?? ""}/${modelPref?.model ?? defaultPref?.model ?? ""}`
                  : "Change provider and model"
              }
            >
              {modelPref?.model ?? defaultPref?.model ?? "model"}
            </button>

            {reasoning && (reasoning.choices?.length ?? 0) > 0 && (
              <>
                <span
                  aria-hidden="true"
                  className="h-2.5 w-px shrink-0 bg-dialog-edge"
                />
                <button
                  type="button"
                  onMouseDown={keepKeyboard}
                  className="shrink-0 px-1 py-1 font-mono text-chip font-semibold uppercase tracking-[0.08em] text-dialog-hint transition-colors duration-150 hover:text-accent-ink focus-visible:text-accent-ink focus-visible:outline-none motion-reduce:transition-none"
                  onClick={() => void cycleReasoning()}
                  disabled={reasoningBusy}
                  aria-busy={reasoningBusy}
                  aria-live="polite"
                  aria-label={`${reasoning.label} — ${reasoningLevel}, tap for the next level`}
                  title={`${reasoning.label}: ${reasoningLevel} — tap to cycle`}
                >
                  {/* Re-keyed on every change so the span REMOUNTS and the swap
                    keyframe replays; a transition on a persistent node cannot
                    animate a text swap at all. */}
                  <span
                    key={reasoningLevel}
                    className="inline-block animate-chip-swap motion-reduce:animate-none"
                  >
                    {reasoningLevel}
                  </span>
                </button>
              </>
            )}

            {(usageTokens || usageCost) && (
              <span
                className="ml-auto flex shrink-0 items-center gap-2 py-1 pl-1 font-mono text-chip font-semibold uppercase tracking-[0.08em] tabular-nums text-dialog-hint"
                title={`Session usage — ${usageTitle}`}
              >
                {usageTokens && (
                  <span className="whitespace-nowrap">{usageTokens}</span>
                )}
                {usageCost && (
                  <span className="whitespace-nowrap text-accent-ink">
                    {usageCost}
                  </span>
                )}
              </span>
            )}
          </div>
        </footer>
      </section>
    </AttachImageContext.Provider>
  );
}
