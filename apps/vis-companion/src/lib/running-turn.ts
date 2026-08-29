/**
 * The transient transcript state of the one turn currently being executed.
 *
 * Running turns, form-owned Activity snapshots, and host-owned Live Views are
 * separate models. This reducer owns only the first. A `block.activity` frame
 * replaces `forms[i].activity`; `turn.progress` is only the turn ticker.
 */
import { activityProjectionFromWire, type ActivityProjection } from "./activity";
import {
  eventBlockKey,
  eventFormKey,
  eventIterationPosition,
  eventString,
} from "./session-stream";
import type {
  ContentBlock,
  GatewayAttachment,
  IterationAttachment,
  SseEvent,
  TranscriptForm,
  TranscriptIteration,
} from "./types";

export interface TurnProgress {
  kind: string;
  iteration?: number;
  command?: string;
  operation?: string;
  label?: string;
  /** The host-authored ticker sentence for a tool whose private op/id is opaque. */
  phrase?: string;
  /** The router-resolved model while a provider call is in flight. */
  model?: string;
}

export interface RunningTurn {
  id?: string;
  request: string;
  answer: string;
  iterations: TranscriptIteration[];
  /** Replay head announced before its older frames, so the ticker starts at NOW. */
  latestIteration?: number;
  progress?: TurnProgress;
  startedAt: number;
  cancelling?: boolean;
  status: "running" | "completed" | "failed" | "cancelled";
  /** Bytes sent by this device; the running-turn wire carries descriptors only. */
  attachments?: GatewayAttachment[];
  /** Terminal-frame content retained until the persisted transcript takes over. */
  content?: ContentBlock[];
}

function applyText(_current: string, event: SseEvent): string {
  return eventString(event, "cumulative");
}

function updateRunningIteration(
  turn: RunningTurn,
  position: number,
  update: (iteration: TranscriptIteration) => TranscriptIteration,
): RunningTurn {
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
    block_id: eventFormKey(event),
    scope: eventString(event, "scope") || undefined,
    code: eventString(event, "code") || undefined,
    display_code: eventString(event, "display_code") || undefined,
    display_language: eventString(event, "display_language") || undefined,
    comment: eventString(event, "comment") || undefined,
    op: eventString(event, "op") || undefined,
    result_summary:
      eventString(event, "result_summary") || (running ? "Running…" : undefined),
    result_render: eventString(event, "result_render") || undefined,
    result_kind: eventString(event, "result_kind") || undefined,
    result: event.result as TranscriptForm["result"],
    error: event.error as TranscriptForm["error"],
    stdout: eventString(event, "stdout") || undefined,
    cards,
    silent: event.silent === true,
    duration_ms:
      typeof event.duration_ms === "number" ? event.duration_ms : undefined,
    // A terminal block frame may carry the settled form-owned Activity snapshot.
    // Undefined keys are ignored by `upsertRunningForm`, so `block.started` does
    // not wipe a transient snapshot already placed by `block.activity`.
    activity: activityProjectionFromWire(event.activity) ?? undefined,
  };
}

function upsertRunningForm(
  iteration: TranscriptIteration,
  next: TranscriptForm,
): TranscriptIteration {
  const key = next.block_id == null ? "" : String(next.block_id);

  const forms = [...(iteration.forms ?? [])];
  const index = forms.findIndex((form) => String(form.block_id ?? "") === key);
  if (index < 0) forms.push(next);
  else {
    const defined = Object.fromEntries(
      Object.entries(next).filter(([, value]) => value !== undefined),
    ) as TranscriptForm;
    forms[index] = { ...forms[index], ...defined };
  }
  return { ...iteration, forms };
}

/** Replace Activity only on the form that already owns this routing key. */
function applyFormActivity(
  turn: RunningTurn,
  event: SseEvent,
  activity: ActivityProjection,
): RunningTurn {
  const position = eventIterationPosition(event);
  const iterationIndex = turn.iterations.findIndex(
    (iteration) => iteration.position === position,
  );
  if (iterationIndex < 0) return turn;

  const key = eventFormKey(event);
  const forms = [...(turn.iterations[iterationIndex].forms ?? [])];
  const formIndex = forms.findIndex(
    (form) => String(form.block_id ?? "") === key,
  );
  // Activity never creates its owner. `block.started`/`block.preview` creates the
  // form; an orphan snapshot is ignored instead of becoming a blank card.
  if (formIndex < 0) return turn;

  forms[formIndex] = { ...forms[formIndex], activity };
  const iterations = [...turn.iterations];
  iterations[iterationIndex] = { ...iterations[iterationIndex], forms };
  return { ...turn, iterations };
}

/** Fold one running-turn SSE frame into the transient turn snapshot. */
export function reduceRunningTurnEvent(
  turn: RunningTurn | null,
  event: SseEvent,
): RunningTurn | null {
  const type = event.type;
  if (type === "turn.started") {
    const startedId = eventString(event, "turn_id");
    return {
      id: startedId,
      request: eventString(event, "request"),
      answer: "",
      iterations: [],
      startedAt:
        typeof event.started_at === "number" ? event.started_at : Date.now(),
      status: "running",
      // `turn.started` for an optimistically painted turn carries no image bytes.
      attachments:
        turn && (!turn.id || turn.id === startedId) ? turn.attachments : undefined,
    };
  }
  if (!turn) return turn;
  // A settled bubble never re-animates on a trailing or replayed body frame.
  if (turn.status !== "running") return turn;

  if (type === "content.block.delta") {
    const field = eventString(event, "field");
    const blockId = eventBlockKey(event);
    const position = eventIterationPosition(event);
    if (field === "text") {
      const next = updateRunningIteration(turn, position, (iteration) => ({
        ...iteration,
        thinking: applyText(iteration.thinking ?? "", event),
      }));
      return { ...next, progress: undefined };
    }
    if (field === "markdown" && blockId.includes(":assistant-prose:")) {
      const next = updateRunningIteration(turn, position, (iteration) => ({
        ...iteration,
        assistant_prose: applyText(iteration.assistant_prose ?? "", event),
      }));
      return { ...next, answer: "", progress: undefined };
    }
    if (field === "markdown") {
      return {
        ...turn,
        answer: applyText(turn.answer, event),
        progress: undefined,
      };
    }
    return turn;
  }

  if (type === "iteration.completed") {
    const position = eventIterationPosition(event);
    const attached = Array.isArray(event.attachments)
      ? (event.attachments as IterationAttachment[])
      : undefined;
    const next = updateRunningIteration(turn, position, (iteration) => ({
      ...iteration,
      // Present null deliberately rejects a raw fragment; only omission falls back.
      thinking: Object.prototype.hasOwnProperty.call(event, "thinking")
        ? eventString(event, "thinking")
        : iteration.thinking,
      assistant_prose:
        eventString(event, "assistant_prose") || iteration.assistant_prose,
      attachments: attached?.length ? attached : iteration.attachments,
      error: undefined,
    }));
    const promoted = next.iterations.find(
      (iteration) => iteration.position === position,
    )?.assistant_prose;
    return {
      ...next,
      answer: promoted ? "" : turn.answer,
      progress: undefined,
    };
  }

  if (type === "block.preview") {
    const form = formFromEvent(event);
    if (!form.block_id) return turn;
    const position = eventIterationPosition(event);
    const next = updateRunningIteration(turn, position, (iteration) =>
      upsertRunningForm(iteration, form),
    );
    return { ...next, progress: undefined };
  }

  if (type === "block.activity") {
    const activity = activityProjectionFromWire(event.activity);
    return activity ? applyFormActivity(turn, event, activity) : turn;
  }

  if (type === "block.started" || type === "block.output") {
    const form = formFromEvent(event, type === "block.started");
    if (!form.block_id) return turn;
    const position = eventIterationPosition(event);
    const next = updateRunningIteration(turn, position, (iteration) =>
      upsertRunningForm(iteration, form),
    );
    if (type === "block.output") return { ...next, progress: undefined };
    return {
      ...next,
      progress: {
        kind: "code",
        iteration: position,
        operation: form.scope,
      },
    };
  }

  if (type === "turn.progress") {
    const kind = eventString(event, "progress");
    const rawIteration = event.iteration;
    const iteration =
      typeof rawIteration === "number"
        ? rawIteration
        : typeof rawIteration === "string" && rawIteration.trim()
          ? Number(rawIteration)
          : undefined;
    return {
      ...turn,
      progress: kind
        ? {
            kind,
            iteration: Number.isFinite(iteration) ? iteration : undefined,
            command: eventString(event, "cmd") || undefined,
            operation: eventString(event, "op") || undefined,
            label: eventString(event, "label") || undefined,
            phrase: eventString(event, "phrase") || undefined,
            model: eventString(event, "model") || undefined,
          }
        : undefined,
    };
  }

  if (type === "iteration.error" || type === "provider.retry") {
    const position = eventIterationPosition(event);
    const next = updateRunningIteration(turn, position, (iteration) => ({
      ...iteration,
      error: (event.error_data ??
        event.error ??
        event.detail ??
        "retrying") as TranscriptIteration["error"],
    }));
    return { ...next, progress: undefined };
  }

  return turn;
}
