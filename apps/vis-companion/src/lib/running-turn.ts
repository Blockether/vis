/**
 * The transient transcript state of the one turn currently being executed.
 *
 * Running turns, form-owned Activity snapshots, and host-owned Live Views are
 * separate models. This reducer owns only the first. A `block.activity` frame
 * replaces `forms[i].activity`; `turn.progress` is only the turn ticker.
 */
import { activityProjectionFromWire, type ActivityProjection } from './activity';
import { eventBlockKey, eventFormKey, eventIterationPosition, eventString } from './session-stream';
import type {
  ContentBlock,
  CouncilRequest,
  DeliveredUserInput,
  RequestKind,
  ExtensionWait,
  GatewayAttachment,
  RecordingTranscription,
  IterationAttachment,
  SseEvent,
  TranscriptForm,
  TranscriptIteration,
  TranscriptTurn,
} from './types';

/**
 * The silence of the model in one provider attempt (`provider-wait` progress).
 * Both clocks move on from `receivedAt`, so the line stays live between two notices.
 */
export interface ProviderWait {
  /** Model silence when the gateway sent the notice. */
  silentMs: number;
  /** Device clock when the notice arrived. */
  receivedAt: number;
  /** True while this attempt has sent no output. */
  awaitingOutput: boolean;
  /** `alive` while stream events still arrive; `silent` when nothing arrives. */
  connection?: 'alive' | 'silent';
  /** When Svar re-sends the request (`retry`) or ends the call (`timeout`). */
  deadline?: { inMs: number; action: 'retry' | 'timeout' };
}

/** A Python block that runs without a tool call. */
export interface PythonBusy {
  /** Time since the last tool call of the block when the notice arrived. */
  busyMs: number;
  /** Device clock when the notice arrived; the clocks move from here. */
  receivedAt: number;
  /** CPU use of the Python worker and its child processes since the last notice, when known. */
  cpuPercent?: number;
  /** Time until Vis stops the block. */
  deadlineInMs?: number;
}

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
  /** The silence of the model while `kind` is `provider-wait`. */
  wait?: ProviderWait;
  /** The busy Python block while `kind` is `python`; absent when the notice ends. */
  busy?: PythonBusy;
  /** Readiness before the first live iteration. */
  extensionWait?: ExtensionWait;
  /** Readiness start on the device clock. */
  extensionWaitStartedAt?: number;
  /** The recording the turn waits on while `kind` is `attachment-transcription`. */
  transcription?: RecordingTranscription;
}

const positiveInteger = (value: unknown): value is number =>
  typeof value === 'number' && Number.isInteger(value) && value > 0;

function transcriptionFromEvent(event: SseEvent): RecordingTranscription | undefined {
  const wait = event.transcription;
  if (!wait || !positiveInteger(wait.recording) || !positiveInteger(wait.recordings)) return undefined;
  const percent =
    typeof wait.percent === 'number' && wait.percent >= 0 && wait.percent <= 100 ? wait.percent : undefined;
  return { recording: wait.recording, recordings: wait.recordings, ...(percent === undefined ? {} : { percent }) };
}

export interface RunningTurn {
  id?: string;
  position?: number;
  /** Canonical wall-clock timestamp, distinct from the device's elapsed clock. */
  createdAt?: number;
  request: string;
  requestKind?: RequestKind;
  council?: CouncilRequest;
  answer: string;
  iterations: TranscriptIteration[];
  /** Earlier persisted steps remain available through the turn trace. */
  iterationsOffset?: number;
  /** Replay head announced before its older frames, so the ticker starts at NOW. */
  latestIteration?: number;
  progress?: TurnProgress;
  startedAt: number;
  cancelling?: boolean;
  status: 'running' | 'completed' | 'failed' | 'cancelled';
  /** Bytes sent by this device; the running-turn wire carries descriptors only. */
  attachments?: GatewayAttachment[];
  /** Percent of the attachment bytes uploaded, until the gateway accepts the turn. */
  upload?: { percent: number };
  /** Typed streamed content, replaced by terminal content until the transcript takes over. */
  content?: ContentBlock[];
  /** Start offset of each live block's known text; absent means offset 0. */
  streamBases?: Record<string, number>;
  /** A gateway restart continued this turn. Set until its first progress frame. */
  resumed?: boolean;
}

/**
 * Block text after one `content.block.delta`. A delta that matches known text leaves
 * it in place. Other deltas replace from their offset or start a new span after a gap.
 */
export function applyBlockDelta(
  current: string,
  base: number,
  event: SseEvent,
): { text: string; base: number } {
  const value = event.offset;
  const offset = typeof value === 'number' && Number.isFinite(value) && value >= 0 ? value : 0;
  const text = eventString(event, 'text');
  if (offset >= base && offset <= base + current.length) {
    const index = offset - base;
    return { text: current.startsWith(text, index) ? current : current.slice(0, index) + text, base };
  }
  return { text, base: offset };
}

function providerWaitFromEvent(event: SseEvent): ProviderWait | undefined {
  const silentMs = event.silent_ms;
  if (typeof silentMs !== 'number' || !Number.isFinite(silentMs)) return undefined;
  const connection = eventString(event, 'connection');
  const deadlineInMs = event.deadline_in_ms;
  return {
    silentMs,
    receivedAt: Date.now(),
    awaitingOutput: event.awaiting_output === true,
    connection: connection === 'alive' || connection === 'silent' ? connection : undefined,
    deadline:
      typeof deadlineInMs === 'number' && Number.isFinite(deadlineInMs)
        ? {
            inMs: deadlineInMs,
            action: eventString(event, 'deadline_action') === 'retry' ? 'retry' : 'timeout',
          }
        : undefined,
  };
}

function finiteNumber(value: unknown): number | undefined {
  return typeof value === 'number' && Number.isFinite(value) ? value : undefined;
}

function pythonBusyFromEvent(event: SseEvent): PythonBusy | undefined {
  const busyMs = finiteNumber(event.busy_ms);
  if (busyMs === undefined) return undefined;
  return {
    busyMs,
    receivedAt: Date.now(),
    cpuPercent: finiteNumber(event.cpu_percent),
    deadlineInMs: finiteNumber(event.deadline_in_ms),
  };
}

/** `25s` or `3m 5s`: whole seconds, because the wait clocks move once a second. */
function waitClock(ms: number): string {
  const seconds = Math.floor(Math.max(0, ms) / 1_000);
  return seconds < 60 ? `${seconds}s` : `${Math.floor(seconds / 60)}m ${seconds % 60}s`;
}

/** The line under the ticker: `No response for 25s · connection alive · retry in 3m 35s`. */
export function providerWaitDetail(wait: ProviderWait, now: number): string {
  const passed = Math.max(0, now - wait.receivedAt);
  const silence = waitClock(wait.silentMs + passed);
  const parts = [`${wait.awaitingOutput ? 'No response' : 'No new output'} for ${silence}`];
  if (wait.connection) parts.push(`connection ${wait.connection}`);
  if (wait.deadline) {
    parts.push(`${wait.deadline.action} in ${waitClock(wait.deadline.inMs - passed)}`);
  }
  return parts.join(' · ');
}

/** The line under the ticker: `No tool call for 1m 12s · Python CPU 99% · stops in 3m 48s`. */
export function pythonBusyDetail(busy: PythonBusy, now: number): string {
  const passed = Math.max(0, now - busy.receivedAt);
  const parts = [`No tool call for ${waitClock(busy.busyMs + passed)}`];
  if (busy.cpuPercent !== undefined) parts.push(`Python CPU ${Math.round(busy.cpuPercent)}%`);
  if (busy.deadlineInMs !== undefined) {
    parts.push(`stops in ${waitClock(busy.deadlineInMs - passed)}`);
  }
  return parts.join(' · ');
}

function updateRunningIteration(
  turn: RunningTurn,
  position: number,
  update: (iteration: TranscriptIteration) => TranscriptIteration,
): RunningTurn {
  const index = turn.iterations.findIndex((iteration) => iteration.position === position);
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

function formFromEvent(event: SseEvent): TranscriptForm {
  const cards = Array.isArray(event.cards) ? (event.cards as TranscriptForm[]) : undefined;
  return {
    block_id: eventFormKey(event),
    scope: eventString(event, 'scope') || undefined,
    code: eventString(event, 'code') || undefined,
    display_code: eventString(event, 'display_code') || undefined,
    display_language: eventString(event, 'display_language') || undefined,
    comment: eventString(event, 'comment') || undefined,
    op: eventString(event, 'op') || undefined,
    result_kind: eventString(event, 'result_kind') || undefined,
    error: event.error as TranscriptForm['error'],
    stdout: eventString(event, 'stdout') || undefined,
    cards,
    silent: event.silent === true,
    started_at_ms: typeof event.started_at_ms === 'number' ? event.started_at_ms : undefined,
    duration_ms: typeof event.duration_ms === 'number' ? event.duration_ms : undefined,
  };
}

function upsertRunningForm(
  iteration: TranscriptIteration,
  next: TranscriptForm,
): TranscriptIteration {
  const key = next.block_id == null ? '' : String(next.block_id);

  const forms = [...(iteration.forms ?? [])];
  const index = forms.findIndex((form) => String(form.block_id ?? '') === key);
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
  const iterationIndex = turn.iterations.findIndex((iteration) => iteration.position === position);
  if (iterationIndex < 0) return turn;

  const key = eventFormKey(event);
  const forms = [...(turn.iterations[iterationIndex].forms ?? [])];
  const formIndex = forms.findIndex((form) => String(form.block_id ?? '') === key);
  // Activity never creates its owner. `block.started`/`block.preview` creates the
  // form; an orphan snapshot is ignored instead of becoming a blank card.
  if (formIndex < 0) return turn;

  const previousHistory = forms[formIndex].activity?.history;
  if (
    previousHistory &&
    activity.history?.id === previousHistory.id &&
    activity.history.revision < previousHistory.revision
  ) {
    return turn;
  }

  forms[formIndex] = { ...forms[formIndex], activity };
  const iterations = [...turn.iterations];
  iterations[iterationIndex] = { ...iterations[iterationIndex], forms };
  return { ...turn, iterations };
}

/**
 * The bubble with the stored steps of its turn that come before its first live step.
 * After a gateway restart the stream replays only the steps of the new process; the
 * stored row of the same turn holds the steps of the earlier process.
 */
export function withStoredSteps(turn: RunningTurn, row: TranscriptTurn | undefined): RunningTurn {
  if (!turn.id || row?.turn_id !== turn.id || !row.iterations?.length) return turn;
  const first = Math.min(
    ...turn.iterations.map((iteration) => iteration.position ?? Number.POSITIVE_INFINITY),
  );
  const earlier = row.iterations.filter(
    (iteration) => typeof iteration.position === 'number' && iteration.position < first,
  );
  if (!earlier.length) return turn;
  return {
    ...turn,
    iterations: [...earlier, ...turn.iterations],
    iterationsOffset: row.iterations_offset,
  };
}

function changedTraceFields<T extends Record<string, unknown>>(live: T, before: T | undefined): Partial<T> {
  return Object.fromEntries(Object.entries(live).filter(([key, value]) => !Object.is(value, before?.[key]))) as Partial<T>;
}

function repairTraceForms(
  snapshot: TranscriptIteration,
  live: TranscriptIteration,
  before: TranscriptIteration | undefined,
): TranscriptForm[] | undefined {
  if (!live.forms?.length) return snapshot.forms;
  const key = (form: TranscriptForm) => form.block_id == null ? undefined : String(form.block_id);
  const forms = [...(snapshot.forms ?? [])];
  const positions = new Map(forms.map((form, index) => [key(form), index]));
  const old = new Map(before?.forms?.map((form) => [key(form), form]));
  for (const form of live.forms) {
    const id = key(form);
    const index = id === undefined ? undefined : positions.get(id);
    if (index === undefined) forms.push(form);
    else forms[index] = { ...form, ...forms[index], ...changedTraceFields(form, old.get(id)) };
  }
  return forms;
}

/** Restore the trace and apply only live changes made since the read began. */
export function repairRunningTrace(
  current: RunningTurn,
  before: RunningTurn,
  restored: TranscriptIteration[],
): { turn: RunningTurn; complete: boolean } {
  const key = (iteration: TranscriptIteration) => iteration.position ?? iteration.id;
  const beforeByKey = new Map(before.iterations.map((iteration) => [key(iteration), iteration]));
  const liveByKey = new Map(current.iterations.map((iteration) => [key(iteration), iteration]));
  const rows = new Map(restored.map((iteration) => [key(iteration), iteration]));
  const streamBases = { ...current.streamBases };
  for (const [id, live] of liveByKey) {
    const snapshot = rows.get(id);
    if (!snapshot) {
      rows.set(id, live);
      continue;
    }
    const old = beforeByKey.get(id);
    const row: TranscriptIteration = {
      ...snapshot,
      ...changedTraceFields(live, old),
      forms: repairTraceForms(snapshot, live, old),
    };
    for (const [field, phase] of [['thinking', 'reasoning'], ['assistant_prose', 'assistant-prose']] as const) {
      const block = `${current.id}:${phase}:${live.position}`;
      const base = streamBases[block] ?? 0;
      const prefix = snapshot[field];
      const text = live[field];
      const changed = text !== old?.[field] || base !== (before.streamBases?.[block] ?? 0);
      if (base > (prefix?.length ?? 0)) {
        // The two spans do not touch yet. Keep the suffix and retry the trace read.
        row[field] = text;
        continue;
      }
      const suffix = text ?? '';
      // A snapshot can already include the live span and more text after it.
      const coversLive = text !== undefined && prefix?.startsWith(text, base);
      const liveExtendsPrefix = base === 0 && prefix !== undefined && suffix.startsWith(prefix);
      const needsLive = changed || liveExtendsPrefix || (base > 0 && base + suffix.length > (prefix?.length ?? 0));
      row[field] = needsLive && !coversLive
        ? (base > 0 ? (prefix ?? '').slice(0, base) + suffix : text)
        : prefix;
      delete streamBases[block];
    }
    rows.set(id, row);
  }
  const complete = ![...rows.values()].some(({ position }) =>
    [
      streamBases[`${current.id}:reasoning:${position}`],
      streamBases[`${current.id}:assistant-prose:${position}`],
    ].some((base) => base > 0),
  );
  return {
    turn: {
      ...current,
      iterations: [...rows.values()].sort((a, b) => (a.position ?? 0) - (b.position ?? 0)),
      iterationsOffset: restored.length ? undefined : current.iterationsOffset,
      streamBases,
    },
    complete,
  };
}

/** Fold one running-turn SSE frame into the transient turn snapshot. */
export function reduceRunningTurnEvent(
  turn: RunningTurn | null,
  event: SseEvent,
): RunningTurn | null {
  const type = event.type;
  if (type === 'turn.started') {
    const startedId = eventString(event, 'turn_id');
    // The bubble we are already painting, if this frame names it. It carries the
    // image bytes `turn.started` never does — and OUR clock.
    const own = turn && (!turn.id || turn.id === startedId) ? turn : null;
    // A gateway restart announces the same turn again. Its earlier steps stay, so
    // the bubble continues instead of starting over as if the message was sent again.
    const same = own?.id === startedId ? own : null;
    return {
      id: startedId,
      position: typeof event.position === 'number' ? event.position : own?.position,
      createdAt: typeof event.created_at === 'number' ? event.created_at : own?.createdAt,
      request: eventString(event, 'request'),
      requestKind: event.request_kind,
      council: event.council,
      answer: same?.answer ?? '',
      iterations: same?.iterations ?? [],
      iterationsOffset: same?.iterationsOffset,
      latestIteration: same?.latestIteration,
      streamBases: same?.streamBases,
      resumed: event.resumed === true || undefined,
      // `started_at` is the GATEWAY host's clock; the elapsed line under "Vis" is
      // `Date.now() - startedAt` on the device. Stamping our own bubble with the
      // foreign clock made that counter jump by the whole difference the instant
      // the frame landed — forward by the skew, or back to `0ms` on a phone
      // running ahead — right at "sent" turning into "accepted". The adoption
      // path rebases through `server_time_ms` for exactly this reason
      // (`SessionScreen`); this frame carries no such pair, so a bubble we
      // started ourselves keeps the stamp it started with.
      startedAt: own
        ? own.startedAt
        : typeof event.started_at === 'number'
          ? event.started_at
          : Date.now(),
      status: 'running',
      // `turn.started` for an optimistically painted turn carries no image bytes.
      attachments: own?.attachments,
    };
  }
  if (!turn) return turn;
  // The gateway allocates the durable row after turn.started. Later frames carry
  // its canonical identity without resetting the trace or the device's clock.
  if (turn.id && event.turn_id === turn.id) {
    const position = typeof event.position === 'number' ? event.position : turn.position;
    const createdAt = typeof event.created_at === 'number' ? event.created_at : turn.createdAt;
    if (position !== turn.position || createdAt !== turn.createdAt) {
      turn = { ...turn, position, createdAt };
    }
  }
  // A settled bubble never re-animates on a trailing or replayed body frame.
  if (turn.status !== 'running') return turn;

  // A queued message sent with `→` lands at the start of the step it names, so its
  // bubble opens before that step's reasoning and work. A replayed frame repeats the
  // same list; it replaces, never doubles.
  if (type === 'turn.input') {
    const messages = Array.isArray(event.messages)
      ? (event.messages as DeliveredUserInput[]).filter(
          (message) => typeof message?.request === 'string',
        )
      : [];
    if (!messages.length) return turn;
    return updateRunningIteration(turn, eventIterationPosition(event), (iteration) => ({
      ...iteration,
      user_input: messages,
    }));
  }

  if (type === 'content.block.started') {
    const block = event.block as ContentBlock | undefined;
    if (
      !block ||
      typeof block.id !== 'string' ||
      !['conversation', 'visual', 'silence'].includes(block.type)
    ) return turn;
    // Complete presentation records are immutable. Replay keeps their first copy.
    if (turn.content?.some((current) => current.id === block.id)) return turn;
    return { ...turn, content: [...(turn.content ?? []), block], progress: undefined };
  }

  if (type === 'content.block.delta') {
    // Presentation records arrive complete, never as reasoning or prose fragments.
    if (turn.content?.some((block) => block.id === eventBlockKey(event))) return turn;
    const field = eventString(event, 'field');
    const blockId = eventBlockKey(event);
    const position = eventIterationPosition(event);
    const previousBase = turn.streamBases?.[blockId] ?? 0;
    let base = previousBase;
    const applyText = (current: string): string => {
      const next = applyBlockDelta(current, previousBase, event);
      base = next.base;
      return next.text;
    };
    const withBase = (next: RunningTurn): RunningTurn =>
      base === previousBase ? next : { ...next, streamBases: { ...turn.streamBases, [blockId]: base } };
    if (field === 'text') {
      const next = updateRunningIteration(turn, position, (iteration) => ({
        ...iteration,
        thinking: applyText(iteration.thinking ?? ''),
      }));
      return withBase({ ...next, progress: undefined });
    }
    if (field === 'markdown' && blockId.includes(':assistant-prose:')) {
      const next = updateRunningIteration(turn, position, (iteration) => ({
        ...iteration,
        assistant_prose: applyText(iteration.assistant_prose ?? ''),
      }));
      return withBase({ ...next, answer: '', progress: undefined });
    }
    if (field === 'markdown') {
      return withBase({
        ...turn,
        answer: applyText(turn.answer),
        progress: undefined,
      });
    }
    return turn;
  }

  if (type === 'iteration.completed') {
    const position = eventIterationPosition(event);
    const attached = Array.isArray(event.attachments)
      ? (event.attachments as IterationAttachment[])
      : undefined;
    const next = updateRunningIteration(turn, position, (iteration) => ({
      ...iteration,
      // Present null deliberately rejects a raw fragment; only omission falls back.
      thinking: Object.prototype.hasOwnProperty.call(event, 'thinking')
        ? eventString(event, 'thinking')
        : iteration.thinking,
      presentation_content: Array.isArray(event.presentation_content)
        ? (event.presentation_content as ContentBlock[])
        : iteration.presentation_content,
      assistant_prose: Array.isArray(event.presentation_content)
        ? undefined
        : eventString(event, 'assistant_prose') || iteration.assistant_prose,
      attachments: attached?.length ? attached : iteration.attachments,
      error: undefined,
    }));
    const promoted = next.iterations.find(
      (iteration) => iteration.position === position,
    )?.assistant_prose;
    const streamBases = next.streamBases && { ...next.streamBases };
    if (streamBases) {
      if (Object.prototype.hasOwnProperty.call(event, 'thinking')) {
        delete streamBases[`${turn.id}:reasoning:${position}`];
      }
      if (Array.isArray(event.presentation_content) || eventString(event, 'assistant_prose')) {
        delete streamBases[`${turn.id}:assistant-prose:${position}`];
      }
    }
    return {
      ...next,
      answer: promoted ? '' : turn.answer,
      progress: undefined,
      streamBases,
    };
  }

  if (type === 'block.preview') {
    const form = formFromEvent(event);
    if (!form.block_id) return turn;
    const position = eventIterationPosition(event);
    const next = updateRunningIteration(turn, position, (iteration) =>
      upsertRunningForm(iteration, form),
    );
    return { ...next, progress: undefined };
  }

  if (type === 'block.activity') {
    const activity = activityProjectionFromWire(event.activity);
    return activity ? applyFormActivity(turn, event, activity) : turn;
  }

  if (type === 'block.started' || type === 'block.output') {
    const form = formFromEvent(event);
    if (!form.block_id) return turn;
    const position = eventIterationPosition(event);
    const next = updateRunningIteration(turn, position, (iteration) =>
      upsertRunningForm(iteration, form),
    );
    if (type === 'block.output') return { ...next, progress: undefined };
    return {
      ...next,
      progress: {
        kind: 'code',
        iteration: position,
        operation: form.scope,
      },
    };
  }

  if (type === 'turn.progress') {
    if (turn.id && typeof event.turn_id === 'string' && event.turn_id !== turn.id) return turn;
    const kind = eventString(event, 'progress');
    const wait = kind === 'extension-readiness' ? event.extension_wait : undefined;
    const extensionWait =
      wait &&
      (wait.stage === 'waiting' || wait.stage === 'preparing') &&
      Number.isFinite(wait.since_ms) &&
      wait.since_ms >= 0
        ? wait
        : undefined;
    // Use the gateway's sampled offset so a clock difference never changes the elapsed time.
    const extensionWaitStartedAt =
      extensionWait && typeof event.ts === 'number'
        ? Date.now() - Math.max(0, event.ts - extensionWait.since_ms)
        : extensionWait?.since_ms;
    const rawIteration = event.iteration;
    const iteration =
      typeof rawIteration === 'number'
        ? rawIteration
        : typeof rawIteration === 'string' && rawIteration.trim()
          ? Number(rawIteration)
          : undefined;
    return {
      ...turn,
      resumed: undefined,
      progress: kind
        ? {
            kind,
            iteration: Number.isFinite(iteration) ? iteration : undefined,
            command: eventString(event, 'cmd') || undefined,
            operation: eventString(event, 'op') || undefined,
            label: eventString(event, 'label') || undefined,
            phrase: eventString(event, 'phrase') || undefined,
            model: eventString(event, 'model') || undefined,
            wait: kind === 'provider-wait' ? providerWaitFromEvent(event) : undefined,
            busy: kind === 'python' ? pythonBusyFromEvent(event) : undefined,
            extensionWait,
            extensionWaitStartedAt,
            transcription: kind === 'attachment-transcription' ? transcriptionFromEvent(event) : undefined,
          }
        : undefined,
    };
  }

  if (type === 'iteration.error' || type === 'provider.retry') {
    const position = eventIterationPosition(event);
    const next = updateRunningIteration(turn, position, (iteration) => ({
      ...iteration,
      error: (event.error_data ??
        event.error ??
        event.detail ??
        'retrying') as TranscriptIteration['error'],
    }));
    return { ...next, progress: undefined };
  }

  return turn;
}
