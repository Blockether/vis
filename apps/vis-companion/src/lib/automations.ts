import schema from '../../../../packages/vis-contract/resources/vis-contract/schema/automations.json';
import { timeLabel } from './fleet';
import { homeifyPath } from './path';

/** One test on the JSON payload of a webhook: `automations.json#/$defs/webhook_filter`. */
export interface AutomationWebhookFilter {
  field: string;
  equals?: string | number | boolean | null;
  contains?: string;
  in?: (string | number | boolean)[];
}

/** One trigger of `automations.json#/$defs/trigger`; `kind` selects the other fields. */
export interface AutomationTrigger {
  kind: string;
  expression?: string;
  timezone?: string;
  seconds?: number;
  at?: number;
  signature?: string;
  events?: string[];
  filters?: AutomationWebhookFilter[];
}

/** Where each run sends its prompt: `automations.json#/$defs/target`. */
export interface AutomationTarget {
  mode: string;
  session_id?: string;
  root?: string;
  group_id?: string;
}

/** One run as the gateway reports it: `automations.json#/$defs/run`. Times are epoch ms. */
export interface AutomationRun {
  id: string;
  automation_id: string;
  automation_name: string;
  trigger: string;
  status: string;
  reason: string | null;
  scheduled_at: number | null;
  created_at: number;
  started_at: number | null;
  finished_at: number | null;
  session_id: string | null;
  turn_id: string | null;
  answer: string | null;
  error: string | null;
  is_silent: boolean;
}

/** `automations.json#/$defs/automation`. `secrets` tells only if a secret exists. */
export interface Automation {
  id: string;
  name: string;
  enabled: boolean;
  triggers: AutomationTrigger[];
  prompt: string;
  target: AutomationTarget;
  delivery: {
    push?: boolean;
    callback?: { url: string; events?: string[] } | null;
  };
  model: { provider?: string; model: string } | null;
  deliver_only: boolean;
  created_at: number;
  updated_at: number;
  next_run_at: number | null;
  webhook: { path: string } | null;
  secrets: { webhook: boolean; callback: boolean };
  last_run: AutomationRun | null;
}

export interface AutomationList {
  automations: Automation[];
}

export interface AutomationRunList {
  runs: AutomationRun[];
}

/** `automations.json#/$defs/automation_patch`. Triggers, target and delivery replace the saved value. */
export type AutomationPatch = Partial<
  Pick<
    Automation,
    'name' | 'enabled' | 'triggers' | 'prompt' | 'target' | 'delivery' | 'model' | 'deliver_only'
  >
>;

/** A new automation: `automations.json#/$defs/automation_input`. */
export type AutomationInput = Pick<Automation, 'name' | 'triggers' | 'prompt' | 'target'> &
  AutomationPatch;

export type AutomationSecretKind = 'webhook' | 'callback';

/** The only answer that carries a secret value. Show it once and never store it. */
export interface AutomationSecret {
  kind: AutomationSecretKind;
  secret: string;
}

/** A gateway word as a label: "completed" becomes "Completed". A new word still shows. */
export function wordLabel(word: string): string {
  return word.charAt(0).toUpperCase() + word.slice(1);
}

const UNITS: [number, string][] = [
  [86_400, 'day'],
  [3_600, 'hour'],
  [60, 'minute'],
  [1, 'second'],
];

const largestUnit = (seconds: number) =>
  UNITS.find(([candidate]) => seconds % candidate === 0) ?? UNITS[3];

/** "Every 15 minutes", "Every hour": the largest whole unit. */
export function everyLabel(seconds: number): string {
  const [size, unit] = largestUnit(seconds);
  const count = seconds / size;
  return count === 1 ? `Every ${unit}` : `Every ${count} ${unit}s`;
}

/** "1 minute", "365 days": the largest whole unit. */
function durationLabel(seconds: number): string {
  const [size, unit] = largestUnit(seconds);
  const count = seconds / size;
  return `${count} ${unit}${count === 1 ? '' : 's'}`;
}

const SIGNATURES: Record<string, string> = {
  github: 'GitHub',
  standard: 'Standard Webhooks',
  generic: 'Signed',
  token: 'Token',
};

/** One trigger on one line: "Every hour", "Cron 0 9 * * 1-5 · Europe/Warsaw", "Webhook · GitHub". */
export function triggerLabel(trigger: AutomationTrigger, now: number = Date.now()): string {
  switch (trigger.kind) {
    case 'cron':
      return [`Cron ${trigger.expression ?? ''}`.trim(), trigger.timezone].filter(Boolean).join(' · ');
    case 'every':
      return everyLabel(trigger.seconds ?? 0);
    case 'once':
      return `Once · ${timeLabel(trigger.at, now)}`;
    case 'webhook':
      return trigger.signature
        ? `Webhook · ${SIGNATURES[trigger.signature] ?? trigger.signature}`
        : 'Webhook';
    default:
      return wordLabel(trigger.kind);
  }
}

/** Where each run sends its prompt, in the words of `automations.json` target modes. */
export function targetLabel(target: AutomationTarget): string {
  const root = target.root ? ` in ${homeifyPath(target.root)}` : '';
  switch (target.mode) {
    case 'session':
      return 'Continues one existing session';
    case 'new':
      return `New session for each run${root}`;
    case 'temporary':
      return `Temporary session for each run${root}, deleted after the run`;
    default:
      return wordLabel(target.mode);
  }
}

/** How a run tells you that it ended. */
export function deliveryLabel(delivery: Automation['delivery']): string {
  const ways = [
    delivery.push === false ? null : 'Phone alert',
    delivery.callback ? `Callback to ${delivery.callback.url}` : null,
  ].filter(Boolean);
  return ways.length > 0 ? ways.join(' · ') : 'Only the target session';
}

const REASONS: Record<string, string> = {
  overlap: 'The previous run was still running.',
  queue_full: 'Too many runs were waiting.',
};

/** Why a run was skipped or ended, as a sentence. An unknown reason shows as it is. */
export function runReason(run: AutomationRun): string | null {
  const reason = run.error ?? run.reason;
  return reason ? (REASONS[reason] ?? reason) : null;
}

/** When a run last moved: finished, else started, else queued. */
export function runMillis(run: AutomationRun): number {
  return run.finished_at ?? run.started_at ?? run.created_at;
}

/** One list row under the name: state, triggers, next run and last result. */
export function automationSummary(automation: Automation, now: number = Date.now()): string {
  return [
    automation.enabled ? null : 'Paused',
    automation.triggers.map((trigger) => triggerLabel(trigger, now)).join(', '),
    automation.enabled && automation.next_run_at
      ? `Next ${timeLabel(automation.next_run_at, now)}`
      : null,
    automation.last_run ? `Last ${automation.last_run.status}` : null,
  ]
    .filter(Boolean)
    .join(' · ');
}

const DEFS = schema.$defs;
const HOUR_MS = 3_600_000;
const NAMED_UNITS = [...UNITS].reverse();
const SIGNATURE_KINDS = DEFS.webhook_trigger.properties.signature.enum;
const RUN_EVENTS = DEFS.run_event_type.enum;
const EVERY = DEFS.every_trigger.properties.seconds;
const PROMPT_BYTES = schema['x-vis-limits'].prompt_bytes;

/** The longest name and the most triggers of one automation. */
export const NAME_MAX = DEFS.name.maxLength;
export const TRIGGERS_MAX = DEFS.automation_input.properties.triggers.maxItems;

const KIND_LABELS: Record<string, string> = {
  cron: 'Cron schedule',
  every: 'Interval',
  once: 'Once',
  webhook: 'Webhook',
};

/** Trigger kinds in contract order. */
export const TRIGGER_KIND_OPTIONS = [
  DEFS.cron_trigger,
  DEFS.every_trigger,
  DEFS.once_trigger,
  DEFS.webhook_trigger,
].map(({ properties }) => ({
  value: properties.kind.const,
  label: KIND_LABELS[properties.kind.const] ?? wordLabel(properties.kind.const),
}));

const KIND_CHOICES: Record<string, { label: string; hint: string }> = {
  every: { label: 'Repeat at an interval', hint: 'For example, every hour or every day.' },
  cron: { label: 'Run at set times', hint: 'For example, at 9:00 on weekdays.' },
  once: { label: 'Run once', hint: 'At one date and time.' },
  webhook: { label: 'Run when a service sends an event', hint: 'GitHub, GitLab or a script calls Vis.' },
};

/** The place of a kind among the wizard choices. Unknown kinds come last. */
function kindRank(kind: string): number {
  const at = Object.keys(KIND_CHOICES).indexOf(kind);
  return at < 0 ? Object.keys(KIND_CHOICES).length : at;
}

/** The first question of the wizard: what starts the automation. */
export const TRIGGER_CHOICES = [...TRIGGER_KIND_OPTIONS]
  .sort((left, right) => kindRank(left.value) - kindRank(right.value))
  .map(({ value, label }) => ({
    value,
    label: KIND_CHOICES[value]?.label ?? label,
    hint: KIND_CHOICES[value]?.hint ?? '',
  }));

export const UNIT_OPTIONS = NAMED_UNITS.map(([, unit]) => ({ value: unit, label: `${unit}s` }));

const SIGNATURE_HINTS: Record<string, string> = {
  github: 'GitHub repositories and organizations.',
  standard: 'Services that follow Standard Webhooks.',
  generic: 'Your own scripts, signed with HMAC.',
  token: 'GitLab, and senders without a signature.',
};

export const SIGNATURE_OPTIONS = SIGNATURE_KINDS.map((kind) => ({
  value: kind,
  label: SIGNATURES[kind] ?? kind,
  hint: SIGNATURE_HINTS[kind] ?? '',
}));

const TARGET_LABELS: Record<string, string> = {
  new: 'New session for each run',
  temporary: 'Temporary session for each run',
  session: 'One existing session',
};

const TARGET_HINTS: Record<string, string> = {
  new: 'The session stays in your list.',
  temporary: 'Vis deletes it and keeps the answer.',
  session: 'Each run adds a turn to it.',
};

export const TARGET_OPTIONS = [DEFS.new_target, DEFS.temporary_target, DEFS.session_target].map(
  ({ properties }) => ({
    value: properties.mode.const,
    label: TARGET_LABELS[properties.mode.const] ?? wordLabel(properties.mode.const),
    hint: TARGET_HINTS[properties.mode.const] ?? '',
  }),
);

/** `prompt` sends the prompt as the answer without a model: `deliver_only`. */
export const ANSWER_OPTIONS = [
  { value: 'default', label: 'Machine default model', hint: 'The machine chooses the model.' },
  { value: 'model', label: 'A specific model', hint: 'You give the model name.' },
  {
    value: 'prompt',
    label: 'No model: send the prompt as the answer',
    hint: 'For reminders. No model call.',
  },
];

/** "run.completed" becomes "Completed". */
export const RUN_EVENT_OPTIONS = RUN_EVENTS.map((event) => ({
  value: event,
  label: wordLabel(event.replace(/^run\./, '')),
}));

/** One trigger as the form edits it. The text fields keep what you typed. */
export interface TriggerDraft {
  kind: string;
  /** Interval: the number of `unit`s. */
  count: string;
  unit: string;
  expression: string;
  timezone: string;
  /** Once: the local value of a `datetime-local` input, `YYYY-MM-DDTHH:mm`. */
  at: string;
  signature: string;
  /** Webhook event names, separated by commas or spaces. */
  events: string;
  /** The saved trigger. The form keeps the fields that it does not show, such as filters. */
  saved: AutomationTrigger | null;
}

/** The values of the create and edit form. */
export interface AutomationDraft {
  name: string;
  prompt: string;
  triggers: TriggerDraft[];
  target: string;
  root: string;
  sessionId: string;
  /** The group of a saved new-session target. The form keeps it but does not show it. */
  groupId: string;
  push: boolean;
  callbackUrl: string;
  callbackEvents: string[];
  /** A value of `ANSWER_OPTIONS`. */
  answer: string;
  provider: string;
  model: string;
}

/** The IANA time zone of this device. A new cron trigger starts with it. */
export function deviceTimeZone(): string {
  return Intl.DateTimeFormat().resolvedOptions().timeZone ?? '';
}

const pad = (value: number) => String(value).padStart(2, '0');

/** Epoch ms as the local value of a `datetime-local` input, to the minute. */
export function localInput(millis: number): string {
  const date = new Date(millis);
  return `${date.getFullYear()}-${pad(date.getMonth() + 1)}-${pad(date.getDate())}T${pad(
    date.getHours(),
  )}:${pad(date.getMinutes())}`;
}

/** The local `datetime-local` value as epoch ms, or NaN. */
function localMillis(text: string): number {
  const match = /^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2})(?::(\d{2}))?$/.exec(text);
  return match
    ? new Date(+match[1], +match[2] - 1, +match[3], +match[4], +match[5], +(match[6] ?? 0)).getTime()
    : NaN;
}

/** A saved trigger in the form, or a new interval of one day. */
export function triggerDraft(
  trigger: AutomationTrigger | null,
  now: number,
  timezone: string,
): TriggerDraft {
  const seconds = trigger?.kind === 'every' ? (trigger.seconds ?? 0) : 86_400;
  const [size, unit] = largestUnit(seconds);
  const at = trigger?.kind === 'once' ? trigger.at : undefined;
  return {
    kind: trigger?.kind ?? 'every',
    count: String(seconds / size),
    unit,
    expression: trigger?.kind === 'cron' ? (trigger.expression ?? '') : '0 9 * * *',
    timezone: trigger?.kind === 'cron' ? (trigger.timezone ?? '') : timezone,
    at: localInput(at ?? (Math.floor(now / HOUR_MS) + 1) * HOUR_MS),
    signature: (trigger?.kind === 'webhook' ? trigger.signature : undefined) ?? SIGNATURE_KINDS[0],
    events: trigger?.kind === 'webhook' ? (trigger.events ?? []).join(', ') : '',
    saved: trigger,
  };
}

/** The form values of a saved automation, or of a new automation when it is null. */
export function automationDraft(
  automation: Automation | null,
  now: number,
  timezone: string,
): AutomationDraft {
  const target = automation?.target;
  const model = automation?.model;
  const callback = automation?.delivery.callback;
  return {
    name: automation?.name ?? '',
    prompt: automation?.prompt ?? '',
    triggers: automation?.triggers.length
      ? automation.triggers.map((trigger) => triggerDraft(trigger, now, timezone))
      : [triggerDraft(null, now, timezone)],
    target: target?.mode ?? 'new',
    root: target?.root ?? '',
    sessionId: target?.session_id ?? '',
    groupId: target?.group_id ?? '',
    push: automation?.delivery.push !== false,
    callbackUrl: callback?.url ?? '',
    callbackEvents: callback?.events ?? [],
    answer: automation?.deliver_only ? 'prompt' : model ? 'model' : 'default',
    provider: model?.provider ?? '',
    model: model?.model ?? '',
  };
}

const unitSeconds = (unit: string) => UNITS.find(([, name]) => name === unit)?.[0] ?? NaN;

const words = (text: string) => [...new Set(text.split(/[\s,]+/).filter(Boolean))];

/** The saved time while the input still shows it, so its seconds stay. */
function onceMillis(draft: TriggerDraft): number {
  const saved = draft.saved?.kind === 'once' ? draft.saved.at : undefined;
  return saved !== undefined && localInput(saved) === draft.at ? saved : localMillis(draft.at);
}

function triggerInput(draft: TriggerDraft): AutomationTrigger {
  const saved = draft.saved?.kind === draft.kind ? draft.saved : null;
  switch (draft.kind) {
    case 'every':
      return { kind: 'every', seconds: Number(draft.count) * unitSeconds(draft.unit) };
    case 'cron': {
      const timezone = draft.timezone.trim();
      return { kind: 'cron', expression: draft.expression.trim(), ...(timezone ? { timezone } : {}) };
    }
    case 'once':
      return { kind: 'once', at: onceMillis(draft) };
    case 'webhook': {
      const events = words(draft.events);
      return {
        kind: 'webhook',
        signature: draft.signature,
        ...(events.length > 0 ? { events } : {}),
        ...(saved?.filters ? { filters: saved.filters } : {}),
      };
    }
    default:
      return saved ?? { kind: draft.kind };
  }
}

function targetInput(draft: AutomationDraft): AutomationTarget {
  const root = draft.root.trim();
  if (draft.target === 'session') return { mode: 'session', session_id: draft.sessionId.trim() };
  return {
    mode: draft.target,
    ...(root ? { root } : {}),
    ...(draft.target === 'new' && draft.groupId ? { group_id: draft.groupId } : {}),
  };
}

/** The `automation_input` body of the form. */
export function automationInput(draft: AutomationDraft): AutomationInput {
  const url = draft.callbackUrl.trim();
  const provider = draft.provider.trim();
  const model = draft.model.trim();
  const events = RUN_EVENTS.filter((event) => draft.callbackEvents.includes(event));
  return {
    name: draft.name.trim(),
    prompt: draft.prompt,
    triggers: draft.triggers.map(triggerInput),
    target: targetInput(draft),
    delivery: {
      push: draft.push,
      callback: url ? { url, ...(events.length > 0 ? { events } : {}) } : null,
    },
    model: draft.answer !== 'default' && model ? { ...(provider ? { provider } : {}), model } : null,
    deliver_only: draft.answer === 'prompt',
  };
}

/** JSON with sorted keys and without undefined values, so equal values compare equal. */
function canonical(value: unknown): unknown {
  if (Array.isArray(value)) return value.map(canonical);
  if (value === null || typeof value !== 'object') return value;
  return Object.fromEntries(
    Object.entries(value)
      .filter(([, item]) => item !== undefined)
      .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
      .map(([key, item]) => [key, canonical(item)]),
  );
}

const sameValue = (left: unknown, right: unknown) =>
  JSON.stringify(canonical(left)) === JSON.stringify(canonical(right));

const PATCH_FIELDS = [
  'name',
  'prompt',
  'triggers',
  'target',
  'delivery',
  'model',
  'deliver_only',
] as const;

/** Only the fields that differ from the saved automation. Empty when nothing changed. */
export function automationPatch(draft: AutomationDraft, automation: Automation): AutomationPatch {
  const next = automationInput(draft);
  return Object.fromEntries(
    PATCH_FIELDS.filter((field) => !sameValue(next[field], automation[field])).map((field) => [
      field,
      next[field],
    ]),
  ) as AutomationPatch;
}

function triggerProblem(draft: TriggerDraft, now: number): string | null {
  switch (draft.kind) {
    case 'every': {
      const count = Number(draft.count);
      const seconds = count * unitSeconds(draft.unit);
      return Number.isInteger(count) && seconds >= EVERY.minimum && seconds <= EVERY.maximum
        ? null
        : `Give an interval from ${durationLabel(EVERY.minimum)} to ${durationLabel(EVERY.maximum)}.`;
    }
    case 'cron':
      return draft.expression.trim() ? null : 'Give a cron expression, for example 0 9 * * 1-5.';
    case 'once': {
      const at = onceMillis(draft);
      if (!Number.isFinite(at)) return 'Choose the date and time of the run.';
      const kept = draft.saved?.kind === 'once' && draft.saved.at === at;
      return at > now || kept ? null : 'Choose a time in the future.';
    }
    default:
      return null;
  }
}

/** The wizard steps that hold fields, in the order of `draftProblem`. */
export type AutomationStep = 'task' | 'when' | 'place' | 'answer';

/** The first reason why the gateway would refuse the fields of one step, or null. */
export function stepProblem(
  step: AutomationStep,
  draft: AutomationDraft,
  now: number = Date.now(),
): string | null {
  switch (step) {
    case 'task':
      if (!draft.name.trim()) return 'Give the automation a name.';
      if (!draft.prompt.trim()) return 'Write the prompt.';
      if (new TextEncoder().encode(draft.prompt).length > PROMPT_BYTES)
        return `Make the prompt shorter. The limit is ${PROMPT_BYTES} bytes.`;
      return null;
    case 'when':
      if (draft.triggers.length === 0) return 'Add a trigger.';
      if (draft.triggers.filter((trigger) => trigger.kind === 'webhook').length > 1)
        return 'An automation can have only one webhook trigger.';
      for (const [index, trigger] of draft.triggers.entries()) {
        const problem = triggerProblem(trigger, now);
        if (problem)
          return draft.triggers.length > 1 ? `Trigger ${index + 1}: ${problem}` : problem;
      }
      return null;
    case 'place':
      return draft.target === 'session' && !draft.sessionId.trim() ? 'Give the session ID.' : null;
    case 'answer': {
      const url = draft.callbackUrl.trim();
      if (url && !/^https?:\/\/\S+$/.test(url))
        return 'Start the callback address with https:// or http://.';
      return draft.answer === 'model' && !draft.model.trim() ? 'Give the model name.' : null;
    }
  }
}

const PROBLEM_ORDER: AutomationStep[] = ['task', 'when', 'place', 'answer'];

/** The first step that the gateway would refuse, with its reason, or null. */
export function draftStepProblem(
  draft: AutomationDraft,
  now: number = Date.now(),
): { step: AutomationStep; problem: string } | null {
  for (const step of PROBLEM_ORDER) {
    const problem = stepProblem(step, draft, now);
    if (problem) return { step, problem };
  }
  return null;
}

/** The first reason why the gateway would refuse the form, or null. */
export function draftProblem(draft: AutomationDraft, now: number = Date.now()): string | null {
  return draftStepProblem(draft, now)?.problem ?? null;
}
