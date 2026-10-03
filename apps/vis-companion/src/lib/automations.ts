import { timeLabel } from './fleet';
import { homeifyPath } from './path';

/** One trigger of `automations.json#/$defs/trigger`; `kind` selects the other fields. */
export interface AutomationTrigger {
  kind: string;
  expression?: string;
  timezone?: string;
  seconds?: number;
  at?: number;
  signature?: string;
  events?: string[];
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
  /** `url` is the public relay address. Without it, senders call `path` on the gateway. */
  webhook: { path: string; url?: string } | null;
  secrets: { webhook: boolean; callback: boolean };
  last_run: AutomationRun | null;
}

export interface AutomationList {
  automations: Automation[];
  /** False while the global `automations` setting stops every run. */
  is_enabled: boolean;
}

export interface AutomationRunList {
  runs: AutomationRun[];
}

export type AutomationPatch = Partial<Pick<Automation, 'name' | 'enabled' | 'prompt' | 'deliver_only'>>;

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

/** "Every 15 minutes", "Every hour": the largest whole unit. */
export function everyLabel(seconds: number): string {
  const [size, unit] = UNITS.find(([candidate]) => seconds % candidate === 0) ?? UNITS[3];
  const count = seconds / size;
  return count === 1 ? `Every ${unit}` : `Every ${count} ${unit}s`;
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
  settings: 'Automations are off for this target.',
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
