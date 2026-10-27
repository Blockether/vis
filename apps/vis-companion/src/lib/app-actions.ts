/**
 * The Save and Send state of an app attachment.
 *
 * A Save is a `draft` or `change` action. A Send is a `submit` action. The surface
 * shows `saved` only after the host acknowledges the action with a new `seq`.
 */

import { GatewayError } from './gateway';
import { submissionFromWire, type LiveSubmission } from './live-view';
import { randomUuid } from './uuid';
import type { ViewActionOutcome } from './view';

/** Every state that a Save or a Send can show. CLOSED. */
export const APP_ACTION_STATUSES = [
  'idle',
  'saving',
  'saved',
  'offline',
  'conflict',
  'stale',
  'closed',
  'invalid',
  'unbound',
  'disconnected',
  'failed',
] as const;

export type AppActionStatus = (typeof APP_ACTION_STATUSES)[number];

export interface AppActionResult {
  status: AppActionStatus;
  /** The host state version after the action, or the current version of a conflict. */
  seq?: number;
  /** The current diff revision of a `stale` refusal. */
  revision?: string;
  /** The thread that a `draft` saved into. */
  threadId?: string;
  /** The receipt of an accepted Send. */
  submission?: LiveSubmission;
  errors?: Record<string, string>;
  message?: string;
}

/** The short words that each state shows next to its control. */
export const APP_ACTION_WORDS: Record<AppActionStatus, string> = {
  idle: '',
  saving: 'Saving',
  saved: 'Saved',
  offline: 'Offline',
  conflict: 'Changed elsewhere',
  stale: 'New revision',
  closed: 'Closed',
  invalid: 'Not valid',
  unbound: 'No such key',
  disconnected: 'Disconnected',
  failed: 'Failed',
};

/** True for a state that a retry of the same action can clear. */
export function isRetryable(status: AppActionStatus): boolean {
  return status === 'offline' || status === 'failed';
}

function record(value: unknown): Record<string, unknown> | undefined {
  return value && typeof value === 'object' && !Array.isArray(value)
    ? (value as Record<string, unknown>)
    : undefined;
}

function refusal(raw: Record<string, unknown>): AppActionResult {
  const seq = typeof raw.seq === 'number' ? raw.seq : undefined;
  const revision = typeof raw.revision === 'string' ? raw.revision : undefined;
  switch (raw.reason) {
    case 'conflict':
      return { status: 'conflict', ...(seq === undefined ? {} : { seq }) };
    case 'stale_revision':
      return {
        status: 'stale',
        ...(seq === undefined ? {} : { seq }),
        ...(revision ? { revision } : {}),
      };
    case 'closed':
      return { status: 'closed' };
    case 'unbound':
      return { status: 'unbound' };
    // The owner is gone, such as after a restart: the app is read only until it comes back.
    case 'disconnected':
      return { status: 'disconnected' };
    case 'invalid': {
      const errors = record(raw.errors) as Record<string, string> | undefined;
      return { status: 'invalid', ...(errors ? { errors } : {}) };
    }
    default:
      return {
        status: 'failed',
        message: typeof raw.reason === 'string' ? raw.reason : 'The app refused the action.',
      };
  }
}

/** The state that one host answer puts a Save or a Send in. */
export function settleAppAction(outcome: ViewActionOutcome): AppActionResult {
  const raw = outcome as unknown as Record<string, unknown>;
  if (!outcome.is_accepted) return refusal(raw);
  const wire = record(outcome.submission);
  const submission = wire ? submissionFromWire(wire) : null;
  return {
    status: 'saved',
    ...(typeof outcome.seq === 'number' ? { seq: outcome.seq } : {}),
    ...(outcome.thread_id ? { threadId: outcome.thread_id } : {}),
    ...(submission ? { submission } : {}),
  };
}

/** The state that a failed request puts a Save or a Send in. */
export function failAppAction(error: unknown): AppActionResult {
  if (error instanceof GatewayError) {
    if (error.status === 0) return { status: 'offline', message: error.message };
    const body = record(error.body);
    if (body && typeof body.reason === 'string') return refusal(body);
    return { status: 'failed', message: error.message };
  }
  return { status: 'failed', message: error instanceof Error ? error.message : String(error) };
}

/** Send one action and read its answer or its failure as a state. */
export async function runAppAction(
  send: () => Promise<ViewActionOutcome>,
): Promise<AppActionResult> {
  try {
    return settleAppAction(await send());
  } catch (error) {
    return failAppAction(error);
  }
}

/**
 * The idempotency key of each Send.
 *
 * A retry after an offline or failed attempt reuses the key, so the host answers
 * the same receipt. An accepted Send or a refusal frees the key for the next Send.
 */
export class SendKeys {
  private pending: string | undefined;
  private readonly make: () => string;

  constructor(make: () => string = randomUuid) {
    this.make = make;
  }

  /** The key for the next attempt. */
  next(): string {
    this.pending ??= this.make();
    return this.pending;
  }

  /** Read the result of the attempt that used the current key. */
  settle(result: AppActionResult): void {
    if (!isRetryable(result.status)) this.pending = undefined;
  }

  /** The key that waits for a retry, if any. */
  get current(): string | undefined {
    return this.pending;
  }
}
