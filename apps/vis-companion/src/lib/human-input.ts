/**
 * Typed human-input requests, as the companion app sees them.
 *
 * An extension can BLOCK a run on the operator. The engine publishes that pause
 * on every channel the request names — `:tui` and `:app` by default — and the
 * gateway turns the `:app` half into the session events `human_input.request`
 * and `human_input.close`. Both are ordinary journal events, so a phone that
 * was asleep replays them and a phone that is closed gets a push instead.
 *
 * This module is the PURE half of the app's side: wire parsing, the form's
 * starting values, and the reduction of those two events into the list of open
 * requests. The dialog itself only renders what happens here, so the rules are
 * testable without a DOM.
 *
 * There is NO validation in here. A field's validators are FUNCTIONS in the
 * extension that asked the question — they never cross the wire — so the only
 * verdict that exists is the one a REFUSED confirmation brings back.
 */

import type { SseEvent } from './types';

export const HUMAN_INPUT_REQUEST_EVENT = 'human_input.request';
export const HUMAN_INPUT_CLOSE_EVENT = 'human_input.close';

/** The closed field set the engine accepts (`human-input/field-types`). */
export const HUMAN_INPUT_FIELD_TYPES = [
  'plaintext',
  'password',
  'multiline',
  'select',
  'multiselect',
  'checkbox',
  'range',
  'otp',
  'group',
] as const;

export type HumanInputFieldType = (typeof HUMAN_INPUT_FIELD_TYPES)[number];

export interface HumanInputOption {
  value: string;
  label: string;
}

export interface HumanInputField {
  /** How the answer is keyed. `name` is the contract; `id` is its alias. */
  id: string;
  name: string;
  type: HumanInputFieldType;
  label: string;
  is_required: boolean;
  /** Prose under the label. Always rendered in italic. */
  description?: string;
  placeholder?: string;
  options?: HumanInputOption[];
  /** Text length bounds; on an `otp` field, how many boxes there are. */
  min_length?: number;
  max_length?: number;
  /** `range` only — the engine defaults these to 0, 100 and 1. */
  min?: number;
  max?: number;
  step?: number;
  default?: unknown;
  /** `group` only — how its children are laid out. */
  direction?: 'row' | 'column';
  /** `group` only — the fields it owns, which may be groups themselves. */
  fields?: HumanInputField[];
}

export interface HumanInputRequest {
  id: string;
  title: string;
  description?: string;
  source?: string;
  session_id?: string;
  fields: HumanInputField[];
  submit_label: string;
  cancel_label: string;
  is_cancellable: boolean;
  /**
   * The engine's deadline for this ask, in milliseconds. ABSENT means there is
   * none: the engine sends `0` for a request that waits indefinitely, so the app
   * never tells the operator a form is about to expire when it is not.
   */
  timeout_ms?: number;
}

/** What one field submits. The engine coerces and validates it again. */
export type HumanInputValue = string | string[] | boolean | number;

export type HumanInputValues = Record<string, HumanInputValue>;

/** `POST .../actions/submit` — the engine's verdict, not the app's. */
export interface HumanInputOutcome {
  is_accepted: boolean;
  request_id: string;
  /** Field id -> message. Present only when the answer was REJECTED. */
  errors?: Record<string, string>;
}

function record(value: unknown): Record<string, unknown> | null {
  return value !== null && typeof value === 'object' && !Array.isArray(value)
    ? (value as Record<string, unknown>)
    : null;
}

function text(value: unknown): string {
  return typeof value === 'string' ? value : '';
}

function optionalText(value: unknown): string | undefined {
  const trimmed = text(value).trim();
  return trimmed === '' ? undefined : trimmed;
}

function fieldType(value: unknown): HumanInputFieldType {
  const name = text(value);
  return (HUMAN_INPUT_FIELD_TYPES as readonly string[]).includes(name)
    ? (name as HumanInputFieldType)
    : 'plaintext';
}

function optionFromWire(raw: unknown): HumanInputOption | null {
  const row = record(raw);
  if (!row) return null;
  const value = text(row.value);
  if (value === '') return null;
  return { value, label: text(row.label) || value };
}

/** One finite number off the wire — a `range` bound, or nothing. */
function bound(raw: unknown): number | undefined {
  return typeof raw === 'number' && Number.isFinite(raw) ? raw : undefined;
}

function fieldFromWire(raw: unknown): HumanInputField | null {
  const row = record(raw);
  if (!row) return null;
  const id = text(row.name) || text(row.id);
  if (id === '') return null;
  const type = fieldType(row.type);
  // A LAYOUT GROUP answers nothing itself: it owns fields, and an empty one is
  // not a form control but a hole, so it never reaches the screen.
  if (type === 'group') {
    const children = (Array.isArray(row.fields) ? row.fields : [])
      .map(fieldFromWire)
      .filter((field): field is HumanInputField => field !== null);
    if (!children.length) return null;
    return {
      id,
      name: id,
      type,
      label: text(row.label),
      is_required: false,
      direction: row.direction === 'row' ? 'row' : 'column',
      fields: children,
      ...(optionalText(row.description) ?? optionalText(row.help)
        ? { description: optionalText(row.description) ?? optionalText(row.help) }
        : {}),
    };
  }
  const options = Array.isArray(row.options)
    ? row.options.map(optionFromWire).filter((option): option is HumanInputOption => option !== null)
    : undefined;
  const description = optionalText(row.description) ?? optionalText(row.help);
  const placeholder = optionalText(row.placeholder);
  const minLength = typeof row.min_length === 'number' && row.min_length > 0 ? row.min_length : undefined;
  const maxLength = typeof row.max_length === 'number' && row.max_length > 0 ? row.max_length : undefined;
  return {
    id,
    name: id,
    type,
    label: text(row.label) || id,
    is_required: row.is_required === true,
    ...(description ? { description } : {}),
    ...(placeholder ? { placeholder } : {}),
    ...(options ? { options } : {}),
    ...(minLength ? { min_length: minLength } : {}),
    ...(maxLength ? { max_length: maxLength } : {}),
    ...(bound(row.min) === undefined ? {} : { min: bound(row.min) }),
    ...(bound(row.max) === undefined ? {} : { max: bound(row.max) }),
    ...(bound(row.step) === undefined ? {} : { step: bound(row.step) }),
    ...(row.default === undefined ? {} : { default: row.default }),
  };
}

/**
 * One pending request off the wire, or `null` when the frame cannot be shown as
 * a form. A malformed request must never park the screen behind a dialog the
 * operator cannot answer — the TUI stays authoritative in that case.
 */
export function humanInputRequestFromWire(raw: unknown): HumanInputRequest | null {
  const row = record(raw);
  if (!row) return null;
  const id = text(row.id);
  const title = text(row.title).trim();
  if (id === '' || title === '') return null;
  const fields = (Array.isArray(row.fields) ? row.fields : [])
    .map(fieldFromWire)
    .filter((field): field is HumanInputField => field !== null);
  if (!fields.length) return null;
  const description = optionalText(row.description);
  const source = optionalText(row.source);
  const sessionId = optionalText(row.session_id);
  // `0` is the engine's INDEFINITE wait, not a deadline that already passed:
  // drop it so nothing downstream counts down to zero on a form that is
  // simply parked until the operator answers it.
  const timeoutMs = typeof row.timeout_ms === 'number' && row.timeout_ms > 0 ? row.timeout_ms : undefined;
  return {
    id,
    title,
    fields,
    submit_label: text(row.submit_label).trim() || 'Submit',
    cancel_label: text(row.cancel_label).trim() || 'Cancel',
    // Only an explicit `false` locks the operator out of dismissing the form.
    is_cancellable: row.is_cancellable !== false,
    ...(description ? { description } : {}),
    ...(source ? { source } : {}),
    ...(sessionId ? { session_id: sessionId } : {}),
    ...(timeoutMs ? { timeout_ms: timeoutMs } : {}),
  };
}

/** Every pending request in a `GET /v1/sessions/:sid/human-input` body. */
export function humanInputRequestsFromWire(raw: unknown): HumanInputRequest[] {
  return (Array.isArray(raw) ? raw : [])
    .map(humanInputRequestFromWire)
    .filter((request): request is HumanInputRequest => request !== null);
}

/** A `range` field's bounds, with the engine's own defaults filled in. */
export function humanInputRange(field: HumanInputField): { min: number; max: number; step: number } {
  const min = Number.isFinite(field.min) ? (field.min as number) : 0;
  const max = Number.isFinite(field.max) && (field.max as number) > min ? (field.max as number) : Math.max(min + 1, 100);
  const step = Number.isFinite(field.step) && (field.step as number) > 0 ? (field.step as number) : 1;
  return { min, max, step };
}

/** Snap a slider value onto the field's step and clamp it into its bounds. */
export function clampHumanInputRange(field: HumanInputField, value: number): number {
  const { min, max, step } = humanInputRange(field);
  if (!Number.isFinite(value)) return min;
  const snapped = min + Math.round((value - min) / step) * step;
  const bounded = Math.min(max, Math.max(min, snapped));
  // Float steps accumulate error (0.1 + 0.2); round to the step's own precision
  // so the label reads 0.3 and not 0.30000000000000004.
  const decimals = (String(step).split('.')[1] ?? '').length;
  return decimals > 0 ? Number(bounded.toFixed(decimals)) : bounded;
}

/** How many digits an `otp` field holds — the engine's own six-by-default. */
export function humanInputOtp(field: HumanInputField): { min: number; max: number } {
  const max = Number.isFinite(field.max_length) ? (field.max_length as number) : 6;
  const min = Number.isFinite(field.min_length) ? Math.min(field.min_length as number, max) : max;
  return { min, max };
}

/**
 * The digits in `raw`, capped at the field's last box. This is the paste
 * handler: providers print a code as `123 456` or `123-456`, and a human who
 * copied the whole SMS should still land six digits in six boxes.
 */
export function humanInputOtpDigits(field: HumanInputField, raw: string): string {
  return raw.replace(/\D+/g, '').slice(0, humanInputOtp(field).max);
}

/**
 * Every field that ANSWERS something, depth first, in reading order. A layout
 * group is a branch of the tree and holds no value of its own, so everything
 * that reads or writes values walks the leaves and never the nodes.
 */
export function humanInputInputFields(fields: readonly HumanInputField[]): HumanInputField[] {
  return fields.flatMap((field) =>
    field.type === 'group' ? humanInputInputFields(field.fields ?? []) : [field],
  );
}

/**
 * The form as the sheet keeps it: the answers, and the engine's last refusal.
 *
 * PRISTINE, and there is only one way to stop being it. A form starts with no
 * errors because nothing has judged it yet — the validators are FUNCTIONS in the
 * extension that asked the question, so the app could not run them if it wanted
 * to. `humanInputFormRefused` is the ONLY writer of `errors`, and it is fed by a
 * confirmation the engine turned down. Touching a field drops that field's
 * message, so the operator is never corrected about an answer already changed,
 * and the next confirmation asks the whole question again.
 *
 * The TUI band is the same state machine (`channel-tui.human-input`: `set-errors`
 * on a refusal, a `dissoc` on every keystroke), which is why both surfaces show
 * the operator the same words at the same moment.
 */
export interface HumanInputForm {
  values: HumanInputValues;
  /** Field id -> message, in the ENGINE's own words. Never the app's. */
  errors: Record<string, string>;
}

/** A pristine form: the request's own defaults, and nothing to complain about. */
export function humanInputFormStart(request: HumanInputRequest | null): HumanInputForm {
  return { values: request ? initialHumanInputValues(request) : {}, errors: {} };
}

/** Answer one field, which also clears whatever the engine said about it. */
export function humanInputFormChange(
  form: HumanInputForm,
  id: string,
  value: HumanInputValue,
): HumanInputForm {
  const errors = { ...form.errors };
  delete errors[id];
  return { ...form, values: { ...form.values, [id]: value }, errors };
}

/**
 * The engine refused this answer: show exactly the fields it named, whether or
 * not they were ever touched. The daemon has the last word — it ran the
 * extension's own validators, once, on the answer that was actually submitted.
 */
export function humanInputFormRefused(
  form: HumanInputForm,
  errors: Record<string, string> = {},
): HumanInputForm {
  return { ...form, errors };
}

function defaultValue(field: HumanInputField): HumanInputValue {
  const fallback = field.default;
  switch (field.type) {
    case 'checkbox':
      return fallback === true;
    case 'multiselect':
      return Array.isArray(fallback) ? fallback.filter((item): item is string => typeof item === 'string') : [];
    case 'range':
      return clampHumanInputRange(field, typeof fallback === 'number' ? fallback : Number(fallback));
    default:
      return typeof fallback === 'string' ? fallback : '';
  }
}

/**
 * The form's starting values — the request's own defaults, exactly what the TUI
 * dialog opens with, so the two surfaces submit the same answer for the same
 * keystrokes.
 */
export function initialHumanInputValues(request: HumanInputRequest): HumanInputValues {
  // `Object.fromEntries` (not `values[id] = …`): a field id of `__proto__` is a
  // plain string to the engine, but assigning it on an object literal hits the
  // prototype setter and silently drops the field — the form could never be
  // answered from the app while the TUI happily edits it.
  return Object.fromEntries(
    humanInputInputFields(request.fields).map((field) => [field.id, defaultValue(field)]),
  );
}

/** Toggle one option of a `multiselect`, preserving the request's option order. */
export function toggleHumanInputOption(
  field: HumanInputField,
  values: HumanInputValues,
  option: string,
): string[] {
  const current = values[field.id];
  const chosen = new Set(Array.isArray(current) ? current : []);
  if (chosen.has(option)) chosen.delete(option);
  else chosen.add(option);
  return (field.options ?? []).map((row) => row.value).filter((value) => chosen.has(value));
}

/** True for the two session events this module reduces. */
export function isHumanInputEvent(event: SseEvent): boolean {
  return event.type === HUMAN_INPUT_REQUEST_EVENT || event.type === HUMAN_INPUT_CLOSE_EVENT;
}

/**
 * Fold one session event into the open-request list.
 *
 * Replay is the normal case (a reconnect re-reads the journal), so a repeated
 * `human_input.request` REPLACES its row instead of stacking a second dialog,
 * and a close for an unknown id is a no-op. The list identity is preserved when
 * nothing changed: the dialog must not remount under the operator's fingers.
 */
export function applyHumanInputEvent(pending: HumanInputRequest[], event: SseEvent): HumanInputRequest[] {
  if (event.type === HUMAN_INPUT_CLOSE_EVENT) {
    const requestId = text(event.request_id);
    if (requestId === '') return pending;
    const kept = pending.filter((request) => request.id !== requestId);
    return kept.length === pending.length ? pending : kept;
  }
  if (event.type !== HUMAN_INPUT_REQUEST_EVENT) return pending;
  const request = humanInputRequestFromWire(event.request);
  if (!request) return pending;
  const index = pending.findIndex((row) => row.id === request.id);
  if (index < 0) return [...pending, request];
  const merged = pending.slice();
  merged[index] = request;
  return merged;
}
