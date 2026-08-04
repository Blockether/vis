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
] as const;

export type HumanInputFieldType = (typeof HUMAN_INPUT_FIELD_TYPES)[number];

export interface HumanInputOption {
  value: string;
  label: string;
}

/**
 * One declarative rule off the wire (`human-input.validation`). The engine is
 * the authority — it re-runs every rule on submit, and the FUNCTION rules a
 * Clojure spec may add never leave it — but running the declarative ones here
 * is the only way the form can say what is wrong before the round trip.
 */
export interface HumanInputRule {
  kind: 'type' | 'pattern' | 'length' | 'bounds' | 'matches';
  /** Already resolved engine-side, so a surface only ever prints it. */
  message: string;
  type?: string;
  pattern?: string;
  field?: string;
  min?: number;
  max?: number;
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
  validate?: HumanInputRule[];
  /** `range` only — the engine defaults these to 0, 100 and 1. */
  min?: number;
  max?: number;
  step?: number;
  default?: unknown;
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

const RULE_KINDS = ['type', 'pattern', 'length', 'bounds', 'matches'] as const;

function ruleFromWire(raw: unknown): HumanInputRule | null {
  const row = record(raw);
  if (!row) return null;
  const kind = RULE_KINDS.find((known) => known === row.kind);
  const message = text(row.message);
  // A rule with no kind or no message could only ever fail silently; the engine
  // still runs its own copy, so dropping it here loses nothing but noise.
  if (!kind || message === '') return null;
  return {
    kind,
    message,
    ...(optionalText(row.type) ? { type: text(row.type) } : {}),
    ...(optionalText(row.pattern) ? { pattern: text(row.pattern) } : {}),
    ...(optionalText(row.field) ? { field: text(row.field) } : {}),
    ...(bound(row.min) === undefined ? {} : { min: bound(row.min) }),
    ...(bound(row.max) === undefined ? {} : { max: bound(row.max) }),
  };
}

function fieldFromWire(raw: unknown): HumanInputField | null {
  const row = record(raw);
  if (!row) return null;
  const id = text(row.name) || text(row.id);
  if (id === '') return null;
  const type = fieldType(row.type);
  const options = Array.isArray(row.options)
    ? row.options.map(optionFromWire).filter((option): option is HumanInputOption => option !== null)
    : undefined;
  const description = optionalText(row.description) ?? optionalText(row.help);
  const placeholder = optionalText(row.placeholder);
  const minLength = typeof row.min_length === 'number' && row.min_length > 0 ? row.min_length : undefined;
  const maxLength = typeof row.max_length === 'number' && row.max_length > 0 ? row.max_length : undefined;
  const validate = Array.isArray(row.validate)
    ? row.validate.map(ruleFromWire).filter((rule): rule is HumanInputRule => rule !== null)
    : undefined;
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
    ...(validate && validate.length > 0 ? { validate } : {}),
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

/** `String` of anything, trimmed — the engine's `text-of`. */
function textOf(value: HumanInputValue | undefined): string {
  return value === undefined || value === null ? '' : String(value).trim();
}

function isBlankValue(value: HumanInputValue | undefined): boolean {
  if (value === undefined || value === null) return true;
  if (typeof value === 'string') return value.trim() === '';
  if (Array.isArray(value)) return value.length === 0;
  return false;
}

function whole(re: RegExp): (value: HumanInputValue) => boolean {
  const anchored = new RegExp(`^(?:${re.source})$`, re.flags);
  return (value) => anchored.test(textOf(value));
}

/**
 * The engine's named `{"type": …}` shapes, mirrored so the form can refuse an
 * answer the engine would refuse. `human-input.validation/value-types` is the
 * original; both surfaces print the message the engine sent, never their own.
 */
const VALUE_TYPES: Record<string, (value: HumanInputValue) => boolean> = {
  email: whole(/[^@\s]+@[^@\s]+\.[^@\s]+/),
  url: whole(/https?:\/\/\S+/i),
  uuid: whole(/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/i),
  digits: whole(/[0-9]+/),
  alpha: whole(/[A-Za-z]+/),
  alphanumeric: whole(/[A-Za-z0-9]+/),
  slug: whole(/[a-z0-9]+(?:-[a-z0-9]+)*/),
  integer: (value) =>
    typeof value === 'number' ? Number.isInteger(value) : /^[+-]?\d+$/.test(textOf(value)),
  number: (value) => (typeof value === 'number' ? true : numberOf(value) !== undefined),
};

function numberOf(value: HumanInputValue | undefined): number | undefined {
  if (typeof value === 'number') return Number.isFinite(value) ? value : undefined;
  const parsed = Number(textOf(value));
  return textOf(value) !== '' && Number.isFinite(parsed) ? parsed : undefined;
}

function checkRule(
  rule: HumanInputRule,
  value: HumanInputValue | undefined,
  values: HumanInputValues,
): string | undefined {
  switch (rule.kind) {
    case 'type': {
      const shape = VALUE_TYPES[rule.type ?? ''];
      // An unknown shape is a newer engine talking to an older app: let it
      // through and let the engine be the one to refuse it.
      return !shape || shape(value as HumanInputValue) ? undefined : rule.message;
    }
    case 'pattern': {
      let re: RegExp;
      try {
        re = new RegExp(rule.pattern ?? '');
      } catch {
        return undefined;
      }
      return re.test(textOf(value)) ? undefined : rule.message;
    }
    case 'length': {
      const n = textOf(value).length;
      const short = rule.min !== undefined && n < rule.min;
      const long = rule.max !== undefined && n > rule.max;
      return short || long ? rule.message : undefined;
    }
    case 'bounds': {
      const n = numberOf(value);
      if (n === undefined) return 'must be a number';
      const under = rule.min !== undefined && n < rule.min;
      const over = rule.max !== undefined && n > rule.max;
      return under || over ? rule.message : undefined;
    }
    case 'matches':
      return textOf(value) === textOf(values[rule.field ?? '']) ? undefined : rule.message;
  }
}

/** The built-in check every field of this type gets, before its own rules. */
function checkShape(field: HumanInputField, value: HumanInputValue | undefined): string | undefined {
  if (field.type === 'otp') {
    const { min, max } = humanInputOtp(field);
    const digits = textOf(value).replace(/[\s-]+/g, '');
    if (!/^[0-9]+$/.test(digits)) return 'must be digits only';
    if (min === max) return digits.length === max ? undefined : `must be ${max} digits`;
    if (digits.length < min) return `must be at least ${min} digits`;
    if (digits.length > max) return `must be at most ${max} digits`;
    return undefined;
  }
  if (field.min_length !== undefined && typeof value === 'string') {
    return value.trim().length < field.min_length
      ? `must be at least ${field.min_length} characters`
      : undefined;
  }
  return undefined;
}

/**
 * What is wrong with ONE answer, in the engine's own words, or `undefined`.
 *
 * The order is the engine's: emptiness first (that is `is_required`'s single
 * job), then the shape of the type, then the field's declared rules in the
 * order they were written. A rule NEVER fires on a blank value, so an optional
 * field the operator skipped stays silent.
 */
export function humanInputFieldError(
  field: HumanInputField,
  value: HumanInputValue | undefined,
  values: HumanInputValues,
): string | undefined {
  if (isHumanInputBlank(field, value)) return 'is required';
  if (isBlankValue(value)) return undefined;
  const shape = checkShape(field, value);
  if (shape) return shape;
  for (const rule of field.validate ?? []) {
    const failed = checkRule(rule, value, values);
    if (failed) return failed;
  }
  return undefined;
}

/** Every field that has something wrong with it, keyed by field id. */
export function humanInputErrors(
  request: HumanInputRequest,
  values: HumanInputValues,
): Record<string, string> {
  return Object.fromEntries(
    request.fields
      .map((field) => [field.id, humanInputFieldError(field, values[field.id], values)] as const)
      .filter((entry): entry is readonly [string, string] => entry[1] !== undefined),
  );
}

/**
 * The errors the operator has EARNED the right to see: a field speaks up once
 * it has been left (`touched`), and the whole form speaks up once submit has
 * been attempted. Shouting "is required" at an untouched form is the oldest
 * form bug there is, so a pristine field stays quiet however wrong it is.
 */
export function visibleHumanInputErrors(
  errors: Record<string, string>,
  touched: ReadonlySet<string>,
  isSubmitAttempted: boolean,
): Record<string, string> {
  if (isSubmitAttempted) return errors;
  return Object.fromEntries(Object.entries(errors).filter(([id]) => touched.has(id)));
}

/**
 * The form as a form library keeps it: the answers, which fields have been
 * TOUCHED (so an error may show), and whether submit has been pressed at least
 * once. Plain data with no React in it — the sheet renders it, the container
 * drives it, and the whole state machine is testable without a DOM.
 */
export interface HumanInputForm {
  values: HumanInputValues;
  touched: readonly string[];
  isSubmitAttempted: boolean;
}

/** A pristine form: the request's own defaults, nothing touched, nothing shouted. */
export function humanInputFormStart(request: HumanInputRequest | null): HumanInputForm {
  return {
    values: request ? initialHumanInputValues(request) : {},
    touched: [],
    isSubmitAttempted: false,
  };
}

function withTouched(touched: readonly string[], id: string): readonly string[] {
  return touched.includes(id) ? touched : [...touched, id];
}

/**
 * Answer one field. Editing a field counts as touching it, exactly as the TUI
 * band does, so a mistake is named while it is being corrected instead of
 * ambushing the operator at submit time.
 */
export function humanInputFormChange(
  form: HumanInputForm,
  id: string,
  value: HumanInputValue,
): HumanInputForm {
  return { ...form, values: { ...form.values, [id]: value }, touched: withTouched(form.touched, id) };
}

/** Leaving a field is the other way to touch it — the classic blur-then-complain. */
export function humanInputFormBlur(form: HumanInputForm, id: string): HumanInputForm {
  return { ...form, touched: withTouched(form.touched, id) };
}

/**
 * Press submit. Either the form is ready and the caller may send `values`, or
 * every error becomes visible at once and NOTHING is sent — a refusal the
 * operator can read is worth more than a button that is merely dead.
 */
export function humanInputFormSubmit(
  form: HumanInputForm,
  request: HumanInputRequest,
): { form: HumanInputForm; errors: Record<string, string>; isReady: boolean } {
  const errors = humanInputErrors(request, form.values);
  const isReady = Object.keys(errors).length === 0;
  return { form: isReady ? form : { ...form, isSubmitAttempted: true }, errors, isReady };
}

/**
 * What this form should SHOW right now: its own visible errors, with the
 * engine's refusals laid on top — the daemon has the last word, and its
 * verdict is shown whether or not the field was ever touched.
 */
export function humanInputFormErrors(
  form: HumanInputForm,
  request: HumanInputRequest,
  engineErrors: Record<string, string> = {},
): Record<string, string> {
  const visible = visibleHumanInputErrors(
    humanInputErrors(request, form.values),
    new Set(form.touched),
    form.isSubmitAttempted,
  );
  return { ...visible, ...engineErrors };
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
  return Object.fromEntries(request.fields.map((field) => [field.id, defaultValue(field)]));
}

/** A required field the operator has not answered yet. */
export function isHumanInputBlank(field: HumanInputField, value: HumanInputValue | undefined): boolean {
  if (!field.is_required) return false;
  if (field.type === 'checkbox') return value !== true;
  // A slider always sits somewhere on its track, so `is_required` on a range can
  // only catch a value that never arrived.
  if (field.type === 'range') return typeof value !== 'number' || !Number.isFinite(value);
  if (Array.isArray(value)) return value.length === 0;
  return typeof value !== 'string' || value.trim() === '';
}

/** True when the form has no errors at all — what the engine checks on submit. */
export function isHumanInputAnswerable(request: HumanInputRequest, values: HumanInputValues): boolean {
  return Object.keys(humanInputErrors(request, values)).length === 0;
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
