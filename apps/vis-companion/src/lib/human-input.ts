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
  max_length?: number;
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
export type HumanInputValue = string | string[] | boolean;

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
    ...(maxLength ? { max_length: maxLength } : {}),
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

function defaultValue(field: HumanInputField): HumanInputValue {
  const fallback = field.default;
  switch (field.type) {
    case 'checkbox':
      return fallback === true;
    case 'multiselect':
      return Array.isArray(fallback) ? fallback.filter((item): item is string => typeof item === 'string') : [];
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
  if (Array.isArray(value)) return value.length === 0;
  return typeof value !== 'string' || value.trim() === '';
}

/** True when nothing is missing — the same rule the engine enforces on submit. */
export function isHumanInputAnswerable(request: HumanInputRequest, values: HumanInputValues): boolean {
  return request.fields.every((field) => !isHumanInputBlank(field, values[field.id]));
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
