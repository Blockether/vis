/**
 * Human Input is the input-capable View: a typed form that BLOCKS its producer.
 * The shared `view.open` / `view.close` envelope carries `kind: 'input'`; this
 * module owns only the capability-specific document parser and reducer.
 *
 * Validation functions stay in the engine and never cross the wire. A refused
 * submit returns field errors while the same View remains open.
 */

import viewSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/view.json';
import type { SseEvent } from './types';
import {
  VIEW_CLOSE_EVENT,
  VIEW_OPEN_EVENT,
  isViewContainerType,
  type ViewFlexLayout,
  type ViewGridLayout,
  viewFlexLayout,
  viewGridLayout,
  viewKind,
  viewCommandIcon,
  type ViewCommandIcon,
} from './view';

/** The closed set of fields that can carry an ANSWER (`view.spec/field-types`). */
export const HUMAN_INPUT_FIELD_TYPES = [
  'plaintext',
  'password',
  'multiline',
  'select',
  'toggle',
  'checkbox',
  'range',
  'datetime',
  'otp',
] as const;

/**
 * Ink rather than a question (`view.spec/decor-types`). A `heading` opens
 * a section of a long form and a `paragraph` explains one; neither is named,
 * answered, or focusable.
 */
export const HUMAN_INPUT_DECOR_TYPES = ['heading', 'paragraph'] as const;

/**
 * Every node a request's tree can hold: the answerable fields, the containers that arrange them, and the decorations that are only read.
 */
export const HUMAN_INPUT_NODE_TYPES = [
  ...HUMAN_INPUT_FIELD_TYPES,
  'flex',
  'grid',
  'section',
  'collapsible',
  ...HUMAN_INPUT_DECOR_TYPES,
] as const;

/**
 * The engine's own fallbacks, copied here because TypeScript needs them at
 * compile time: `view.spec/range-defaults` and `view.spec/otp-defaults`.
 * A slider with no bounds is a PERCENTAGE and a code with no width is six
 * digits — on both surfaces, because a Clojure test reads these very lines and
 * fails when the engine moves and the app does not.
 */
export const HUMAN_INPUT_RANGE_DEFAULTS = { min: 0, max: 100, step: 1 } as const;
export const HUMAN_INPUT_OTP_DEFAULTS = { length: 6, ceiling: 12 } as const;

export type HumanInputDatetimeMode = 'date' | 'time' | 'datetime';
type DatetimeModeSchema = {
  const: HumanInputDatetimeMode;
  'x-vis-form': string;
  'x-vis-pattern': string;
};
type DatetimeFieldSchema = {
  properties: { type?: { const?: string }; mode?: { default: HumanInputDatetimeMode } };
};
const DATETIME_MODES = viewSchema.$defs.datetime_mode.oneOf as unknown as DatetimeModeSchema[];
/** What a `datetime` field asks for (`datetime_mode` in view.json). CLOSED. */
export const HUMAN_INPUT_DATETIME_MODES: readonly HumanInputDatetimeMode[] = DATETIME_MODES.map(
  (mode) => mode.const,
);
/** The form of the text of each mode, as a person reads it: `YYYY-MM-DD`. */
export const HUMAN_INPUT_DATETIME_FORMS = Object.fromEntries(
  DATETIME_MODES.map((mode) => [mode.const, mode['x-vis-form']]),
) as Record<HumanInputDatetimeMode, string>;
const DATETIME_PATTERNS = Object.fromEntries(
  DATETIME_MODES.map((mode) => [mode.const, new RegExp(mode['x-vis-pattern'])]),
) as Record<HumanInputDatetimeMode, RegExp>;
const DATETIME_DEFAULT_MODE =
  (viewSchema.$defs.component.oneOf as unknown as DatetimeFieldSchema[]).find(
    (branch) => branch.properties.type?.const === 'datetime',
  )?.properties.mode?.default ?? 'datetime';
/** From this many options on, a `select` is one line that opens a list (`x-vis-fold-at`). */
export const HUMAN_INPUT_SELECT_FOLD_AT = (
  viewSchema.$defs.component.oneOf as unknown as (DatetimeFieldSchema & { 'x-vis-fold-at'?: number })[]
).find((branch) => branch.properties.type?.const === 'select')!['x-vis-fold-at'] as number;

/** Read the mode of a `datetime` field; an absent or unknown one is the schema default. */
export function humanInputDatetimeMode(raw: unknown): HumanInputDatetimeMode {
  return (HUMAN_INPUT_DATETIME_MODES as readonly unknown[]).includes(raw)
    ? (raw as HumanInputDatetimeMode)
    : DATETIME_DEFAULT_MODE;
}

/** True when `value` has the form of `mode`. */
export function isHumanInputDatetimeText(mode: HumanInputDatetimeMode, value: unknown): boolean {
  return typeof value === 'string' && DATETIME_PATTERNS[mode].test(value);
}

/**
 * The status glyph in front of a choice, from the one place the terminal keeps
 * it (`channel-tui.dialogs/choice-mark`): a filled/hollow dot for an EXCLUSIVE
 * choice — pick one and the other drops — and a box for an INCLUSIVE one, so
 * "choose one" can never look like "choose any" on either surface.
 */
export const HUMAN_INPUT_CHOICE_MARKS = {
  exclusiveOn: '●',
  exclusiveOff: '○',
  inclusiveOn: '[✓]',
  inclusiveOff: '[ ]',
} as const;

export type HumanInputFieldType = (typeof HUMAN_INPUT_NODE_TYPES)[number];

/** True when this node is pure decoration — read on the form, never answered. */
export function humanInputIsDecoration(node: { type: HumanInputFieldType }): boolean {
  return (HUMAN_INPUT_DECOR_TYPES as readonly string[]).includes(node.type);
}

export interface HumanInputOption {
  value: string;
  label: string;
  icon?: ViewCommandIcon;
}

export interface HumanInputField extends Partial<ViewFlexLayout>, ViewGridLayout {
  /** How the answer is keyed. `name` is the contract; `id` is its alias. */
  id: string;
  name: string;
  type: HumanInputFieldType;
  label: string;
  is_required: boolean;
  /** Prose under the label. Always rendered in italic. */
  description?: string;
  placeholder?: string;
  /** `plaintext` only — a picture in the box. The label stays the accessible name. */
  icon?: ViewCommandIcon;
  options?: HumanInputOption[];
  /** Text length bounds; on an `otp` field, how many boxes there are. */
  min_length?: number;
  max_length?: number;
  /** `range` only — the engine defaults these to 0, 100 and 1. */
  min?: number;
  max?: number;
  step?: number;
  /** `datetime` only — what it asks for, and the wire `min` and `max` in the form of the mode. */
  mode?: HumanInputDatetimeMode;
  earliest?: string;
  latest?: string;
  default?: unknown;
  is_growing?: boolean;
  is_multiple?: boolean;
  default_expanded?: boolean;
  children?: HumanInputField[];
  /** A DECORATION only — the words it paints. It has no name and no label. */
  text?: string;
}

/** The session that asks. A subagent's form also names its parent session. */
export interface HumanInputOrigin {
  session_id: string;
  title?: string;
  parent_session_id?: string;
  parent_title?: string;
}

export interface HumanInputRequest {
  id: string;
  title: string;
  description?: string;
  source?: string;
  session_id?: string;
  /** Present on forms from the gateway. See {@link originLabel}. */
  origin?: HumanInputOrigin;
  nodes: HumanInputField[];
  submit_label: string;
  cancel_label: string;
  is_cancellable: boolean;
  /**
   * The engine's deadline for this ask, in milliseconds. ABSENT means there is
   * none: the engine sends `0` for a request that waits indefinitely, so the app
   * never tells the operator a form is about to expire when it is not.
   */
  timeout_ms?: number;
  created_at?: number;
}

/** Fields whose answers must not leave the active form except on submit. */
export function isHumanInputSecret(field: HumanInputField): boolean {
  return field.type === 'password' || field.type === 'otp';
}

/** What one field submits. The engine coerces and validates it again. */
export type HumanInputValue = string | string[] | boolean | number;

export type HumanInputValues = Record<string, HumanInputValue>;

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

function fieldType(value: unknown): HumanInputFieldType | null {
  const name = text(value);
  return (HUMAN_INPUT_NODE_TYPES as readonly string[]).includes(name)
    ? (name as HumanInputFieldType)
    : null;
}

function optionFromWire(raw: unknown): HumanInputOption | null {
  const row = record(raw);
  if (!row) return null;
  const value = text(row.value);
  if (value === '') return null;
  return { value, label: text(row.label) || value, ...viewCommandIcon(row.icon) };
}

/** One finite number off the wire — a `range` bound, or nothing. */
function bound(raw: unknown): number | undefined {
  return typeof raw === 'number' && Number.isFinite(raw) ? raw : undefined;
}

function fieldFromWire(raw: unknown): HumanInputField | null {
  const row = record(raw);
  if (!row) return null;
  const type = fieldType(row.type);
  if (!type) return null;
  const growth = row.is_growing === true ? { is_growing: true } : {};
  // A DECORATION is parsed BEFORE identity: nobody answers a heading, so it has
  // no name to key and nothing to check but the words it paints — and blank ink
  // paints nothing, so it never reaches the screen.
  if (humanInputIsDecoration({ type })) {
    const words = optionalText(row.text);
    if (!words) return null;
    return { id: '', name: '', type, label: '', is_required: false, text: words, ...growth };
  }
  const id = text(row.name) || text(row.id);
  if (id === '') return null;
  if (isViewContainerType(type)) {
    const children = (Array.isArray(row.children) ? row.children : [])
      .map(fieldFromWire)
      .filter((field): field is HumanInputField => field !== null);
    if (!children.length) return null;
    return {
      id,
      name: id,
      type,
      label: text(row.label),
      is_required: false,
      children,
      ...growth,
      ...(type === 'flex' ? viewFlexLayout(row) : {}),
      ...(type === 'grid' ? viewGridLayout(row) : {}),
      ...(type === 'section' || type === 'collapsible' ? viewCommandIcon(row.icon) : {}),
      ...(type === 'collapsible' ? { default_expanded: row.default_expanded === true } : {}),
      ...(optionalText(row.description) ? { description: optionalText(row.description) } : {}),
    };
  }
  const options = Array.isArray(row.options)
    ? row.options
        .map(optionFromWire)
        .filter((option): option is HumanInputOption => option !== null)
    : undefined;
  const description = optionalText(row.description);
  const placeholder = optionalText(row.placeholder);
  const minLength =
    typeof row.min_length === 'number' && row.min_length > 0 ? row.min_length : undefined;
  const maxLength =
    typeof row.max_length === 'number' && row.max_length > 0 ? row.max_length : undefined;
  if (type === 'datetime') {
    // A bound in another form is no bound: the engine refuses it, and the native
    // control would ignore it.
    const mode = humanInputDatetimeMode(row.mode);
    return {
      id,
      name: id,
      type,
      label: text(row.label) || id,
      is_required: row.is_required === true,
      ...(description ? { description } : {}),
      ...(placeholder ? { placeholder } : {}),
      mode,
      ...growth,
      ...(isHumanInputDatetimeText(mode, row.min) ? { earliest: row.min as string } : {}),
      ...(isHumanInputDatetimeText(mode, row.max) ? { latest: row.max as string } : {}),
      ...(row.default === undefined ? {} : { default: row.default }),
    };
  }
  return {
    id,
    name: id,
    type,
    label: text(row.label) || id,
    is_required: row.is_required === true,
    ...(description ? { description } : {}),
    ...(placeholder ? { placeholder } : {}),
    ...growth,
    ...(type === 'plaintext' ? viewCommandIcon(row.icon) : {}),
    ...((type === 'select' || type === 'toggle') && row.is_multiple === true ? { is_multiple: true } : {}),
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
  const nodes = (Array.isArray(row.nodes) ? row.nodes : [])
    .map(fieldFromWire)
    .filter((field): field is HumanInputField => field !== null);
  if (!nodes.length) return null;
  const description = optionalText(row.description);
  const source = optionalText(row.source);
  const sessionId = optionalText(row.session_id);
  const origin = originFromWire(row.origin);
  // `0` is the engine's INDEFINITE wait, not a deadline that already passed:
  // drop it so nothing downstream counts down to zero on a form that is
  // simply parked until the operator answers it.
  const timeoutMs =
    typeof row.timeout_ms === 'number' && row.timeout_ms > 0 ? row.timeout_ms : undefined;
  return {
    id,
    title,
    nodes,
    submit_label: text(row.submit_label).trim() || 'Submit',
    cancel_label: text(row.cancel_label).trim() || 'Cancel',
    // Only an explicit `false` locks the operator out of dismissing the form.
    is_cancellable: row.is_cancellable !== false,
    ...(description ? { description } : {}),
    ...(source ? { source } : {}),
    ...(sessionId ? { session_id: sessionId } : {}),
    ...(origin ? { origin } : {}),
    ...(timeoutMs ? { timeout_ms: timeoutMs } : {}),
    ...(typeof row.created_at === 'number' ? { created_at: row.created_at } : {}),
  };
}

/** One form input from the wire, the same parser a live view uses for its inputs. */
export const humanInputFieldFromWire = fieldFromWire;
function originFromWire(raw: unknown): HumanInputOrigin | undefined {
  const row = record(raw);
  const sessionId = row ? optionalText(row.session_id) : undefined;
  if (!row || !sessionId) return undefined;
  const title = optionalText(row.title);
  const parentId = optionalText(row.parent_session_id);
  const parentTitle = optionalText(row.parent_title);
  return {
    session_id: sessionId,
    ...(title ? { title } : {}),
    ...(parentId ? { parent_session_id: parentId } : {}),
    ...(parentTitle ? { parent_title: parentTitle } : {}),
  };
}

/**
 * The subagent and parent names a form header shows, or null for a form that a
 * top-level session raised. The leader session shows the form of a subagent,
 * so the header must say which session asks.
 */
export function originLabel(
  origin: HumanInputOrigin | undefined,
): { agent: string; parent: string } | null {
  if (!origin?.parent_session_id) return null;
  return {
    agent: origin.title ?? origin.session_id.slice(0, 8),
    parent: origin.parent_title ?? origin.parent_session_id.slice(0, 8),
  };
}

/** Every pending request in a `GET /v1/sessions/:sid/views/input` body. */
export function inputViewsFromWire(raw: unknown): HumanInputRequest[] {
  return (Array.isArray(raw) ? raw : [])
    .map(humanInputRequestFromWire)
    .filter((request): request is HumanInputRequest => request !== null);
}

/** A `range` field's bounds, with the engine's own defaults filled in. */
export function humanInputRange(field: HumanInputField): {
  min: number;
  max: number;
  step: number;
} {
  const min = Number.isFinite(field.min) ? (field.min as number) : HUMAN_INPUT_RANGE_DEFAULTS.min;
  const max =
    Number.isFinite(field.max) && (field.max as number) > min
      ? (field.max as number)
      : Math.max(min + 1, HUMAN_INPUT_RANGE_DEFAULTS.max);
  const step =
    Number.isFinite(field.step) && (field.step as number) > 0
      ? (field.step as number)
      : HUMAN_INPUT_RANGE_DEFAULTS.step;
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
  const max = Number.isFinite(field.max_length)
    ? (field.max_length as number)
    : HUMAN_INPUT_OTP_DEFAULTS.length;
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
 * Every field that ANSWERS something, depth first, in reading order. A container is a branch of the tree and holds no value of its own, and a decoration
 * holds none either, so everything that reads or writes values walks the leaves
 * and never the nodes.
 */
export function humanInputInputFields(fields: readonly HumanInputField[]): HumanInputField[] {
  return fields.flatMap((field) => {
    if (isViewContainerType(field.type)) return humanInputInputFields(field.children ?? []);
    return humanInputIsDecoration(field) ? [] : [field];
  });
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

/** The value a field starts with: its own default, or the empty value of its type. */
export function humanInputDefaultValue(field: HumanInputField): HumanInputValue {
  const fallback = field.default;
  if ((field.type === 'select' || field.type === 'toggle') && field.is_multiple) {
    return Array.isArray(fallback)
      ? fallback.filter((item): item is string => typeof item === 'string')
      : [];
  }
  switch (field.type) {
    case 'checkbox':
      return fallback === true;
    case 'range':
      return clampHumanInputRange(
        field,
        typeof fallback === 'number' ? fallback : Number(fallback),
      );
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
    humanInputInputFields(request.nodes).map((field) => [field.id, humanInputDefaultValue(field)]),
  );
}

/** Toggle one option of a multiple choice, preserving the request's option order. */
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

/** True for lifecycle events owned by the input capability. */
export function isInputViewEvent(event: SseEvent): boolean {
  return (
    viewKind(event) === 'input' &&
    (event.type === VIEW_OPEN_EVENT || event.type === VIEW_CLOSE_EVENT)
  );
}

/** Fold one input View lifecycle event into the open-form list. */
export function applyInputViewEvent(
  pending: HumanInputRequest[],
  event: SseEvent,
): HumanInputRequest[] {
  if (!isInputViewEvent(event)) return pending;
  if (event.type === VIEW_CLOSE_EVENT) {
    const viewId = text(event.view_id);
    if (viewId === '') return pending;
    const kept = pending.filter((request) => request.id !== viewId);
    return kept.length === pending.length ? pending : kept;
  }
  if (event.type !== VIEW_OPEN_EVENT) return pending;
  const request = humanInputRequestFromWire(event.view);
  if (!request) return pending;
  const index = pending.findIndex((row) => row.id === request.id);
  if (index < 0) {
    const createdAt = request.created_at;
    const after = createdAt === undefined ? -1 : pending.findIndex(
      (row) => row.created_at !== undefined && row.created_at > createdAt,
    );
    return after < 0 ? [...pending, request] : [...pending.slice(0, after), request, ...pending.slice(after)];
  }
  const merged = pending.slice();
  merged[index] = request;
  return merged;
}
