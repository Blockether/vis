/**
 * The live capability of a View: a non-blocking semantic picture driven by patches.
 * Shared `view.open`, `view.patch`, and `view.close` frames carry `kind: 'live'`;
 * this module owns the closed document vocabulary and its pure materializer.
 */

import type { SseEvent } from './types';
import type { ActivityProjection, ActivityRow } from './activity';
import {
  VIEW_CLOSE_EVENT,
  VIEW_GROUP_DIRECTIONS,
  VIEW_OPEN_EVENT,
  VIEW_PATCH_EVENT,
  VIEW_SHORTCUT_LABEL_MAX,
  VIEW_SHORTCUT_NAMED_KEYS,
  VIEW_SHORTCUTS_MAX,
  viewGroupDirection,
  viewKind,
  viewMaxColumns,
} from './view';
import type { ViewAnchor } from './view';
import {
  HUMAN_INPUT_FIELD_TYPES,
  humanInputFieldFromWire,
  type HumanInputField,
} from './human-input';

/** What a view can be MADE of (`view.spec/live-node-types`). CLOSED. */
export const LIVE_NODE_TYPES = [
  'status',
  'progress',
  'stat',
  'steps',
  'log',
  'table',
  'link',
  'paragraph',
  'heading',
  'code',
  'spinner',
  'button',
  'divider',
  'card',
  'diff',
  'thread',
] as const;

/** The inputs a live view can hold: the form's own input types. */
export const LIVE_INPUT_TYPES = HUMAN_INPUT_FIELD_TYPES;

/**
 * The one node that paints NOTHING of its own: it ARRANGES the nodes it holds,
 * and it is the FORM's group verbatim (`view.spec/group-type-name`), so
 * a run lays a view out with the same words it lays a question out with.
 */
export const LIVE_GROUP_TYPE = 'group';

/** How a group stands what it holds (`view.spec/group-directions`). CLOSED. */
export const LIVE_GROUP_DIRECTIONS = VIEW_GROUP_DIRECTIONS;
/** How a surface COLOURS one line, row, step or stat (`live-tones`). CLOSED. */
export const LIVE_TONES = ['idle', 'running', 'ok', 'warn', 'error'] as const;

/**
 * Everything a patch can do to a view (`live-ops`). CLOSED.
 *
 * Most address ONE node by id: `set-value` puts an input's value in place, the
 * value the engine answers each accepted `change` and `submit` with. `add-node`
 * and `remove-node` change the view's SHAPE. There is no seventh: protocol 7 took Activity off the Live View rail
 * and gave it to the form that produced it, so a view no longer carries a
 * projection for a patch to replace.
 */
export const LIVE_OPS = [
  'set',
  'set-value',
  'append',
  'remove',
  'clear',
  'add-node',
  'remove-node',
  'thread',
  'submission',
  'shortcuts',
  'agent-note',
] as const;

/** Where a view stands (`view.spec/placements`). CLOSED. An `attachment` is an app. */
export const LIVE_PLACEMENTS = ['inline', 'panel', 'attachment'] as const;

/** The media type of the transcript row that lists an app attachment. */
export const APP_ATTACHMENT_MEDIA = 'application/vnd.vis.app+json';

/** The delivery stages of one Send, in order (`submission_status`). CLOSED. */
export const LIVE_SUBMISSION_STATUSES = [
  'accepted',
  'delivered',
  'processing',
  'answered',
  'failed',
] as const;

/** An app holds at most this many receipts (`live_view.submissions.maxItems`). */
export const LIVE_SUBMISSIONS_MAX = 50;

/** The longest message or draft text (`message.body.maxLength`). */
export const LIVE_MESSAGE_CHARS = 20000;

/** What a link POINTS AT (`live-link-targets`). CLOSED. */
export const LIVE_LINK_TARGETS = ['attachment', 'path', 'url'] as const;

/** How a column's cells sit under their header (`live-aligns`). CLOSED. */
export const LIVE_ALIGNS = ['left', 'right'] as const;

/** The paint order a table can declare (`live-orders`). CLOSED. */
export const LIVE_ORDERS = ['insertion', 'newest-first'] as const;

/** Which way a `{by: …}` order runs (`live-sort-dirs`). CLOSED. */
export const LIVE_SORT_DIRS = ['asc', 'desc'] as const;

/** The window a log declares when the wire leaves it out; a panel keeps every line. */
export const LIVE_LOG_WINDOW = 2000;
/** A table holds this many rows. */
export const LIVE_TABLE_MAX_ROWS = 5000;

/**
 * The most characters a stop note carries (`note-chars`). The engine cuts a
 * longer one rather than refusing the stop, so the field stops growing here for
 * the same reason: a comment the human cannot see the end of would be a comment
 * the model reads differently than they wrote it.
 */
export const LIVE_NOTE_CHARS = 500;

export type LiveNodeType = (typeof LIVE_NODE_TYPES)[number];
export type LiveInputType = (typeof LIVE_INPUT_TYPES)[number];
export type LiveTone = (typeof LIVE_TONES)[number];
export type LivePlacement = (typeof LIVE_PLACEMENTS)[number];
export type LiveLinkTarget = (typeof LIVE_LINK_TARGETS)[number];
export type LiveAlign = (typeof LIVE_ALIGNS)[number];
export type LiveSortDir = (typeof LIVE_SORT_DIRS)[number];
export type LiveGroupDirection = (typeof LIVE_GROUP_DIRECTIONS)[number];
export type LiveSubmissionStatus = (typeof LIVE_SUBMISSION_STATUSES)[number];

/** `insertion`, `newest-first`, or one DECLARED column, ties kept in insertion order. */
export type LiveOrder = (typeof LIVE_ORDERS)[number] | { by: string; dir?: LiveSortDir };

interface LiveNodeBase {
  id: string;
  /** Absent on a bare status line: the sentence is its own label. */
  label?: string;
}

/** One line that REPLACES itself — what the run is doing right now. */
export interface LiveStatusNode extends LiveNodeBase {
  type: 'status';
  text: string;
  detail?: string;
  tone: LiveTone;
}

/** A fraction, or a count with no known end (`value`, else `done` of `total`). */
export interface LiveProgressNode extends LiveNodeBase {
  type: 'progress';
  value?: number;
  done?: number;
  total?: number;
}

export interface LiveStat {
  id: string;
  label: string;
  value_text: string;
  tone: LiveTone;
}

/** A strip of counters that keep their slot as their numbers move. */
export interface LiveStatNode extends LiveNodeBase {
  type: 'stat';
  stats: LiveStat[];
}

export interface LiveStep {
  id: string;
  label: string;
  tone: LiveTone;
  detail?: string;
  value?: string;
}

/** A known list of phases, each carrying its own tone. */
export interface LiveStepsNode extends LiveNodeBase {
  type: 'steps';
  steps: LiveStep[];
}

/** Output as it arrives. `lines` is what this panel holds of the `total_lines` recorded. */
export interface LiveLogNode extends LiveNodeBase {
  type: 'log';
  lines: string[];
  window_lines: number;
  total_lines: number;
  /** Semantic tones aligned with `lines`; null/absent entries are plain. */
  line_tones?: (LiveTone | null)[];
  default_expanded?: boolean;
}

export interface LiveColumn {
  id: string;
  label: string;
  align: LiveAlign;
}

export interface LiveRow {
  id: string;
  cells: string[];
  tone: LiveTone;
  /** The id of the GROUP this row hangs under; an id nobody declared is a group of its own. */
  parent?: string;
}

/**
 * A group the table DECLARED: the fold its rows hang under, the order it paints
 * in, the label its head wears and the tone that says a leg failed before anybody
 * opens it. `is_open` is an initial hint — the reader's own hand wins after that.
 */
export interface LiveGroup {
  id: string;
  label?: string;
  tone?: LiveTone;
  order?: number;
  is_open?: boolean;
}

/** Rows keyed by id: an update lands in the slot the eye left it in. */
export interface LiveTableNode extends LiveNodeBase {
  type: 'table';
  columns: LiveColumn[];
  rows: LiveRow[];
  max_rows: number;
  order: LiveOrder;
  is_selectable: boolean;
  selected_ids: string[];
  groups: LiveGroup[];
}

export interface LiveLink {
  id: string;
  label: string;
  target: string;
  target_kind: LiveLinkTarget;
  tone?: LiveTone;
}

/** Where the work ALSO lives: a run page, a report, an attachment. */
export interface LiveLinkNode extends LiveNodeBase {
  type: 'link';
  links: LiveLink[];
}

export interface LiveParagraphNode extends LiveNodeBase {
  type: 'paragraph';
  text: string;
}
export interface LiveHeadingNode extends LiveNodeBase {
  type: 'heading';
  text: string;
  level: 1 | 2 | 3 | 4 | 5 | 6;
}
export interface LiveCodeNode extends LiveNodeBase {
  type: 'code';
  text: string;
  language?: string;
}
export type LiveSpinnerVariant = 'braille' | 'dots' | 'line' | 'pulse';
export interface LiveSpinnerNode extends LiveNodeBase {
  type: 'spinner';
  text: string;
  variant: LiveSpinnerVariant;
  is_active: boolean;
}
export interface LiveButtonNode extends LiveNodeBase {
  type: 'button';
  label: string;
  /** `activate` sends one click; `submit` sends the inputs of the nearest group. */
  action: 'activate' | 'submit';
  is_disabled: boolean;
  clicks: number;
}

/** A choice tile: a title, optional text and meta line, and a click count. */
export interface LiveCardNode extends LiveNodeBase {
  type: 'card';
  title: string;
  text?: string;
  meta?: string;
  tone?: LiveTone;
  is_selectable: boolean;
  clicks: number;
}

/**
 * A form input inside a live view. `field` is the form's own field; `value` is
 * the current value, absent for a secret, which only says `is_set`.
 */
export interface LiveInputNode extends LiveNodeBase {
  type: LiveInputType;
  field: HumanInputField;
  value?: unknown;
  is_set?: boolean;
}

/**
 * Nodes side by side (`row`), one under the other (`column`), in equal columns
 * that wrap (`grid`, capped by `max_columns`) or at their own width (`wrap`). The run
 * declares an arrangement ONCE and no op carries it, so a layout never
 * rearranges itself under a reader; a narrow screen stacks a row anyway.
 */
export interface LiveGroupNode extends LiveNodeBase {
  type: 'group';
  direction: LiveGroupDirection;
  max_columns?: number;
  children: LiveNode[];
  is_collapsible?: boolean;
  default_expanded?: boolean;
}

/** Who wrote a message. The host sets it; a client never sends it. */
export interface LiveAuthor {
  kind: 'human' | 'agent' | 'extension';
  name?: string;
}

/** One posted message of a thread, with a stable id from the host. */
export interface LiveMessage {
  id: string;
  author: LiveAuthor;
  body: string;
  created_at: number;
  submission_id?: string;
}

/** A discussion. `draft` is the saved, unsent text of the human. */
export interface LiveThread {
  id: string;
  anchor?: ViewAnchor;
  messages: LiveMessage[];
  draft?: string;
  is_resolved?: boolean;
}

/** A unified diff with its revisions and the threads anchored to its lines. */
export interface LiveDiffNode extends LiveNodeBase {
  type: 'diff';
  patch: string;
  old_revision?: string;
  new_revision?: string;
  threads: LiveThread[];
}

/** One thread as a node of its own. The node id is the thread id. */
export interface LiveThreadNode extends LiveNodeBase {
  type: 'thread';
  anchor?: ViewAnchor;
  messages: LiveMessage[];
  draft?: string;
  is_resolved?: boolean;
}

/** The host receipt of one Send. */
export interface LiveSubmission {
  id: string;
  idempotency_key: string;
  node_id?: string;
  status: LiveSubmissionStatus;
  created_at: number;
  updated_at: number;
  revision?: string;
  note?: string;
  comments?: { node_id: string; thread_id: string; message_id: string; body: string }[];
}

/** A static thematic break spanning its allocated content width. */
export interface LiveDividerNode {
  id: string;
  type: 'divider';
  label?: never;
}

/** Every node that PAINTS — what an op may name and what a surface draws. */
export type LiveLeafNode =
  | LiveStatusNode
  | LiveProgressNode
  | LiveStatNode
  | LiveStepsNode
  | LiveLogNode
  | LiveTableNode
  | LiveLinkNode
  | LiveParagraphNode
  | LiveHeadingNode
  | LiveCodeNode
  | LiveSpinnerNode
  | LiveButtonNode
  | LiveCardNode
  | LiveInputNode
  | LiveDividerNode
  | LiveDiffNode
  | LiveThreadNode;

/** A node either paints something, or arranges the nodes it holds. */
export type LiveNode = LiveLeafNode | LiveGroupNode;

/** Trusted engine identity of the operation that opened a live view. */
export interface LiveViewOwner {
  invocation_id: string;
  activity_id?: string;
}

export function liveOwnerFromWire(value: unknown): LiveViewOwner | undefined {
  const owner = record(value);
  const invocation = owner && optionalText(owner.invocation_id);
  if (!invocation) return undefined;
  return { invocation_id: invocation, activity_id: optionalText(owner?.activity_id) };
}

/** Match the original source, never merged row ids or the most recent operation. */
export function liveOwnerMatches(
  owner: LiveViewOwner | undefined,
  activity: ActivityProjection | undefined,
): boolean {
  if (!owner || !activity) return false;
  if (owner.activity_id && owner.activity_id === activity.history?.id) return true;
  const contains = (rows: ActivityRow[]): boolean =>
    rows.some((row) => row.id === owner.invocation_id || contains(row.children ?? []));
  return contains(activity.rows);
}

export interface LiveView {
  id: string;
  title: string;
  description?: string;
  nodes: LiveNode[];
  /** The last patch this picture has folded in. */
  seq: number;
  created_at?: number;
  source?: string;
  owner?: LiveViewOwner;
  /** `inline` in the transcript, `panel` beside it, or `attachment` for an app. */
  placement?: LivePlacement;
  /** The newest Send receipts of an app, oldest first. */
  submissions?: LiveSubmission[];
  /** The owner archived this app. It keeps its state. */
  is_archived?: boolean;
  /** The owner closed this app. It keeps its last state and takes no actions. */
  is_closed?: boolean;
  /**
   * The host's `is_stale`: the saved picture of an owned view whose extension is
   * not loaded. It is kept apart from `is_stale`, which asks for a re-read.
   */
  is_disconnected?: boolean;
  /** The session that shows the view. */
  session_id?: string;
  /** The extension that owns a panel view. */
  extension?: string;
  /** The owner's own stable name for the view. */
  key?: string;
  /** Local reconnect marker, never sent on the wire. */
  is_stale?: boolean;
  /** The keys an app binds, in the order the owner declared them. */
  shortcuts?: LiveShortcut[];
}

/**
 * The named keys an app can bind (`shortcut_key`). Any other key is ONE printable
 * ASCII character, `!` to `~`, compared case-sensitively. Escape and Tab never
 * bind: they close the app and move focus.
 */
export const LIVE_SHORTCUT_NAMED_KEYS = VIEW_SHORTCUT_NAMED_KEYS;

/** The most keys one app binds. */
export const LIVE_SHORTCUTS_MAX = VIEW_SHORTCUTS_MAX;

/**
 * One key that an app binds. A key with `node_id` presses that node. A key with
 * `action` goes to the owner as `{action: "shortcut", key}`.
 */
export interface LiveShortcut {
  key: string;
  label?: string;
  action?: string;
  node_id?: string;
}

/** True when `key` is a key that an app can bind. */
export function isLiveShortcutKey(key: string): boolean {
  if (LIVE_SHORTCUT_NAMED_KEYS.includes(key)) return true;
  if (key.length !== 1) return false;
  const code = key.charCodeAt(0);
  return code >= 0x21 && code <= 0x7e;
}

function nonBlank(value: unknown): string | undefined {
  const one = optionalText(value);
  return one && one.trim() !== '' ? one : undefined;
}

function shortcutFromWire(raw: Record<string, unknown>): LiveShortcut | null {
  const key = text(raw.key);
  if (!isLiveShortcutKey(key)) return null;
  const label = optionalText(raw.label)?.trim().slice(0, VIEW_SHORTCUT_LABEL_MAX);
  const action = nonBlank(raw.action);
  const nodeId = nonBlank(raw.node_id);
  // A shortcut has exactly one target: an owner action or a node to press.
  if (Boolean(action) === Boolean(nodeId)) return null;
  return {
    key,
    ...(label ? { label } : {}),
    ...(action ? { action } : {}),
    ...(nodeId ? { node_id: nodeId } : {}),
  };
}

/** The bindings read from the wire: valid keys only, the first of a repeated key, at most the bound. */
export function liveShortcutsFromWire(raw: unknown): LiveShortcut[] {
  const seen = new Set<string>();
  const kept: LiveShortcut[] = [];
  for (const one of keyed(raw, shortcutFromWire)) {
    if (seen.has(one.key) || kept.length >= LIVE_SHORTCUTS_MAX) continue;
    seen.add(one.key);
    kept.push(one);
  }
  return kept;
}

/**
 * One page of a log node's RECORD, as `GET …/views/live/:view/log?node=…` answers
 * it. A panel reads the pages that came before the lines it was sent, so what it
 * shows is the whole log; search answers from the same route.
 */
export interface LiveLogPage {
  node_id: string;
  from: number;
  lines: string[];
  total: number;
  /** Total matching lines; `from` is the zero-based offset within those matches. */
  matched: number;
  /** Original, one-based line numbers corresponding to `lines`. */
  line_numbers: number[];
  line_tones?: (LiveTone | null)[];
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

function optionalNumber(value: unknown): number | undefined {
  return typeof value === 'number' && Number.isFinite(value) ? value : undefined;
}

function count(value: unknown, fallback: number): number {
  const given = optionalNumber(value);
  return given === undefined ? fallback : Math.max(0, Math.trunc(given));
}

function rows(value: unknown): Record<string, unknown>[] {
  if (!Array.isArray(value)) return [];
  return value.map(record).filter((row): row is Record<string, unknown> => row !== null);
}

function lines(value: unknown): string[] {
  return Array.isArray(value) ? value.map((line) => text(line)) : [];
}

/** Distinct selected ids which still name a row, in the engine's order. */
function selectedIds(value: unknown, tableRows: LiveRow[]): string[] {
  const existing = new Set(tableRows.map((row) => row.id));
  const seen = new Set<string>();
  return (Array.isArray(value) ? value : [])
    .map(text)
    .filter((id) => id !== '' && existing.has(id) && !seen.has(id) && Boolean(seen.add(id)));
}

/** An unknown tone paints as `idle` rather than as nothing: the LINE still matters. */
function tone(value: unknown): LiveTone {
  const name = text(value);
  return (LIVE_TONES as readonly string[]).includes(name) ? (name as LiveTone) : 'idle';
}

function align(value: unknown): LiveAlign {
  return text(value) === 'right' ? 'right' : 'left';
}

function linkTarget(value: unknown): LiveLinkTarget {
  const name = text(value);
  return (LIVE_LINK_TARGETS as readonly string[]).includes(name)
    ? (name as LiveLinkTarget)
    : 'path';
}

function order(value: unknown): LiveOrder {
  const name = text(value);
  if ((LIVE_ORDERS as readonly string[]).includes(name)) return name as LiveOrder;
  const sorted = record(value);
  const by = sorted ? optionalText(sorted.by) : undefined;
  if (!by) return 'insertion';
  const dir = text(sorted?.dir) === 'desc' ? 'desc' : 'asc';
  return { by, dir };
}

function statFromWire(raw: Record<string, unknown>): LiveStat | null {
  const id = text(raw.id);
  if (id === '') return null;
  return {
    id,
    label: text(raw.label) || id,
    value_text: text(raw.value_text),
    tone: tone(raw.tone),
  };
}

function stepFromWire(raw: Record<string, unknown>): LiveStep | null {
  const id = text(raw.id);
  if (id === '') return null;
  return {
    id,
    label: text(raw.label) || id,
    tone: tone(raw.tone),
    detail: optionalText(raw.detail),
    value: optionalText(raw.value),
  };
}

function columnFromWire(raw: Record<string, unknown>): LiveColumn | null {
  const id = text(raw.id);
  if (id === '') return null;
  return { id, label: text(raw.label) || id, align: align(raw.align) };
}

function rowFromWire(raw: Record<string, unknown>): LiveRow | null {
  const id = text(raw.id);
  if (id === '') return null;
  return {
    id,
    cells: Array.isArray(raw.cells) ? raw.cells.map((cell) => text(cell)) : [],
    tone: tone(raw.tone),
    parent: optionalText(raw.parent),
  };
}

function groupFromWire(raw: Record<string, unknown>): LiveGroup | null {
  const id = text(raw.id);
  if (id === '') return null;
  const label = optionalText(raw.label);
  const given = optionalNumber(raw.order);
  // Only what was DECLARED is written down: an absent key must stay absent, so a
  // later declaration can merge into this one instead of erasing it.
  return {
    id,
    ...(label === undefined ? {} : { label }),
    ...(raw.tone === undefined ? {} : { tone: tone(raw.tone) }),
    ...(given === undefined ? {} : { order: Math.trunc(given) }),
    ...(raw.is_open === undefined ? {} : { is_open: raw.is_open === true }),
  };
}

function linkFromWire(raw: Record<string, unknown>): LiveLink | null {
  const id = text(raw.id);
  const target = text(raw.target);
  if (id === '' || target === '') return null;
  return {
    id,
    label: text(raw.label) || target,
    target,
    target_kind: linkTarget(raw.target_kind),
    tone: raw.tone === undefined ? undefined : tone(raw.tone),
  };
}

function anchorFromWire(value: unknown): ViewAnchor | undefined {
  const raw = record(value);
  const path = raw ? optionalText(raw.path) : undefined;
  const line = raw ? optionalNumber(raw.line) : undefined;
  if (!raw || !path || line === undefined || line < 1) return undefined;
  if (raw.side !== 'old' && raw.side !== 'new') return undefined;
  return { path, side: raw.side, line: Math.trunc(line) };
}

function authorFromWire(value: unknown): LiveAuthor {
  const raw = record(value);
  const kind = raw?.kind === 'agent' || raw?.kind === 'extension' ? raw.kind : 'human';
  const name = optionalText(raw?.name);
  return name ? { kind, name } : { kind };
}

function messageFromWire(raw: Record<string, unknown>): LiveMessage | null {
  const id = text(raw.id);
  const body = text(raw.body);
  if (id === '' || body === '') return null;
  const submission = optionalText(raw.submission_id);
  return {
    id,
    author: authorFromWire(raw.author),
    body,
    created_at: count(raw.created_at, 0),
    ...(submission ? { submission_id: submission } : {}),
  };
}

/** A saved draft is never blank, so blank text is no draft. */
function draftFromWire(value: unknown): string | undefined {
  return typeof value === 'string' && value.trim() !== '' ? value : undefined;
}

/** The thread fields that a thread node and a diff thread share. */
function threadPartsFromWire(raw: Record<string, unknown>): Omit<LiveThread, 'id'> {
  const anchor = anchorFromWire(raw.anchor);
  const draft = draftFromWire(raw.draft);
  return {
    ...(anchor ? { anchor } : {}),
    messages: keyed(raw.messages, messageFromWire),
    ...(draft === undefined ? {} : { draft }),
    ...(raw.is_resolved === undefined ? {} : { is_resolved: raw.is_resolved === true }),
  };
}

function threadFromWire(raw: Record<string, unknown>): LiveThread | null {
  const id = text(raw.id);
  if (id === '') return null;
  return { id, ...threadPartsFromWire(raw) };
}

/** One Send receipt read from the wire, or null when it has no id or no known stage. */
export function submissionFromWire(raw: Record<string, unknown>): LiveSubmission | null {
  const id = text(raw.id);
  const status = text(raw.status);
  if (id === '' || !(LIVE_SUBMISSION_STATUSES as readonly string[]).includes(status)) return null;
  const nodeId = optionalText(raw.node_id);
  const revision = optionalText(raw.revision);
  const note = optionalText(raw.note);
  const comments = rows(raw.comments)
    .map((comment) => ({
      node_id: text(comment.node_id),
      thread_id: text(comment.thread_id),
      message_id: text(comment.message_id),
      body: text(comment.body),
    }))
    .filter((comment) => comment.thread_id !== '' && comment.body !== '');
  const created = count(raw.created_at, 0);
  return {
    id,
    idempotency_key: text(raw.idempotency_key),
    status: status as LiveSubmissionStatus,
    created_at: created,
    updated_at: count(raw.updated_at, created),
    ...(nodeId ? { node_id: nodeId } : {}),
    ...(revision ? { revision } : {}),
    ...(note ? { note } : {}),
    ...(comments.length > 0 ? { comments } : {}),
  };
}

/**
 * `submission` folded into the receipts by id (`app/upsert-submission`). Past the
 * bound the oldest settled receipt goes first, then the oldest receipt.
 */
export function upsertSubmission(
  receipts: readonly LiveSubmission[],
  submission: LiveSubmission,
): LiveSubmission[] {
  const at = receipts.findIndex((one) => one.id === submission.id);
  const all = receipts.slice();
  if (at >= 0) all[at] = submission;
  else all.push(submission);
  while (all.length > LIVE_SUBMISSIONS_MAX) {
    const settled = all.findIndex((one) => one.status === 'answered' || one.status === 'failed');
    all.splice(settled >= 0 ? settled : 0, 1);
  }
  return all;
}

/** The threads a node holds: every thread of a diff, or the thread node itself. */
export function liveThreads(node: LiveNode): LiveThread[] {
  if (node.type === 'diff') return node.threads;
  if (node.type === 'thread') {
    return [
      {
        id: node.id,
        messages: node.messages,
        ...(node.anchor ? { anchor: node.anchor } : {}),
        ...(node.draft === undefined ? {} : { draft: node.draft }),
        ...(node.is_resolved === undefined ? {} : { is_resolved: node.is_resolved }),
      },
    ];
  }
  return [];
}

/** `thread` upserted into a diff by id, or put in place of a thread node's thread. */
function upsertThread(node: LiveLeafNode, thread: LiveThread): LiveNode {
  if (node.type === 'diff') {
    return { ...node, threads: upsert(node.threads, [thread]) };
  }
  if (node.type === 'thread' && node.id === thread.id) {
    const { id: _id, ...parts } = thread;
    void _id;
    return {
      id: node.id,
      type: 'thread',
      ...(node.label ? { label: node.label } : {}),
      ...parts,
    };
  }
  return node;
}

function keyed<T>(value: unknown, parse: (raw: Record<string, unknown>) => T | null): T[] {
  return rows(value)
    .map(parse)
    .filter((item): item is T => item !== null);
}

/**
 * One node, or `null` when the app cannot paint it: an unknown type is a NEWER
 * engine talking to an older phone, and half a node is worse than none.
 */
function liveNodeFromWire(raw: unknown): LiveNode | null {
  const node = record(raw);
  if (!node) return null;
  const id = text(node.id);
  const type = text(node.type);
  if (id === '') return null;
  // What every node carries whatever it paints: its address and its heading.
  const base = { id, label: optionalText(node.label) };
  // A group is a layout, not content: an empty one is a hole rather than an
  // arrangement, so it never reaches the screen (`:live-view/children`).
  if (type === LIVE_GROUP_TYPE) {
    const children = liveNodesFromWire(node.children);
    if (children.length === 0) return null;
    const direction = viewGroupDirection(node.direction);
    const maxColumns = direction === 'grid' ? viewMaxColumns(node.max_columns) : undefined;
    return {
      ...base,
      type: 'group',
      direction,
      ...(maxColumns === undefined ? {} : { max_columns: maxColumns }),
      children,
      ...(node.is_collapsible === undefined
        ? {}
        : { is_collapsible: node.is_collapsible === true }),
      ...(node.default_expanded === undefined
        ? {}
        : { default_expanded: node.default_expanded === true }),
    };
  }
  if ((LIVE_INPUT_TYPES as readonly string[]).includes(type)) {
    const field = humanInputFieldFromWire(node);
    if (!field) return null;
    return {
      ...base,
      label: field.label,
      type: field.type as LiveInputType,
      field,
      ...('value' in node ? { value: node.value } : {}),
      ...(node.is_set === undefined ? {} : { is_set: node.is_set === true }),
    };
  }
  if (!(LIVE_NODE_TYPES as readonly string[]).includes(type)) return null;
  switch (type as LiveNodeType) {
    case 'divider':
      return { id, type: 'divider' };
    case 'paragraph':
      return { ...base, type: 'paragraph', text: text(node.text) };
    case 'heading':
      return {
        ...base,
        type: 'heading',
        text: text(node.text),
        level: Math.min(6, Math.max(1, count(node.level, 2))) as LiveHeadingNode['level'],
      };
    case 'code':
      return {
        ...base,
        type: 'code',
        text: text(node.text),
        language: optionalText(node.language),
      };
    case 'spinner':
      return {
        ...base,
        type: 'spinner',
        text: text(node.text) || 'Working',
        variant: (['braille', 'dots', 'line', 'pulse'].includes(text(node.variant))
          ? node.variant
          : 'braille') as LiveSpinnerVariant,
        is_active: node.is_active !== false,
      };
    case 'button':
      return {
        ...base,
        type: 'button',
        label: base.label || id,
        action: node.action === 'submit' ? 'submit' : 'activate',
        is_disabled: node.is_disabled === true,
        clicks: count(node.clicks, 0),
      };
    case 'status':
      return {
        ...base,
        type: 'status',
        text: text(node.text),
        detail: optionalText(node.detail),
        tone: tone(node.tone),
      };
    case 'progress':
      return {
        ...base,
        type: 'progress',
        value: optionalNumber(node.value),
        done: optionalNumber(node.done),
        total: optionalNumber(node.total),
      };
    case 'stat':
      return { ...base, type: 'stat', stats: keyed(node.stats, statFromWire) };
    case 'steps':
      return { ...base, type: 'steps', steps: keyed(node.steps, stepFromWire) };
    case 'log':
      return {
        ...base,
        type: 'log',
        lines: lines(node.lines),
        ...(Array.isArray(node.line_tones)
          ? {
              line_tones: node.line_tones.map((value) => (value == null ? null : tone(value))),
            }
          : {}),
        window_lines: count(node.window_lines, LIVE_LOG_WINDOW),
        total_lines: count(node.total_lines, lines(node.lines).length),
        ...(node.default_expanded === undefined
          ? {}
          : { default_expanded: node.default_expanded === true }),
      };
    case 'table': {
      const tableRows = keyed(node.rows, rowFromWire);
      const isSelectable = node.is_selectable === true;
      return {
        ...base,
        type: 'table',
        columns: keyed(node.columns, columnFromWire),
        rows: tableRows,
        groups: keyed(node.groups, groupFromWire),
        max_rows: count(node.max_rows, LIVE_TABLE_MAX_ROWS),
        order: order(node.order),
        is_selectable: isSelectable,
        selected_ids: isSelectable ? selectedIds(node.selected_ids, tableRows) : [],
      };
    }
    case 'link':
      return { ...base, type: 'link', links: keyed(node.links, linkFromWire) };
    case 'card':
      return {
        ...base,
        type: 'card',
        title: text(node.title) || id,
        text: optionalText(node.text),
        meta: optionalText(node.meta),
        ...(node.tone === undefined ? {} : { tone: tone(node.tone) }),
        is_selectable: node.is_selectable === true,
        clicks: count(node.clicks, 0),
      };
    case 'diff': {
      const oldRevision = optionalText(node.old_revision);
      const newRevision = optionalText(node.new_revision);
      return {
        ...base,
        type: 'diff',
        patch: text(node.patch),
        ...(oldRevision ? { old_revision: oldRevision } : {}),
        ...(newRevision ? { new_revision: newRevision } : {}),
        threads: keyed(node.threads, threadFromWire),
      };
    }
    case 'thread':
      return { ...base, type: 'thread', ...threadPartsFromWire(node) };
  }
}

/** The nodes of a view or of a group, unpaintable ones dropped. */
function liveNodesFromWire(raw: unknown): LiveNode[] {
  return (Array.isArray(raw) ? raw : [])
    .map(liveNodeFromWire)
    .filter((node): node is LiveNode => node !== null);
}
/** One view, or `null` when its current closed contract cannot be painted. */
export function liveViewFromWire(raw: unknown): LiveView | null {
  const view = record(raw);
  if (!view) return null;
  const id = text(view.id);
  const title = text(view.title).trim();
  if (id === '' || title === '') return null;

  // Protocol 7 emits no classified view and no `activity` key on one. A frame
  // still carrying either is an OLDER gateway's shape reaching a client that
  // cannot paint it, and the compatibility gate has already refused that
  // gateway — so this is a contract violation, not a variant to tolerate.
  if (view.classification !== undefined || view.activity !== undefined) return null;

  const nodes = liveNodesFromWire(view.nodes);
  if (nodes.length === 0) return null;

  return {
    id,
    title,
    description: optionalText(view.description),
    nodes,
    seq: count(view.seq, 0),
    created_at: optionalNumber(view.created_at),
    source: optionalText(view.source),
    owner: liveOwnerFromWire(view.owner),
    placement: (LIVE_PLACEMENTS as readonly unknown[]).includes(view.placement)
      ? (view.placement as LivePlacement)
      : 'inline',
    session_id: optionalText(view.session_id),
    extension: optionalText(view.extension),
    key: optionalText(view.key),
    ...(Array.isArray(view.submissions)
      ? { submissions: keyed(view.submissions, submissionFromWire) }
      : {}),
    ...(view.is_archived === true ? { is_archived: true } : {}),
    ...(view.is_closed === true ? { is_closed: true } : {}),
    ...(view.is_stale === true ? { is_disconnected: true } : {}),
    ...(Array.isArray(view.shortcuts) ? { shortcuts: liveShortcutsFromWire(view.shortcuts) } : {}),
  };
}

/** Every view the snapshot holds, oldest first, unpaintable ones dropped. */
export function liveViewsFromWire(raw: unknown): LiveView[] {
  return Array.isArray(raw)
    ? raw.map(liveViewFromWire).filter((view): view is LiveView => view !== null)
    : [];
}

/**
 * `incoming` merged into `existing` BY ID (`live/upsert`): an id already there
 * is REPLACED IN PLACE so the row keeps its slot, an unseen id joins the end.
 */
function upsert<T extends { id: string }>(existing: T[], incoming: T[]): T[] {
  if (incoming.length === 0) return existing;
  const merged = existing.slice();
  const at = new Map(existing.map((item, index) => [item.id, index]));
  for (const item of incoming) {
    const index = at.get(item.id);
    if (index === undefined) {
      at.set(item.id, merged.length);
      merged.push(item);
    } else {
      merged[index] = item;
    }
  }
  return merged;
}

/**
 * A group is a DECLARATION, so a second one MERGES into the first: re-toning a
 * group mid-run must not drop the label it was declared with. Rows replace in
 * place (`upsert`); declarations accumulate.
 */
function mergedGroups(existing: LiveGroup[], incoming: LiveGroup[]): LiveGroup[] {
  if (incoming.length === 0) return existing;
  const merged = existing.slice();
  const at = new Map(existing.map((group, index) => [group.id, index]));
  for (const group of incoming) {
    const index = at.get(group.id);
    if (index === undefined) {
      at.set(group.id, merged.length);
      merged.push(group);
    } else {
      merged[index] = { ...merged[index], ...group };
    }
  }
  return merged;
}

function withoutIds<T extends { id: string }>(existing: T[], ids: unknown): T[] {
  const dropped = new Set(Array.isArray(ids) ? ids.map((id) => text(id)) : []);
  if (dropped.size === 0) return existing;
  return existing.filter((item) => !dropped.has(item.id));
}

/** The key an `append` fills for this node type (`live/appendable-key`). */
function appendKey(node: LiveNode): 'lines' | 'rows' | 'stats' | 'steps' | 'links' | null {
  switch (node.type) {
    case 'log':
      return 'lines';
    case 'table':
      return 'rows';
    case 'stat':
      return 'stats';
    case 'steps':
      return 'steps';
    case 'link':
      return 'links';
    default:
      return null;
  }
}

/** `set` MERGES the keys it carries onto the node (`live/apply-set`). */
function applySet(node: LiveLeafNode, op: Record<string, unknown>): LiveNode {
  switch (node.type) {
    case 'divider':
      return node;
    case 'paragraph':
    case 'heading':
    case 'code':
    case 'spinner':
    case 'button':
    case 'card':
      return liveNodeFromWire({ ...node, ...op }) ?? node;
    case 'status':
      return {
        ...node,
        label: 'label' in op ? optionalText(op.label) : node.label,
        text: 'text' in op ? text(op.text) : node.text,
        detail: 'detail' in op ? optionalText(op.detail) : node.detail,
        tone: 'tone' in op ? tone(op.tone) : node.tone,
      };
    case 'progress':
      return {
        ...node,
        label: 'label' in op ? optionalText(op.label) : node.label,
        value: 'value' in op ? optionalNumber(op.value) : node.value,
        done: 'done' in op ? optionalNumber(op.done) : node.done,
        total: 'total' in op ? optionalNumber(op.total) : node.total,
      };
    case 'stat':
      return {
        ...node,
        label: 'label' in op ? optionalText(op.label) : node.label,
        stats: 'stats' in op ? keyed(op.stats, statFromWire) : node.stats,
      };
    case 'steps':
      return {
        ...node,
        label: 'label' in op ? optionalText(op.label) : node.label,
        steps: 'steps' in op ? keyed(op.steps, stepFromWire) : node.steps,
      };
    case 'table':
      return {
        ...node,
        label: 'label' in op ? optionalText(op.label) : node.label,
        selected_ids:
          'selected_ids' in op && node.is_selectable
            ? selectedIds(op.selected_ids, node.rows)
            : node.selected_ids,
      };
    case 'link':
      return {
        ...node,
        label: 'label' in op ? optionalText(op.label) : node.label,
        links: 'links' in op ? keyed(op.links, linkFromWire) : node.links,
      };
    case 'diff':
      return {
        ...node,
        label: 'label' in op ? optionalText(op.label) : node.label,
        patch: 'patch' in op ? text(op.patch) : node.patch,
        old_revision: 'old_revision' in op ? optionalText(op.old_revision) : node.old_revision,
        new_revision: 'new_revision' in op ? optionalText(op.new_revision) : node.new_revision,
      };
    case 'thread':
      return 'label' in op ? { ...node, label: optionalText(op.label) } : node;
    default:
      if ('field' in node) {
        return {
          ...node,
          ...('value' in op ? { value: op.value } : {}),
          ...('is_set' in op ? { is_set: op.is_set === true } : {}),
        };
      }
      return 'label' in op ? { ...node, label: optionalText(op.label) } : node;
  }
}

/**
 * `append` (`live/apply-append`). A log keeps EVERY line it is handed: a live view
 * shows the whole log, never a tail of it, and `window_lines` stays what the MODEL
 * reads of it. `total_lines` still counts the record, so a panel that joined late
 * knows how much came before the lines it was sent. Every other node upserts by id.
 */
function applyAppend(node: LiveLeafNode, op: Record<string, unknown>): LiveNode {
  const key = appendKey(node);
  if (!key) return node;
  if (node.type === 'log') {
    const arriving = lines(op.lines);
    if (arriving.length === 0) return node;
    return {
      ...node,
      lines: node.lines.concat(arriving),
      total_lines: node.total_lines + arriving.length,
      ...(node.line_tones || op.tone != null
        ? {
            line_tones: [
              ...(node.line_tones ?? node.lines.map(() => null)),
              ...arriving.map(() => (op.tone == null ? null : tone(op.tone))),
            ],
          }
        : {}),
    };
  }
  switch (node.type) {
    case 'table':
      return {
        ...node,
        rows: upsert(node.rows, keyed(op.rows, rowFromWire)),
        ...('groups' in op
          ? { groups: mergedGroups(node.groups, keyed(op.groups, groupFromWire)) }
          : {}),
      };
    case 'stat':
      return {
        ...node,
        stats: upsert(node.stats, keyed(op.stats, statFromWire)),
      };
    case 'steps':
      return {
        ...node,
        steps: upsert(node.steps, keyed(op.steps, stepFromWire)),
      };
    case 'link':
      return {
        ...node,
        links: upsert(node.links, keyed(op.links, linkFromWire)),
      };
    default:
      return node;
  }
}

function applyRemove(node: LiveLeafNode, op: Record<string, unknown>): LiveNode {
  switch (node.type) {
    case 'table': {
      const tableRows = withoutIds(node.rows, op.item_ids);
      return {
        ...node,
        rows: tableRows,
        selected_ids: selectedIds(node.selected_ids, tableRows),
      };
    }
    case 'stat':
      return { ...node, stats: withoutIds(node.stats, op.item_ids) };
    case 'steps':
      return { ...node, steps: withoutIds(node.steps, op.item_ids) };
    case 'link':
      return { ...node, links: withoutIds(node.links, op.item_ids) };
    case 'diff':
      return { ...node, threads: withoutIds(node.threads, op.item_ids) };
    default:
      return node;
  }
}

/**
 * `clear` empties a log's WINDOW and starts its record over (`live/apply-clear`):
 * `live-sink/log-range` folds a `clear` to nothing, so a kept `total_lines` would
 * offer earlier lines the gateway can no longer serve.
 */
function applyClear(node: LiveLeafNode): LiveNode {
  switch (node.type) {
    case 'log':
      return { ...node, lines: [], line_tones: undefined, total_lines: 0 };
    case 'table':
      return { ...node, rows: [], selected_ids: [] };
    case 'stat':
      return { ...node, stats: [] };
    case 'steps':
      return { ...node, steps: [] };
    case 'link':
      return { ...node, links: [] };
    default:
      return node;
  }
}

/** `thread` puts one whole thread in place (`app/upsert-thread`). */
function applyThread(node: LiveLeafNode, op: Record<string, unknown>): LiveNode {
  const raw = record(op.thread);
  const thread = raw ? threadFromWire(raw) : null;
  return thread ? upsertThread(node, thread) : node;
}

/** Every id in the tree, groups included (`live/node-ids`). */
function treeIds(nodes: LiveNode[]): string[] {
  return nodes.flatMap((node) =>
    node.type === 'group' ? [node.id, ...treeIds(node.children)] : [node.id],
  );
}

/**
 * The tree with the node this op names rewritten wherever it stands, or the SAME
 * array when nothing matched — a frame naming a node that is gone must leave the
 * picture, and React's identity with it, exactly where it was (`live/node-path`).
 */
function mapNode(nodes: LiveNode[], id: string, rewrite: (node: LiveNode) => LiveNode): LiveNode[] {
  let changed = false;
  const next = nodes.map((node) => {
    if (node.id === id) {
      const patched = rewrite(node);
      changed = changed || patched !== node;
      return patched;
    }
    if (node.type !== 'group') return node;
    const children = mapNode(node.children, id, rewrite);
    if (children === node.children) return node;
    changed = true;
    return { ...node, children };
  });
  return changed ? next : nodes;
}

/**
 * A new node lands in the list its `after` sibling lives in, so a run grows a
 * row by naming what the newcomer stands beside (`live/apply-add-node`); `null`
 * when no such sibling exists anywhere in the tree.
 */
function insertAfter(nodes: LiveNode[], after: string, node: LiveNode): LiveNode[] | null {
  const at = nodes.findIndex((existing) => existing.id === after);
  if (at >= 0) {
    const next = nodes.slice();
    next.splice(at + 1, 0, node);
    return next;
  }
  for (let index = 0; index < nodes.length; index += 1) {
    const child = nodes[index];
    if (child.type !== 'group') continue;
    const children = insertAfter(child.children, after, node);
    if (!children) continue;
    const next = nodes.slice();
    next[index] = { ...child, children };
    return next;
  }
  return null;
}

/** Dropping a group takes the nodes it arranges with it (`live/apply-remove-node`). */
function dropNode(nodes: LiveNode[], id: string): LiveNode[] {
  const kept = nodes.filter((node) => node.id !== id);
  if (kept.length !== nodes.length) return kept;
  let changed = false;
  const next = nodes.map((node) => {
    if (node.type !== 'group') return node;
    const children = dropNode(node.children, id);
    if (children === node.children) return node;
    changed = true;
    return { ...node, children };
  });
  return changed ? next : nodes;
}

/** The secret inputs: the host keeps no value of theirs, only `is_set`. */
const LIVE_SECRET_TYPES: readonly LiveInputType[] = ['password', 'otp'];

/** True when the host never holds or sends the value of this input. */
export function isLiveSecretInput(node: LiveInputNode): boolean {
  return LIVE_SECRET_TYPES.includes(node.type);
}

/** True when `value` fits the domain of `node` (`spec/value-in-domain?`). */
function inLiveInputDomain(node: LiveInputNode, value: unknown): boolean {
  const options = (node.field.options ?? []).map((option) => option.value);
  switch (node.type) {
    case 'checkbox':
      return typeof value === 'boolean';
    case 'range':
      return typeof value === 'number' && Number.isFinite(value);
    case 'select':
      return typeof value === 'string' && (options.length === 0 || options.includes(value));
    case 'multiselect':
      return (
        Array.isArray(value) &&
        value.every(
          (item) => typeof item === 'string' && (options.length === 0 || options.includes(item)),
        )
      );
    default:
      return typeof value === 'string';
  }
}

/**
 * `set-value` (`live/apply-set-value`): the input with its new value, or without
 * one when the value is `null`. A secret input, a node that is no input and a
 * value outside the input's domain leave the node as it was.
 */
function applySetValue(node: LiveLeafNode, op: Record<string, unknown>): LiveNode {
  if (!isLiveInputNode(node) || isLiveSecretInput(node) || !('value' in op)) return node;
  if (op.value === null) {
    if (!('value' in node)) return node;
    const { value: _cleared, ...rest } = node;
    return rest;
  }
  return inLiveInputDomain(node, op.value) ? { ...node, value: op.value } : node;
}

function applyOp(view: LiveView, raw: unknown): LiveView {
  const op = record(raw);
  if (!op) return view;
  const name = text(op.op);
  if (name === 'submission') {
    const raw = record(op.submission);
    const submission = raw ? submissionFromWire(raw) : null;
    if (!submission) return view;
    return { ...view, submissions: upsertSubmission(view.submissions ?? [], submission) };
  }
  if (name === 'shortcuts') {
    // The op REPLACES every binding; an empty list clears them.
    return { ...view, shortcuts: liveShortcutsFromWire(op.shortcuts) };
  }
  if (name === 'agent-note') {
    // The note is for the model. The Companion paints nothing for it.
    return view;
  }
  if (name === 'add-node') {
    const node = liveNodeFromWire(op.node_spec);
    if (!node) return view;
    // Ids are unique across the whole TREE, so a newcomer carrying an id the
    // view already knows — its own children included — is refused, not merged.
    const taken = new Set(treeIds(view.nodes));
    if (treeIds([node]).some((id) => taken.has(id))) return view;
    const after = optionalText(op.after);
    const grown = after ? insertAfter(view.nodes, after, node) : null;
    return { ...view, nodes: grown ?? [...view.nodes, node] };
  }
  const nodeId = text(op.node_id);
  if (nodeId === '') return view;
  if (name === 'remove-node') {
    const kept = dropNode(view.nodes, nodeId);
    return kept === view.nodes ? view : { ...view, nodes: kept };
  }
  const nodes = mapNode(view.nodes, nodeId, (node) => {
    // Layout is declared and no op carries it (`live-op-key-sets` names no
    // group), so a frame naming a row leaves the arrangement where it stands.
    if (node.type === 'group') return node;
    switch (name) {
      case 'set':
        return applySet(node, op);
      case 'set-value':
        return applySetValue(node, op);
      case 'append':
        return applyAppend(node, op);
      case 'remove':
        return applyRemove(node, op);
      case 'clear':
        return applyClear(node);
      case 'thread':
        return applyThread(node, op);
      default:
        return node;
    }
  });
  return nodes === view.nodes ? view : { ...view, nodes };
}

/**
 * One patch folded into the view it names (`live/apply-patch`).
 *
 * The gateway COALESCES patches on a tick, so a frame states the range it
 * stands for: `first_seq` is the earliest patch it carries and `seq` the
 * latest. A frame whose `seq` does not advance is a replay — the journal is
 * re-read on every reconnect — and is dropped. A frame whose `first_seq` skips
 * past what this picture has seen means frames were LOST, so the view is marked
 * stale for the section to re-read the snapshot rather than paint a hole.
 */
export function applyLivePatch(view: LiveView, frame: unknown): LiveView {
  const envelope = record(frame);
  const patch = record(envelope?.patch);
  if (!patch) return view;
  const seq = count(patch.seq, 0);
  if (seq <= view.seq) return view;
  const firstSeq = count(envelope?.first_seq, seq);
  if (firstSeq > view.seq + 1) return { ...view, is_stale: true };
  const ops = Array.isArray(patch.ops) ? patch.ops : [];
  const patched = ops.reduce(applyOp, view);
  return { ...patched, seq };
}

/** Where `view` stands. A view without a placement is inline. */
export function livePlacement(view: Pick<LiveView, 'placement'>): LivePlacement {
  return view.placement ?? 'inline';
}

/** True when `node` is a form input inside a live view. */
export function isLiveInputNode(node: LiveNode): node is LiveInputNode {
  return (LIVE_INPUT_TYPES as readonly string[]).includes(node.type);
}

/** Every input of `nodes` in reading order, groups included. */
export function liveInputNodes(nodes: readonly LiveNode[]): LiveInputNode[] {
  return nodes.flatMap((node) =>
    node.type === 'group' ? liveInputNodes(node.children) : isLiveInputNode(node) ? [node] : [],
  );
}

/** True for lifecycle events owned by the live capability. */
export function isLiveViewEvent(event: SseEvent): boolean {
  return (
    viewKind(event) === 'live' &&
    (event.type === VIEW_OPEN_EVENT ||
      event.type === VIEW_PATCH_EVENT ||
      event.type === VIEW_CLOSE_EVENT)
  );
}

/**
 * Fold one session event into the views on screen.
 *
 * A repeated `open` REPLACES its view instead of stacking a second copy, and a
 * close DROPS its view: nothing survives its own close frame. The list identity
 * is preserved when nothing changed so the section does not remount under the
 * operator's finger.
 *
 * A close does NOT have to carry that picture. The patch stream already ends on
 * it — every one of a real session's closes was verified byte-identical to the
 * state `open` + patches produce — so a gateway that omits `result.view` settles
 * exactly the same view for a fraction of the bytes. One that still sends it is
 * still obeyed: it stays authoritative, which is what keeps this readable by an
 * older gateway and by the NDJSON record, where the close frame is the only
 * place the finished picture is written down.
 */
export function applyLiveViewEvent(views: LiveView[], event: SseEvent): LiveView[] {
  if (event.type === VIEW_CLOSE_EVENT) {
    const viewId = text(event.view_id);
    if (viewId === '') return views;
    const at = views.findIndex((view) => view.id === viewId);
    if (at < 0) return views;
    // A closed view is GONE. Protocol 7 has no classified Activity view to keep
    // as a settled record here — the form that produced the work carries its own
    // terminal snapshot, so nothing survives its own close frame any more.
    const kept = views.filter((view) => view.id !== viewId);
    return kept.length === views.length ? views : kept;
  }
  if (event.type === VIEW_OPEN_EVENT) {
    const view = liveViewFromWire(event.view);
    if (!view) return views;
    const current = views;
    const at = current.findIndex((open) => open.id === view.id);
    if (at < 0) return [...current, view];
    const merged = current.slice();
    merged[at] = view;
    return merged;
  }
  if (event.type !== VIEW_PATCH_EVENT) return views;
  const viewId = text(event.view_id);
  const at = views.findIndex((view) => view.id === viewId);
  if (at < 0) return views;
  const patched = applyLivePatch(views[at], event);
  if (patched === views[at]) return views;
  const merged = views.slice();
  merged[at] = patched;
  return merged;
}

/** The views whose picture is behind — the section re-reads the snapshot for these. */
export function staleLiveViews(views: LiveView[]): LiveView[] {
  return views.filter((view) => view.is_stale === true);
}

/**
 * The RECORD of one settled view, folded back into the picture it ended on.
 *
 * What durably outlives a closed view is the NDJSON the engine appended while it
 * ran (`view.sink`): the declared view, one line per ACCEPTED patch,
 * and the verdict that sealed it. Folding those lines here is the same reduction
 * `applyLiveViewEvent` runs over the stream, run over a file instead — which is
 * what lets an artifact opened months later paint the run without the gateway
 * ever having held a frame of it.
 *
 * The trailer's own picture WINS when it carries one: that is the state the model
 * was handed on the close, so the app shows exactly it rather than its own replay.
 * An unreadable line ENDS the fold instead of voiding it — a run killed mid-write
 * still shows everything it managed to record.
 */
export interface LiveSelectionSnapshot {
  node_id: string;
  selected_ids: string[];
  view: LiveView;
}

export interface LiveRecord {
  view: LiveView;
  /** How it ended, in the engine's own word — `completed`, `interrupted`, `failed`. */
  reason?: string;
  /** Whether it reached its own end rather than being stopped or failing. */
  is_completed?: boolean;
  /** The comment the human left with a stop, when they left one. */
  note?: string;
  /** Finished pictures explicitly sealed for rows of a selectable table. */
  selection_snapshots?: LiveSelectionSnapshot[];
  /** When the record was sealed, epoch ms. */
  ended_at?: number;
}

/** A verdict carries only mutable presentation fields; restore declaration identity. */
function recordedViewFromWire(raw: unknown, declared: LiveView | null): LiveView | null {
  const partial = record(raw);
  if (!partial) return null;
  if (!declared) return liveViewFromWire(partial);
  return liveViewFromWire({
    ...declared,
    ...partial,
    id: declared.id,
  });
}

/** The NDJSON record of one view, or `null` when not one line of it is paintable. */
export function liveRecordFromText(source: string): LiveRecord | null {
  let view: LiveView | null = null;
  let sealed: Omit<LiveRecord, 'view'> = {};
  for (const line of source.split('\n')) {
    if (line.trim() === '') continue;
    let parsed: unknown;
    try {
      parsed = JSON.parse(line);
    } catch {
      break;
    }
    const frame = record(parsed);
    if (!frame) break;
    const kind = text(frame.kind);
    if (kind === 'open') {
      view = liveViewFromWire(frame.view) ?? view;
    } else if (kind === 'patch' && view) {
      view = applyLivePatch(view, frame);
    } else if (kind === 'close') {
      const result = record(frame.result);
      if (!result) continue;
      // The verdict states the reason; `is_completed` is the engine's own answer
      // to "did it finish", and a stop carries the note the human typed into it.
      view = recordedViewFromWire(result.view, view) ?? view;
      const selectionSnapshots = Array.isArray(result.selection_snapshots)
        ? result.selection_snapshots
            .map(record)
            .filter((snapshot): snapshot is Record<string, unknown> => snapshot !== null)
            .map((snapshot) => {
              const snapshotView = liveViewFromWire(snapshot.view);
              const nodeId = text(snapshot.node_id);
              const ids = Array.isArray(snapshot.selected_ids)
                ? snapshot.selected_ids.map(text).filter((id) => id !== '')
                : [];
              return snapshotView && nodeId && ids.length > 0
                ? { node_id: nodeId, selected_ids: ids, view: snapshotView }
                : null;
            })
            .filter((snapshot): snapshot is LiveSelectionSnapshot => snapshot !== null)
        : [];
      sealed = {
        reason: optionalText(result.reason),
        is_completed: result.is_completed === true,
        note: optionalText(result.note),
        selection_snapshots: selectionSnapshots,
        ended_at: optionalNumber(frame.at),
      };
    }
  }
  return view ? { view, ...sealed } : null;
}
/**
 * How far a progress has come, as a fraction of one (`live/fraction`): its
 * declared `value`, or what `done` of `total` works out to. `null` is
 * INDETERMINATE — started, size unknown — which is the honest picture while a
 * job queues, and it is why a bar is not always the answer.
 */
export function liveFraction(node: LiveProgressNode): number | null {
  if (node.value !== undefined) return node.value;
  if (node.done !== undefined && node.total !== undefined && node.total > 0) {
    return node.done / node.total;
  }
  return null;
}

/** A fraction as whole percent — the ONE rounding every surface shows (`live/percent`). */
export function livePercent(value: number): number {
  return Math.round(100 * value);
}

function numericColumn(values: string[]): boolean {
  const given = values.filter((value) => value.trim() !== '');
  return given.length > 0 && given.every((value) => Number.isFinite(Number(value)));
}

function cellAt(columns: LiveColumn[], row: LiveRow, columnId: string): string {
  const at = columns.findIndex((column) => column.id === columnId);
  return at < 0 ? '' : (row.cells[at] ?? '');
}

/**
 * A table's rows in the order it DECLARED (`live/ordered-rows`) — applied at
 * paint time, never by re-sorting the record, so a row keeps the identity the
 * eye is following. Blanks sort last whichever way the order runs, and ties
 * keep insertion order, so the terminal and the phone agree row for row.
 */
export function orderedRows(node: LiveTableNode): LiveRow[] {
  if (node.order === 'newest-first') return node.rows.slice().reverse();
  if (node.order === 'insertion' || typeof node.order === 'string') return node.rows;
  const { by, dir } = node.order;
  const numeric = numericColumn(node.rows.map((row) => cellAt(node.columns, row, by)));
  const keyOf = (row: LiveRow): string | number | null => {
    const cell = cellAt(node.columns, row, by);
    if (cell.trim() === '') return null;
    return numeric ? Number(cell) : cell.toLowerCase();
  };
  const descending = dir === 'desc';
  return node.rows.slice().sort((a, b) => {
    const ka = keyOf(a);
    const kb = keyOf(b);
    if (ka === null && kb === null) return 0;
    if (ka === null) return 1;
    if (kb === null) return -1;
    const compared = ka < kb ? -1 : ka > kb ? 1 : 0;
    return descending ? -compared : compared;
  });
}
