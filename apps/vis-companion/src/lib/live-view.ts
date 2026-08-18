/**
 * A live view — a run SHOWING its work — as the companion app sees it.
 *
 * An extension that has nothing to ASK can still have something to SHOW: a scan
 * sweeping a fleet, a build draining a log, a table filling in row by row. The
 * engine mounts such a view once and then PATCHES it by node id, and the
 * gateway turns the three channel events into the session events
 * `human_input.live.open`, `human_input.live.patch` and `human_input.live.close`.
 *
 * A view is not a question. Nothing here blocks a run, nothing answers, and the
 * only thing the operator can do to one is INTERRUPT it — one POST, not a form.
 * That is why a view is a section on the session screen rather than a dialog,
 * and why it LEAVES the screen the moment it ends: the record keeps it, the
 * model is handed it as data, and the rows go back to the transcript.
 *
 * This module is the pure half: the closed vocabularies the engine declares,
 * wire parsing, and the reduction of those three events into what is on screen.
 * The reducer is the engine's own materializer in TypeScript — `set` merges,
 * `append` upserts keyed items BY ID and keeps the slot the eye left them in, a
 * log's window slides while its record count keeps growing, `clear` empties the
 * window but never the record. Every rule below names the engine function it
 * mirrors (`human-input.live`), because divergence here is a phone painting a
 * picture the terminal never showed.
 */

import type { SseEvent } from './types';

export const LIVE_VIEW_OPEN_EVENT = 'human_input.live.open';
export const LIVE_VIEW_PATCH_EVENT = 'human_input.live.patch';
export const LIVE_VIEW_CLOSE_EVENT = 'human_input.live.close';

/** What a view can be MADE of (`human-input.spec/live-node-types`). CLOSED. */
export const LIVE_NODE_TYPES = [
  'status',
  'progress',
  'stat',
  'steps',
  'log',
  'table',
  'link',
] as const;

/** How a surface COLOURS one line, row, step or stat (`live-tones`). CLOSED. */
export const LIVE_TONES = ['idle', 'running', 'ok', 'warn', 'error'] as const;

/** Everything a patch can do to a view (`live-ops`). CLOSED. */
export const LIVE_OPS = [
  'set',
  'append',
  'remove',
  'clear',
  'add-node',
  'remove-node',
] as const;

/** Why a view ended (`live-reasons`). CLOSED. */
export const LIVE_REASONS = [
  'completed',
  'interrupted',
  'timeout',
  'undeliverable',
  'failed',
] as const;

/** What a link POINTS AT (`live-link-targets`). CLOSED. */
export const LIVE_LINK_TARGETS = ['attachment', 'path', 'url'] as const;

/** How a column's cells sit under their header (`live-aligns`). CLOSED. */
export const LIVE_ALIGNS = ['left', 'right'] as const;

/** The paint order a table can declare (`live-orders`). CLOSED. */
export const LIVE_ORDERS = ['insertion', 'newest-first'] as const;

/** Which way a `{by: …}` order runs (`live-sort-dirs`). CLOSED. */
export const LIVE_SORT_DIRS = ['asc', 'desc'] as const;

/** A log holds this many lines in its window; the record keeps them all. */
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
export type LiveTone = (typeof LIVE_TONES)[number];
export type LiveReason = (typeof LIVE_REASONS)[number];
export type LiveLinkTarget = (typeof LIVE_LINK_TARGETS)[number];
export type LiveAlign = (typeof LIVE_ALIGNS)[number];
export type LiveSortDir = (typeof LIVE_SORT_DIRS)[number];

/** `insertion`, `newest-first`, or one DECLARED column, ties kept in insertion order. */
export type LiveOrder = (typeof LIVE_ORDERS)[number] | { by: string; dir?: LiveSortDir };

interface LiveNodeBase {
  id: string;
  /** Absent on a bare status line: the sentence is its own label. */
  label?: string;
  /**
   * Stand BESIDE the node declared before this one, where the surface has room:
   * a sentence next to the table it is about, counters next to their bar. The
   * run declares it ONCE and no op carries it, so a layout never rearranges
   * itself under a reader; a narrow screen stacks it anyway.
   */
  is_aside?: boolean;
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

/** Output as it arrives. `lines` is a WINDOW onto `total_lines` in the record. */
export interface LiveLogNode extends LiveNodeBase {
  type: 'log';
  lines: string[];
  window_lines: number;
  total_lines: number;
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
}

/** Rows keyed by id: an update lands in the slot the eye left it in. */
export interface LiveTableNode extends LiveNodeBase {
  type: 'table';
  columns: LiveColumn[];
  rows: LiveRow[];
  max_rows: number;
  order: LiveOrder;
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

export type LiveNode =
  | LiveStatusNode
  | LiveProgressNode
  | LiveStatNode
  | LiveStepsNode
  | LiveLogNode
  | LiveTableNode
  | LiveLinkNode;

export interface LiveView {
  id: string;
  title: string;
  description?: string;
  nodes: LiveNode[];
  /** The last patch this picture has folded in. */
  seq: number;
  created_at?: number;
  source?: string;
  /**
   * NOT on the wire: this app's own note that a patch frame arrived for a seq
   * it never saw the predecessor of, so the picture is behind and the snapshot
   * has to be re-read. A view that quietly drops a frame paints a table with a
   * missing row and says nothing about it.
   */
  is_stale?: boolean;
}

/**
 * One page of a log node's RECORD, as `GET …/human-input/live/:view/log/:node`
 * answers it. The section shows a WINDOW; this is how the operator walks back
 * past it without the phone ever holding the whole run.
 */
export interface LiveLogPage {
  node_id: string;
  from: number;
  lines: string[];
  total: number;
}

/** Why a view ended, as the close event carries it. */
export interface LiveVerdict {
  view_id: string;
  reason: LiveReason;
  is_completed: boolean;
  /** A PERSON ended it — Escape in the terminal, Interrupt here. */
  is_from_human: boolean;
  /** The comment they left with that stop, when they left one. */
  note?: string;
  summary?: string;
  error?: string;
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

function keyed<T>(value: unknown, parse: (raw: Record<string, unknown>) => T | null): T[] {
  return rows(value)
    .map(parse)
    .filter((item): item is T => item !== null);
}

/**
 * One node, or `null` when the app cannot paint it: an unknown type is a NEWER
 * engine talking to an older phone, and half a node is worse than none.
 */
export function liveNodeFromWire(raw: unknown): LiveNode | null {
  const node = record(raw);
  if (!node) return null;
  const id = text(node.id);
  const type = text(node.type);
  if (id === '' || !(LIVE_NODE_TYPES as readonly string[]).includes(type)) return null;
  // What every node carries whatever it paints: its address, its heading, and
  // whether it stands beside the node before it.
  const base = {
    id,
    label: optionalText(node.label),
    ...(node.is_aside === true ? { is_aside: true as const } : {}),
  };
  switch (type as LiveNodeType) {
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
        window_lines: count(node.window_lines, LIVE_LOG_WINDOW),
        total_lines: count(node.total_lines, lines(node.lines).length),
      };
    case 'table':
      return {
        ...base,
        type: 'table',
        columns: keyed(node.columns, columnFromWire),
        rows: keyed(node.rows, rowFromWire),
        max_rows: count(node.max_rows, LIVE_TABLE_MAX_ROWS),
        order: order(node.order),
      };
    case 'link':
      return { ...base, type: 'link', links: keyed(node.links, linkFromWire) };
  }
}

/** One view, or `null` when it carries no id, no title or nothing to paint. */
export function liveViewFromWire(raw: unknown): LiveView | null {
  const view = record(raw);
  if (!view) return null;
  const id = text(view.id);
  const title = text(view.title).trim();
  if (id === '' || title === '') return null;
  const nodes = Array.isArray(view.nodes)
    ? view.nodes.map(liveNodeFromWire).filter((node): node is LiveNode => node !== null)
    : [];
  // A view IS its nodes — `human-input.spec/::nodes` refuses an empty vector — so
  // a frame this app cannot paint a single node of is dropped rather than painted
  // as an empty box the operator would read as "nothing is happening here".
  if (nodes.length === 0) return null;
  return {
    id,
    title,
    description: optionalText(view.description),
    nodes,
    seq: count(view.seq, 0),
    created_at: optionalNumber(view.created_at),
    source: optionalText(view.source),
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
function applySet(node: LiveNode, op: Record<string, unknown>): LiveNode {
  switch (node.type) {
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
    case 'link':
      return {
        ...node,
        label: 'label' in op ? optionalText(op.label) : node.label,
        links: 'links' in op ? keyed(op.links, linkFromWire) : node.links,
      };
    default:
      return 'label' in op ? { ...node, label: optionalText(op.label) } : node;
  }
}

/**
 * `append` (`live/apply-append`). A log grows its WINDOW and its record: the
 * window slides off the front while `total_lines` keeps counting, which is what
 * lets the section say how much of the story is behind what it shows. Every
 * other node upserts by id.
 */
function applyAppend(node: LiveNode, op: Record<string, unknown>): LiveNode {
  const key = appendKey(node);
  if (!key) return node;
  if (node.type === 'log') {
    const arriving = lines(op.lines);
    if (arriving.length === 0) return node;
    const all = node.lines.concat(arriving);
    const overflow = Math.max(0, all.length - node.window_lines);
    return {
      ...node,
      lines: overflow > 0 ? all.slice(overflow) : all,
      total_lines: node.total_lines + arriving.length,
    };
  }
  switch (node.type) {
    case 'table':
      return { ...node, rows: upsert(node.rows, keyed(op.rows, rowFromWire)) };
    case 'stat':
      return { ...node, stats: upsert(node.stats, keyed(op.stats, statFromWire)) };
    case 'steps':
      return { ...node, steps: upsert(node.steps, keyed(op.steps, stepFromWire)) };
    case 'link':
      return { ...node, links: upsert(node.links, keyed(op.links, linkFromWire)) };
    default:
      return node;
  }
}

function applyRemove(node: LiveNode, op: Record<string, unknown>): LiveNode {
  switch (node.type) {
    case 'table':
      return { ...node, rows: withoutIds(node.rows, op.item_ids) };
    case 'stat':
      return { ...node, stats: withoutIds(node.stats, op.item_ids) };
    case 'steps':
      return { ...node, steps: withoutIds(node.steps, op.item_ids) };
    case 'link':
      return { ...node, links: withoutIds(node.links, op.item_ids) };
    default:
      return node;
  }
}

/** `clear` empties a log's WINDOW, never its record (`live/apply-clear`). */
function applyClear(node: LiveNode): LiveNode {
  switch (node.type) {
    case 'log':
      return { ...node, lines: [] };
    case 'table':
      return { ...node, rows: [] };
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

function applyOp(view: LiveView, raw: unknown): LiveView {
  const op = record(raw);
  if (!op) return view;
  const name = text(op.op);
  if (name === 'add-node') {
    const node = liveNodeFromWire(op.node_spec);
    if (!node || view.nodes.some((existing) => existing.id === node.id)) return view;
    const after = optionalText(op.after);
    const at = after ? view.nodes.findIndex((existing) => existing.id === after) : -1;
    if (at < 0) return { ...view, nodes: [...view.nodes, node] };
    const nodes = view.nodes.slice();
    nodes.splice(at + 1, 0, node);
    return { ...view, nodes };
  }
  const nodeId = text(op.node_id);
  if (nodeId === '') return view;
  if (name === 'remove-node') {
    const kept = view.nodes.filter((node) => node.id !== nodeId);
    return kept.length === view.nodes.length ? view : { ...view, nodes: kept };
  }
  const at = view.nodes.findIndex((node) => node.id === nodeId);
  if (at < 0) return view;
  const node = view.nodes[at];
  const patched =
    name === 'set'
      ? applySet(node, op)
      : name === 'append'
        ? applyAppend(node, op)
        : name === 'remove'
          ? applyRemove(node, op)
          : name === 'clear'
            ? applyClear(node)
            : node;
  if (patched === node) return view;
  const nodes = view.nodes.slice();
  nodes[at] = patched;
  return { ...view, nodes };
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

/** True for the three session events this module reduces. */
export function isLiveViewEvent(event: SseEvent): boolean {
  return (
    event.type === LIVE_VIEW_OPEN_EVENT ||
    event.type === LIVE_VIEW_PATCH_EVENT ||
    event.type === LIVE_VIEW_CLOSE_EVENT
  );
}

/**
 * Fold one session event into the views on screen.
 *
 * A repeated `open` REPLACES its view instead of stacking a second copy, a
 * close DROPS it (the run is over; its record and the model's copy survive
 * elsewhere), and the list identity is preserved when nothing changed so the
 * section does not remount under the operator's finger.
 */
export function applyLiveViewEvent(views: LiveView[], event: SseEvent): LiveView[] {
  if (event.type === LIVE_VIEW_CLOSE_EVENT) {
    const viewId = text(event.view_id);
    if (viewId === '') return views;
    const kept = views.filter((view) => view.id !== viewId);
    return kept.length === views.length ? views : kept;
  }
  if (event.type === LIVE_VIEW_OPEN_EVENT) {
    const view = liveViewFromWire(event.view);
    if (!view) return views;
    const at = views.findIndex((open) => open.id === view.id);
    if (at < 0) return [...views, view];
    const merged = views.slice();
    merged[at] = view;
    return merged;
  }
  if (event.type !== LIVE_VIEW_PATCH_EVENT) return views;
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
