/**
 * A fake app host for the gallery: real app logic in memory, behind the same
 * gateway calls and session events that a real owner answers through.
 *
 * Each app is a reducer. An action returns the new picture, which goes out as a
 * `view.open` frame, or a list of ops, which goes out as a `view.patch` frame.
 */

import type { GatewayClient } from '../lib/gateway';
import { liveViewFromWire, type LiveLogPage } from '../lib/live-view';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import type { SseEvent } from '../lib/types';
import { VIEW_OPEN_EVENT, VIEW_PATCH_EVENT, type ViewAction, type ViewActionOutcome } from '../lib/view';

/** One app picture on the wire. */
export type WireApp = {
  id: string;
  title: string;
  seq: number;
  nodes: Record<string, unknown>[];
  shortcuts?: Record<string, unknown>[];
  is_closed?: boolean;
  description?: string;
};

/** What an app does with one action. */
type AppAnswer =
  | { nodes: Record<string, unknown>[]; shortcuts?: Record<string, unknown>[] }
  | { ops: Record<string, unknown>[] }
  | { refuse: string; errors?: Record<string, string> };

/** The logic of one app. `state` is the app's own memory. */
export interface StoryApp<S> {
  id: string;
  title: string;
  initial: () => S;
  render: (state: S) => { nodes: Record<string, unknown>[]; shortcuts?: Record<string, unknown>[] };
  act: (state: S, action: ViewAction, values: Record<string, unknown>) => { state: S; answer?: AppAnswer };
}

const STORY_INPUT_TYPES = ['plaintext', 'multiline', 'select', 'multiselect', 'checkbox', 'range'];

/** True when `id` names an input whose value the host keeps: no secret. */
function isOpenInput(nodes: Record<string, unknown>[], id: string): boolean {
  return nodes.some((node) =>
    node.id === id
      ? STORY_INPUT_TYPES.includes(String(node.type))
      : node.type === 'group' && Array.isArray(node.children) && isOpenInput(node.children as Record<string, unknown>[], id),
  );
}

/** The nodes with input `id` set to `value`; `null` clears it, as `set-value` does. */
function withValue(nodes: Record<string, unknown>[], id: string, value: unknown): Record<string, unknown>[] {
  return nodes.map((node) => {
    if (node.id === id) {
      const { value: _old, ...rest } = node;
      return value === null || value === undefined ? rest : { ...rest, value };
    }
    if (node.type === 'group' && Array.isArray(node.children)) {
      return { ...node, children: withValue(node.children as Record<string, unknown>[], id, value) };
    }
    return node;
  });
}

/** A gateway, a session hub and a host for one app, all in memory. */
export class StoryAppHost<S> {
  readonly sid = 'story-session';
  readonly actions: ViewAction[] = [];
  private state: S;
  private picture: WireApp;
  private readonly values: Record<string, unknown> = {};
  private readonly listeners = new Set<(event: SseEvent) => void>();
  private readonly logs = new Map<string, string[]>();
  private readonly app: StoryApp<S>;

  private readonly refuse?: (action: ViewAction) => string | undefined;
  private isOwnerGone = false;

  /** `refuse` answers a refusal reason for an action that the host turns down. */
  constructor(app: StoryApp<S>, over: Partial<WireApp> = {}, refuse?: (action: ViewAction) => string | undefined) {
    this.app = app;
    this.refuse = refuse;
    this.state = app.initial();
    const first = app.render(this.state);
    this.picture = { id: app.id, title: app.title, seq: 1, ...first, ...over };
    this.track(this.picture.nodes);
  }

  /** The picture as the host holds it now. */
  get view(): WireApp {
    return this.picture;
  }

  readonly hub = {
    subscribeSession: (_sid: string, listener: (event: SseEvent) => void) => {
      this.listeners.add(listener);
      return () => this.listeners.delete(listener);
    },
    subscribeConnection: (listener: (connected: boolean) => void) => {
      listener(true);
      return () => undefined;
    },
  } as unknown as SessionSubscriptionHub;

  readonly client = {
    liveView: async (_sid: string, viewId: string) =>
      viewId === this.picture.id ? liveViewFromWire({ ...this.wire(), ...(this.isOwnerGone ? { is_stale: true } : {}) }) : null,
    viewAction: async (_sid: string, _viewId: string, action: ViewAction) => this.handle(action),
    liveViewLog: async (_sid: string, _viewId: string, nodeId: string, from: number, limit: number) =>
      this.page(nodeId, from, limit),
  } as unknown as GatewayClient;

  /** The owner goes away, as after a restart: the host sends the saved picture as stale. */
  disconnect(): void {
    this.isOwnerGone = true;
    this.emit({ type: VIEW_OPEN_EVENT, kind: 'live', view_id: this.picture.id, view: { ...this.wire(), is_stale: true } } as SseEvent);
  }

  /** The owner adopts the app again: the same picture, live. */
  adopt(): void {
    this.isOwnerGone = false;
    this.emit({ type: VIEW_OPEN_EVENT, kind: 'live', view_id: this.picture.id, view: this.wire() } as SseEvent);
  }

  /** The app sends ops on its own, as a running job streams its log. */
  push(ops: Record<string, unknown>[]): void {
    this.patch(ops);
  }

  /** Run `step` each `ms` milliseconds, as a live owner streams. The returned stop ends it. */
  stream(step: (state: S) => { state: S; ops: Record<string, unknown>[] }, ms: number): () => void {
    const timer = setInterval(() => this.tick(step), ms);
    return () => clearInterval(timer);
  }

  /** Change the app's memory from outside, as a timer does, and send what changed. */
  tick(step: (state: S) => { state: S; ops: Record<string, unknown>[] }): void {
    if (this.picture.is_closed) return;
    const next = step(this.state);
    this.state = next.state;
    this.patch(next.ops);
  }

  private wire() {
    return { ...this.picture, placement: 'attachment' };
  }

  private emit(event: SseEvent): void {
    for (const listener of [...this.listeners]) listener(event);
  }

  private track(nodes: Record<string, unknown>[]): void {
    for (const node of nodes) {
      if (node.type === 'log' && Array.isArray(node.lines)) this.logs.set(String(node.id), [...(node.lines as string[])]);
      if (node.type === 'group' && Array.isArray(node.children)) this.track(node.children as Record<string, unknown>[]);
    }
  }

  private page(nodeId: string, from: number, limit: number): LiveLogPage {
    const lines = this.logs.get(nodeId) ?? [];
    const slice = lines.slice(from, from + limit);
    return {
      node_id: nodeId,
      from,
      lines: slice,
      total: lines.length,
      matched: lines.length,
      line_numbers: slice.map((_, index) => from + index + 1),
    };
  }

  private replace(next: { nodes: Record<string, unknown>[]; shortcuts?: Record<string, unknown>[] }): void {
    this.picture = { ...this.picture, ...next, seq: this.picture.seq + 1 };
    this.track(this.picture.nodes);
    this.emit({ type: VIEW_OPEN_EVENT, kind: 'live', view_id: this.picture.id, view: this.wire() } as SseEvent);
  }

  private patch(ops: Record<string, unknown>[]): void {
    if (ops.length === 0) return;
    const seq = this.picture.seq + 1;
    for (const op of ops) {
      if (op.op === 'append' && Array.isArray(op.lines)) {
        const held = this.logs.get(String(op.node_id)) ?? [];
        this.logs.set(String(op.node_id), held.concat(op.lines as string[]));
      }
      if (op.op === 'set-value') {
        this.picture = { ...this.picture, nodes: withValue(this.picture.nodes, String(op.node_id), op.value) };
      }
    }
    this.picture = { ...this.picture, seq };
    this.emit({
      type: VIEW_PATCH_EVENT,
      kind: 'live',
      view_id: this.picture.id,
      first_seq: seq,
      patch: { seq, ops },
    } as SseEvent);
  }

  private handle(action: ViewAction): ViewActionOutcome {
    this.actions.push(action);
    const base = { action: action.action, view_id: this.picture.id };
    if (this.picture.is_closed) return { ...base, is_accepted: false, reason: 'closed' };
    if (this.isOwnerGone && action.action !== 'draft' && action.action !== 'submit')
      return { ...base, is_accepted: false, reason: 'disconnected' };
    const refused = this.refuse?.(action);
    if (refused) return { ...base, is_accepted: false, reason: refused };
    if (action.action === 'shortcut') {
      const bound = (this.picture.shortcuts ?? []).some((one) => one.key === action.key && one.action);
      if (!bound) return { ...base, is_accepted: false, reason: 'unbound', key: action.key };
    }
    if (action.action === 'change') this.values[action.node_id] = action.value;
    const values = action.action === 'submit' ? { ...this.values, ...action.values } : { ...this.values };
    const { state, answer } = this.app.act(this.state, action, values);
    if (answer && 'refuse' in answer) {
      return { ...base, is_accepted: false, reason: answer.refuse, ...(answer.errors ? { errors: answer.errors } : {}) };
    }
    this.state = state;
    // The engine answers an accepted `change` and `submit` with `set-value` for
    // each input it now holds, at a new seq. A secret input gets no such patch.
    const held = action.action === 'change' ? { [action.node_id]: action.value } : action.action === 'submit' ? action.values : {};
    this.patch(
      Object.entries(held)
        .filter(([id]) => isOpenInput(this.picture.nodes, id))
        .map(([id, value]) => ({ op: 'set-value', node_id: id, value: value ?? null })),
    );
    if (answer && 'ops' in answer) this.patch(answer.ops);
    else if (answer) this.replace(answer);
    return { ...base, is_accepted: true, seq: this.picture.seq };
  }
}

// ── Counter: the smallest app. Buttons, a stat and keys. ───────────────────────

export const COUNTER: StoryApp<{ count: number }> = {
  id: 'app-counter',
  title: 'Counter',
  initial: () => ({ count: 0 }),
  render: ({ count }) => ({
    nodes: [
      { id: 'count', type: 'stat', stats: [{ id: 'n', label: 'Count', value_text: String(count), tone: count < 0 ? 'warn' : 'ok' }] },
      {
        id: 'controls',
        type: 'group',
        direction: 'wrap',
        children: [
          { id: 'dec', type: 'button', label: 'Minus one' },
          { id: 'inc', type: 'button', label: 'Plus one' },
          { id: 'reset', type: 'button', label: 'Reset', is_disabled: count === 0 },
        ],
      },
    ],
    shortcuts: [
      { key: '+', node_id: 'inc' },
      { key: '-', node_id: 'dec' },
      { key: 'ArrowUp', action: 'up', label: 'Plus ten' },
      { key: 'ArrowDown', action: 'down', label: 'Minus ten' },
      { key: 'r', node_id: 'reset' },
    ],
  }),
  act: (state, action) => {
    let count = state.count;
    if (action.action === 'activate') {
      if (action.node_id === 'inc') count += 1;
      if (action.node_id === 'dec') count -= 1;
      if (action.node_id === 'reset') count = 0;
    }
    if (action.action === 'shortcut') count += action.key === 'ArrowUp' ? 10 : -10;
    const next = { count };
    return { state: next, answer: COUNTER.render(next) };
  },
};

// ── Sort: six rows, one choice for each item and ranked buttons (#430). ──────────

type SortState = { kinds: Record<string, string>; picked: string[]; saved: boolean };

const SORT_ITEMS = ['Item one', 'Item two', 'Item three', 'Item four', 'Item five', 'Item six'];

function sortNodes(state: SortState) {
  return {
    nodes: [
      {
        id: 'summary',
        type: 'status',
        text: state.saved ? 'Saved' : `${state.picked.length} of ${SORT_ITEMS.length} picked`,
        tone: state.saved ? 'ok' : 'idle',
      },
      {
        id: 'items',
        type: 'table',
        label: 'Items',
        columns: [
          { id: 'item', label: 'Item' },
          { id: 'kind', label: 'Kind' },
        ],
        rows: SORT_ITEMS.map((label, index) => ({
          id: `item-${index + 1}`,
          cells: [label, state.kinds[`item-${index + 1}`] ?? 'Work'],
        })),
        is_selectable: true,
        selected_ids: state.picked,
      },
      {
        id: 'sort',
        type: 'group',
        direction: 'column',
        label: 'Kind of each item',
        children: [
          ...SORT_ITEMS.map((label, index) => ({
            id: `kind-${index + 1}`,
            type: 'select',
            name: `kind-${index + 1}`,
            label,
            options: [
              { value: 'Work', label: 'Work' },
              { value: 'Personal', label: 'Personal' },
            ],
            value: state.kinds[`item-${index + 1}`] ?? 'Work',
          })),
          { id: 'save', type: 'button', label: 'Save', action: 'submit', variant: 'primary' },
          { id: 'cancel', type: 'button', label: 'Cancel' },
          { id: 'discard', type: 'button', label: 'Discard', variant: 'destructive', is_disabled: state.picked.length === 0 },
        ],
      },
    ],
  };
}

export const SORT: StoryApp<SortState> = {
  id: 'app-sort',
  title: 'Sort items',
  initial: () => ({ kinds: {}, picked: ['item-2'], saved: false }),
  render: sortNodes,
  act: (state, action, values) => {
    if (action.action === 'select') {
      const next = { ...state, picked: action.item_ids, saved: false };
      return { state: next, answer: sortNodes(next) };
    }
    if (action.action === 'submit') {
      const kinds = Object.fromEntries(
        SORT_ITEMS.map((_, index) => [`item-${index + 1}`, String(values[`kind-${index + 1}`] ?? 'Work')]),
      );
      const next = { ...state, kinds, saved: true };
      return { state: next, answer: sortNodes(next) };
    }
    return { state };
  },
};

// ── Todo: an input, a selectable table and owner validation. ───────────────────

type TodoState = { items: { id: string; text: string; done: boolean }[]; next: number };

function todoNodes(state: TodoState) {
  const open = state.items.filter((item) => !item.done).length;
  return {
    nodes: [
      {
        id: 'summary',
        type: 'status',
        text: state.items.length === 0 ? 'Nothing to do' : `${open} of ${state.items.length} open`,
        tone: open === 0 && state.items.length > 0 ? 'ok' : 'idle',
      },
      { id: 'new', type: 'plaintext', label: 'New task', placeholder: 'What needs doing?', value: '' },
      {
        id: 'actions',
        type: 'group',
        direction: 'wrap',
        children: [
          { id: 'add', type: 'button', label: 'Add' },
          { id: 'clear', type: 'button', label: 'Clear done', is_disabled: state.items.every((item) => !item.done) },
        ],
      },
      {
        id: 'items',
        type: 'table',
        label: 'Tasks',
        columns: [
          { id: 'task', label: 'Task' },
          { id: 'state', label: 'State' },
        ],
        rows: state.items.map((item) => ({
          id: item.id,
          cells: [item.text, item.done ? 'Done' : 'Open'],
          tone: item.done ? 'ok' : 'idle',
        })),
        is_selectable: true,
      },
    ],
    shortcuts: [
      { key: 'Enter', node_id: 'add' },
      { key: 'c', node_id: 'clear' },
      { key: 'x', action: 'toggle-first', label: 'Finish first' },
    ],
  };
}

export const TODO: StoryApp<TodoState> = {
  id: 'app-todo',
  title: 'Todo list',
  initial: () => ({
    items: [
      { id: 't1', text: 'Write the release notes', done: false },
      { id: 't2', text: 'Check the phone layout', done: true },
    ],
    next: 3,
  }),
  render: todoNodes,
  act: (state, action, values) => {
    if (action.action === 'activate' && action.node_id === 'add') {
      const text = String(values.new ?? '').trim();
      if (text === '') return { state, answer: { refuse: 'invalid', errors: { new: 'Type a task first.' } } };
      const next = { items: [...state.items, { id: `t${state.next}`, text, done: false }], next: state.next + 1 };
      return { state: next, answer: todoNodes(next) };
    }
    if (action.action === 'activate' && action.node_id === 'clear') {
      const next = { ...state, items: state.items.filter((item) => !item.done) };
      return { state: next, answer: todoNodes(next) };
    }
    const toggle = (ids: string[]) => {
      const next = { ...state, items: state.items.map((item) => (ids.includes(item.id) ? { ...item, done: !item.done } : item)) };
      return { state: next, answer: todoNodes(next) };
    };
    if (action.action === 'select') return toggle(action.item_ids);
    if (action.action === 'shortcut') {
      const first = state.items.find((item) => !item.done);
      return first ? toggle([first.id]) : { state, answer: todoNodes(state) };
    }
    return { state };
  },
};

// ── 2048: a 4x4 table and arrow keys. ──────────────────────────────────────────

export type Game = { grid: number[][]; score: number; seed: number; isOver: boolean };

function random(seed: number): [number, number] {
  const next = (seed * 1103515245 + 12345) % 2147483648;
  return [next / 2147483648, next];
}

function spawn(game: Game): Game {
  const empty: [number, number][] = [];
  game.grid.forEach((row, r) => row.forEach((cell, c) => cell === 0 && empty.push([r, c])));
  if (empty.length === 0) return game;
  const [pick, seed1] = random(game.seed);
  const [value, seed2] = random(seed1);
  const [r, c] = empty[Math.floor(pick * empty.length)];
  const grid = game.grid.map((row) => row.slice());
  grid[r][c] = value < 0.9 ? 2 : 4;
  return { ...game, grid, seed: seed2 };
}

function slideRow(row: number[]): { row: number[]; gained: number } {
  const kept = row.filter((cell) => cell !== 0);
  const out: number[] = [];
  let gained = 0;
  for (let index = 0; index < kept.length; index += 1) {
    if (kept[index] === kept[index + 1]) {
      out.push(kept[index] * 2);
      gained += kept[index] * 2;
      index += 1;
    } else out.push(kept[index]);
  }
  while (out.length < 4) out.push(0);
  return { row: out, gained };
}

const transpose = (grid: number[][]) => grid[0].map((_, c) => grid.map((row) => row[c]));

/** One move of 2048. A move that changes nothing spawns nothing. */
export function move(game: Game, key: string): Game {
  if (game.isOver) return game;
  let grid = game.grid;
  const vertical = key === 'ArrowUp' || key === 'ArrowDown';
  const reverse = key === 'ArrowRight' || key === 'ArrowDown';
  if (vertical) grid = transpose(grid);
  let gained = 0;
  grid = grid.map((row) => {
    const slid = slideRow(reverse ? row.slice().reverse() : row);
    gained += slid.gained;
    return reverse ? slid.row.reverse() : slid.row;
  });
  if (vertical) grid = transpose(grid);
  if (JSON.stringify(grid) === JSON.stringify(game.grid)) return game;
  const next = spawn({ ...game, grid, score: game.score + gained });
  const canMove = ['ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight'].some(
    (k) => JSON.stringify(slide(next.grid, k)) !== JSON.stringify(next.grid),
  );
  return { ...next, isOver: !canMove };
}

function slide(grid: number[][], key: string): number[][] {
  const vertical = key === 'ArrowUp' || key === 'ArrowDown';
  const reverse = key === 'ArrowRight' || key === 'ArrowDown';
  let g = vertical ? transpose(grid) : grid;
  g = g.map((row) => {
    const slid = slideRow(reverse ? row.slice().reverse() : row).row;
    return reverse ? slid.reverse() : slid;
  });
  return vertical ? transpose(g) : g;
}

// ── Dashboard: status, progress, stats, steps and a log that streams. ──────────

export type Job = { done: number; total: number; isPaused: boolean; line: number };

const STAGES = ['Fetch', 'Build', 'Test', 'Publish'];

function stageTone(job: Job, index: number) {
  const at = Math.floor((job.done / job.total) * STAGES.length);
  return index < at ? 'ok' : index === at ? (job.isPaused ? 'warn' : 'running') : 'idle';
}

function jobNodes(job: Job) {
  const isDone = job.done >= job.total;
  return {
    nodes: [
      {
        id: 'state',
        type: 'status',
        text: isDone ? 'Release ready' : job.isPaused ? 'Paused' : 'Building release',
        detail: `${job.done} of ${job.total} packages`,
        tone: isDone ? 'ok' : job.isPaused ? 'warn' : 'running',
      },
      { id: 'bar', type: 'progress', label: 'Packages', done: job.done, total: job.total },
      {
        id: 'facts',
        type: 'stat',
        stats: [
          { id: 'ok', label: 'Passed', value_text: String(job.done), tone: 'ok' },
          { id: 'left', label: 'Left', value_text: String(job.total - job.done) },
          { id: 'warn', label: 'Warnings', value_text: String(Math.floor(job.done / 7)), tone: 'warn' },
        ],
      },
      { id: 'stages', type: 'steps', steps: STAGES.map((label, index) => ({ id: label, label, tone: stageTone(job, index) })) },
      {
        id: 'output',
        type: 'log',
        label: 'Build log',
        lines: Array.from({ length: job.line }, (_, index) => `[${String(index + 1).padStart(3, '0')}] package-${index + 1} built`),
        default_expanded: true,
      },
      {
        id: 'packages',
        type: 'table',
        label: 'Slowest packages',
        columns: [
          { id: 'name', label: 'Package' },
          { id: 'time', label: 'Seconds', align: 'right' },
          { id: 'owner', label: 'Owner' },
          { id: 'note', label: 'Note' },
        ],
        rows: Array.from({ length: 12 }, (_, index) => ({
          id: `p${index}`,
          cells: [`package-with-a-long-name-${index}`, String(40 - index * 3), `team-${index % 4}`, index % 3 === 0 ? 'Cache miss on the first build' : ''],
          tone: index === 0 ? 'warn' : 'idle',
        })),
      },
      { id: 'pause', type: 'button', label: job.isPaused ? 'Resume' : 'Pause', is_disabled: isDone },
    ],
    shortcuts: [
      { key: 'p', node_id: 'pause' },
      { key: 'Space', node_id: 'pause', label: 'Pause or resume' },
    ],
  };
}

export const DASHBOARD: StoryApp<Job> = {
  id: 'app-dashboard',
  title: 'Release build',
  initial: () => ({ done: 9, total: 40, isPaused: false, line: 9 }),
  render: jobNodes,
  act: (job, action) => {
    if (action.action === 'activate' && action.node_id === 'pause') {
      const next = { ...job, isPaused: !job.isPaused };
      return {
        state: next,
        answer: {
          ops: [
            { op: 'set', node_id: 'pause', label: next.isPaused ? 'Resume' : 'Pause' },
            { op: 'set', node_id: 'state', text: next.isPaused ? 'Paused' : 'Building release', tone: next.isPaused ? 'warn' : 'running' },
          ],
        },
      };
    }
    return { state: job };
  },
};

/** One step of the build: one more package and one more log line. */
export function buildStep(job: Job): { state: Job; ops: Record<string, unknown>[] } {
  if (job.isPaused || job.done >= job.total) return { state: job, ops: [] };
  const next = { ...job, done: job.done + 1, line: job.line + 1 };
  const isDone = next.done >= next.total;
  return {
    state: next,
    ops: [
      { op: 'set', node_id: 'bar', done: next.done, total: next.total },
      { op: 'set', node_id: 'state', text: isDone ? 'Release ready' : 'Building release', detail: `${next.done} of ${next.total} packages`, tone: isDone ? 'ok' : 'running' },
      { op: 'set', node_id: 'facts', stats: jobNodes(next).nodes[2].stats },
      { op: 'set', node_id: 'stages', steps: STAGES.map((label, index) => ({ id: label, label, tone: stageTone(next, index) })) },
      { op: 'append', node_id: 'output', lines: [`[${String(next.line).padStart(3, '0')}] package-${next.line} built`] },
    ],
  };
}

// ── Deploy wizard: a form across steps, with radios, checkboxes and keys. ──────

type Wizard = { step: number; env: string; regions: string[]; isChecked: boolean };

const ENVS = ['staging', 'production'];

function wizardNodes(wizard: Wizard) {
  const steps = ['Target', 'Check', 'Done'].map((label, index) => ({
    id: label,
    label,
    tone: index < wizard.step ? 'ok' : index === wizard.step ? 'running' : 'idle',
  }));
  const body =
    wizard.step === 0
      ? [
          {
            id: 'env',
            type: 'select',
            label: 'Environment',
            options: ENVS.map((value) => ({ value, label: value === 'staging' ? 'Staging' : 'Production' })),
            value: wizard.env,
          },
          {
            id: 'regions',
            type: 'multiselect',
            label: 'Regions',
            options: [
              { value: 'eu', label: 'Europe' },
              { value: 'us', label: 'United States' },
            ],
            value: wizard.regions,
          },
        ]
      : wizard.step === 1
        ? [{ id: 'confirm', type: 'checkbox', label: 'I read the release notes', value: wizard.isChecked }]
        : [{ id: 'result', type: 'status', text: `Deployed to ${wizard.env}`, tone: 'ok' }];
  return {
    nodes: [
      { id: 'steps', type: 'steps', steps },
      ...body,
      {
        id: 'nav',
        type: 'group',
        direction: 'wrap',
        children: [
          { id: 'back', type: 'button', label: 'Back', is_disabled: wizard.step === 0 },
          wizard.step === 2
            ? { id: 'again', type: 'button', label: 'Start again' }
            : { id: 'next', type: 'button', label: 'Next', action: 'submit' },
        ],
      },
    ],
    shortcuts: [
      { key: 'n', node_id: 'next' },
      { key: 'b', node_id: 'back' },
      { key: 'r', node_id: 'again' },
      { key: 'ArrowUp', action: 'staging', label: 'Up' },
      { key: 'k', action: 'staging', label: 'Up' },
    ].filter((one) => !one.node_id || one.node_id !== (wizard.step === 2 ? 'next' : 'again')),
  };
}

const WIZARD: StoryApp<Wizard> = {
  id: 'app-wizard',
  title: 'Deploy wizard',
  initial: () => ({ step: 0, env: 'staging', regions: [], isChecked: false }),
  render: wizardNodes,
  act: (wizard, action, values) => {
    const held: Wizard = {
      ...wizard,
      env: typeof values.env === 'string' ? values.env : wizard.env,
      regions: Array.isArray(values.regions) ? (values.regions as string[]) : wizard.regions,
      isChecked: typeof values.confirm === 'boolean' ? values.confirm : wizard.isChecked,
    };
    if (action.action === 'change') return { state: held };
    if (action.action === 'shortcut') {
      const next = { ...held, env: 'staging' };
      return { state: next, answer: wizardNodes(next) };
    }
    if (action.action !== 'activate' && action.action !== 'submit') return { state: held };
    if (action.node_id === 'next' && held.step === 1 && !held.isChecked) {
      return { state: held, answer: { refuse: 'invalid', errors: { confirm: 'Read the release notes first.' } } };
    }
    const step =
      action.node_id === 'next' ? held.step + 1 : action.node_id === 'back' ? held.step - 1 : action.node_id === 'again' ? 0 : held.step;
    const next = action.node_id === 'again' ? WIZARD.initial() : { ...held, step: Math.max(0, Math.min(2, step)) };
    return { state: next, answer: wizardNodes(next) };
  },
};
