/**
 * The reducer against the engine's own laws.
 *
 * `src/com/blockether/vis/internal/human_input/live.clj` materializes a view in
 * Clojure; this module does it again in TypeScript so a phone that missed no
 * frame never has to ask. Every case here names the law it holds, because a rule
 * that drifts here paints a picture the terminal does not.
 */
import { describe, expect, it } from 'vitest';
import fixture from './live-view.fixture.json';
import {
  applyLivePatch,
  applyLiveViewEvent,
  isLiveViewEvent,
  liveFraction,
  livePercent,
  liveRecordFromText,
  liveViewFromWire,
  liveViewsFromWire,
  orderedRows,
  staleLiveViews,
  LIVE_LOG_WINDOW,
  LIVE_VIEW_CLOSE_EVENT,
  LIVE_VIEW_OPEN_EVENT,
  LIVE_VIEW_PATCH_EVENT,
  type LiveNode,
  type LiveView,
} from './live-view';
import type { SseEvent } from './types';

/** The engine's own fixture, read the way the section reads a snapshot. */
function opened(): LiveView {
  const view = liveViewFromWire(fixture);
  if (!view) throw new Error('the engine fixture must be paintable');
  return view;
}

/** One coalesced patch frame, shaped exactly as the gateway publishes it. */
function frame(view: LiveView, seq: number, ops: unknown[], firstSeq = seq): SseEvent {
  return {
    type: LIVE_VIEW_PATCH_EVENT,
    view_id: view.id,
    first_seq: firstSeq,
    patch: { view_id: view.id, seq, ops },
  };
}

function patched(view: LiveView, seq: number, ops: unknown[], firstSeq = seq): LiveView {
  return applyLivePatch(view, frame(view, seq, ops, firstSeq));
}

/** Every node the view PAINTS, rows flattened — the engine's own `live/leaf-nodes`. */
function leaves(nodes: LiveNode[]): LiveNode[] {
  return nodes.flatMap((node) => (node.type === 'group' ? leaves(node.fields) : [node]));
}

function nodeOfType<K extends LiveNode['type']>(
  view: LiveView,
  id: string,
  type: K,
): Extract<LiveNode, { type: K }> {
  const found = [...view.nodes, ...leaves(view.nodes)].find((node) => node.id === id);
  if (!found || found.type !== type) throw new Error(`the view has no ${type} node ${id}`);
  return found as Extract<LiveNode, { type: K }>;
}

const ids = (view: LiveView) => view.nodes.map((node) => node.id);

describe('a live view read off the wire', () => {
  it('reads the engine fixture node for node, in the order it was declared', () => {
    const view = opened();
    expect(view.title).toBe('Fleet scan');
    expect(ids(view)).toEqual(['now', 'swept', 'score', 'phases', 'tail', 'reading', 'links']);
    expect(view.nodes.map((node) => node.type)).toEqual([
      'status',
      'progress',
      'stat',
      'steps',
      'log',
      'group',
      'link',
    ]);
    expect(nodeOfType(view, 'hosts', 'table').rows.map((row) => row.id)).toEqual(['db-1', 'db-2']);
    expect(nodeOfType(view, 'score', 'stat').stats[0]).toMatchObject({
      id: 'critical',
      value_text: '1',
      tone: 'error',
    });
  });

  // WHERE a node stands is the run's declaration, carried by no op: a view lays
  // itself out with the FORM's own group, so both surfaces put the same sentence
  // beside the same table and neither guesses.
  it('reads the row that stands two nodes side by side', () => {
    const view = opened();
    const row = nodeOfType(view, 'reading', 'group');
    expect(row.direction).toBe('row');
    expect(row.fields.map((node) => node.id)).toEqual(['hosts', 'why']);
    expect(ids(view)).not.toContain('hosts');
    expect(leaves(view.nodes).map((node) => node.id)).toContain('why');
  });

  // An app that paints half a node paints a lie. A node it cannot read is
  // dropped, and a view with nothing left to paint is not a view.
  it('drops what it cannot paint instead of guessing', () => {
    const view = liveViewFromWire({
      ...fixture,
      nodes: [{ id: 'now', type: 'hologram' }, { type: 'status', text: 'unnamed' }, fixture.nodes[0]],
    });
    expect(view && ids(view)).toEqual(['now']);
    expect(liveViewFromWire({ ...fixture, id: '' })).toBeNull();
    expect(liveViewFromWire({ ...fixture, nodes: [] })).toBeNull();
    expect(liveViewsFromWire([fixture, { id: 'x' }]).map((one) => one.title)).toEqual([
      'Fleet scan',
    ]);
  });
});

describe('a patch frame', () => {
  it('upserts keyed items in the slot the eye left them in', () => {
    const view = patched(opened(), 1, [
      {
        op: 'append',
        node_id: 'hosts',
        rows: [
          { id: 'db-3', cells: ['db-3', 'clean', '0'], tone: 'ok' },
          { id: 'db-2', cells: ['db-2', 'patched', '0'], tone: 'ok' },
        ],
      },
    ]);
    const table = nodeOfType(view, 'hosts', 'table');
    expect(table.rows.map((row) => row.id)).toEqual(['db-1', 'db-2', 'db-3']);
    expect(table.rows[1].cells[1]).toBe('patched');
    expect(table.rows[1].tone).toBe('ok');
    expect(view.seq).toBe(1);
  });

  it('slides the log window and keeps the count the record holds', () => {
    const arriving = Array.from({ length: LIVE_LOG_WINDOW }, (_, at) => `line ${at}`);
    const view = patched(opened(), 1, [{ op: 'append', node_id: 'tail', lines: arriving }]);
    const log = nodeOfType(view, 'tail', 'log');
    expect(log.lines.length).toBe(LIVE_LOG_WINDOW);
    expect(log.lines[0]).toBe('line 0');
    expect(log.lines.at(-1)).toBe(`line ${LIVE_LOG_WINDOW - 1}`);
    expect(log.total_lines).toBe(LIVE_LOG_WINDOW + 2);
  });

  // `clear` frees the window, never the record: the count stays, so the section
  // can still offer the earlier lines the gateway can still serve.
  it('empties a log window without forgetting the record', () => {
    const log = nodeOfType(patched(opened(), 1, [{ op: 'clear', node_id: 'tail' }]), 'tail', 'log');
    expect(log.lines).toEqual([]);
    expect(log.total_lines).toBe(2);
  });

  it('merges what `set` carries and leaves the rest of the node alone', () => {
    const status = nodeOfType(
      patched(opened(), 1, [{ op: 'set', node_id: 'now', text: 'Scanning db-3' }]),
      'now',
      'status',
    );
    expect(status.text).toBe('Scanning db-3');
    expect(status.tone).toBe('running');
    expect(status.detail).toBe('host 2 of 3');
  });

  it('removes named items and leaves the unnamed ones standing', () => {
    const view = patched(opened(), 1, [
      { op: 'remove', node_id: 'hosts', item_ids: ['db-1', 'never-here'] },
    ]);
    expect(nodeOfType(view, 'hosts', 'table').rows.map((row) => row.id)).toEqual(['db-2']);
  });

  it('inserts a node after the one the op names, and drops one by id', () => {
    const view = patched(opened(), 1, [
      { op: 'add-node', after: 'now', node_spec: { id: 'queued', type: 'status', text: 'Queued' } },
      { op: 'remove-node', node_id: 'links' },
    ]);
    expect(ids(view)).toEqual(['now', 'queued', 'swept', 'score', 'phases', 'tail', 'reading']);
    // A newcomer lands in the list its sibling lives in, so naming a node inside
    // a row grows THAT row and not the column the view itself stands in.
    const grown = patched(view, 2, [
      { op: 'add-node', after: 'hosts', node_spec: { id: 'note', type: 'status', text: 'db-2 only' } },
    ]);
    expect(nodeOfType(grown, 'reading', 'group').fields.map((node) => node.id)).toEqual([
      'hosts',
      'note',
      'why',
    ]);
    // …and dropping a row takes the nodes it arranges with it.
    const dropped = patched(view, 2, [{ op: 'remove-node', node_id: 'reading' }]);
    expect(leaves(dropped.nodes).map((node) => node.id)).toEqual([
      'now',
      'queued',
      'swept',
      'score',
      'phases',
      'tail',
    ]);
  });

  // The journal is re-read on every reconnect, so the same frame arrives twice.
  // Folding it twice would append the same rows again.
  it('drops a frame that does not advance the picture', () => {
    const view = patched(opened(), 1, [{ op: 'set', node_id: 'now', text: 'Scanning db-3' }]);
    expect(applyLivePatch(view, frame(view, 1, [{ op: 'set', node_id: 'now', text: 'again' }]))).toBe(
      view,
    );
  });

  // The frame still ADVANCES the picture even when every op named something it
  // does not have: a seq that stood still would make the next frame look like a
  // gap. What must not move is the nodes — the section paints the same list.
  it('leaves the nodes untouched when an op names something it has never seen', () => {
    const view = opened();
    for (const op of [
      { op: 'set', node_id: 'ghost', text: 'x' },
      { op: 'remove-node', node_id: 'ghost' },
      { op: 'detonate', node_id: 'now' },
    ]) {
      const after = patched(view, 1, [op]);
      expect(after.nodes).toBe(view.nodes);
      expect(after.seq).toBe(1);
    }
  });

  // A frame whose range starts past this picture's seq means frames were LOST.
  // Painting it anyway would show a table with a row missing and say nothing.
  it('marks the picture stale when the range skipped a frame', () => {
    const view = patched(opened(), 4, [{ op: 'set', node_id: 'now', text: 'Scanning db-3' }], 3);
    expect(view.is_stale).toBe(true);
    expect(view.seq).toBe(0);
    expect(nodeOfType(view, 'now', 'status').text).toBe('Scanning db-2');
    expect(staleLiveViews([opened(), view]).map((one) => one.id)).toEqual([view.id]);
  });

  it('folds a coalesced range that carries on where the picture stands', () => {
    const view = patched(opened(), 4, [{ op: 'set', node_id: 'now', text: 'Scanning db-3' }], 1);
    expect(view.is_stale).toBeUndefined();
    expect(view.seq).toBe(4);
  });
});

describe('the three session events', () => {
  const other: SseEvent = { type: 'turn.delta' };

  it('answers for its own events and no others', () => {
    expect([LIVE_VIEW_OPEN_EVENT, LIVE_VIEW_PATCH_EVENT, LIVE_VIEW_CLOSE_EVENT].map((type) =>
      isLiveViewEvent({ type }),
    )).toEqual([true, true, true]);
    expect(isLiveViewEvent(other)).toBe(false);
    expect(isLiveViewEvent({ type: 'human_input.request' })).toBe(false);
  });

  it('opens once, however many times the frame arrives', () => {
    const open: SseEvent = { type: LIVE_VIEW_OPEN_EVENT, view_id: fixture.id, view: fixture };
    const once = applyLiveViewEvent([], open);
    const twice = applyLiveViewEvent(once, open);
    expect(once.map((view) => view.id)).toEqual([fixture.id]);
    expect(twice.length).toBe(1);
  });

  // The run is over: its record and the model's copy live elsewhere, and the
  // rows go back to the transcript exactly as the terminal gives its band back.
  it('drops the view the close event names, and keeps the list identity otherwise', () => {
    const views = applyLiveViewEvent([], {
      type: LIVE_VIEW_OPEN_EVENT,
      view_id: fixture.id,
      view: fixture,
    });
    expect(
      applyLiveViewEvent(views, { type: LIVE_VIEW_CLOSE_EVENT, view_id: fixture.id, result: {} }),
    ).toEqual([]);
    expect(applyLiveViewEvent(views, { type: LIVE_VIEW_CLOSE_EVENT, view_id: 'other' })).toBe(views);
    expect(applyLiveViewEvent(views, frame({ ...views[0], id: 'other' }, 1, []))).toBe(views);
    expect(applyLiveViewEvent(views, other)).toBe(views);
  });

  it('folds a patch into the view it names', () => {
    const views = applyLiveViewEvent([], {
      type: LIVE_VIEW_OPEN_EVENT,
      view_id: fixture.id,
      view: fixture,
    });
    const moved = applyLiveViewEvent(
      views,
      frame(views[0], 1, [{ op: 'set', node_id: 'now', text: 'Scanning db-3' }]),
    );
    expect(nodeOfType(moved[0], 'now', 'status').text).toBe('Scanning db-3');
    expect(moved).not.toBe(views);
  });
});

describe('what a table and a progress state', () => {
  const table = nodeOfType(opened(), 'hosts', 'table');

  it('keeps declared order, reverses for newest-first, and sorts by a column', () => {
    expect(orderedRows(table).map((row) => row.id)).toEqual(['db-1', 'db-2']);
    expect(orderedRows({ ...table, order: 'newest-first' }).map((row) => row.id)).toEqual([
      'db-2',
      'db-1',
    ]);
    expect(
      orderedRows({ ...table, order: { by: 'findings', dir: 'desc' } }).map((row) => row.id),
    ).toEqual(['db-2', 'db-1']);
  });

  // A cell with nothing in it answers no question, so it waits at the end
  // whichever way the order runs — the same rule the engine sorts by.
  it('sorts blanks last and keeps ties in the order they arrived', () => {
    const rows = [
      { id: 'a', cells: ['a', 'clean', ''], tone: 'idle' as const },
      { id: 'b', cells: ['b', 'clean', '2'], tone: 'idle' as const },
      { id: 'c', cells: ['c', 'clean', '2'], tone: 'idle' as const },
    ];
    expect(
      orderedRows({ ...table, rows, order: { by: 'findings', dir: 'asc' } }).map((row) => row.id),
    ).toEqual(['b', 'c', 'a']);
  });

  it('is indeterminate until a fraction is knowable', () => {
    expect(liveFraction({ id: 'p', type: 'progress' })).toBeNull();
    expect(liveFraction({ id: 'p', type: 'progress', done: 2, total: 0 })).toBeNull();
    expect(liveFraction({ id: 'p', type: 'progress', done: 2, total: 3 })).toBeCloseTo(2 / 3);
    expect(liveFraction({ id: 'p', type: 'progress', value: 0.5, done: 2, total: 3 })).toBe(0.5);
    expect(livePercent(2 / 3)).toBe(67);
  });
});

/**
 * A record is the only thing a settled view leaves behind, so what it folds back
 * into is the whole contract of re-opening one: the picture the run ENDED on, and
 * the verdict that says how. `human-input.live-sink` writes these three lines.
 */
describe('the record of a settled view', () => {
  const openLine = JSON.stringify({ kind: 'open', at: 1, view: fixture });
  const status = (id: string, text: string) => ({
    id,
    type: 'status',
    text,
    tone: 'ok',
  });
  const patchLine = (seq: number, ops: unknown[]) =>
    JSON.stringify({
      kind: 'patch',
      at: 1 + seq,
      patch: { view_id: opened().id, seq, ops },
    });
  const closeLine = (result: Record<string, unknown>) =>
    JSON.stringify({ kind: 'close', at: 99, result });

  it('folds the declared view and every accepted patch, in file order', () => {
    const record = liveRecordFromText(
      [
        openLine,
        patchLine(1, [{ op: 'add-node', node_spec: status('one', 'first') }]),
        patchLine(2, [{ op: 'add-node', node_spec: status('two', 'second') }]),
        '',
      ].join('\n'),
    );
    expect(record?.view.seq).toBe(2);
    expect(ids(record!.view)).toEqual([...ids(opened()), 'one', 'two']);
  });

  it("takes the verdict's own picture over its replay of the patches", () => {
    // The trailer carries the state the MODEL was handed, so the app shows that
    // one — a record read at its two ends never folded the middle at all.
    const sealed = {
      ...fixture,
      nodes: [status('sealed', 'the run ended here')],
    };
    const record = liveRecordFromText(
      [
        openLine,
        patchLine(1, [{ op: 'add-node', node_spec: status('one', 'first') }]),
        closeLine({ view_id: opened().id, reason: 'completed', is_completed: true, view: sealed }),
      ].join('\n'),
    );
    expect(ids(record!.view)).toEqual(['sealed']);
    expect(record?.reason).toBe('completed');
    expect(record?.is_completed).toBe(true);
    expect(record?.ended_at).toBe(99);
  });

  it('carries the comment the human left with a stop', () => {
    const record = liveRecordFromText(
      [
        openLine,
        closeLine({
          view_id: opened().id,
          reason: 'interrupted',
          is_completed: false,
          note: 'flaky on rerun',
          view: fixture,
        }),
      ].join('\n'),
    );
    expect(record?.reason).toBe('interrupted');
    expect(record?.is_completed).toBe(false);
    expect(record?.note).toBe('flaky on rerun');
  });

  it('keeps what it managed to read when the record was cut mid-write', () => {
    // A run killed while appending leaves half a line. Everything before it is
    // still the truth, and a view with no trailer simply has no verdict yet.
    const record = liveRecordFromText(
      [
        openLine,
        patchLine(1, [{ op: 'add-node', node_spec: status('one', 'first') }]),
        '{"kind":"patch","at":3,"pat',
      ].join('\n'),
    );
    expect(ids(record!.view)).toEqual([...ids(opened()), 'one']);
    expect(record?.reason).toBeUndefined();
  });

  it('is null when not one line of it is paintable', () => {
    expect(liveRecordFromText('')).toBeNull();
    expect(liveRecordFromText('not json at all')).toBeNull();
    expect(liveRecordFromText(JSON.stringify({ kind: 'open', view: { id: 'x' } }))).toBeNull();
  });
});
