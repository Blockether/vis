import { describe, expect, it } from 'vitest';

import { foldGlobalApps, foldGlobalAppView } from './global-apps';
import { liveViewFromWire, type LiveView } from './live-view';
import type { SseEvent } from './types';
import { VIEW_CLOSE_EVENT, VIEW_OPEN_EVENT, VIEW_PATCH_EVENT } from './view';

const wire = (id: string, scope?: 'global', seq = 1) => ({
  id,
  title: `App ${id}`,
  placement: 'attachment',
  seq,
  key: id,
  ...(scope ? { scope } : {}),
  nodes: [{ id: 'notes', type: 'thread', messages: [] }],
});

function global(id: string, seq = 1): LiveView {
  const view = liveViewFromWire(wire(id, 'global', seq));
  if (!view) throw new Error('fixture must parse');
  return view;
}

const open = (id: string, extra: Partial<SseEvent> = {}, scope: 'global' | null = 'global'): SseEvent => ({
  type: VIEW_OPEN_EVENT,
  view_id: id,
  view: wire(id, scope ?? undefined),
  ...extra,
});
const close = (id: string, extra: Partial<SseEvent> = {}): SseEvent => ({
  type: VIEW_CLOSE_EVENT,
  view_id: id,
  result: {},
  ...extra,
});
const patch = (id: string, firstSeq: number): SseEvent => ({
  type: VIEW_PATCH_EVENT,
  kind: 'live',
  view_id: id,
  first_seq: firstSeq,
  patch: { seq: firstSeq, ops: [] },
});
const ready: SseEvent = { type: 'subscription.ready', scope: 'fleet', seq: 0 };

describe('global apps on the fleet stream', () => {
  it('adds a global app that opens', () => {
    expect(foldGlobalApps([], open('board')).apps.map((one) => one.id)).toEqual(['board']);
  });

  // The shared stream also brings session frames to a fleet listener.
  it('leaves out the frames of a session', () => {
    const held = [global('board')];
    expect(foldGlobalApps([], open('board', { session_id: 's1' })).apps).toEqual([]);
    expect(foldGlobalApps(held, close('board', { session_id: 's1' })).apps).toBe(held);
  });

  it('leaves out an opened view that is not a global app', () => {
    expect(foldGlobalApps([], open('panel', {}, null)).apps).toEqual([]);
  });

  it('marks a closed app and reads the list again', () => {
    const folded = foldGlobalApps([global('board')], close('board'));
    expect(folded.apps[0]?.is_closed).toBe(true);
    expect(folded.isReread).toBe(true);
  });

  it('reads the list again after a lost patch', () => {
    expect(foldGlobalApps([global('board', 1)], patch('board', 4)).isReread).toBe(true);
    expect(foldGlobalApps([global('board', 1)], patch('board', 2)).isReread).toBe(false);
  });

  // The fleet stream has no replay: a new stream may have missed frames.
  it('reads the list again when the fleet stream is ready', () => {
    expect(foldGlobalApps([], ready).isReread).toBe(true);
  });
});

describe('one opened global app', () => {
  it('folds its own frames and leaves out session frames', () => {
    expect(foldGlobalAppView(global('board'), close('board')).view.is_closed).toBe(true);
    expect(foldGlobalAppView(global('board'), close('board', { session_id: 's1' })).view.is_closed).toBeUndefined();
  });

  it('reads the app again when the fleet stream is ready', () => {
    expect(foldGlobalAppView(global('board'), ready).isReread).toBe(true);
  });
});
