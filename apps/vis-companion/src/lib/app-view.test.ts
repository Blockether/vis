import { describe, expect, it } from 'vitest';

import {
  appFallbackTitle,
  appStateWord,
  draftAction,
  foldAppEvent,
  newerAppView,
  sendAction,
} from './app-view';
import { applyLivePatch, liveViewFromWire, LIVE_SUBMISSIONS_MAX, type LiveView } from './live-view';
import { VIEW_CLOSE_EVENT, VIEW_PATCH_EVENT } from './view';

const PATCH = '--- a/a.txt\n+++ b/a.txt\n@@ -1 +1 @@\n-old\n+new';

function app(): LiveView {
  const view = liveViewFromWire({
    id: 'app-1',
    title: 'Review',
    placement: 'attachment',
    seq: 3,
    nodes: [
      {
        id: 'review',
        type: 'diff',
        patch: PATCH,
        old_revision: 'aaa',
        new_revision: 'bbb',
        threads: [
          {
            id: 't1',
            anchor: { path: 'a.txt', side: 'new', line: 1 },
            messages: [{ id: 'm1', author: { kind: 'agent' }, body: 'Why?', created_at: 1 }],
            draft: 'Because',
          },
        ],
      },
      { id: 'notes', type: 'thread', messages: [] },
    ],
  });
  if (!view) throw new Error('fixture must parse');
  return view;
}

const patch = (seq: number, ops: unknown[], firstSeq = seq) => ({
  type: VIEW_PATCH_EVENT,
  kind: 'live',
  view_id: 'app-1',
  first_seq: firstSeq,
  patch: { seq, ops },
});

describe('app patch ops', () => {
  it('reads diff and thread nodes and the disconnected flag', () => {
    const view = liveViewFromWire({
      id: 'x',
      title: 'X',
      is_stale: true,
      nodes: [{ id: 'n', type: 'paragraph', text: 'Hi' }],
    });
    expect(view?.is_disconnected).toBe(true);
    expect(view?.is_stale).toBeUndefined();
    const [diff] = app().nodes;
    expect(diff.type === 'diff' && diff.threads[0].draft).toBe('Because');
  });

  it('upserts a whole thread into a diff and into a thread node', () => {
    const next = applyLivePatch(app(), patch(4, [
      {
        op: 'thread',
        node_id: 'review',
        thread: {
          id: 't2',
          anchor: { path: 'a.txt', side: 'old', line: 1 },
          messages: [{ id: 'm2', author: { kind: 'human', name: 'Ann' }, body: 'Gone?', created_at: 2 }],
        },
      },
      {
        op: 'thread',
        node_id: 'notes',
        thread: { id: 'notes', messages: [], draft: 'Later' },
      },
    ]));
    const [diff, notes] = next.nodes;
    expect(diff.type === 'diff' && diff.threads.map((thread) => thread.id)).toEqual(['t1', 't2']);
    expect(notes.type === 'thread' && notes.draft).toBe('Later');
    expect(next.seq).toBe(4);
  });

  it('removes diff threads by id', () => {
    const next = applyLivePatch(app(), patch(4, [{ op: 'remove', node_id: 'review', item_ids: ['t1'] }]));
    const [diff] = next.nodes;
    expect(diff.type === 'diff' && diff.threads).toEqual([]);
  });

  it('folds receipts by id and keeps them bounded', () => {
    let view = app();
    for (let at = 0; at < LIVE_SUBMISSIONS_MAX + 5; at++) {
      view = applyLivePatch(view, patch(view.seq + 1, [
        {
          op: 'submission',
          submission: {
            id: `s${at}`,
            idempotency_key: `k${at}`,
            status: at === 0 ? 'answered' : 'accepted',
            created_at: at,
          },
        },
      ]));
    }
    expect(view.submissions).toHaveLength(LIVE_SUBMISSIONS_MAX);
    expect(view.submissions?.some((one) => one.id === 's0')).toBe(false);
    view = applyLivePatch(view, patch(view.seq + 1, [
      { op: 'submission', submission: { id: 's54', idempotency_key: 'k54', status: 'delivered' } },
    ]));
    expect(view.submissions?.filter((one) => one.id === 's54')).toEqual([
      expect.objectContaining({ status: 'delivered' }),
    ]);
  });
});

describe('foldAppEvent', () => {
  it('folds a patch of its own view and ignores other views', () => {
    const view = app();
    const own = foldAppEvent(view, patch(4, [{ op: 'set', node_id: 'review', label: 'Diff' }]));
    expect(own.view.seq).toBe(4);
    expect(own.isReread).toBe(false);
    const other = foldAppEvent(view, { ...patch(4, []), view_id: 'other' });
    expect(other.view).toBe(view);
  });

  it('reads the app again after a seq gap and keeps the held picture', () => {
    const view = app();
    const gap = foldAppEvent(view, patch(9, [], 8));
    expect(gap.isReread).toBe(true);
    expect(gap.view).toBe(view);
  });

  it('marks a closed app closed and reads it again', () => {
    const closed = foldAppEvent(app(), { type: VIEW_CLOSE_EVENT, kind: 'live', view_id: 'app-1' });
    expect(closed.view.is_closed).toBe(true);
    expect(closed.isReread).toBe(true);
    expect(appStateWord(closed.view)).toBe('Closed');
  });
});

describe('newerAppView', () => {
  it('never lets a slow read undo a newer patch', () => {
    const held = { ...app(), seq: 8 };
    expect(newerAppView(held, { ...app(), seq: 5 })).toBe(held);
    expect(newerAppView(held, { ...app(), seq: 9 }).seq).toBe(9);
    expect(newerAppView(held, { ...app(), seq: 8, is_closed: true }).is_closed).toBe(true);
  });

  it('marks an app disconnected and live again at the same seq', () => {
    const live = { ...app(), seq: 8 };
    const lost = newerAppView(live, { ...app(), seq: 8, is_disconnected: true });
    expect(lost.is_disconnected).toBe(true);
    const back = newerAppView(lost, { ...app(), seq: 8 });
    expect(back.is_disconnected).toBeUndefined();
    expect(appStateWord(back)).toBe('Live');
  });
});

describe('app actions', () => {
  it('drafts on a thread or on a new anchor, with the state version and revision', () => {
    const view = app();
    expect(draftAction(view, 'review', { threadId: 't1', body: 'Ok' })).toEqual({
      action: 'draft',
      node_id: 'review',
      thread_id: 't1',
      body: 'Ok',
      base_seq: 3,
      revision: 'bbb',
    });
    const anchor = { path: 'a.txt', side: 'old' as const, line: 1 };
    expect(draftAction(view, 'review', { anchor, body: 'Why' })).toMatchObject({ anchor });
    expect(draftAction(view, 'notes', { threadId: 'notes', body: '' })).not.toHaveProperty(
      'revision',
    );
  });

  it('sends with the idempotency key and the reviewed revision', () => {
    expect(sendAction(app(), 'review', 'k1')).toEqual({
      action: 'submit',
      values: {},
      node_id: 'review',
      idempotency_key: 'k1',
      base_seq: 3,
      revision: 'bbb',
    });
  });

  it('names an app from its filename until its view arrives', () => {
    expect(appFallbackTitle('code-review.app.json')).toBe('code-review');
    expect(appFallbackTitle(undefined)).toBe('app');
  });
});
