// @vitest-environment jsdom
import { act, fireEvent, screen } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

// The fleet effects are the seam: `syncBadge` is keyed on the machines array itself, so
// it runs again exactly when — and only when — that array is replaced.
const badge = vi.hoisted(() => ({
  syncBadge: vi.fn(() => Promise.resolve()),
  reassertBadge: vi.fn(() => Promise.resolve()),
}));
vi.mock('../lib/badge', () => badge);

import { listSession, renderSessionsScreen } from './sessions-screen-harness';
import { GatewayClient } from '../lib/gateway';

import { forgetListScroll, parkedListScroll, rememberListScroll } from '../lib/list-scroll';

/** Let every poll, repaint and effect that fits inside `ms` happen. */
const settle = async (ms = 0) => {
  await act(async () => {
    await vi.advanceTimersByTimeAsync(ms);
  });
};

// A project's page is a read of ITS own now (`GatewayClient.listProjectPage`), so
// the FLEET read — the one this poll owns — is the one without a `root=`.
const listReads = (requests: { path: string }[]) =>
  requests.filter(
    (request) => request.path.startsWith('/v1/sessions?') && !request.path.includes('root='),
  ).length;
let restore = () => {};

// Regression, user report (paraphrased: "entering the session list makes it flicker, and
// it keeps fetching page after page of sessions"): every poll patched its machine even
// when the gateway answered with the rows already on screen, so the fleet array was
// replaced ten seconds apart forever — and with it the scope filter, the sort, the
// project grouping and the pager built from it, under a reader who was only reading.
describe('a poll that changes nothing leaves the list alone', () => {
  beforeEach(() => {
    vi.useFakeTimers();
    badge.syncBadge.mockClear();
  });
  afterEach(() => {
    restore();
    vi.useRealTimers();
  });

  it('keeps the fleet it already has when the poll answers with the same rows', async () => {
    const view = renderSessionsScreen({
      machines: [{ label: 'alpha', sessions: [listSession({ id: 'a1', title: 'First' })] }],
    });
    restore = view.restore;
    await settle(50);
    expect(screen.getByText('First')).toBeVisible();

    const painted = badge.syncBadge.mock.calls.length;
    const read = listReads(view.requests);

    await settle(4_900);
    expect(listReads(view.requests)).toBe(read);
    await settle(100);

    // The five-second reachability poll really ran...
    expect(listReads(view.requests)).toBeGreaterThan(read);
    // ...and said nothing new, so the fleet the whole list is derived from is the
    // very array it was before.
    expect(badge.syncBadge.mock.calls.length).toBe(painted);
    expect(screen.getByText('First')).toBeVisible();
  });

  it('reads only the changed project page after a poll', async () => {
    const alpha = listSession({ id: 'a1', title: 'First', workspace: { root: '/work/alpha' } });
    const beta = listSession({ id: 'b1', title: 'Second', workspace: { root: '/work/beta' } });
    const view = renderSessionsScreen({ machines: [{ sessions: [alpha, beta] }] });
    restore = view.restore;
    await settle(50);

    fireEvent.click(screen.getByRole('button', { name: 'Expand beta' }));
    await settle(50);

    const pages = (root: string) => view.requests.filter(({ path }) =>
      path.startsWith('/v1/sessions?') && path.includes(`root=${encodeURIComponent(root)}`),
    ).length;
    const before = [pages('/work/alpha'), pages('/work/beta')];
    expect(before).toEqual([1, 1]);

    view.setRows(0, [{ ...alpha, title: 'Changed' }, beta]);
    await settle(5_000);

    expect(screen.getByText('Changed')).toBeVisible();
    expect([pages('/work/alpha'), pages('/work/beta')]).toEqual([2, 1]);
  });

  it('checks an open archive for edits the fleet window does not report', async () => {
    const root = '/work/alpha';
    const active = listSession({ id: 'active', title: 'Active', workspace: { root } });
    const archived = listSession({ id: 'archived', title: 'Old title', workspace: { root }, archived_at: 1 });
    const view = renderSessionsScreen({ machines: [{ sessions: [active, archived] }] });
    restore = view.restore;
    await settle(50);

    fireEvent.click(screen.getByRole('button', { name: `Actions for sessions in ${root}` }));
    fireEvent.click(screen.getByText('Show archived sessions'));
    await settle(50);
    expect(screen.getByText('Old title')).toBeVisible();

    view.setRows(0, [active, { ...archived, title: 'New title' }]);
    await settle(30_000);
    expect(screen.getByText('Old title')).toBeVisible();
    await settle(30_000);
    expect(screen.getByText('New title')).toBeVisible();
  });

  it('keeps polling while an iOS webview reports the document hidden', async () => {
    vi.spyOn(document, 'visibilityState', 'get').mockReturnValue('hidden');
    const view = renderSessionsScreen({
      machines: [{ label: 'RBI', sessions: [listSession({ id: 'a1', title: 'First' })] }],
    });
    restore = view.restore;
    await settle(50);
    const read = listReads(view.requests);

    await settle(5_000);

    expect(listReads(view.requests)).toBeGreaterThan(read);
  });
});

// Regression, user report (paraphrased: "I come back to the app in a session view, and
// leaving it flickers while the session list loads — we already know that list, so hold
// it while I am reading the session"): the fleet's FIRST read was gated on this screen
// being visible, so a relaunch straight into a transcript left the list mounted on
// nothing but its skeleton, and its rows only started arriving on the frame the reader
// pressed Back on.
describe('the list parked behind an open session', () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    restore();
    forgetListScroll();
    vi.useRealTimers();
  });

  const parkedList = () =>
    renderSessionsScreen({
      isVisible: false,
      machines: [{ label: 'alpha', sessions: [listSession({ id: 'a1', title: 'First' })] }],
    });

  it('reads only the accepting gateway after a turn is sent', async () => {
    const view = renderSessionsScreen({
      isVisible: false,
      machines: [
        { label: 'alpha', sessions: [listSession({ id: 'same', title: 'First' })] },
        {
          label: 'beta',
          sessions: [listSession({ id: 'same', title: 'Second' })],
          routes: { '/v1/sessions/same/turns': { turn_id: 'turn-1', status: 'queued' } },
        },
      ],
    });
    restore = view.restore;
    await settle(50);
    const reads = () => view.conns.map((conn) =>
      listReads(view.requests.filter(({ machine }) => machine === conn.url)),
    );
    expect(reads()).toEqual([1, 1]);

    await act(async () => {
      await new GatewayClient(view.conns[1]!).submitTurn('same', 'Next question');
    });
    await settle(50);

    expect(reads()).toEqual([1, 2]);
  });

  it('reads its rows while nobody is looking at it', async () => {
    const view = parkedList();
    restore = view.restore;

    await settle(50);

    expect(listReads(view.requests)).toBe(1);
    // What the reader arrives on is the list itself, never its loading state.
    expect(screen.getByText('First')).toBeVisible();
    expect(screen.queryByLabelText('Loading sessions')).toBeNull();
  });

  it('reads once and leaves the poll to the screen on the glass', async () => {
    const view = parkedList();
    restore = view.restore;
    await settle(50);
    expect(listReads(view.requests)).toBe(1);

    await settle(30_000);

    expect(listReads(view.requests)).toBe(1);
  });

  it('asks for nothing when it already has rows to paint', async () => {
    const view = renderSessionsScreen({
      machines: [{ label: 'alpha', sessions: [listSession({ id: 'a1', title: 'First' })] }],
    });
    restore = view.restore;
    await settle(50);
    const read = listReads(view.requests);

    view.setVisible(false);
    await settle(50);

    expect(listReads(view.requests)).toBe(read);
  });

  it('keeps the reading position it has nowhere to put back yet', async () => {
    rememberListScroll({ top: 900, anchor: { id: 'a1', offset: 0 } });
    const view = parkedList();
    restore = view.restore;

    await settle(50);

    // The rows landed off the glass, where the scroller has no height to restore into.
    // Spending the mark there costs the reader their place on the frame they come back on.
    expect(parkedListScroll()?.top).toBe(900);
  });
});
