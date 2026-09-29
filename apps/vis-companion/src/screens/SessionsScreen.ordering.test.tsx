// @vitest-environment jsdom
import { act, screen, within } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { listSession, renderSessionsScreen } from './sessions-screen-harness';

// Regression: the list must follow recency and liveness while it remains open.
// Opening a session does not change its content clock or move project headers.

let restore = () => {};

const rowOrder = () =>
  Array.from(document.querySelectorAll<HTMLElement>('[data-session-id]')).map(
    (row) => row.dataset.sessionId,
  );

const projectOrder = () =>
  Array.from(document.querySelectorAll<HTMLElement>('[data-project-root]')).map(
    (group) => group.dataset.projectRoot,
  );

const at = (hour: number) => new Date(Date.UTC(2024, 4, 1, hour, 0, 0)).toISOString();

const row = (id: string, hour: number) =>
  listSession({
    id,
    title: `Session ${id}`,
    modified_at: at(hour),
    workspace: { root: '/Users/dev/project' },
  });

describe('the order the reader is looking at', () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    restore();
    vi.useRealTimers();
  });

  /** Let every poll, repaint and effect that fits inside `ms` happen. */
  const settle = async (ms = 0) => {
    await act(async () => {
      await vi.advanceTimersByTimeAsync(ms);
    });
  };

  // Regression: repository headers jumped even while session order was held.
  // Snapshot/response order must not override the canonical root ordering.
  it('keeps repository headers fixed across reordered poll answers and search', async () => {
    const a = listSession({ ...row('a', 12), workspace: { root: '/repo/a' } });
    const b = listSession({ ...row('b', 11), workspace: { root: '/repo/b' } });
    const view = renderSessionsScreen({ machines: [{ sessions: [a, b] }] });
    restore = view.restore;
    await settle(50);
    expect(projectOrder()).toEqual(['/repo/a', '/repo/b']);

    act(() => screen.getByRole('button', { name: 'Expand b' }).click());
    await settle(50);
    expect(rowOrder()).toEqual(['a', 'b']);

    // The fixture returns the overview in activity order, reversing the headers.
    view.setRows(0, [{ ...b, modified_at: at(18), live: true }, a]);
    await settle(10_000);
    expect(projectOrder()).toEqual(['/repo/a', '/repo/b']);
    expect(rowOrder()).toEqual(['a', 'b']);

    act(() => view.setQuery('Session'));
    await settle(1_000);
    expect(projectOrder()).toEqual(['/repo/a', '/repo/b']);
    act(() => view.setQuery(''));
    await settle(50);
    expect(projectOrder()).toEqual(['/repo/a', '/repo/b']);
  });

  it('uses the same repository order for a cached first paint and its revalidation', async () => {
    const a = listSession({ ...row('a', 11), workspace: { root: '/repo/a' } });
    const b = listSession({ ...row('b', 12), workspace: { root: '/repo/b' } });
    const previous = renderSessionsScreen({ machines: [{ sessions: [b, a] }] });
    restore = previous.restore;
    await settle(50);
    previous.unmount();
    previous.restore();

    const resumed = renderSessionsScreen({
      at: previous.conns,
      machines: [{ sessions: [{ ...a, modified_at: at(18) }, b], holdsList: true }],
    });
    restore = resumed.restore;
    expect(projectOrder()).toEqual(['/repo/a', '/repo/b']);
    resumed.releaseList();
    await settle(50);
    expect(projectOrder()).toEqual(['/repo/a', '/repo/b']);
  });

  it('moves a session to its current recency position while the list stays open', async () => {
    const view = renderSessionsScreen({
      machines: [{ label: 'alpha', sessions: [row('a1', 12), row('a2', 11), row('a3', 10)] }],
    });
    restore = view.restore;
    await settle(50);
    expect(rowOrder()).toEqual(['a1', 'a2', 'a3']);

    // A turn lands on the deepest row: the gateway is right to answer it first.
    view.setRows(0, [row('a3', 18), row('a1', 12), row('a2', 11)]);
    await settle(10_000);

    // Regression: polling updated row content but kept the old accepted order.
    expect(rowOrder()).toEqual(['a3', 'a1', 'a2']);
    expect(screen.queryByRole('button', { name: /newer session/ })).toBeNull();
  });

  it('promotes live sessions and restores recency order when they stop', async () => {
    const recent = row('recent', 12);
    const older = row('older', 10);
    const view = renderSessionsScreen({ machines: [{ sessions: [recent, older] }] });
    restore = view.restore;
    await settle(50);
    expect(rowOrder()).toEqual(['recent', 'older']);

    view.setRows(0, [{ ...older, live: true }, recent]);
    await settle(10_000);
    expect(rowOrder()).toEqual(['older', 'recent']);

    view.setRows(0, [recent, older]);
    await settle(10_000);
    expect(rowOrder()).toEqual(['recent', 'older']);
  });

  it('shows sessions created elsewhere in the next response without a tap', async () => {
    const view = renderSessionsScreen({
      machines: [{ label: 'alpha', sessions: [row('a1', 12), row('a2', 11)] }],
    });
    restore = view.restore;
    await settle(50);
    expect(rowOrder()).toEqual(['a1', 'a2']);

    view.setRows(0, [row('new-1', 20), row('new-2', 19), row('a1', 12), row('a2', 11)]);
    await settle(10_000);

    const header = screen.getByRole('button', { name: 'Collapse project' }).closest('header')!;
    expect(within(header).getByText('4 sessions')).toBeVisible();
    expect(rowOrder()).toEqual(['new-1', 'new-2', 'a1', 'a2']);
    expect(screen.queryByRole('button', { name: /newer session/ })).toBeNull();
  });

  // Regression, user report (forking in the app left its new title hidden behind
  // "1 new" in its group until the reader tapped that count).
  it('shows a fork and its later title in its group without accepting newer rows', async () => {
    const groupId = 'group-forks';
    const source = { ...row('source', 12), group_id: groupId };
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [source],
          groups: [
            {
              id: groupId,
              project_id: null,
              name: 'Forks',
              color: 'blue',
              position: 0,
              session_count: 1,
            },
          ],
        },
      ],
    });
    restore = view.restore;
    await settle(50);
    expect(rowOrder()).toEqual(['source']);
    expect(screen.getByRole('button', { name: 'Collapse Forks' })).toBeVisible();

    const fork = { ...row('fork', 20), group_id: groupId, title: 'Session source (fork)' };
    view.setOpenSession({ conn: view.conns[0]!, sid: fork.id, fresh: true });
    view.setRows(0, [fork, source]);
    await settle(10_000);

    expect(rowOrder()).toEqual(['fork', 'source']);
    expect(screen.getByText('Session source (fork)')).toBeVisible();
    expect(screen.queryByRole('button', { name: /newer session/ })).toBeNull();

    // Naming finishes after the fork has already appeared in the project.
    view.setRows(0, [{ ...fork, title: 'Renamed fork' }, source]);
    await settle(10_000);
    expect(screen.getByText('Renamed fork')).toBeVisible();
    expect(screen.queryByText('Session source (fork)')).toBeNull();
    expect(screen.queryByRole('button', { name: /newer session/ })).toBeNull();
  });

  it('updates each project without opening projects the reader collapsed', async () => {
    const inProject = (id: string, hour: number, root: string) =>
      listSession({ ...row(id, hour), workspace: { root } });
    const a = inProject('a', 12, '/repo/a');
    const b = inProject('b', 11, '/repo/b');
    const quiet = inProject('quiet', 10, '/repo/quiet');
    const view = renderSessionsScreen({ machines: [{ sessions: [a, b, quiet] }] });
    restore = view.restore;
    await settle(50);

    const aNew = inProject('a-new', 20, '/repo/a');
    const bNew = inProject('b-new', 19, '/repo/b');
    const bNext = inProject('b-next', 18, '/repo/b');
    view.setRows(0, [aNew, bNew, bNext, a, b, quiet]);
    await settle(10_000);

    const header = (name: string) =>
      screen.getByRole('button', { name: new RegExp(`^(Expand|Collapse) ${name}$`) }).closest('header')!;
    expect(within(header('a')).getByText('2 sessions')).toBeVisible();
    expect(within(header('b')).getByText('3 sessions')).toBeVisible();
    expect(screen.getByRole('button', { name: 'Expand b' })).toHaveAttribute('aria-expanded', 'false');
    expect(rowOrder()).toEqual(['a-new', 'a']);
    expect(screen.queryByRole('button', { name: /newer session/ })).toBeNull();

    act(() => screen.getByRole('button', { name: 'Expand b' }).click());
    await settle(50);
    expect(rowOrder()).toEqual(['a-new', 'a', 'b-new', 'b-next', 'b']);
    expect(screen.queryByRole('button', { name: /newer session/ })).toBeNull();
  });

  it('appends a row deeper than everything held, because that is the next page', async () => {
    const view = renderSessionsScreen({
      machines: [{ label: 'alpha', sessions: [row('a1', 12), row('a2', 11)] }],
    });
    restore = view.restore;
    await settle(50);

    view.setRows(0, [row('a1', 12), row('a2', 11), row('older', 4)]);
    await settle(10_000);

    expect(rowOrder()).toEqual(['a1', 'a2', 'older']);
    expect(screen.queryByRole('button', { name: /newer session/ })).toBeNull();
  });

  it('comes back to what is current after a real absence', async () => {
    const view = renderSessionsScreen({
      machines: [{ label: 'alpha', sessions: [row('a1', 12), row('a2', 11)] }],
    });
    restore = view.restore;
    await settle(50);

    view.setRows(0, [row('new-1', 20), row('a1', 12), row('a2', 11)]);
    await settle(10_000);
    expect(rowOrder()).toEqual(['new-1', 'a1', 'a2']);

    // Away for two minutes, then foreground again: a glance at a notification is not
    // this (`WakeInfo.awayMs`), and the list a reader comes back to is the current one.
    window.dispatchEvent(new Event('pagehide'));
    await settle(120_000);
    await act(async () => {
      window.dispatchEvent(new Event('pageshow'));
      await vi.advanceTimersByTimeAsync(1_000);
    });

    expect(rowOrder()).toEqual(['new-1', 'a1', 'a2']);
    expect(screen.queryByRole('button', { name: /newer session/ })).toBeNull();
  });
});
