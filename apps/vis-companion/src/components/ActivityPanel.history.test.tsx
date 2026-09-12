// @vitest-environment jsdom
// #212: complete durable history stays reachable through bounded visible pages.
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import { ActivityHistoryContext, ActivityPanel } from './ActivityPanel';
import {
  activityHistoryPage as page,
  groupedActivityHistoryPage as groupPage,
  GROUPED_ACTIVITY_HISTORY_IDS as ids,
} from '../dev/activity-history';

afterEach(() => {
  cleanup();
  vi.restoreAllMocks();
});

it('opens retained operations without a search or bulk-action toolbar', () => {
  render(<ActivityPanel activity={page()} />);
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  expect(screen.getByText('Operation 1')).toBeVisible();
  expect(screen.queryByRole('searchbox')).toBeNull();
  expect(
    screen.queryByRole('button', {
      name: /^(Search activity|First operations|Next operations|Copy all activity|Export all activity)$/,
    }),
  ).toBeNull();
});

it.each([false, true])('pages grouped histories without a toolbar (mixed: %s)', async (mixed) => {
  const activities = ids.map((id) => groupPage(id));
  if (mixed) delete activities[1].history;
  const original = JSON.stringify(activities);
  const source = { load: vi.fn(async (id, after, q) => groupPage(id, after, q)) };
  render(
    <ActivityHistoryContext.Provider value={source}>
      <ActivityPanel activity={activities} />
    </ActivityHistoryContext.Provider>,
  );
  expect(source.load).not.toHaveBeenCalled();
  expect(screen.getByRole('button', { name: 'Expand Activity' })).toHaveTextContent('7 operations');
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  expect(screen.queryByRole('searchbox')).toBeNull();
  expect(screen.queryByRole('button', { name: 'Copy all activity' })).toBeNull();
  expect(screen.queryByRole('button', { name: 'Export all activity' })).toBeNull();
  expect(screen.queryByRole('button', { name: 'Show earlier operations' })).toBeNull();
  expect(screen.queryByRole('status')).toBeNull();
  fireEvent.click(screen.getByRole('button', { name: /Read ×6/ }));
  expect(document.querySelectorAll('[data-activity-row]')).toHaveLength(6);
  const more = screen.getByRole('button', { name: 'Show more operations' });
  const chronology = screen.getByRole('list', { name: 'Operation groups' });
  expect(chronology.compareDocumentPosition(more) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
  fireEvent.click(more);
  await screen.findByText('review-3-3.clj');
  expect(document.querySelectorAll('[data-activity-row]')).toHaveLength(1);
  expect(screen.queryByText('review-1-1.clj')).toBeNull();
  expect(screen.queryByRole('button', { name: 'Show more operations' })).toBeNull();
  expect(source.load).toHaveBeenCalledExactlyOnceWith(ids[2], 2, '', expect.any(AbortSignal));
  fireEvent.click(screen.getByRole('button', { name: 'Show earlier operations' }));
  await screen.findByRole('button', { name: /Read ×6/ });
  expect(source.load.mock.calls.slice(1).map(([id, after]) => [id, after])).toEqual(
    activities.filter((activity) => activity.history).map((activity) => [activity.history!.id, 0]),
  );
  expect(JSON.stringify(activities)).toBe(original);
});

it('keeps grouped pages and disclosures on a failed load, then retries', async () => {
  const load = vi.fn(async (id, after, q) => groupPage(id, after, q));
  load.mockRejectedValueOnce(new Error('History unavailable'));
  render(
    <ActivityHistoryContext.Provider value={{ load }}>
      <ActivityPanel activity={ids.map((id) => groupPage(id))} />
    </ActivityHistoryContext.Provider>,
  );
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  fireEvent.click(screen.getByRole('button', { name: /Read ×6/ }));
  fireEvent.click(screen.getByRole('button', { name: 'Show more operations' }));
  expect(await screen.findByRole('alert')).toHaveTextContent('History unavailable');
  expect(document.querySelectorAll('[data-activity-row]')).toHaveLength(6);
  expect(screen.getByRole('button', { name: /Read ×6/ })).toHaveAttribute('aria-expanded', 'true');
  fireEvent.click(screen.getByRole('button', { name: 'Show more operations' }));
  expect(await screen.findByText('review-3-3.clj')).toBeVisible();
  expect(screen.queryByRole('alert')).toBeNull();
});

it('keeps one band open and aborts stale grouped work when a live history settles', async () => {
  const activities = ids.map((id) => groupPage(id));
  activities[2].state = 'running';
  activities[2].counts = { running: 1, succeeded: 2, failed: 0, cancelled: 0 };
  let complete: (result: ReturnType<typeof groupPage>) => void = () => {};
  const load = vi.fn(
    (_id: string, _after: number, _q: string, _signal: AbortSignal) =>
      new Promise<ReturnType<typeof groupPage>>((resolve) => {
        complete = resolve;
      }),
  );
  const paint = (receipts: typeof activities) => (
    <ActivityHistoryContext.Provider value={{ load }}>
      <ActivityPanel activity={receipts} />
    </ActivityHistoryContext.Provider>
  );
  const view = render(paint(activities));
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  expect(screen.getByRole('button', { name: 'Collapse Activity' })).toHaveTextContent('1 running');
  fireEvent.click(screen.getByRole('button', { name: 'Show more operations' }));
  expect(screen.getByRole('status')).toHaveTextContent('Loading operations');
  expect(screen.getByRole('button', { name: 'Show more operations' })).toBeDisabled();
  const settled = structuredClone(activities);
  settled[2].history!.revision = 2;
  settled[2].state = 'failed';
  settled[2].counts = { running: 0, succeeded: 1, failed: 1, cancelled: 1 };
  settled[2].rows[0].state = 'failed';
  settled[2].rows[1].state = 'cancelled';
  view.rerender(paint(settled));
  expect(load.mock.calls[0][3].aborted).toBe(true);
  complete(groupPage(ids[2], 2));
  await waitFor(() => expect(screen.queryByRole('status')).toBeNull());
  expect(screen.getByRole('button', { name: 'Collapse Activity' })).toHaveTextContent(
    '7 operations · 1 failed · 1 cancelled',
  );
  expect(screen.getAllByRole('list', { name: 'Operation groups' })).toHaveLength(1);
  expect(screen.queryByText('review-3-3.clj')).toBeNull();
});

it('reaches the tail without accumulating prior rows and returns to earlier operations', async () => {
  const load = vi.fn(async (_id: string, after: number) => page(after));
  render(
    <ActivityHistoryContext.Provider value={{ load }}>
      <ActivityPanel activity={page()} />
    </ActivityHistoryContext.Provider>,
  );
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  for (let i = 0; i < 4; i++) {
    fireEvent.click(screen.getByRole('button', { name: 'Show more operations' }));
    await screen.findByText(`Operation ${(i + 2) * 32}`);
    expect(document.querySelectorAll('[data-activity-row]')).toHaveLength(32);
  }
  expect(screen.getByText('Operation 159')).toBeVisible();
  expect(screen.getByText('Operation 160')).toBeVisible();
  expect(screen.queryByText('Operation 1')).toBeNull();
  expect(screen.queryByRole('button', { name: 'Show more operations' })).toBeNull();
  fireEvent.click(screen.getByRole('button', { name: 'Show earlier operations' }));
  expect(await screen.findByText('Operation 1')).toBeVisible();
  expect(screen.queryByText('Operation 160')).toBeNull();
});

it.each(['revision', 'cursor', 'id', 'after'])(
  'rejects a changed %s without dropping current rows and offers a reload',
  async (field) => {
    const load = vi.fn(async (_id: string, after: number) => {
      const result = page(after);
      if (after) {
        if (field === 'revision') result.history!.revision = 2;
        if (field === 'cursor') result.history!.next_after = after;
        if (field === 'id') result.history!.id = ids[0];
        if (field === 'after') result.history!.after = 0;
      }
      return result;
    });
    render(
      <ActivityHistoryContext.Provider value={{ load }}>
        <ActivityPanel activity={page()} />
      </ActivityHistoryContext.Provider>,
    );
    fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
    fireEvent.click(screen.getByRole('button', { name: 'Show more operations' }));
    expect(await screen.findByRole('alert')).toHaveTextContent('Activity changed');
    expect(screen.getByText('Operation 1')).toBeVisible();
    expect(screen.queryByText('Operation 64')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Reload operations' }));
    await waitFor(() => expect(screen.queryByRole('alert')).toBeNull());
    expect(load).toHaveBeenLastCalledWith(page().history!.id, 0, '', expect.any(AbortSignal));
  },
);

it('aborts a pending load when unmounted', () => {
  const load = vi.fn(
    (_id: string, _after: number, _q: string, _signal: AbortSignal) =>
      new Promise<ReturnType<typeof page>>(() => {}),
  );
  const view = render(
    <ActivityHistoryContext.Provider value={{ load }}>
      <ActivityPanel activity={page()} />
    </ActivityHistoryContext.Provider>,
  );
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  fireEvent.click(screen.getByRole('button', { name: 'Show more operations' }));
  view.unmount();
  expect(load.mock.calls[0][3].aborted).toBe(true);
});

it('keeps retained rows readable offline and explains an empty history', () => {
  const view = render(<ActivityPanel activity={page()} />);
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  expect(screen.getByText('Operation 1')).toBeVisible();
  expect(screen.getByText('Reconnect to load more operations.')).toBeVisible();
  expect(screen.queryByRole('button', { name: 'Show more operations' })).toBeNull();
  const empty = page();
  empty.rows = [];
  empty.history!.next_after = null;
  view.rerender(<ActivityPanel activity={empty} />);
  expect(screen.getByText('No operations available.')).toBeVisible();
  expect(screen.queryByText('Reconnect to load more operations.')).toBeNull();
});

it('keeps the band open but discards stale pages on a live revision', async () => {
  const source = { load: vi.fn(async (_id: string, after: number) => page(after)) };
  const paint = (activity: ReturnType<typeof page>) => (
    <ActivityHistoryContext.Provider value={source}>
      <ActivityPanel activity={activity} />
    </ActivityHistoryContext.Provider>
  );
  const view = render(paint(page()));
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  fireEvent.click(screen.getByRole('button', { name: 'Show more operations' }));
  await screen.findByText('Operation 64');
  const updated = page();
  updated.history!.revision = 2;
  view.rerender(paint(updated));
  expect(screen.getByText('Operation 1')).toBeVisible();
  expect(screen.queryByText('Operation 64')).toBeNull();
});

// #212: grouping is presentation, not another retained invocation or a lost outcome.
it('pages grouped invocations through tail failures and cancellations', async () => {
  const grouped = (after = 0) => {
    const result = page(after);
    result.rows = [
      {
        ...result.rows[0],
        id: `group-${after}`,
        operation: 'shell',
        presentation: undefined,
        state: after === 128 ? 'failed' : 'succeeded',
        children: result.rows,
      },
    ];
    return result;
  };
  const load = vi.fn(async (_id: string, after: number) => grouped(after));
  render(
    <ActivityHistoryContext.Provider value={{ load }}>
      <ActivityPanel activity={grouped()} />
    </ActivityHistoryContext.Provider>,
  );
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  for (let i = 0; i < 4; i++) {
    fireEvent.click(screen.getByRole('button', { name: 'Show more operations' }));
    await waitFor(() => expect(load).toHaveBeenCalledTimes(i + 1));
    await waitFor(() =>
      expect(screen.getByRole('button', { name: 'Show earlier operations' })).not.toBeDisabled(),
    );
  }
  expect(screen.getByText('Operation 159')).toBeVisible();
  expect(screen.getByText('Operation 160')).toBeVisible();
  expect(screen.queryByText('Operation 1')).toBeNull();
  expect(grouped(128).rows[0].children?.slice(-2).map((row) => row.state)).toEqual([
    'failed',
    'cancelled',
  ]);
});
