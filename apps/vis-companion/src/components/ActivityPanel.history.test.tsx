// @vitest-environment jsdom
// #212: complete durable history stays reachable; the band reads it whole, never in pages.
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
  vi.unstubAllGlobals();
});

it('copies all retained operations while collapsed, then opens the whole history', async () => {
  const writeText = vi.fn().mockResolvedValue(undefined);
  vi.stubGlobal('navigator', { ...navigator, clipboard: { writeText } });
  const load = vi.fn(async (_id: string, after: number) => page(after));
  render(
    <ActivityHistoryContext.Provider value={{ load }}>
      <ActivityPanel activity={page()} />
    </ActivityHistoryContext.Provider>,
  );
  const copy = screen.getByRole('button', { name: 'Copy activity' });
  expect(copy.closest('[data-disclosure-toggle]')).toBeNull();
  expect(load).not.toHaveBeenCalled();
  fireEvent.click(copy);
  await screen.findByRole('button', { name: 'Copied' });
  expect(writeText).toHaveBeenCalledTimes(1);
  expect(writeText.mock.calls[0][0]).toContain('retained-1\n');
  expect(writeText.mock.calls[0][0]).toContain('retained-160\n');
  expect(screen.getByRole('button', { name: 'Expand Activity' })).toHaveAttribute(
    'aria-expanded',
    'false',
  );
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  expect(await screen.findByText('Operation 160')).toBeVisible();
  expect(screen.getByText('Operation 1')).toBeVisible();
});

it('keeps offline Copy available and explains how to retry incomplete history', async () => {
  const writeText = vi.fn().mockResolvedValue(undefined);
  vi.stubGlobal('navigator', { ...navigator, clipboard: { writeText } });
  render(<ActivityPanel activity={page()} />);
  fireEvent.click(screen.getByRole('button', { name: 'Copy activity' }));
  const error = await screen.findByRole('alert');
  expect(error).toBeVisible();
  expect(error).toHaveTextContent('Reconnect');
  expect(screen.getByRole('button', { name: 'Copy failed. Try again.' })).toBeEnabled();
  expect(writeText).not.toHaveBeenCalled();
});

it.each(['running', 'succeeded', 'failed', 'cancelled'] as const)(
  'copies a complete %s history offline without retrieval',
  async (state) => {
    const writeText = vi.fn().mockResolvedValue(undefined);
    vi.stubGlobal('navigator', { ...navigator, clipboard: { writeText } });
    const activity = groupPage(ids[0]);
    activity.state = state;
    activity.counts = { running: 0, succeeded: 0, failed: 0, cancelled: 0, [state]: 2 };
    activity.rows = activity.rows.map((row) => ({ ...row, state }));
    render(<ActivityPanel activity={activity} />);
    fireEvent.click(screen.getByRole('button', { name: 'Copy activity' }));
    await screen.findByRole('button', { name: 'Copied' });
    expect(writeText).toHaveBeenCalledTimes(1);
    expect(writeText.mock.calls[0][0]).toContain('Read result 1-1');
    expect(writeText.mock.calls[0][0]).toContain('Read result 1-2');
  },
);

it('cancels a pending copy when a new history revision arrives', async () => {
  let finish!: (activity: ReturnType<typeof page>) => void;
  const load = vi.fn(
    (_id: string, _after: number, _query: string, _signal: AbortSignal) =>
      new Promise<ReturnType<typeof page>>((resolve) => {
        finish = resolve;
      }),
  );
  const writeText = vi.fn().mockResolvedValue(undefined);
  vi.stubGlobal('navigator', { ...navigator, clipboard: { writeText } });
  const paint = (activity: ReturnType<typeof page>) => (
    <ActivityHistoryContext.Provider value={{ load }}>
      <ActivityPanel activity={activity} />
    </ActivityHistoryContext.Provider>
  );
  const view = render(paint(page()));
  fireEvent.click(screen.getByRole('button', { name: 'Copy activity' }));
  await waitFor(() => expect(load).toHaveBeenCalledTimes(1));
  const latest = page();
  latest.history!.revision++;
  view.rerender(paint(latest));
  expect(load.mock.calls[0][3].aborted).toBe(true);
  finish(page());
  await waitFor(() => expect(screen.queryByRole('button', { name: 'Copying…' })).toBeNull());
  expect(screen.getByRole('button', { name: 'Copy activity' })).toBeEnabled();
  expect(writeText).not.toHaveBeenCalled();
});

it('opens retained operations without a search or bulk-action toolbar', () => {
  render(<ActivityPanel activity={page()} />);
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  expect(screen.getByText('Operation 1')).toBeVisible();
  expect(screen.queryByRole('searchbox')).toBeNull();
  expect(
    screen.queryByRole('button', {
      name: /^(Search activity|First operations|Next operations|Show more operations|Show earlier operations|Copy all activity|Export all activity)$/,
    }),
  ).toBeNull();
});

it.each([false, true])('reads grouped histories whole without a toolbar (mixed: %s)', async (mixed) => {
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
  expect(screen.queryByRole('button', { name: 'Show more operations' })).toBeNull();
  expect(screen.queryByRole('button', { name: 'Show earlier operations' })).toBeNull();
  fireEvent.click(await screen.findByRole('button', { name: /Read ×7/ }));
  expect(document.querySelectorAll('[data-activity-row]')).toHaveLength(7);
  expect(screen.getByText('review-1-1.clj')).toBeVisible();
  expect(screen.getByText('review-3-3.clj')).toBeVisible();
  expect(screen.queryByRole('status')).toBeNull();
  expect(source.load).toHaveBeenCalledExactlyOnceWith(ids[2], 2, '', expect.any(AbortSignal));
  expect(JSON.stringify(activities)).toBe(original);
});

it('keeps grouped rows and disclosures on a failed read, then retries', async () => {
  const load = vi.fn(async (id, after, q) => groupPage(id, after, q));
  load.mockRejectedValueOnce(new Error('History unavailable'));
  render(
    <ActivityHistoryContext.Provider value={{ load }}>
      <ActivityPanel activity={ids.map((id) => groupPage(id))} />
    </ActivityHistoryContext.Provider>,
  );
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  expect(await screen.findByRole('alert')).toHaveTextContent('History unavailable');
  fireEvent.click(screen.getByRole('button', { name: /Read ×6/ }));
  expect(document.querySelectorAll('[data-activity-row]')).toHaveLength(6);
  fireEvent.click(screen.getByRole('button', { name: 'Reload operations' }));
  expect(await screen.findByText('review-3-3.clj')).toBeVisible();
  expect(screen.queryByRole('alert')).toBeNull();
  expect(screen.getByRole('button', { name: /Read ×7/ })).toHaveAttribute('aria-expanded', 'true');
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
  expect(await screen.findByRole('status')).toHaveTextContent('Loading operations');
  const settled = structuredClone(activities);
  settled[2].history!.revision = 2;
  settled[2].state = 'failed';
  settled[2].counts = { running: 0, succeeded: 1, failed: 1, cancelled: 1 };
  settled[2].rows[0].state = 'failed';
  settled[2].rows[1].state = 'cancelled';
  view.rerender(paint(settled));
  expect(load.mock.calls[0][3].aborted).toBe(true);
  expect(load).toHaveBeenCalledTimes(2);
  complete(groupPage(ids[2], 2));
  await waitFor(() => expect(screen.queryByRole('status')).toBeNull());
  expect(screen.getByRole('button', { name: 'Collapse Activity' })).toHaveTextContent(
    '7 operations · 1 failed · 1 cancelled',
  );
  expect(screen.getAllByRole('list', { name: 'Operation groups' })).toHaveLength(1);
  expect(screen.getByRole('button', { name: /Read ×7/ })).toBeVisible();
});

it('reads every retained page to the tail with no control to press', async () => {
  const load = vi.fn(async (_id: string, after: number) => page(after));
  render(
    <ActivityHistoryContext.Provider value={{ load }}>
      <ActivityPanel activity={page()} />
    </ActivityHistoryContext.Provider>,
  );
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  expect(await screen.findByText('Operation 160')).toBeVisible();
  expect(screen.getByText('Operation 1')).toBeVisible();
  expect(screen.getByText('Operation 159')).toBeVisible();
  expect(document.querySelectorAll('[data-activity-row]')).toHaveLength(160);
  expect(load.mock.calls.map(([, after]) => after)).toEqual([32, 64, 96, 128]);
  expect(screen.queryByLabelText('Show more operations')).toBeNull();
  expect(screen.queryByLabelText('Show earlier operations')).toBeNull();
  expect(screen.queryByRole('status')).toBeNull();
});

it.each(['cursor', 'id', 'after'])(
  'rejects a changed %s without dropping current rows and offers a reload',
  async (field) => {
    let broken = true;
    const load = vi.fn(async (_id: string, after: number) => {
      const result = page(after);
      if (after === 32 && broken) {
        broken = false;
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
    expect(await screen.findByRole('alert')).toHaveTextContent('Activity changed');
    expect(screen.getByText('Operation 1')).toBeVisible();
    expect(screen.queryByText('Operation 64')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Reload operations' }));
    expect(await screen.findByText('Operation 160')).toBeVisible();
    expect(screen.queryByRole('alert')).toBeNull();
  },
);

// A run that grows while its tail is read is not a changed history: pages are keyset ranges.
it('reads a live history whose revision moves while the tail loads', async () => {
  const load = vi.fn(async (_id: string, after: number) => {
    const result = page(after);
    result.history!.revision += after / 32;
    return result;
  });
  render(
    <ActivityHistoryContext.Provider value={{ load }}>
      <ActivityPanel activity={page()} />
    </ActivityHistoryContext.Provider>,
  );
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  expect(await screen.findByText('Operation 160')).toBeVisible();
  expect(screen.queryByRole('alert')).toBeNull();
});

it('aborts a pending read when unmounted', () => {
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
  view.unmount();
  expect(load.mock.calls[0][3].aborted).toBe(true);
});

it('keeps retained rows readable offline and explains an empty history', () => {
  const view = render(<ActivityPanel activity={page()} />);
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  expect(screen.getByText('Operation 1')).toBeVisible();
  expect(screen.getByText('Reconnect to load every operation.')).toBeVisible();
  expect(screen.queryByRole('button', { name: 'Show more operations' })).toBeNull();
  const empty = page();
  empty.rows = [];
  empty.history!.next_after = null;
  view.rerender(<ActivityPanel activity={empty} />);
  expect(screen.getByText('No operations available.')).toBeVisible();
  expect(screen.queryByText('Reconnect to load every operation.')).toBeNull();
});

// The band keeps what it has read; a live revision re-reads the tail behind those rows.
it('keeps every loaded operation while a live revision re-reads the tail', async () => {
  const source = { load: vi.fn(async (_id: string, after: number) => page(after)) };
  const paint = (activity: ReturnType<typeof page>) => (
    <ActivityHistoryContext.Provider value={source}>
      <ActivityPanel activity={activity} />
    </ActivityHistoryContext.Provider>
  );
  const view = render(paint(page()));
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  expect(await screen.findByText('Operation 160')).toBeVisible();
  const updated = page();
  updated.history!.revision = 2;
  view.rerender(paint(updated));
  expect(screen.getByText('Operation 1')).toBeVisible();
  expect(screen.getByText('Operation 160')).toBeVisible();
  expect(screen.queryByRole('status')).toBeNull();
  await waitFor(() => expect(source.load).toHaveBeenCalledTimes(8));
  expect(screen.getByText('Operation 160')).toBeVisible();
});

// #212: grouping is presentation, not another retained invocation or a lost outcome.
it('reads grouped invocations through tail failures and cancellations', async () => {
  const grouped = (after = 0) => {
    const result = page(after);
    result.rows = [
      {
        ...result.rows[0],
        id: `group-${after}`,
        operation: `shell-${after}`,
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
  await waitFor(() => expect(load).toHaveBeenCalledTimes(4));
  // A page that ended badly keeps its own children until the reader opens that step.
  fireEvent.click(
    document.querySelector<HTMLElement>(
      '[data-activity-row="0:group-128"] [data-disclosure-toggle]',
    )!,
  );
  expect(screen.getByText('Operation 159')).toBeVisible();
  expect(screen.getByText('Operation 160')).toBeVisible();
  expect(screen.queryByText('Operation 1')).toBeNull();
  expect(grouped(128).rows[0].children?.slice(-2).map((row) => row.state)).toEqual([
    'failed',
    'cancelled',
  ]);
});
