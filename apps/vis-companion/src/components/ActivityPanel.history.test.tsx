// @vitest-environment jsdom
// #212: complete durable history stays reachable through bounded visible pages.
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import { ActivityHistoryContext, ActivityPanel } from './ActivityPanel';
import { activityHistoryPage as page } from '../dev/activity-history';
afterEach(() => {
  cleanup();
  vi.restoreAllMocks();
});

it('reaches the tail without accumulating prior rows and searches the whole history', async () => {
  const load = vi.fn(async (_id: string, after: number, q: string) => page(after, q));
  render(
    <ActivityHistoryContext.Provider value={{ load, export: vi.fn() }}>
      <ActivityPanel activity={page()} />
    </ActivityHistoryContext.Provider>,
  );
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  for (let i = 0; i < 4; i++) {
    fireEvent.click(screen.getByRole('button', { name: 'Next operations' }));
    await screen.findByText(`Operation ${(i + 2) * 32}`);
  }
  expect(screen.getByText('Operation 160')).toBeTruthy();
  expect(screen.queryByText('Operation 1')).toBeNull();
  expect(document.querySelectorAll('[data-activity-row]')).toHaveLength(32);
  fireEvent.change(screen.getByRole('searchbox', { name: 'Search all operations' }), {
    target: { value: 'operation 159' },
  });
  fireEvent.click(screen.getByRole('button', { name: 'Search activity' }));
  await waitFor(() =>
    expect(load).toHaveBeenLastCalledWith(
      page().history!.id,
      0,
      'operation 159',
      expect.any(AbortSignal),
    ),
  );
  await waitFor(() => expect(screen.queryByText('Operation 160')).toBeNull());
  expect(screen.getByText('Operation 159')).toBeTruthy();
});

it('copies all pages, not only the visible window, and never copies a partial failure', async () => {
  const writeText = vi.fn().mockResolvedValue(undefined);
  Object.defineProperty(navigator, 'clipboard', {
    value: { writeText },
    configurable: true,
  });
  const load = vi.fn(async (_id: string, after: number) => page(after));
  render(
    <ActivityHistoryContext.Provider value={{ load, export: vi.fn() }}>
      <ActivityPanel activity={page()} />
    </ActivityHistoryContext.Provider>,
  );
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  fireEvent.click(screen.getByRole('button', { name: 'Copy all activity' }));
  await waitFor(() => expect(writeText).toHaveBeenCalledOnce());
  expect(writeText.mock.calls[0][0]).toContain('retained-160');
  expect(writeText.mock.calls[0][0].length).toBeGreaterThan(65536);
  load.mockRejectedValueOnce(new Error('Offline'));
  fireEvent.click(screen.getByRole('button', { name: 'Copy all activity' }));
  await screen.findByRole('alert');
  expect(writeText).toHaveBeenCalledOnce();
});

it('reports changed history instead of mixing revisions, and cancels copy without clipboard writes', async () => {
  const writeText = vi.fn();
  Object.defineProperty(navigator, 'clipboard', {
    value: { writeText },
    configurable: true,
  });
  const load = vi.fn(async (_id: string, after: number) => {
    const result = page(after);
    result.history!.revision = after ? 2 : 1;
    return result;
  });
  render(
    <ActivityHistoryContext.Provider value={{ load, export: vi.fn() }}>
      <ActivityPanel activity={page()} />
    </ActivityHistoryContext.Provider>,
  );
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  fireEvent.click(screen.getByRole('button', { name: 'Copy all activity' }));
  expect(await screen.findByRole('alert')).toHaveTextContent('changed while copying');
  expect(writeText).not.toHaveBeenCalled();
  let complete: (result: ReturnType<typeof page>) => void = () => {};
  load.mockImplementationOnce(
    () =>
      new Promise((resolve) => {
        complete = resolve;
      }),
  );
  fireEvent.click(screen.getByRole('button', { name: 'Copy all activity' }));
  fireEvent.click(screen.getByRole('button', { name: 'Cancel' }));
  complete(page());
  await waitFor(() => expect(screen.getByRole('status')).toHaveTextContent('Cancelled'));
  expect(writeText).not.toHaveBeenCalled();
});

it('keeps the band open but discards stale pages on a live revision or reopen', async () => {
  const source = {
    load: vi.fn(async (_id: string, after: number) => page(after)),
    export: vi.fn(),
  };
  const view = render(
    <ActivityHistoryContext.Provider value={source}>
      <ActivityPanel activity={page()} />
    </ActivityHistoryContext.Provider>,
  );
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  fireEvent.click(screen.getByRole('button', { name: 'Next operations' }));
  await screen.findByText('Operation 64');
  const updated = page();
  updated.history!.revision = 2;
  view.rerender(
    <ActivityHistoryContext.Provider value={source}>
      <ActivityPanel activity={updated} />
    </ActivityHistoryContext.Provider>,
  );
  expect(screen.getByText('Operation 1')).toBeVisible();
  expect(screen.queryByText('Operation 64')).toBeNull();
  view.unmount();
  render(
    <ActivityHistoryContext.Provider value={source}>
      <ActivityPanel activity={updated} />
    </ActivityHistoryContext.Provider>,
  );
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  fireEvent.click(screen.getByRole('button', { name: 'Next operations' }));
  expect(await screen.findByRole('alert')).toHaveTextContent('Activity changed');
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
    <ActivityHistoryContext.Provider value={{ load, export: vi.fn() }}>
      <ActivityPanel activity={grouped()} />
    </ActivityHistoryContext.Provider>,
  );
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  for (let i = 0; i < 4; i++) {
    fireEvent.click(screen.getByRole('button', { name: 'Next operations' }));
    await waitFor(() => expect(load).toHaveBeenCalledTimes(i + 1));
    await waitFor(() =>
      expect(screen.getByRole('button', { name: 'First operations' })).not.toBeDisabled(),
    );
  }
  expect(screen.getByText('Operation 159')).toBeVisible();
  expect(screen.getByText('Operation 160')).toBeVisible();
  expect(screen.queryByText('Operation 1')).toBeNull();
  expect(
    grouped(128)
      .rows[0].children?.slice(-2)
      .map((row) => row.state),
  ).toEqual(['failed', 'cancelled']);
});
