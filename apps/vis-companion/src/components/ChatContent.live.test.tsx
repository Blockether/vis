// @vitest-environment jsdom
import { fireEvent, render, waitFor } from '@testing-library/react';
import { expect, it, vi } from 'vitest';
import { LiveViewPanel } from './LiveView';
import { STORY_LIVE_VIEW } from '../dev/story-data';
import { IterationTrace } from './ChatContent';
import { activityHistoryPage } from '../dev/activity-history';
import type { GatewayClient } from '../lib/gateway';
import { liveOwnerMatches, liveRecordFromText, liveViewFromWire } from '../lib/live-view';

// Regression #222: ACTIVITY and RUN remain independent sibling sections.
it('separates the embedded live view with horizontal rules on its shared background', () => {
  const view = render(<LiveViewPanel view={STORY_LIVE_VIEW} embedded />);
  const panel = view.getByText(STORY_LIVE_VIEW.title).closest('section');
  expect(panel).toHaveClass('border-y', 'border-dialog-hint');
  expect(panel).not.toHaveClass('border', 'px-3');
  // The run band owns no vertical gap: it continues the CODE/ACTIVITY rhythm.
  expect(panel).not.toHaveClass('mt-3', 'pt-3');
  expect(panel).not.toHaveClass('bg-panel');
  expect(panel?.querySelector('header')).not.toHaveClass('bg-panel-2');
});

// Regression #222: opening watches the current projection, and dismissal never interrupts it.
it('updates an open live screen and keeps the inline preview when closed or escaped', () => {
  const onInterrupt = vi.fn();
  const view = {
    ...STORY_LIVE_VIEW,
    nodes: [{ id: 'status', type: 'status' as const, text: 'Building', tone: 'running' as const }],
  };
  const mounted = render(<LiveViewPanel view={view} embedded onInterrupt={onInterrupt} />);
  fireEvent.click(mounted.getByRole('button', { name: `Open run ${view.title}` }));
  expect(mounted.getAllByText('Building')).toHaveLength(2);
  const updated = { ...view, nodes: [{ ...view.nodes[0], text: 'Testing' }] };
  mounted.rerender(<LiveViewPanel view={updated} embedded onInterrupt={onInterrupt} />);
  expect(mounted.queryByText('Building')).toBeNull();
  expect(mounted.getAllByText('Testing')).toHaveLength(2);
  fireEvent.click(mounted.getByRole('button', { name: `Close ${view.title}` }));
  expect(mounted.getAllByText('Testing')).toHaveLength(1);
  fireEvent.click(mounted.getByRole('button', { name: `Open run ${view.title}` }));
  fireEvent.keyDown(window, { key: 'Escape' });
  expect(mounted.queryByRole('button', { name: `Close ${view.title}` })).toBeNull();
  expect(mounted.getByText('Testing')).toBeVisible();
  expect(onInterrupt).not.toHaveBeenCalled();
  fireEvent.click(mounted.getByRole('button', { name: `Open run ${view.title}` }));
  mounted.unmount();
  expect(mounted.queryByRole('button', { name: `Close ${view.title}` })).toBeNull();
  expect(onInterrupt).not.toHaveBeenCalled();
});

it('preserves the explicit interrupt action inside an opened live screen', () => {
  const onInterrupt = vi.fn();
  const mounted = render(
    <LiveViewPanel view={STORY_LIVE_VIEW} embedded onInterrupt={onInterrupt} />,
  );
  fireEvent.click(mounted.getByRole('button', { name: `Open run ${STORY_LIVE_VIEW.title}` }));
  fireEvent.click(mounted.getAllByRole('button', { name: 'Interrupt' }).at(-1)!);
  fireEvent.change(mounted.getByRole('textbox', { name: 'Why are you stopping Fleet scan?' }), {
    target: { value: 'Wrong pool' },
  });
  fireEvent.click(mounted.getAllByRole('button', { name: 'Interrupt' }).at(-1)!);
  expect(onInterrupt).toHaveBeenCalledExactlyOnceWith('Wrong pool');
});

// Regression #222: explicit ownership survives bounded Activity windows.
it.each([
  { showCode: true, source: 'monitor()' },
  { showCode: false, source: 'monitor()' },
  { showCode: false, source: undefined },
])(
  'moves an owned live view once (Python shown: $showCode, source: $source)',
  ({ showCode, source }) => {
    const activity = activityHistoryPage();
    const liveView = {
      ...STORY_LIVE_VIEW,
      owner: { invocation_id: 'paged-out', activity_id: activity.history!.id },
    };
    const client = {} as GatewayClient;
    const mounted = render(
      <IterationTrace
        iterations={[]}
        liveViews={[liveView]}
        client={client}
        sid="session"
        showCode={showCode}
        whole
      />,
    );
    expect(mounted.getByText(liveView.title).closest('[data-execution-group]')).toBeNull();
    expect(mounted.getByText(liveView.title).closest('section')).toHaveClass('border-y');
    mounted.rerender(
      <IterationTrace
        iterations={[{ forms: [{ source, activity }] }]}
        liveViews={[liveView]}
        client={client}
        sid="session"
        showCode={showCode}
        whole
      />,
    );
    expect(mounted.getAllByText(liveView.title)).toHaveLength(1);
    const title = mounted.getByText(liveView.title);
    const group = title.closest('[data-execution-group]');
    expect(group).toHaveClass('bg-code');
    expect(group).not.toHaveClass('border');
    expect(title.closest('.border-y')).toBe(title.closest('section'));
  },
);

it('keeps concurrent owned views and unmatched views separate, including streamed updates', () => {
  const first = activityHistoryPage();
  const second = { ...activityHistoryPage(), history: { ...first.history!, id: 'second-history' } };
  const views = [
    {
      ...STORY_LIVE_VIEW,
      id: 'first',
      title: 'First monitor',
      owner: { invocation_id: 'absent', activity_id: first.history!.id },
    },
    {
      ...STORY_LIVE_VIEW,
      id: 'second',
      title: 'Second monitor',
      owner: { invocation_id: 'absent', activity_id: second.history!.id },
    },
    {
      ...STORY_LIVE_VIEW,
      id: 'other',
      title: 'Other monitor',
      owner: { invocation_id: 'unknown' },
    },
  ];
  const props = {
    iterations: [
      {
        forms: [
          { source: 'one()', activity: first, comment: 'First execution' },
          { source: 'two()', activity: second, comment: 'Second execution' },
        ],
      },
    ],
    liveViews: views,
    client: {} as GatewayClient,
    sid: 'session',
    whole: true,
  };
  const mounted = render(<IterationTrace {...props} />);
  const firstSurface = mounted.getByText('First monitor').closest('[data-execution-group]');
  const secondSurface = mounted.getByText('Second monitor').closest('[data-execution-group]');
  expect(firstSurface).not.toBeNull();
  expect(secondSurface).not.toBe(firstSurface);
  expect(mounted.getByText('Other monitor').closest('[data-execution-group]')).toBeNull();
  mounted.rerender(
    <IterationTrace
      {...props}
      liveViews={[{ ...views[0], title: 'First updated' }, ...views.slice(1)]}
    />,
  );
  expect(mounted.getByText('First updated').closest('[data-execution-group]')).toBe(firstSurface);
  expect(mounted.queryByText('First monitor')).toBeNull();
});

it('replaces the live view with one retained run receipt beside the same Activity', () => {
  const activity = activityHistoryPage();
  const owner = { invocation_id: 'paged-out', activity_id: activity.history!.id };
  const props = { client: {} as GatewayClient, sid: 'session', whole: true };
  const iteration = { id: 'iteration', forms: [{ source: 'monitor()', activity }] };
  const mounted = render(
    <IterationTrace
      {...props}
      iterations={[iteration]}
      liveViews={[{ ...STORY_LIVE_VIEW, owner }]}
    />,
  );
  // Regression #222: settlement removes the optional live screen, not just its row.
  fireEvent.click(mounted.getByRole('button', { name: `Open run ${STORY_LIVE_VIEW.title}` }));
  expect(mounted.getByRole('button', { name: `Close ${STORY_LIVE_VIEW.title}` })).toBeVisible();
  mounted.rerender(
    <IterationTrace
      {...props}
      iterations={[
        {
          ...iteration,
          attachments: [
            {
              index: 0,
              iteration_id: 'iteration',
              media_type: 'application/vnd.vis.live+ndjson',
              filename: 'Monitor.live.ndjson',
              owner,
            },
          ],
        },
      ]}
      liveViews={[]}
    />,
  );
  expect(mounted.queryByRole('button', { name: `Close ${STORY_LIVE_VIEW.title}` })).toBeNull();
  expect(mounted.queryByRole('button', { name: 'Interrupt' })).toBeNull();
  expect(mounted.queryByText(STORY_LIVE_VIEW.title)).toBeNull();
  const receipts = mounted.getAllByRole('button', { name: 'Open run Monitor' });
  expect(receipts).toHaveLength(1);
  const receipt = receipts[0];
  const group = receipt.closest('[data-execution-group]');
  expect(group).not.toBeNull();
  expect(receipt.closest('[data-execution-activity]')).toBeNull();
  expect(group).not.toHaveClass('border');
  expect(receipt.closest('.border')).toBeNull();
  expect(receipt.closest('.bg-input')).toBeNull();
  fireEvent.click(mounted.getByRole('button', { name: 'Expand Activity' }));
  fireEvent.click(mounted.getByRole('button', { name: 'Collapse Activity' }));
  expect(receipt).toBeVisible();
});

it('preserves interrupt actions in the sibling run', async () => {
  const activity = activityHistoryPage();
  const viewAction = vi.fn().mockResolvedValue({ is_accepted: true });
  const mounted = render(
    <IterationTrace
      iterations={[{ forms: [{ source: 'monitor()', activity }] }]}
      liveViews={[
        {
          ...STORY_LIVE_VIEW,
          owner: { invocation_id: 'absent', activity_id: activity.history!.id },
        },
      ]}
      client={{ viewAction } as unknown as GatewayClient}
      sid="session"
      whole
    />,
  );
  fireEvent.click(mounted.getByRole('button', { name: 'Interrupt' }));
  fireEvent.change(mounted.getByRole('textbox', { name: 'Why are you stopping Fleet scan?' }), {
    target: { value: 'Wrong pool' },
  });
  fireEvent.click(mounted.getByRole('button', { name: 'Interrupt' }));
  await waitFor(() =>
    expect(viewAction).toHaveBeenCalledWith('session', STORY_LIVE_VIEW.id, {
      action: 'interrupt',
      note: 'Wrong pool',
    }),
  );
});

it('matches exact nested invocation ids without guessing from title or source', () => {
  const activity = activityHistoryPage();
  const child = activity.rows[0];
  const nested = { ...activity, rows: [{ ...child, id: 'parent', children: [child] }] };
  expect(liveOwnerMatches({ invocation_id: child.id }, nested)).toBe(true);
  expect(liveOwnerMatches({ invocation_id: 'missing' }, nested)).toBe(false);
  expect(liveOwnerMatches(undefined, nested)).toBe(false);
});

it('retains host ownership through wire parsing and a sealed record', () => {
  const owner = {
    invocation_id: '22222222-2222-2222-2222-222222222222',
    activity_id: '11111111-1111-1111-1111-111111111111',
  };
  const view = { ...STORY_LIVE_VIEW, owner };
  expect(liveViewFromWire(view)?.owner).toEqual(owner);
  const record = liveRecordFromText(
    JSON.stringify({ kind: 'open', view }) +
      '\n' +
      JSON.stringify({
        kind: 'close',
        result: { reason: 'completed', view: { ...view, seq: 2 } },
      }),
  );
  expect(record?.view.owner).toEqual(owner);
});

// Regression #222: a folded or deliberately hidden owner cannot swallow the live view.
it.each(['hidden', 'ramped'])(
  'keeps the %s owner view in the fallback until renderable',
  (mode) => {
    const activity = activityHistoryPage();
    const liveView = {
      ...STORY_LIVE_VIEW,
      owner: { invocation_id: 'absent', activity_id: activity.history!.id },
    };
    const iterations = [
      {
        assistant_prose: 'Owner execution',
        forms: [{ source: 'monitor()', activity, silent: mode === 'hidden' }],
      },
      ...Array.from({ length: 40 }, (_, index) => ({
        assistant_prose: `Later step ${index}`,
        forms: [{ source: `step(${index})` }],
      })),
    ];
    const mounted = render(
      <IterationTrace
        iterations={iterations}
        liveViews={[liveView]}
        client={{} as GatewayClient}
        sid="session"
      />,
    );
    expect(mounted.getAllByText(liveView.title)).toHaveLength(1);
    expect(mounted.getByText(liveView.title).closest('[data-execution-group]')).toBeNull();
    if (mode === 'ramped') {
      mounted.rerender(
        <IterationTrace
          iterations={iterations}
          liveViews={[liveView]}
          client={{} as GatewayClient}
          sid="session"
          whole
        />,
      );
      expect(mounted.getAllByText(liveView.title)).toHaveLength(1);
      expect(mounted.getByText(liveView.title).closest('[data-execution-group]')).not.toBeNull();
    }
  },
);

// Regression #222: association must not add hierarchy or another horizontal inset.
it('renders RUN beside Activity and opens it without folding the preview', () => {
  const activity = activityHistoryPage();
  const mounted = render(
    <IterationTrace
      iterations={[{ forms: [{ source: 'monitor()', activity }] }]}
      liveViews={[
        {
          ...STORY_LIVE_VIEW,
          owner: { invocation_id: 'absent', activity_id: activity.history!.id },
        },
      ]}
      client={{} as GatewayClient}
      sid="session"
      whole
    />,
  );
  const run = mounted.getByText(STORY_LIVE_VIEW.title).closest('section');
  const activitySection = mounted.getByText('ACTIVITY').closest('[data-execution-activity]');
  expect(run?.parentElement).toBe(activitySection?.parentElement);
  expect(run?.closest('[data-execution-activity]')).toBeNull();
  expect(run?.querySelector('header')).toHaveClass('px-(--live-view-inset)');
  expect(mounted.getByText('RUN')).toBeVisible();
  fireEvent.click(mounted.getByRole('button', { name: 'Expand Activity' }));
  fireEvent.click(mounted.getByRole('button', { name: 'Collapse Activity' }));
  expect(mounted.getByText(STORY_LIVE_VIEW.title)).toBeVisible();
  const launch = mounted.getByRole('button', {
    name: `Open run ${STORY_LIVE_VIEW.title}`,
  });
  expect(launch).not.toHaveAttribute('aria-expanded');
  expect(launch.querySelector('svg')).toBeNull();
  fireEvent.click(launch);
  const close = mounted.getByRole('button', {
    name: `Close ${STORY_LIVE_VIEW.title}`,
  });
  expect(close).toBeVisible();
  fireEvent.click(close);
  expect(mounted.queryByRole('button', { name: `Close ${STORY_LIVE_VIEW.title}` })).toBeNull();
  expect(mounted.getByText(STORY_LIVE_VIEW.title)).toBeVisible();
});

// Regression #222: one execution may own several independent RUN sections.
it('keeps multiple views as siblings even when their activity rows are absent', () => {
  const original = activityHistoryPage();
  const activity = {
    ...original,
    rows: [],
    counts: { running: 0, succeeded: 0, failed: 0, cancelled: 0 },
  };
  const owner = { invocation_id: 'paged-out', activity_id: activity.history!.id };
  const mounted = render(
    <IterationTrace
      iterations={[{ forms: [{ source: 'monitor()', activity }] }]}
      liveViews={[
        { ...STORY_LIVE_VIEW, owner },
        { ...STORY_LIVE_VIEW, id: 'second', title: 'Second monitor', owner },
      ]}
      client={{} as GatewayClient}
      sid="session"
      whole
    />,
  );
  const first = mounted.getByText(STORY_LIVE_VIEW.title).closest('section');
  const second = mounted.getByText('Second monitor').closest('section');
  expect(first?.parentElement).toBe(second?.parentElement);
  expect(first?.parentElement).toHaveAttribute('data-execution-group');
  expect(first?.parentElement).not.toHaveClass('border');
  expect(first?.closest('.border-y')).toBe(first);
  expect(second?.closest('.border-y')).toBe(second);
  expect(mounted.getAllByText('RUN')).toHaveLength(2);
});

it('keeps only the newest owned receipt version in the shared surface', () => {
  const activity = activityHistoryPage();
  const owner = { invocation_id: 'paged-out', activity_id: activity.history!.id };
  const mounted = render(
    <IterationTrace
      iterations={[
        {
          id: 'iteration',
          forms: [{ source: 'monitor()', activity }],
          attachments: [0, 1].map((index) => ({
            index,
            iteration_id: 'iteration',
            filename: 'Monitor.live.ndjson',
            media_type: 'application/vnd.vis.live+ndjson',
            owner,
          })),
        },
      ]}
      liveViews={[]}
      client={{} as GatewayClient}
      sid="session"
      whole
    />,
  );
  expect(mounted.getAllByRole('button', { name: 'Open run Monitor' })).toHaveLength(1);
});
