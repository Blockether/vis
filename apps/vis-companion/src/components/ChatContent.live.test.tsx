// @vitest-environment jsdom
import { fireEvent, render, waitFor } from '@testing-library/react';
import { expect, it, vi } from 'vitest';
import { LiveViewPanel } from './LiveView';
import { STORY_LIVE_VIEW } from '../dev/story-data';
import { IterationTrace } from './ChatContent';
import { activityHistoryPage } from '../dev/activity-history';
import type { GatewayClient } from '../lib/gateway';
import { liveOwnerMatches, liveRecordFromText, liveViewFromWire } from '../lib/live-view';

// Regression #222: a nested live view shares the execution frame, not another card.
it('lets the owning Activity supply the frame and background', () => {
  const view = render(<LiveViewPanel view={STORY_LIVE_VIEW} embedded />);
  const panel = view.getByText(STORY_LIVE_VIEW.title).closest('section');
  expect(panel).not.toHaveClass('border');
  expect(panel).not.toHaveClass('bg-panel');
  expect(panel?.querySelector('header')).not.toHaveClass('bg-panel-2');
});

// Regression #222: explicit ownership survives bounded Activity windows.
it('moves a live view into its exact Activity when the owner arrives', () => {
  const activity = activityHistoryPage();
  const liveView = {
    ...STORY_LIVE_VIEW,
    owner: { invocation_id: 'paged-out', activity_id: activity.history!.id },
  };
  const client = {} as GatewayClient;
  const mounted = render(
    <IterationTrace iterations={[]} liveViews={[liveView]} client={client} sid="session" whole />,
  );
  expect(mounted.getByText(liveView.title).closest('[data-execution-activity]')).toBeNull();
  mounted.rerender(
    <IterationTrace
      iterations={[{ forms: [{ source: 'monitor()', activity }] }]}
      liveViews={[liveView]}
      client={client}
      sid="session"
      whole
    />,
  );
  expect(mounted.getAllByText(liveView.title)).toHaveLength(1);
  expect(mounted.getByText(liveView.title).closest('[data-execution-activity]')).not.toBeNull();
});

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
  const firstSurface = mounted.getByText('First monitor').closest('[data-execution-activity]');
  const secondSurface = mounted.getByText('Second monitor').closest('[data-execution-activity]');
  expect(firstSurface).not.toBeNull();
  expect(secondSurface).not.toBe(firstSurface);
  expect(mounted.getByText('Other monitor').closest('[data-execution-activity]')).toBeNull();
  mounted.rerender(
    <IterationTrace
      {...props}
      liveViews={[{ ...views[0], title: 'First updated' }, ...views.slice(1)]}
    />,
  );
  expect(mounted.getByText('First updated').closest('[data-execution-activity]')).toBe(
    firstSurface,
  );
  expect(mounted.queryByText('First monitor')).toBeNull();
});

it('replaces the live view with one retained run receipt inside the same Activity', () => {
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
  expect(mounted.queryByText(STORY_LIVE_VIEW.title)).toBeNull();
  const receipts = mounted.getAllByRole('button', { name: 'Open run Monitor' });
  expect(receipts).toHaveLength(1);
  expect(receipts[0].closest('[data-execution-activity]')).not.toBeNull();
});

it('preserves interrupt actions in the nested view', async () => {
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
    expect(mounted.getByText(liveView.title).closest('[data-execution-activity]')).toBeNull();
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
      expect(mounted.getByText(liveView.title).closest('[data-execution-activity]')).not.toBeNull();
    }
  },
);
