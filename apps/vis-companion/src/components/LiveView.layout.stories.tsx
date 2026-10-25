import type { Meta, StoryObj } from '@storybook/react-vite';
import { useState } from 'react';
import { expect, fn, userEvent, waitFor, within } from 'storybook/test';

import { LIVE_LAYOUT_VIEW, liveLargeView } from '../dev/live-layout-data';
import { applyLivePatch, liveViewFromWire, type LiveView } from '../lib/live-view';
import { VIEW_PATCH_EVENT } from '../lib/view';
import { LiveViewPanel } from './LiveView';
import { Button } from './ui';

/**
 * Every layout direction of a live view: a capped `grid` of cards, a `wrap` row
 * of buttons, a `row` of inputs with a submit button and a `column` of results.
 * The large view measures render cost with many rows, lines and cards.
 */
function view(wire: Record<string, unknown>): LiveView {
  const parsed = liveViewFromWire(wire);
  if (!parsed) throw new Error('the layout fixture must be paintable');
  return parsed;
}

const meta = {
  title: 'Live View/Layouts',
  component: LiveViewPanel,
  parameters: { layout: 'padded' },
  args: {
    view: view(LIVE_LAYOUT_VIEW),
    onActivate: fn(),
    onChange: fn(),
    onSubmit: fn(),
    onSelect: fn(),
  },
} satisfies Meta<typeof LiveViewPanel>;

export default meta;

type Story = StoryObj<typeof meta>;

/** jsdom lays nothing out, so layout checks run only in a browser Storybook. */
function hasLayout(element: Element): boolean {
  return element.getBoundingClientRect().width > 0;
}

/** How many columns the children of a layout stand in, read from where they start. */
function tracks(element: Element): number {
  return new Set(Array.from(element.children, (child) => (child as HTMLElement).offsetLeft)).size;
}

/** A wide panel: the grid stops at three columns, and every control answers. */
export const Dashboard: Story = {
  decorators: [
    (Story) => (
      <div className="w-[64rem]">
        <Story />
      </div>
    ),
  ],
  play: async ({ args, canvasElement }) => {
    const canvas = within(canvasElement);
    if (hasLayout(canvasElement)) {
      const grid = canvasElement.querySelector('[data-view-layout="grid"]')!;
      await expect(tracks(grid)).toBe(3);
      const wrap = canvasElement.querySelector<HTMLElement>('[data-view-layout="wrap"]')!;
      const buttons = Array.from(wrap.children) as HTMLElement[];
      await expect(new Set(buttons.map((child) => child.offsetTop)).size).toBe(1);
      await expect(buttons[0].offsetWidth).toBeLessThan(wrap.offsetWidth / 2);
    }

    await userEvent.click(canvas.getByRole('button', { name: 'Approve release' }));
    await expect(args.onActivate).toHaveBeenCalledWith('approve');
    await userEvent.click(canvas.getByRole('button', { name: /^Worker/ }));
    await expect(args.onActivate).toHaveBeenCalledWith('svc-worker');

    const window = canvas.getByRole('textbox', { name: 'Window' });
    await userEvent.clear(window);
    await userEvent.type(window, '19:30');
    await userEvent.click(canvas.getByRole('button', { name: 'Schedule' }));
    await expect(args.onSubmit).toHaveBeenCalledWith(
      'schedule',
      expect.objectContaining({ region: 'eu', window: '19:30' }),
    );
  },
};

/** A phone: the grid and the row stack, and the wrap row keeps natural widths. */
export const Phone: Story = {
  decorators: [
    (Story) => (
      <div className="max-w-[358px]">
        <Story />
      </div>
    ),
  ],
  play: async ({ canvasElement }) => {
    if (!hasLayout(canvasElement)) return;
    await expect(tracks(canvasElement.querySelector('[data-view-layout="grid"]')!)).toBe(1);
    await expect(tracks(canvasElement.querySelector('[data-view-layout="row"]')!)).toBe(1);
    const wrap = canvasElement.querySelector<HTMLElement>('[data-view-layout="wrap"]')!;
    for (const child of Array.from(wrap.children) as HTMLElement[]) {
      await expect(child.offsetWidth).toBeLessThanOrEqual(wrap.offsetWidth);
    }
    await expect(canvasElement.scrollWidth).toBeLessThanOrEqual(canvasElement.clientWidth);
  },
};

/** The record of the board: nothing answers once the run is over. */
export const DashboardReceipt: Story = {
  args: { isSettled: true },
  play: async ({ canvasElement }) => {
    const approve = within(canvasElement).getByRole('button', { name: 'Approve release' });
    await expect(approve).toBeDisabled();
  },
};

/** 500 table rows, 2000 log lines and 48 cards in an open grid. */
export const LargeView: Story = {
  args: { view: view(liveLargeView(500, 2000, 48)) },
  decorators: Dashboard.decorators,
  // Axe over thousands of rows takes most of a minute in jsdom. The Dashboard
  // and Phone stories check the same nodes.
  parameters: { a11y: { test: 'off' } },
  play: async ({ canvasElement }) => {
    const grid = canvasElement.querySelector('[data-view-layout="grid"]')!;
    await expect(grid.children).toHaveLength(48);
    // Rows arrive in chunks: the first ones at once, the rest in transition steps.
    await waitFor(() =>
      expect(canvasElement.querySelectorAll('tbody tr').length).toBeGreaterThanOrEqual(500),
    );
    if (!hasLayout(canvasElement)) return;
    await expect(tracks(grid)).toBeGreaterThan(1);
  },
};

/**
 * The large view under live patches, applied the way the session applies them.
 * Each press sends one patch: a status change or one appended table row.
 */
// Stable handlers, as an app passes them, so a patch repaints only the rows it touched.
const onUpdatesActivate = fn();
const onUpdatesSelect = fn();

function LiveUpdates() {
  const [current, setCurrent] = useState(() => view(liveLargeView(500, 2000, 48)));
  const send = (ops: unknown[]) =>
    setCurrent((was) =>
      applyLivePatch(was, {
        type: VIEW_PATCH_EVENT,
        kind: 'live',
        view_id: was.id,
        first_seq: was.seq + 1,
        patch: { view_id: was.id, seq: was.seq + 1, ops },
      }),
    );
  return (
    <div className="w-[64rem] space-y-3">
      <div className="flex gap-2">
        <Button
          variant="secondary"
          onClick={() =>
            send([{ op: 'set', node_id: 'now', text: `Patch ${current.seq + 1}`, tone: 'running' }])
          }
        >
          Change status
        </Button>
        <Button
          variant="secondary"
          onClick={() =>
            send([
              {
                op: 'append',
                node_id: 'hosts',
                rows: [{ id: `new-${current.seq}`, cells: ['new.example.com', 'Up', '1 ms'], tone: 'ok' }],
              },
            ])
          }
        >
          Append row
        </Button>
      </div>
      <LiveViewPanel view={current} onActivate={onUpdatesActivate} onSelect={onUpdatesSelect} />
    </div>
  );
}

export const LargeViewUpdates: Story = {
  render: () => <LiveUpdates />,
  parameters: LargeView.parameters,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Change status' }));
    await expect(canvas.getByText('Patch 2')).toBeInTheDocument();
    await userEvent.click(canvas.getByRole('button', { name: 'Append row' }));
    await waitFor(() =>
      expect(canvasElement.querySelectorAll('tbody tr').length).toBeGreaterThanOrEqual(501),
    );
  },
};
