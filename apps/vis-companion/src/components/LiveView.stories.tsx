import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent } from 'storybook/test';

import { STORY_LIVE_VIEW } from '../dev/story-data';
import { LiveViewPanel } from './LiveView';

/**
 * A RUN, WATCHED WHILE IT IS BEING WRITTEN.
 *
 * The panel is pure — every node it paints arrived as a prop — so the gallery can
 * draw it from the ENGINE's own fixture (`lib/live-view.fixture.json`, the
 * projection `gateway/human_input_test.clj` pins) instead of a client. Nothing is
 * fetched, and a wire change fails these frames first.
 *
 * The states worth looking at are the ones a run passes through: writing, stopped
 * but not yet answered, over, and refused.
 */
const meta = {
  title: 'Components/Live view',
  component: LiveViewPanel,
  parameters: { layout: 'padded' },
  args: { view: STORY_LIVE_VIEW, onInterrupt: fn(), onSelect: () => {} },
} satisfies Meta<typeof LiveViewPanel>;

export default meta;

type Story = StoryObj<typeof meta>;

/** In flight: the stop is ARMED before it is sent, and the note travels with it. */
export const Running: Story = {
  play: async ({ args, canvas }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Interrupt' }));
    const reason = await canvas.findByRole('textbox', {
      name: 'Why are you stopping Fleet scan?',
    });
    await userEvent.type(reason, 'wrong subnet');
    await expect(reason).toHaveValue('wrong subnet');
    await userEvent.click(canvas.getByRole('button', { name: 'Interrupt' }));
    await expect(args.onInterrupt).toHaveBeenCalledWith('wrong subnet');
    await expect(canvas.queryByRole('textbox')).not.toBeInTheDocument();
  },
};

/** The stop was pressed and the engine has not answered yet. */
export const Interrupting: Story = {
  args: { isInterrupting: true },
};

/**
 * The run is OVER and this is its record: nothing spins, and the section stops
 * announcing itself to a screen reader as a picture that can still change.
 */
export const Settled: Story = {
  args: { isSettled: true },
};

/** The patch could not be applied: the picture stays, the reason is said once. */
export const Failed: Story = {
  args: { error: 'The run ended before this view was closed.' },
};

// Phone regression: padding on the table's enclosing node shifted the first job
// down and left extra space beneath the last job, outside their row separators.
export const FinishedJobs: Story = {
  args: {
    isSettled: true,
    onInterrupt: undefined,
    view: {
      ...STORY_LIVE_VIEW,
      title: 'Finished jobs',
      description: '2 jobs completed',
      nodes: [
        {
          id: 'jobs',
          type: 'table',
          columns: [
            { id: 'job', label: 'Job', align: 'left' },
            { id: 'result', label: 'Result', align: 'left' },
          ],
          rows: [
            { id: 'test', cells: ['Typecheck and test', 'success · 17s'], tone: 'ok' },
            { id: 'deploy', cells: ['Deploy to Cloudflare', 'success · 19s'], tone: 'ok' },
          ],
          max_rows: 100,
          order: 'insertion',
          is_selectable: true,
          selected_ids: [],
        },
      ],
    },
    onSelect: fn(),
  },
  play: async ({ canvas, args }) => {
    const first = canvas.getByRole('button', { name: 'Select Typecheck and test' });
    const last = canvas.getByRole('button', { name: 'Select Deploy to Cloudflare' });
    const table = first.closest('table')!;
    const node = table.closest('li')!;
    const nodeStyle = getComputedStyle(node);
    await expect(nodeStyle.paddingTop).toBe('0px');
    await expect(nodeStyle.paddingBottom).toBe('0px');
    await expect(first.getBoundingClientRect().height).toBe(last.getBoundingClientRect().height);
    await expect(first.getBoundingClientRect().height).toBeGreaterThanOrEqual(44);
    await expect(table.getBoundingClientRect().top).toBe(node.getBoundingClientRect().top);
    await expect(
      node.getBoundingClientRect().bottom - table.getBoundingClientRect().bottom,
    ).toBeLessThanOrEqual(1);
    await userEvent.click(last);
    await expect(args.onSelect).toHaveBeenCalledWith('jobs', ['deploy']);
  },
};

export const LabelledJobs: Story = {
  args: {
    ...FinishedJobs.args,
    view: {
      ...FinishedJobs.args!.view!,
      nodes: FinishedJobs.args!.view!.nodes.map((node) => ({
        ...node,
        label: 'Jobs',
        is_selectable: false,
      })),
    },
  },
  play: async ({ canvas }) => {
    const table = canvas.getByRole('table');
    const node = table.closest('li')!;
    await expect(canvas.getByText('Jobs')).toBeVisible();
    await expect(getComputedStyle(node).paddingTop).toBe('10px');
    await expect(getComputedStyle(node).paddingBottom).toBe('0px');
    await expect(
      node.getBoundingClientRect().bottom - table.getBoundingClientRect().bottom,
    ).toBeLessThanOrEqual(1);
    await expect(canvas.queryByRole('button', { name: /Select/ })).not.toBeInTheDocument();
  },
};
