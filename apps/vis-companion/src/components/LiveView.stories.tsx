import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent } from 'storybook/test';

import { STORY_LIVE_VIEW, STORY_LIVE_PRIMITIVES } from '../dev/story-data';
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
    const interrupt = canvas.getByRole('button', { name: 'Interrupt' });
    const pointer = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    await expect(interrupt.getBoundingClientRect().height).toBe(pointer ? 28 : 32);
    if (!pointer) {
      const box = interrupt.getBoundingClientRect();
      await expect(
        parseFloat(getComputedStyle(interrupt, '::after').height),
      ).toBeGreaterThanOrEqual(44);
      await expect(
        interrupt.contains(document.elementFromPoint(box.left + box.width / 2, box.top - 5)),
      ).toBe(true);
    }
    await userEvent.click(interrupt);
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
  args: { isSettled: true, onSelect: undefined },
};

/** The patch could not be applied: the picture stays, the reason is said once. */
export const Failed: Story = {
  args: { error: 'The run ended before this view was closed.' },
};

// Phone regression: padding on the table's enclosing node shifted the first job
// down and left extra space beneath the last job, outside their row separators.
export const FinishedJobs: Story = {
  args: {
    onInterrupt: undefined,
    isSettled: false,
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
            {
              id: 'test',
              cells: ['Typecheck and test', 'success · 17s'],
              tone: 'ok',
            },
            {
              id: 'deploy',
              cells: ['Deploy to Cloudflare', 'success · 19s'],
              tone: 'ok',
            },
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
    const first = canvas.getByRole('button', {
      name: 'Select Typecheck and test',
    });
    const last = canvas.getByRole('button', {
      name: 'Select Deploy to Cloudflare',
    });
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

/** All supported nodes, heading levels and spinner variants from the shared contract fixture. */
export const AllPrimitives: Story = {
  args: { view: STORY_LIVE_PRIMITIVES, onActivate: fn() },
  play: async ({ canvas, args }) => {
    for (let level = 1; level <= 6; level++) {
      await expect(
        canvas.getByRole('heading', { level, name: `Heading level ${level}` }),
      ).toBeVisible();
    }
    await expect(canvas.getByRole('button', { name: 'Unavailable action' })).toBeDisabled();
    const pointer = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    const refresh = canvas.getByRole('button', { name: 'Refresh results' });
    await expect(refresh.getBoundingClientRect().height).toBe(pointer ? 28 : 32);
    if (!pointer) {
      await expect(parseFloat(getComputedStyle(refresh, '::after').height)).toBeGreaterThanOrEqual(
        44,
      );
      const box = refresh.getBoundingClientRect();
      await expect(
        refresh.contains(document.elementFromPoint(box.left + box.width / 2, box.top - 5)),
      ).toBe(true);
    }
    await expect(
      canvas.getByRole('button', { name: 'Details' }).getBoundingClientRect().height,
    ).toBeGreaterThanOrEqual(pointer ? 28 : 44);
    await userEvent.click(canvas.getByRole('button', { name: 'Refresh results' }));
    await expect(args.onActivate).toHaveBeenCalledWith('refresh');
    await userEvent.click(canvas.getByRole('button', { name: 'Details' }));
    await expect(canvas.queryByText('A started')).not.toBeInTheDocument();
    await userEvent.click(canvas.getByRole('button', { name: 'Build A logs' }));
    await expect(canvas.getByText(/A started/)).toBeVisible();
    await expect(canvas.queryByText('B started')).not.toBeInTheDocument();
  },
};

export const AllPrimitivesReceipt: Story = {
  args: {
    view: STORY_LIVE_PRIMITIVES,
    isSettled: true,
    onActivate: fn(),
    onSelect: undefined,
  },
  play: async ({ canvas }) => {
    await expect(canvas.queryByRole('button', { name: 'Interrupt' })).not.toBeInTheDocument();
    await expect(
      canvas.queryByRole('button', { name: 'Select Tests passed' }),
    ).not.toBeInTheDocument();
    await expect(canvas.getByRole('button', { name: 'Refresh results' })).toBeDisabled();
    await expect(canvas.getByRole('button', { name: 'Details' })).toHaveAttribute(
      'aria-expanded',
      'false',
    );
  },
};

export const SearchableLog: Story = {
  args: {
    view: {
      id: 'search-fixture',
      title: 'Build output',
      seq: 1,
      nodes: [
        {
          id: 'log',
          type: 'log',
          label: 'Build log',
          default_expanded: true,
          window_lines: 3,
          total_lines: 503,
          lines: ['501 · Linking', '502 · Build finished', '503 · Saved report'],
        },
      ],
    },
    load: async (nodeId, from, limit, query = '') => {
      const lines = [
        'ERROR [disk] · Could not write cache',
        'Cache directory created',
        'error · Retry succeeded',
      ];
      const matches = lines.flatMap((line, index) =>
        line.toLowerCase().includes(query.toLowerCase()) ? [{ line, number: index + 7 }] : [],
      );
      const page = matches.slice(from, from + limit);
      return {
        node_id: nodeId,
        from,
        total: 503,
        matched: matches.length,
        lines: page.map((item) => item.line),
        line_numbers: page.map((item) => item.number),
      };
    },
  },
  play: async ({ canvas }) => {
    const search = canvas.getByRole('searchbox', { name: 'Search Build log' });
    const pointer = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    const box = search.getBoundingClientRect();
    await expect(box.height).toBe(
      canvas.getByRole('button', { name: 'Search' }).getBoundingClientRect().height,
    );
    // The shared compact face keeps its larger touch target outside the native field.
    const reach =
      box.height +
      (pointer
        ? 0
        : parseFloat(getComputedStyle(search.parentElement!, '::before').height) +
          parseFloat(getComputedStyle(search.parentElement!, '::after').height));
    await expect(reach).toBeGreaterThanOrEqual(pointer ? 28 : 44);
    await userEvent.type(search, 'error');
    await userEvent.click(canvas.getByRole('button', { name: 'Search' }));
    await expect(await canvas.findByText(/2 matches.*503 recorded lines/)).toBeVisible();
    await expect(canvas.getByRole('region', { name: 'Build log output' })).toHaveTextContent(
      '7: ERROR [disk]',
    );
    await userEvent.click(canvas.getByRole('button', { name: 'Clear search' }));
    await expect(canvas.getByText(/501 · Linking/)).toBeVisible();
    await userEvent.type(search, 'missing');
    await userEvent.click(canvas.getByRole('button', { name: 'Search' }));
    await expect(await canvas.findByText('No matching lines.')).toBeVisible();
  },
};

export const SearchableLogReceipt: Story = {
  args: { ...SearchableLog.args, isSettled: true },
  play: async ({ canvas }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Build log' }));
    await userEvent.type(canvas.getByRole('searchbox'), '[[DISK]');
    await userEvent.click(canvas.getByRole('button', { name: 'Search' }));
    await expect(await canvas.findByText(/1 matches.*503 recorded lines/)).toBeVisible();
    await expect(canvas.queryByRole('button', { name: 'Interrupt' })).not.toBeInTheDocument();
  },
};

/** #209: semantic colors on literal output, with no loss of readable severity words. */
export const StyledLog: Story = {
  args: {
    view: {
      id: 'styled-log',
      title: 'Build output',
      seq: 1,
      nodes: [
        { id: 'status', type: 'status', text: 'Build failed', tone: 'error' },
        {
          id: 'log',
          type: 'log',
          label: 'Build log',
          default_expanded: true,
          window_lines: 200,
          total_lines: 6,
          lines: [
            '10:42:00 INFO $ npm test',
            '10:42:01 OK dependencies ready',
            '10:42:02 WARN cache unavailable',
            '10:42:03 ERROR compiler failed',
            '    at compile (src/build.ts:42:7)',
            '<script>literal</script> \\u001b[2J',
          ],
          line_tones: ['running', 'ok', 'warn', 'error', null, null],
        },
      ],
    },
  },
  play: async ({ canvas }) => {
    await expect(canvas.getByTitle('Severity: error')).toHaveTextContent('ERROR compiler failed');
    await expect(canvas.getByRole('region', { name: 'Build log output' })).toHaveTextContent(
      '<script>literal</script>',
    );
  },
};
