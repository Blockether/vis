import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, waitFor, within } from 'storybook/test';

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
  title: 'Session/Live View/Panel',
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
    await userEvent.click(canvas.getByRole('button', { name: 'Refresh results' }));
    await expect(args.onActivate).toHaveBeenCalledWith('refresh');
    await userEvent.click(canvas.getByRole('button', { name: 'Details' }));
    await expect(canvas.queryByText('A started')).not.toBeInTheDocument();
    await userEvent.click(canvas.getByRole('button', { name: 'Build A logs' }));
    await expect(canvas.getByText(/A started/)).toBeVisible();
    await expect(canvas.queryByText('B started')).not.toBeInTheDocument();
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
    await userEvent.type(search, 'error');
    await userEvent.click(canvas.getByRole('button', { name: 'Search' }));
    await waitFor(() =>
      expect(canvas.getByRole('region', { name: 'Build log output' })).toHaveTextContent(
        '7: ERROR [disk]',
      ),
    );
    await userEvent.click(canvas.getByRole('button', { name: 'Clear search' }));
    await expect(canvas.getByText(/501 · Linking/)).toBeVisible();
  },
};

/** A semantic horizontal break stays within its column, including retained output. */
export const Dividers: Story = {
  args: {
    onInterrupt: undefined,
    view: {
      id: 'divider-review',
      title: 'Build review',
      seq: 1,
      nodes: [
        { id: 'summary', type: 'paragraph', text: 'Build completed. Review the results below.' },
        { id: 'results-break', type: 'divider' },
        {
          id: 'results',
          type: 'group',
          direction: 'row',
          children: [
            {
              id: 'checks',
              type: 'group',
              direction: 'column',
              children: [
                { id: 'checks-title', type: 'heading', text: 'Checks', level: 3 },
                { id: 'checks-break', type: 'divider' },
                { id: 'checks-summary', type: 'paragraph', text: 'All checks passed.' },
              ],
            },
            { id: 'report', type: 'paragraph', text: 'The report is ready for review.' },
          ],
        },
        {
          id: 'details',
          type: 'group',
          direction: 'column',
          is_collapsible: true,
          label: 'Build details',
          children: [
            { id: 'details-before', type: 'paragraph', text: 'Compiled successfully.' },
            { id: 'details-break', type: 'divider' },
            { id: 'details-after', type: 'paragraph', text: 'No warnings reported.' },
          ],
        },
      ],
    },
  },
  play: async ({ canvas }) => {
    await expect(canvas.getAllByRole('separator')).toHaveLength(2);
    const disclosure = canvas.getByRole('button', { name: 'Build details' });
    await userEvent.click(disclosure);
    const dividers = canvas.getAllByRole('separator');
    await expect(dividers).toHaveLength(3);
    await expect(canvas.getByText('No warnings reported.')).toBeVisible();
    for (const divider of dividers) {
      await expect(divider.tagName).toBe('HR');
      await expect(divider.tabIndex).toBe(-1);
      await expect(divider.textContent).toBe('');
    }
    await userEvent.click(disclosure);
    await expect(canvas.getAllByRole('separator')).toHaveLength(2);
    await expect(canvas.queryByText('No warnings reported.')).not.toBeInTheDocument();
  },
};

// THE TRANSCRIPT STATES A RUN. Embedded, the panel is ONE row that opens: the picture —
// dividers and all — is painted in the run's own screen rather than in the trace.
export const EmbeddedRow: Story = {
  args: { ...Dividers.args, embedded: true },
  play: async ({ canvas }) => {
    await expect(canvas.queryAllByRole('separator')).toHaveLength(0);
    await userEvent.click(canvas.getByRole('button', { name: /^Open run / }));
    const page = within(document.body);
    await expect(page.getAllByRole('separator')).toHaveLength(2);
    await userEvent.click(page.getByRole('button', { name: /^Close / }));
    await expect(canvas.queryAllByRole('separator')).toHaveLength(0);
  },
};
