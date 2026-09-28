import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

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
          groups: [],
        },
      ],
    },
    onSelect: fn(),
  },
  play: async ({ canvas, args }) => {
    const last = canvas.getByRole('button', {
      name: 'Select Deploy to Cloudflare',
    });
    await userEvent.click(last);
    await expect(args.onSelect).toHaveBeenCalledWith('jobs', ['deploy']);
  },
};

export const LabelledJobs: Story = {
  args: {
    ...FinishedJobs.args,
    view: {
      ...FinishedJobs.args!.view!,
      nodes: FinishedJobs.args!.view!.nodes.map((node) =>
        node.type === 'table' ? { ...node, label: 'Jobs', is_selectable: false } : node,
      ),
    },
  },
  play: async ({ canvas }) => {
    await expect(canvas.getByText('Jobs')).toBeVisible();
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

/** #219: every expanded disclosure adds one inset, including nested log controls. */
export const NestedDisclosures: Story = {
  args: {
    onInterrupt: undefined,
    view: {
      id: 'nested-disclosures',
      title: 'Worker inspection',
      seq: 1,
      nodes: [
        {
          id: 'pool',
          type: 'group',
          label: 'Observed pool state',
          direction: 'column',
          is_collapsible: true,
          default_expanded: true,
          fields: [
            { id: 'summary', type: 'paragraph', text: '2 workers observed' },
            {
              id: 'log',
              type: 'log',
              label: 'Worker output',
              default_expanded: true,
              window_lines: 200,
              total_lines: 2,
              lines: ['monitor active=2/2', 'worker=running'],
            },
          ],
        },
        { id: 'sibling', type: 'paragraph', text: 'Runtime not checked' },
      ],
    },
  },
  play: async ({ canvas }) => {
    const group = canvas.getByRole('button', { name: 'Observed pool state' });
    if (group.getAttribute('aria-expanded') !== 'true') await userEvent.click(group);
    const log = canvas.getByRole('button', { name: 'Worker output' });
    if (log.getAttribute('aria-expanded') !== 'true') await userEvent.click(log);
    log.focus();
    await userEvent.keyboard('{Enter}');
    await expect(canvas.queryByRole('searchbox')).not.toBeInTheDocument();
    await userEvent.keyboard('{Enter}');
    await expect(canvas.getByRole('region', { name: 'Worker output output' })).toHaveTextContent(
      'monitor active=2/2',
    );
    await userEvent.click(group);
    await expect(canvas.queryByRole('button', { name: 'Worker output' })).not.toBeInTheDocument();
    await userEvent.click(group);
    await expect(canvas.getByRole('region', { name: 'Worker output output' })).toHaveTextContent(
      'worker=running',
    );
  },
};

export const NestedDisclosuresReceipt: Story = {
  args: { ...NestedDisclosures.args, isSettled: true },
  play: NestedDisclosures.play,
};

/** #221: a result set has one frame and uses available width, not viewport width. */
export const LinkResults: Story = {
  args: {
    onInterrupt: undefined,
    view: {
      id: 'build-links',
      title: 'Build results',
      seq: 1,
      nodes: [
        {
          id: 'links',
          type: 'link',
          label: 'Build references',
          links: [
            {
              id: 'console',
              label: 'Full console · router #4344',
              target_kind: 'url',
              target: 'https://gateway.example.com/build/4344',
            },
            {
              id: 'review',
              label: 'Review 24206 · PS 9 · SUCCESS',
              target_kind: 'url',
              target: 'https://gateway.example.com/review/24206',
            },
            {
              id: 'checks',
              label: 'Checks · gateway #812',
              target_kind: 'url',
              target: 'https://gateway.example.com/build/812',
            },
            {
              id: 'report',
              label: 'Build report',
              target_kind: 'path',
              target: '/tmp/build-report.txt',
            },
          ],
        },
      ],
    },
  },
  play: async ({ canvas }) => {
    const links = canvas.getAllByRole('link');
    await expect(links.map((link) => link.getAttribute('href'))).toEqual([
      'https://gateway.example.com/build/4344',
      'https://gateway.example.com/review/24206',
      'https://gateway.example.com/build/812',
    ]);
    links[0].focus();
    await userEvent.tab();
    await expect(links[1]).toHaveFocus();
    await expect(canvas.getByText('/tmp/build-report.txt')).toBeVisible();
  },
};

export const LinkResultsReceipt: Story = {
  args: { ...LinkResults.args, isSettled: true },
  play: LinkResults.play,
};

export const NarrowLinkResults: Story = {
  args: LinkResults.args,
  decorators: [
    (Story) => (
      <div className="max-w-72">
        <Story />
      </div>
    ),
  ],
  play: LinkResults.play,
};

export const LinkResultStates: Story = {
  decorators: [
    (Story) => (
      <div className="max-w-72">
        <Story />
      </div>
    ),
  ],
  args: {
    onInterrupt: undefined,
    view: {
      id: 'link-states',
      title: 'Result references',
      seq: 1,
      nodes: [
        {
          id: 'long',
          type: 'link',
          label: 'Detailed references',
          links: [
            {
              id: 'long-url',
              label: 'Full console · gateway-router-integration-checks #4344 · SUCCESS',
              target_kind: 'url',
              target: 'https://gateway.example.com/build/4344',
            },
            {
              id: 'long-path',
              label: 'Retained integration report',
              target_kind: 'path',
              target:
                '/tmp/gateway-router-integration-checks/build-report-with-complete-results.txt',
            },
          ],
        },
        {
          id: 'single',
          type: 'link',
          label: 'One reference',
          links: [
            {
              id: 'one',
              label: 'Build summary',
              target_kind: 'url',
              target: 'https://gateway.example.com/summary',
            },
          ],
        },
        { id: 'empty', type: 'link', label: 'Pending references', links: [] },
      ],
    },
  },
  play: async ({ canvas }) => {
    await expect(canvas.getByText('no links')).toBeVisible();
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
          fields: [
            {
              id: 'checks',
              type: 'group',
              direction: 'column',
              fields: [
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
          fields: [
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

export const DividersReceipt: Story = {
  args: { ...Dividers.args, isSettled: true },
  play: Dividers.play,
};

export const NarrowDividers: Story = {
  args: Dividers.args,
  decorators: [
    (Story) => (
      <div className="max-w-72">
        <Story />
      </div>
    ),
  ],
  play: Dividers.play,
};

/** A narrow desktop panel must stack a row, regardless of the viewport width. */
export const NarrowGroups: Story = {
  args: {
    onInterrupt: undefined,
    view: {
      id: 'narrow-groups',
      title: 'Connection review',
      seq: 1,
      nodes: [
        {
          id: 'connection',
          type: 'group',
          direction: 'row',
          fields: [
            { id: 'host', type: 'paragraph', text: 'gateway.example.com' },
            {
              id: 'nested',
              type: 'group',
              direction: 'row',
              fields: [
                { id: 'port', type: 'paragraph', text: 'Port 5432' },
                { id: 'transport', type: 'paragraph', text: 'Encrypted transport' },
              ],
            },
          ],
        },
      ],
    },
  },
  decorators: [
    (Story) => (
      <div className="max-w-72">
        <Story />
      </div>
    ),
  ],
};
