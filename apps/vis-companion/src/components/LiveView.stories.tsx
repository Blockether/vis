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
          groups: [],
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
    // A ROW IS A BAND, NOT A TOUCH CELL: under a thumb the compact face answers at 44px through
    // its invisible slop instead of standing 48px tall, and under a pointer it keeps the rhythm
    // of the head and parent rows beside it.
    if (matchMedia('(min-width: 640px) and (pointer: fine)').matches) {
      await expect(first.getBoundingClientRect().height).toBeGreaterThanOrEqual(28);
    } else {
      await expect(parseFloat(getComputedStyle(first, '::after').height)).toBeGreaterThanOrEqual(
        44,
      );
    }
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
      nodes: FinishedJobs.args!.view!.nodes.map((node) =>
        node.type === 'table' ? { ...node, label: 'Jobs', is_selectable: false } : node,
      ),
    },
  },
  play: async ({ canvas }) => {
    const table = canvas.getByRole('table');
    const node = table.closest('li')!;
    await expect(canvas.getByText('Jobs')).toBeVisible();
    // A heading rides the row's own air under the rule above it; only the table's
    // rails run to the panel's inset edge.
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
    const summary = canvas.getByText('2 workers observed');
    const search = canvas.getByRole('searchbox', { name: 'Search Worker output' });
    const output = canvas.getByRole('region', { name: 'Worker output output' });
    await expect(summary.getBoundingClientRect().left - group.getBoundingClientRect().left).toBe(18);
    await expect(log.getBoundingClientRect().left).toBe(summary.getBoundingClientRect().left);
    await expect(search.getBoundingClientRect().left - log.getBoundingClientRect().left).toBe(18);
    await expect(output.getBoundingClientRect().left).toBe(search.getBoundingClientRect().left);
    await expect(canvas.getByText('Runtime not checked').getBoundingClientRect().left).toBe(
      group.getBoundingClientRect().left,
    );
    for (const element of [summary, search, output]) {
      await expect(element.getBoundingClientRect().right).toBeLessThanOrEqual(
        group.getBoundingClientRect().right,
      );
    }
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
    const list = canvas.getByRole('link', { name: 'Full console · router #4344' }).closest('ul')!;
    const items = Array.from(list.children);
    const box = list.getBoundingClientRect();
    await expect(getComputedStyle(list).borderTopStyle).toBe('solid');
    await expect(parseFloat(getComputedStyle(list).borderTopWidth)).toBeGreaterThan(0);
    const luminance = (color: string) => color.match(/\d+/g)!.slice(0, 3)
      .map((channel) => Number(channel) / 255)
      .map((channel) => channel <= 0.04045 ? channel / 12.92 : ((channel + 0.055) / 1.055) ** 2.4)
      .reduce((sum, channel, index) => sum + channel * [0.2126, 0.7152, 0.0722][index], 0);
    const ink = luminance(getComputedStyle(list).borderTopColor);
    const paper = luminance(getComputedStyle(list.closest('section')!).backgroundColor);
    await expect((Math.max(ink, paper) + 0.05) / (Math.min(ink, paper) + 0.05)).toBeGreaterThanOrEqual(3);
    if (box.width >= 550) {
      await expect(items[0].getBoundingClientRect().top).toBe(items[1].getBoundingClientRect().top);
      await expect(items[1].getBoundingClientRect().left).toBeGreaterThan(
        items[0].getBoundingClientRect().right,
      );
    } else {
      await expect(items[1].getBoundingClientRect().top).toBeGreaterThan(
        items[0].getBoundingClientRect().bottom,
      );
    }
    for (const item of items) {
      await expect(item.getBoundingClientRect().right).toBeLessThanOrEqual(box.right);
    }
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
    const long = canvas.getByRole('link', {
      name: 'Full console · gateway-router-integration-checks #4344 · SUCCESS',
    });
    const list = long.closest('ul')!;
    const path = canvas.getByText(
      '/tmp/gateway-router-integration-checks/build-report-with-complete-results.txt',
    );
    const label = canvas.getByText('Retained integration report');
    for (const element of [long, path, label]) {
      await expect(element.scrollWidth).toBeLessThanOrEqual(element.clientWidth);
      await expect(element.getBoundingClientRect().right).toBeLessThanOrEqual(
        list.getBoundingClientRect().right,
      );
    }
    await expect(long.getBoundingClientRect().height).toBeGreaterThan(28);
    const single = canvas.getByRole('link', { name: 'Build summary' });
    await expect(parseFloat(getComputedStyle(single.closest('ul')!).borderTopWidth)).toBe(0);
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
    const dividerRow = canvas.getAllByRole('separator')[0].closest('li')!;
    // An explicit break replaces adjacent automatic rules; it is not three lines.
    await expect(parseFloat(getComputedStyle(dividerRow).borderBottomWidth)).toBe(0);
    await expect(
      parseFloat(getComputedStyle(dividerRow.previousElementSibling!).borderBottomWidth),
    ).toBe(0);
    const disclosure = canvas.getByRole('button', { name: 'Build details' });
    await userEvent.click(disclosure);
    const dividers = canvas.getAllByRole('separator');
    await expect(dividers).toHaveLength(3);
    await expect(canvas.getByText('No warnings reported.')).toBeVisible();
    for (const divider of dividers) {
      const box = divider.getBoundingClientRect();
      const parent = divider.parentElement!.getBoundingClientRect();
      const style = getComputedStyle(divider);
      await expect(divider.tagName).toBe('HR');
      await expect(divider.tabIndex).toBe(-1);
      await expect(divider.textContent).toBe('');
      await expect(box.width).toBeGreaterThan(0);
      await expect(box.left).toBe(parent.left);
      await expect(box.right).toBe(parent.right);
      await expect(style.borderTopStyle).toBe('solid');
      await expect(parseFloat(style.borderTopWidth)).toBeGreaterThan(0);
      const luminance = (color: string) =>
        color
          .match(/\d+/g)!
          .slice(0, 3)
          .map((channel) => Number(channel) / 255)
          .map((channel) =>
            channel <= 0.04045 ? channel / 12.92 : ((channel + 0.055) / 1.055) ** 2.4,
          )
          .reduce((sum, channel, index) => sum + channel * [0.2126, 0.7152, 0.0722][index], 0);
      let surface: Element = divider;
      while (
        surface.parentElement &&
        getComputedStyle(surface).backgroundColor === 'rgba(0, 0, 0, 0)'
      ) {
        surface = surface.parentElement;
      }
      const ink = luminance(style.borderTopColor);
      const paper = luminance(getComputedStyle(surface).backgroundColor);
      await expect(
        (Math.max(ink, paper) + 0.05) / (Math.min(ink, paper) + 0.05),
      ).toBeGreaterThanOrEqual(3);
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
  play: async ({ canvas }) => {
    const host = canvas.getByText('gateway.example.com').getBoundingClientRect();
    const port = canvas.getByText('Port 5432').getBoundingClientRect();
    const transport = canvas.getByText('Encrypted transport').getBoundingClientRect();
    await expect(port.left).toBe(host.left);
    await expect(port.top).toBeGreaterThanOrEqual(host.bottom + 12);
    await expect(transport.left).toBe(port.left);
    await expect(transport.top).toBeGreaterThanOrEqual(port.bottom + 12);
  },
};
