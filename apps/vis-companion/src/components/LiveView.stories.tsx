import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, waitFor, within } from 'storybook/test';

import {
  STORY_LIVE_CATALOG,
  STORY_LIVE_DASHBOARD,
  STORY_LIVE_FORM,
  STORY_LIVE_VIEW,
  STORY_LIVE_PRIMITIVES,
} from '../dev/story-data';
import { LiveViewPanel } from './LiveView';
import { liveViewFromWire, type LiveView } from '../lib/live-view';
import { useState } from 'react';
import { BandSlotContext } from './ui';

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
  title: 'Session/Live view/Panel',
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
    const chime = args.view.nodes.find((node) => node.type === 'media' && node.id === 'chime-change');
    if (chime?.type !== 'media') throw new Error('The shared fixture has no chime');
    for (const version of ['Before', 'After']) {
      const player = canvas.getByRole('group', { name: `${version}: ${chime.alt}` });
      await expect(within(player).getByRole('button', { name: 'Play' })).toBeVisible();
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

/**
 * A whole dashboard: a strip of tabs, counter tiles, a table and lanes. Its rhythm is
 * the thing to look at, at a phone width and at a desk width.
 */
export const Dashboard: Story = {
  args: { view: STORY_LIVE_DASHBOARD, onChange: fn(), onInterrupt: undefined },
  play: async ({ canvas, args }) => {
    const tabs = canvas.getByRole('radiogroup', { name: 'Workflow view' });
    await expect(within(tabs).getByRole('radio', { name: 'History' })).toBeChecked();
    await userEvent.click(within(tabs).getByRole('radio', { name: 'Run' }));
    await waitFor(() => expect(args.onChange).toHaveBeenCalledWith('tabs', 'run'));
  },
};

/** Badges, lists, bar and line charts and a date field of each mode. */
export const Catalog: Story = {
  args: { view: STORY_LIVE_CATALOG, onChange: fn(), onInterrupt: undefined },
  play: async ({ canvas }) => {
    await expect(canvas.getByRole('img', { name: /^p50 120, / })).toBeVisible();
    await expect(canvas.getByLabelText('Day')).toHaveAttribute('type', 'date');
  },
};

/** Every kind of field in one view: a short choice, a long choice, text and a slider. */
export const Inputs: Story = {
  args: { view: STORY_LIVE_FORM, onActivate: fn(), onChange: fn(), onSubmit: fn(), onInterrupt: undefined },
  play: async ({ canvas }) => {
    await expect(canvas.getByText('Workflow')).toBeVisible();
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
          type: 'flex',
          direction: 'row',
          children: [
            {
              id: 'checks',
              type: 'flex',
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
          type: 'collapsible',
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

// The lanes start relative to the page load, so the running lane shows a short time.
const LOADED = Date.now();

/**
 * A TEAM-like header made only of public nodes: a `wrap` line of paragraphs, a
 * button and dividers that stand upright between them. Under it, the lanes of the
 * team on one time scale. The running lane ticks once a second.
 */
export const TeamTimeline: Story = {
  args: {
    onInterrupt: undefined,
    onActivate: fn(),
    view: {
      id: 'team-timeline',
      title: 'Team',
      seq: 1,
      nodes: [
        {
          id: 'head',
          type: 'flex',
          direction: 'row',
          is_wrapping: true,
          align: 'center',
          children: [
            { id: 'team', type: 'paragraph', text: '**TEAM**' },
            { id: 'after-team', type: 'divider' },
            { id: 'time', type: 'paragraph', text: '1m 20s' },
            {
              id: 'toggle',
              type: 'button',
              label: 'Open timeline',
              action: 'activate',
              variant: 'secondary',
              is_disabled: false,
              clicks: 0,
            },
            { id: 'after-toggle', type: 'divider' },
            { id: 'counts', type: 'paragraph', text: '4 running · 1 queued · 1 done' },
          ],
        },
        {
          id: 'lanes',
          type: 'timeline',
          label: 'Agents',
          lanes: [
            { id: 'map', label: 'Map the routes', tone: 'ok', start: LOADED - 80_000, end: LOADED - 30_000 },
            { id: 'build', label: 'Build the dock', tone: 'running', detail: 'step 3 of 5', start: LOADED - 60_000 },
            { id: 'verify', label: 'Verify the sheet', tone: 'error', start: LOADED - 45_000, end: LOADED - 10_000 },
            { id: 'audit', label: 'Audit', tone: 'idle', start: LOADED - 5_000, end: LOADED - 5_000 },
          ],
        },
      ],
    },
  },
  play: async ({ canvas, args }) => {
    const separators = canvas.getAllByRole('separator');
    await expect(separators).toHaveLength(2);
    for (const separator of separators) {
      await expect(separator).toHaveAttribute('aria-orientation', 'vertical');
    }
    await userEvent.click(canvas.getByRole('button', { name: 'Open timeline' }));
    await expect(args.onActivate).toHaveBeenCalledWith('toggle');
    await expect(canvas.getByText('Build the dock').closest('li')).toHaveTextContent('Running');
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

// User report (screenshots of a GitHub watch): opening a folded group moved every column.
// The folded group rows span all columns, and the browser spread their width over them.
const groupedJobs: LiveView = {
  id: 'grouped-jobs',
  title: 'CI · run 44419661029',
  seq: 1,
  nodes: [
    {
      id: 'jobs',
      type: 'table',
      columns: [
        { id: 'job', label: 'Job', align: 'left' },
        { id: 'now', label: 'Now', align: 'left' },
        { id: 'took', label: 'Took', align: 'left' },
      ],
      rows: [
        { id: 'tui-linux', cells: ['linux', 'success', '2m 20s'], tone: 'ok', parent: 'tui' },
        { id: 'tui-macos', cells: ['macos', 'success', '1m 41s'], tone: 'ok', parent: 'tui' },
        { id: 'sdk-linux', cells: ['ubuntu-latest', 'success', '3m 2s'], tone: 'ok', parent: 'sdk' },
        { id: 'sdk-macos', cells: ['macos-26', 'in progress', '5m 16s'], tone: 'running', parent: 'sdk' },
      ],
      max_rows: 500,
      order: 'insertion',
      is_selectable: false,
      selected_ids: [],
      groups: [
        { id: 'tui', label: 'tui' },
        { id: 'sdk', label: 'python-package / real SDK engine' },
      ],
    },
  ],
};

/** Open a group: no column moves. Check it in a real browser; jsdom has no layout. */
export const GroupedColumns: Story = {
  args: { view: groupedJobs },
};

const layoutText = (id: string, text: string) => ({ id, type: 'paragraph', text });
const layoutToggle = {
  id: 'mode', name: 'mode', type: 'toggle', label: 'Mode', is_required: true, value: 'active',
  options: [{ value: 'active', label: 'Active' }, { value: 'all', label: 'All' }],
};
const layoutMenu = {
  id: 'actions', type: 'menu', label: 'Actions', icon: 'more',
  children: [
    { id: 'refresh', type: 'button', label: 'Refresh', is_selected: true },
    { id: 'menu-rule', type: 'divider' },
    { id: 'menu-more', type: 'menu', label: 'More options', children: [
      { id: 'export', type: 'button', label: 'Export' },
      { id: 'archive', type: 'button', label: 'Archive', is_disabled: true },
    ] },
  ],
};
const layoutFlex = {
  id: 'edit-row', type: 'flex', direction: 'row', is_wrapping: true, gap: 'sm', align: 'end', justify: 'between',
  children: [
    { id: 'filter', type: 'plaintext', label: 'Item filter', placeholder: 'Filter items', is_growing: true },
    { id: 'apply', type: 'button', label: 'Apply' },
    layoutMenu,
  ],
};
const layoutSection = {
  id: 'settings', type: 'section', label: 'Item settings', description: 'Choose which items to show.', icon: 'filter',
  children: [layoutFlex],
};
const layoutGrid = {
  id: 'tiles', type: 'grid', columns: 3, min_width: 'xs', gap: 'md',
  children: ['Alpha', 'Beta', 'Gamma', 'Delta'].map((label, i) => ({
    id: `tile-${i}`, type: 'section', label, children: [layoutText(`text-${i}`, 'An item in the grid.')],
  })),
};
const layoutDisclosure = {
  id: 'details', type: 'collapsible', label: 'Details', description: 'Show setup information.',
  children: [layoutText('about', 'This layout adapts to the width of its view.')],
};
const layoutDialog = (is_open: boolean) => ({
  id: 'settings-dialog', type: 'dialog', label: 'Add an item', is_open,
  description: 'Enter a name for the item.',
  children: [{ id: 'item', type: 'plaintext', label: 'Item name' }],
});

function LayoutDemo({ nodes, composed = false, initiallyOpen = false }: {
  nodes: Record<string, unknown>[]; composed?: boolean; initiallyOpen?: boolean;
}) {
  const [slot, setSlot] = useState<HTMLDivElement | null>(null);
  const [open, setOpen] = useState(initiallyOpen);
  const [lastAction, setLastAction] = useState('');
  const header = composed ? [layoutToggle, { id: 'add', type: 'button', icon: 'plus', label: 'Add item' }] : [];
  const view = liveViewFromWire({
    id: 'layouts', title: 'Layout components', seq: 0,
    nodes: [...header, ...nodes, ...(composed || initiallyOpen ? [layoutDialog(open)] : [])],
    header: header.map((node) => node.id),
  })!;
  return (
    <BandSlotContext.Provider value={slot}>
      {composed && <div ref={setSlot} className="flex min-w-0 flex-wrap items-center gap-2 border-b border-edge-strong py-2" />}
      <LiveViewPanel
        view={view}
        onActivate={(id) => {
          if (id === 'add') setOpen(true);
          else if (id === 'settings-dialog') setOpen(false);
          else setLastAction(id);
        }}
        onChange={() => {}}
      />
      {lastAction && <p role="status">Last action: {lastAction}</p>}
    </BandSlotContext.Provider>
  );
}

export const Flex: Story = { render: () => <LayoutDemo nodes={[layoutFlex]} /> };
export const Grid: Story = { render: () => <LayoutDemo nodes={[layoutGrid]} /> };
export const Section: Story = { render: () => <LayoutDemo nodes={[layoutSection]} /> };
export const Collapsible: Story = { render: () => <LayoutDemo nodes={[layoutDisclosure]} /> };
export const Dialog: Story = { render: () => <LayoutDemo nodes={[]} initiallyOpen /> };
export const Menu: Story = { render: () => <LayoutDemo nodes={[layoutMenu]} /> };
export const Toggle: Story = {
  render: () => <LayoutDemo nodes={[{ ...layoutToggle, options: [
    { value: 'active', label: 'Active', icon: 'list' },
    { value: 'all', label: 'All', icon: 'grid' },
  ] }]} />,
  play: async ({ canvas }) => {
    const active = canvas.getByRole('radio', { name: 'Active' });
    const all = canvas.getByRole('radio', { name: 'All' });
    active.focus();
    await userEvent.keyboard('{ArrowRight}');
    await expect(all).toHaveFocus();
    await expect(active).toBeChecked();
    await userEvent.keyboard('{Enter}');
    await expect(all).toBeChecked();
  },
};
export const ComposedLayout: Story = {
  render: () => <LayoutDemo nodes={[layoutSection, layoutGrid, layoutDisclosure]} composed />,
};
