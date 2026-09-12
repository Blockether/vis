import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent, within } from 'storybook/test';
import {
  ACTIVITY_ALL_GROUPS,
  ACTIVITY_CHRONOLOGY,
  ACTIVITY_INTERLEAVED,
  ACTIVITY_REPEATED_ARGUMENTS,
  ACTIVITY_RESULTS,
  ACTIVITY_REPL,
  ACTIVITY_TABLES,
  ACTIVITY_LONG_RUNNING,
  ACTIVITY_LONG_LABELS,
  ACTIVITY_LISTING,
  ACTIVITY_LISTING_BATCH,
  ACTIVITY_RICH,
  ACTIVITY_FAILED,
  ACTIVITY_RUNNING,
  ACTIVITY_SETTLED,
  ACTIVITY_TREE_CHANGES,
} from '../dev/story-data';
import { ActivityHistoryContext, ActivityPanel } from './ActivityPanel';
import { activityHistoryPage } from '../dev/activity-history';
import { activityProjectionFromWire } from '../lib/activity';
import groupingCases from '../../../../packages/vis-contract/resources/vis-contract/fixtures/activity-groups.json';

/**
 * WHAT THE MODEL IS DOING, WHILE IT IS DOING IT.
 *
 * The axis has one job the transcript cannot do: report a bounded run — how many
 * calls, which one is moving, what it produced — in one reading, with nothing
 * inside it behind a second chevron. It is what the invocation's own band opens
 * onto, and the reason a reader opens that band at all. The states below are the
 * sentences it can say, and they are drawn here rather than described, because
 * `running` has to read as one moving thread, `succeeded` has to go quiet
 * without disappearing, and `failed` has to be findable in a settled transcript
 * scrolled past — with the head of its output already on the page.
 *
 * The payloads are the ENGINE's own (`activityProjectionFromWire`), so a wire
 * change breaks this sheet before it reaches a screen.
 */
const meta = {
  title: 'Components/Activity panel',
  component: ActivityPanel,
  parameters: { layout: 'fullscreen' },
  // Match the transcript gutter and the execution band's inset around this panel.
  decorators: [
    (Story) => (
      <div className="mx-auto w-full max-w-3xl px-3.5 pt-4 sm:px-6 sm:pt-6">
        <div className="px-3">
          <Story />
        </div>
      </div>
    ),
  ],
} satisfies Meta<typeof ActivityPanel>;

export default meta;

type Story = StoryObj<typeof meta>;

/** Regression: long test paths must not paint over the duration or disclosure. */
export const LongLabels: Story = {
  args: { activity: ACTIVITY_LONG_LABELS },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    const rows = canvasElement.querySelectorAll<HTMLElement>('[data-activity-row]');
    await expect(rows).toHaveLength(ACTIVITY_LONG_LABELS.rows.length);
    for (const row of rows) {
      const toggle = within(row).getByRole('button');
      const label = toggle.firstElementChild!;
      const summary = label.querySelector<HTMLElement>('.flex-1[title]')!;
      const duration = within(row).getByLabelText(/^Duration /);
      const chevron = toggle.lastElementChild!;
      // A row can fit its container while visible text still spills across siblings.
      await expect(getComputedStyle(summary).overflowX).toBe('hidden');
      await expect(getComputedStyle(summary).textOverflow).toBe('ellipsis');
      await expect(getComputedStyle(summary).whiteSpace).toBe('nowrap');
      await expect(summary.getBoundingClientRect().right + 8).toBeLessThanOrEqual(
        duration.getBoundingClientRect().left + 1,
      );
      await expect(duration.getBoundingClientRect().right + 6).toBeLessThanOrEqual(
        chevron.getBoundingClientRect().left + 1,
      );
      await expect(toggle.scrollWidth).toBeLessThanOrEqual(toggle.clientWidth);
      await expect(toggle.getBoundingClientRect().height).toBe(24);
      const path = summary.querySelector('[data-path]');
      for (const part of path ? Array.from(path.children) : [summary]) {
        await expect(toggle).toHaveAccessibleName(expect.stringContaining(part.textContent!));
      }
      await userEvent.click(toggle);
      await expect(toggle).toHaveAttribute('aria-expanded', 'true');
      // Both the row label and the revealed file list constrain long basenames.
      for (const path of row.querySelectorAll<HTMLElement>('[data-path]')) {
        await expect(path.scrollWidth).toBeLessThanOrEqual(path.clientWidth);
        if (path.dataset.path?.endsWith('/render.clj')) {
          const basename = path.lastElementChild!;
          await expect(basename.scrollWidth).toBe(basename.clientWidth);
        }
      }
      await expect(row.scrollWidth).toBeLessThanOrEqual(row.clientWidth);
      toggle.focus();
      await userEvent.keyboard('{Enter}');
      await expect(toggle).toHaveAttribute('aria-expanded', 'false');
    }
    await expect(canvasElement.scrollWidth).toBeLessThanOrEqual(canvasElement.clientWidth);
  },
};

export const LongLabelsNarrow: Story = {
  ...LongLabels,
  decorators: [
    (Story) => (
      <div className="max-w-xs">
        <Story />
      </div>
    ),
  ],
};

export const ReplResults: Story = {
  args: { activity: ACTIVITY_REPL },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    await userEvent.click(canvas.getByRole('button', { name: /Eval ×5/ }));
    const rows = Array.from(canvasElement.querySelectorAll<HTMLElement>('[data-activity-row]'));
    await expect(rows).toHaveLength(5);
    for (const [index, row] of rows.entries()) {
      const toggle = within(row).getByRole('button');
      await userEvent.click(toggle);
      const program = within(row).getByRole('heading', { name: 'Program' });
      await expect(program).toBeVisible();
      // The first section needs one line of separation from the activity header.
      await expect(
        program.getBoundingClientRect().top - toggle.getBoundingClientRect().bottom,
      ).toBe(parseFloat(getComputedStyle(program).lineHeight));
      for (const title of index < 2
        ? ['Stdout', 'Stderr', 'Result']
        : [index === 4 ? 'Timeout' : 'Error']) {
        await expect(within(row).getByRole('heading', { name: title })).toBeVisible();
      }
      for (const heading of Array.from(row.querySelectorAll('[data-activity-content] > h5')).slice(
        1,
      )) {
        await expect(parseFloat(getComputedStyle(heading).marginTop)).toBeGreaterThanOrEqual(12);
      }
      for (const code of row.querySelectorAll('pre')) {
        await expect(parseFloat(getComputedStyle(code).paddingTop)).toBeGreaterThan(0);
        await expect(
          parseFloat(getComputedStyle(code.querySelector('code > div')!).paddingLeft),
        ).toBeGreaterThan(0);
      }
      await expect(within(row).queryByRole('table')).not.toBeInTheDocument();
      await expect(row.scrollWidth).toBeLessThanOrEqual(row.clientWidth);
    }
  },
};

/** One step disclosure opens the retained result, not invocation parameters. */
export const ResultFirst: Story = {
  args: { activity: ACTIVITY_RESULTS },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    const rows = Array.from(canvasElement.querySelectorAll<HTMLElement>('[data-activity-row]'));
    await expect(rows).toHaveLength(6);
    const expected = [
      'greeting',
      '"Hi "',
      'greeting_test.clj',
      'captured lines',
      'one disclosure',
      '12 tests passed.',
    ];
    for (const [index, row] of rows.entries()) {
      const step = within(row).getByRole('button');
      await expect(step.getBoundingClientRect().height).toBe(24);
      await userEvent.click(step);
      await expect(row.textContent).toContain(expected[index]);
      await expect(within(row).queryByRole('button', { name: /^Diff/ })).not.toBeInTheDocument();
      await expect(row.textContent).not.toMatch(/12:abc|13:def|\["src\/com/);
      await expect(row.scrollWidth).toBeLessThanOrEqual(row.clientWidth);
    }
    for (const [index, heading] of [[5, 'Metric']] as const) {
      const table = within(rows[index]);
      await expect(table.getByRole('columnheader', { name: heading })).toBeVisible();
      await expect(table.getByRole('columnheader', { name: 'Result' })).toBeVisible();
    }
    await expect(rows[4].textContent).not.toMatch(/Thread id|Title|42/);
    await expect(rows[5].textContent).not.toContain('Is pass');
    await expect(canvas.queryByRole('columnheader', { name: 'Field' })).not.toBeInTheDocument();
    await expect(canvas.queryByRole('columnheader', { name: 'Value' })).not.toBeInTheDocument();
  },
};

/** The Storybook clipboard is a boundary stub, as for the shared Code copy control. */
export const CopyActivity: Story = {
  args: { activity: ACTIVITY_REPEATED_ARGUMENTS },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const user = userEvent.setup();
    const copy = canvas.getByRole('button', { name: 'Copy activity' });
    const mouse = matchMedia('(min-width: 40rem) and (pointer: fine)').matches;
    const minimum = mouse ? 28 : 44;
    const box = copy.getBoundingClientRect();
    const reach = getComputedStyle(copy, '::after');
    const rightReach = reach.content === 'none' ? 0 : -parseFloat(reach.right);
    await expect(box.width + rightReach).toBeGreaterThanOrEqual(minimum);
    await expect(box.height).toBeGreaterThanOrEqual(minimum);
    // The invisible reach remains clickable and never extends toward the disclosure.
    const targetRight = box.left + minimum;
    await expect(targetRight).toBeLessThanOrEqual(copy.ownerDocument.documentElement.clientWidth);
    await expect(
      copy.contains(copy.ownerDocument.elementFromPoint(targetRight - 1, box.top + box.height / 2)),
    ).toBe(true);
    const toggle = canvas.getByRole('button', { name: 'Expand Activity' });
    await expect(box.left - toggle.getBoundingClientRect().right).toBeCloseTo(8, 0);
    await user.click(copy);
    await expect(navigator.clipboard.readText()).resolves.toContain('First search: 2 matches');
    await expect(canvas.getByRole('button', { name: 'Expand Activity' })).toHaveAttribute(
      'aria-expanded',
      'false',
    );
    await user.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    copy.focus();
    await user.keyboard('{Enter}');
    await expect(navigator.clipboard.readText()).resolves.toContain('Search directory unavailable');
    await expect(canvas.getByRole('button', { name: 'Collapse Activity' })).toHaveAttribute(
      'aria-expanded',
      'true',
    );
  },
};

export const RepeatedArguments: Story = {
  args: { activity: ACTIVITY_REPEATED_ARGUMENTS },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    const group = canvas.getByRole('button', { name: /Search ×6/ });
    await expect(group.getBoundingClientRect().height).toBe(24);
    await userEvent.click(group);
    const repeated = canvas.getByRole('button', { name: /same query ×3/ });
    await expect(repeated.getBoundingClientRect().height).toBe(24);
    await expect(repeated).toHaveAttribute('aria-expanded', 'false');
    await expect(canvas.getByText(/Search directory unavailable/)).toBeVisible();
    repeated.focus();
    await userEvent.keyboard('{Enter}');
    const first = within(
      canvasElement.querySelector<HTMLElement>('[data-activity-row="search-1"]')!,
    );
    await userEvent.click(first.getByRole('button'));
    await expect(canvas.getByText('First search: 2 matches')).toBeVisible();
    await userEvent.click(repeated);
    await expect(canvas.queryByText('First search: 2 matches')).not.toBeInTheDocument();
    await expect(canvasElement.querySelector('[data-activity-row="search-2"]')).toBeVisible();
  },
};

/** A turn in flight: one call answered, one still running. */
export const Running: Story = {
  args: { activity: ACTIVITY_RUNNING },
};

/** Pointer and keyboard can open retained evidence without waiting for settlement. */
export const LiveDisclosure: Story = {
  args: { activity: ACTIVITY_LONG_RUNNING },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    await userEvent.click(canvas.getByRole('button', { name: /Search ×7/ }));
    const step = canvas.getByRole('button', { name: /Searched search-4/ });
    step.focus();
    await userEvent.keyboard('{Enter}');
    await expect(step).toHaveAttribute('aria-expanded', 'true');
    await userEvent.click(canvas.getByRole('button', { name: /Search ×7/ }));
    await expect(canvas.queryByText('result-4')).not.toBeInTheDocument();
  },
};

/** Settled and read: three calls, a diff among them, nothing moving. */
export const Settled: Story = {
  args: { activity: ACTIVITY_SETTLED },
};

/** Failure counts remain visible; details wait for explicit expansion. */
export const Failed: Story = {
  args: { activity: ACTIVITY_FAILED },
};

/** No projection at all — what a turn has before its first tool call. */
export const Idle: Story = {
  args: {},
};

/** The thread doing its job: reads, a patch, a failed check, one step still moving. */
export const Chronology: Story = {
  args: { activity: ACTIVITY_CHRONOLOGY },
};

/** Repeated operations share one group even when ten read/search runs are interleaved. */
export const InterleavedOperations: Story = {
  args: { activity: ACTIVITY_INTERLEAVED },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    const groups = canvas.getByRole('list', { name: 'Operation groups' });
    await expect(groups.children).toHaveLength(4);
    await expect(canvas.getByRole('button', { name: /Search ×10/ })).toBeVisible();
    await expect(canvas.getByRole('button', { name: /Test ×2/ })).toHaveTextContent(
      '1 running · 1 failed',
    );
    await expect(canvas.getByText(/Assertion failed/)).toBeVisible();
    const reads = canvas.getByRole('button', { name: /Read ×10/ });
    reads.focus();
    await userEvent.keyboard('{Enter}');
    const members = canvas.getByRole('list', { name: 'Read ×10 operations' });
    await expect(members.children).toHaveLength(10);
    await expect([...members.children].map((row) => row.getAttribute('data-activity-row'))).toEqual(
      Array.from({ length: 10 }, (_, index) => `cat-${index + 1}`),
    );
    await userEvent.click(reads);
    await expect(reads).toHaveAttribute('aria-expanded', 'false');
  },
};

/** All groups are available immediately; individual step details still fold independently. */
export const AllOperationGroups: Story = {
  args: { activity: ACTIVITY_ALL_GROUPS },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    const groups = canvas.getByRole('list', { name: 'Operation groups' });
    await expect(groups.children).toHaveLength(7);
    await expect(
      canvas.queryByRole('button', {
        name: /(?:show|hide).*(?:more|fewer).*groups?/i,
      }),
    ).not.toBeInTheDocument();
    const lastStep = within(groups.children[6] as HTMLElement).getByRole('button', {
      expanded: false,
    });
    await expect(lastStep).toBeVisible();
    lastStep.focus();
    await userEvent.keyboard('{Enter}');
    await expect(lastStep).toHaveAttribute('aria-expanded', 'true');
    await expect(canvas.getByText('Build completed')).toBeVisible();
    await userEvent.click(lastStep);
    await expect(lastStep).toHaveAttribute('aria-expanded', 'false');
  },
};

/** What a block did to the tree with no tool call of its own: one row per kind. */
export const TreeChanges: Story = {
  args: { activity: ACTIVITY_TREE_CHANGES },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    const step = canvas.getByRole('button', {
      name: /Changed 13 files and 2 directories/,
    });
    await userEvent.click(step);
    const children = canvasElement.querySelector('[data-activity-children]')!;
    await expect(children).toBeVisible();
    await expect(getComputedStyle(children).marginTop).toBe('0px');
    await userEvent.click(step);
    await expect(canvasElement.querySelector('[data-activity-children]')).toBeNull();
  },
};

export const SymbolContent: Story = { args: { activity: ACTIVITY_RICH } };

/** Repeated headers and cells must share one column edge, including wrapped values. */
export const AlignedTables: Story = {
  args: { activity: ACTIVITY_TABLES },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    await userEvent.click(canvas.getByRole('button', { name: /Listed Council threads/ }));
    const headers = canvas.getAllByRole('columnheader', { name: 'Result' });
    await expect(headers).toHaveLength(3);
    const left = headers[0].getBoundingClientRect().left;
    for (const header of headers) {
      await expect(header.getBoundingClientRect().left).toBe(left);
    }
    for (const row of canvasElement.querySelectorAll('tbody tr')) {
      const cell = row.children[1];
      await expect(cell.getBoundingClientRect().left).toBe(left);
      await expect(getComputedStyle(cell).verticalAlign).toBe('top');
    }
    const body = canvasElement.querySelector('[data-activity-content]')!;
    await expect(body.scrollWidth).toBeLessThanOrEqual(body.clientWidth);
  },
};

export const Listing: Story = {
  args: { activity: ACTIVITY_LISTING },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    const step = canvas.getByRole('button', { name: /Listed apps/ });
    await expect(canvas.getByText('3 directories · 2 files')).toBeVisible();
    await expect(canvas.queryByRole('table')).not.toBeInTheDocument();
    step.focus();
    await userEvent.keyboard('{Enter}');
    await expect(canvas.getByRole('table')).toBeVisible();
    await userEvent.click(step);
    await expect(canvas.getByText('3 directories · 2 files')).toBeVisible();
    await expect(canvas.queryByRole('table')).not.toBeInTheDocument();
  },
};

export const ListingBatch: Story = {
  args: { activity: ACTIVITY_LISTING_BATCH },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    await expect(canvas.getByText('0 directories · 2 files')).toBeVisible();
    const step = canvas.getByRole('button', { name: /Listed 2 directories/ });
    await userEvent.click(step);
    await expect(canvas.getAllByRole('table')).toHaveLength(2);
    // The first result follows its header closely; separate results keep one line.
    const sections = [...canvasElement.querySelectorAll('[data-activity-section]')];
    const bodies = [...canvasElement.querySelectorAll('[data-activity-content]')];
    await expect(
      sections[0].getBoundingClientRect().top - step.getBoundingClientRect().bottom,
    ).toBe(4);
    await expect(
      sections[1].getBoundingClientRect().top - sections[0].getBoundingClientRect().bottom,
    ).toBe(16);
    for (const body of bodies) {
      await expect(getComputedStyle(body).marginTop).toBe('0px');
      await expect(getComputedStyle(body).rowGap).toBe('4px');
    }
    const chronology = canvas.getByRole('list', {
      name: 'Operation groups',
    });
    await expect(getComputedStyle(chronology).paddingBottom).toBe('4px');
    const reach = getComputedStyle(step, '::after');
    const targetHeight =
      step.getBoundingClientRect().height - parseFloat(reach.top) - parseFloat(reach.bottom);
    const minimum = matchMedia('(width >= 40rem) and (pointer: fine)').matches ? 28 : 44;
    await expect(targetHeight).toBeGreaterThanOrEqual(minimum);
    step.focus();
    await userEvent.keyboard('{Enter}');
    await expect(step).toHaveAttribute('aria-expanded', 'false');
    await expect(canvas.queryByRole('table')).not.toBeInTheDocument();
  },
};

export const CompactMiddle: Story = {
  args: {
    activity: {
      ...ACTIVITY_SETTLED,
      rows: ['Before', 'Middle', 'After'].map((headline, index) => ({
        ...ACTIVITY_SETTLED.rows[0],
        id: headline.toLowerCase(),
        sequence: index + 1,
        operation: headline.toLowerCase(),
        state: 'succeeded',
        resources: [],
        evidence: [],
        result_summary: undefined,
        presentation: {
          headline,
          summary: '',
          content: [{ type: 'code', language: 'text', text: 'alpha\n\nbeta' }],
        },
      })),
    },
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    const rows = ['before', 'middle', 'after'].map((id) =>
      canvasElement.querySelector<HTMLElement>(`[data-activity-row="${id}"]`)!,
    );
    const middle = within(rows[1]).getByRole('button');
    // Adjacent boxes alone miss the blank space inside an oversized toggle.
    const checkSiblings = async () => {
      for (const row of rows) {
        const toggle = within(row).getByRole('button');
        const box = toggle.getBoundingClientRect();
        await expect(box.height).toBe(24);
        // Invisible reach must not cover any part of a neighboring toggle.
        for (const x of [box.left + 2, box.right - 2]) {
          for (const y of [box.top + 1, box.top + box.height / 2, box.bottom - 1]) {
            await expect(document.elementFromPoint(x, y)?.closest('button')).toBe(toggle);
          }
        }
      }
      await expect(rows[1].getBoundingClientRect().top).toBe(
        rows[0].getBoundingClientRect().bottom,
      );
      await expect(rows[2].getBoundingClientRect().top).toBe(
        rows[1].getBoundingClientRect().bottom,
      );
    };
    await checkSiblings();
    await userEvent.click(middle);
    const body = rows[1].querySelector<HTMLElement>('[data-activity-content]')!;
    const code = within(rows[1]).getByRole('group', { name: 'text code' });
    await expect(getComputedStyle(body).marginTop).toBe('0px');
    await expect(getComputedStyle(code).paddingTop).toBe('8px');
    await expect(getComputedStyle(code).paddingBottom).toBe('8px');
    await expect(code.textContent).toContain('alpha');
    await expect(code.querySelector('code')!.children).toHaveLength(3);
    await expect(body.getBoundingClientRect().top).toBe(middle.getBoundingClientRect().bottom);
    await expect(rows[1].getBoundingClientRect().bottom).toBe(body.getBoundingClientRect().bottom);
    await checkSiblings();
    await userEvent.click(middle);
    await checkSiblings();
  },
};

/** Regression #201: extensions group by operation, not by their shared display headline. */
export const ExtensionGroups: Story = {
  args: {
    activity: activityProjectionFromWire(
      groupingCases.find(
        (sample) => sample.name === 'extension presentations label exact-operation groups',
      )!.projection,
    )!,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    await expect(canvasElement.querySelectorAll('[data-activity-group]')).toHaveLength(3);
    await expect(canvas.getByRole('button', { name: /Search reviews ×2/ })).toBeVisible();
    await expect(canvas.getByRole('button', { name: /Check review deployment ×2/ })).toBeVisible();
    await expect(canvas.getByText(/Waiting for deployment · running/)).toBeVisible();
    await expect(canvas.getByText(/Closed reviews · Review service unavailable/)).toBeVisible();
    await userEvent.click(canvas.getByRole('button', { name: /Search reviews ×3/ }));
    await userEvent.click(canvas.getByRole('button', { name: /Changes: 0 ×2/ }));
    await expect(canvasElement.querySelectorAll('[data-activity-row]')).toHaveLength(3);
    await expect(canvasElement).not.toHaveTextContent('reviews.search');
    await expect(canvasElement).not.toHaveTextContent('reviews.deployment_status');
  },
};

/** #212: the data source stands in for authenticated paged retrieval, not the UI. */
export const RetainedHistory: Story = {
  args: { activity: activityHistoryPage() },
  decorators: [
    (Story) => (
      <ActivityHistoryContext.Provider
        value={{
          load: async (_id, after, query) => activityHistoryPage(after, query),
        }}
      >
        <Story />
      </ActivityHistoryContext.Provider>
    ),
  ],
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    await expect(canvas.queryByRole('searchbox')).toBeNull();
    await expect(canvas.queryByRole('button', { name: 'Copy all activity' })).toBeNull();
    await expect(canvas.queryByRole('button', { name: 'Export all activity' })).toBeNull();
    for (let i = 0; i < 4; i++) {
      await userEvent.click(canvas.getByRole('button', { name: 'Show more operations' }));
      await expect(await canvas.findByText(`Operation ${(i + 2) * 32}`)).toBeVisible();
    }
    await expect(canvas.queryByText('Operation 1')).toBeNull();
    await expect(canvas.getByText('Operation 159')).toBeVisible();
    await expect(canvas.queryByRole('button', { name: 'Show more operations' })).toBeNull();
    const earlier = canvas.getByRole('button', { name: 'Show earlier operations' });
    earlier.focus();
    await userEvent.keyboard('{Enter}');
    await expect(await canvas.findByText('Operation 1')).toBeVisible();
    await expect(canvas.queryByText('Operation 160')).toBeNull();
  },
};

export const RetainedHistoryOffline: Story = {
  args: { activity: activityHistoryPage() },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    await expect(canvas.getByText('Operation 1')).toBeVisible();
    await expect(canvas.getByText('Reconnect to load more operations.')).toBeVisible();
    await expect(canvas.queryByRole('searchbox')).toBeNull();
    await expect(canvas.queryByRole('button', { name: 'Show more operations' })).toBeNull();
  },
};

export const RetainedHistoryUnavailable: Story = {
  args: { activity: activityHistoryPage() },
  decorators: [
    (Story) => (
      <ActivityHistoryContext.Provider
        value={{
          load: async () => {
            throw new Error('Operations could not be loaded. Reconnect and try again.');
          },
        }}
      >
        <Story />
      </ActivityHistoryContext.Provider>
    ),
  ],
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    await userEvent.click(canvas.getByRole('button', { name: 'Show more operations' }));
    await expect(await canvas.findByRole('alert')).toHaveTextContent('Operations could not be loaded');
    await expect(canvas.getByText('Operation 1')).toBeVisible();
    await expect(canvas.getByRole('button', { name: 'Reload operations' })).toBeVisible();
  },
};
