import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent } from 'storybook/test';

import { STORY_LIVE_VIEW } from '../dev/story-data';
import { LiveViewPanel } from './LiveView';

const meta = {
  title: 'Components/Live view spacing',
  component: LiveViewPanel,
  parameters: { layout: 'padded' },
  args: {
    view: {
      ...STORY_LIVE_VIEW,
      title: 'Release checks',
      description: 'Three jobs',
      nodes: [{ id: 'status', type: 'status', text: 'Watching', tone: 'running' }],
    },
  },
  // Regression #220: no blank line before the description. The rule under the header
  // closes the title block, and the body starts with the same air the rule sits on.
  play: async ({ canvas }) => {
    const title = canvas.getByText('Release checks');
    const description = canvas.getByText('Three jobs');
    const header = title.closest('header')!;
    await expect(description.getBoundingClientRect().top).toBe(
      title.getBoundingClientRect().bottom,
    );
    await expect(getComputedStyle(header).borderBottomWidth).toBe('1px');
    await expect(getComputedStyle(header).paddingBottom).toBe(getComputedStyle(header).paddingTop);
  },
} satisfies Meta<typeof LiveViewPanel>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Status: Story = {};

export const Table: Story = {
  args: {
    view: {
      ...meta.args.view,
      nodes: [
        {
          id: 'jobs',
          type: 'table',
          columns: [{ id: 'job', label: 'Job', align: 'left' }],
          rows: [{ id: 'tests', cells: ['Tests'], tone: 'ok' }],
          max_rows: 10,
          order: 'insertion',
          is_selectable: false,
          selected_ids: [],
          groups: [],
        },
      ],
    },
  },
};

// Regression: job headings touched the preceding rule, metadata sat above the
// job names, and the selected button masked the row's background.
const releaseJobs: Story = {
  args: {
    onSelect: fn(),
    view: {
      ...meta.args.view,
      title: 'Desktop packages',
      description: 'Watching four packaging jobs',
      nodes: [
        {
          id: 'summary',
          type: 'stat',
          stats: [
            { id: 'running', label: 'running', value_text: '3', tone: 'running' },
            { id: 'passed', label: 'passed', value_text: '0', tone: 'ok' },
            { id: 'queued', label: 'queued', value_text: '0', tone: 'idle' },
            { id: 'elapsed', label: 'elapsed', value_text: '1m 41s', tone: 'idle' },
          ],
        },
        {
          id: 'jobs',
          type: 'table',
          columns: [
            { id: 'job', label: 'Job', align: 'left' },
            { id: 'now', label: 'Now', align: 'left' },
            { id: 'took', label: 'Took', align: 'right' },
          ],
          rows: [
            {
              id: 'macos',
              cells: ['Package macOS universal', 'in progress', '1m 41s'],
              tone: 'running',
            },
            { id: 'windows', cells: ['Package Windows x64', 'failure', '54s'], tone: 'error' },
            {
              id: 'linux-x64',
              cells: ['Package Linux x64', 'in progress', '1m 41s'],
              tone: 'running',
            },
            {
              id: 'linux-arm64',
              cells: ['Package Linux ARM64', 'in progress', '1m 38s'],
              tone: 'running',
            },
          ],
          max_rows: 10,
          order: 'insertion',
          is_selectable: true,
          selected_ids: ['macos', 'linux-x64', 'linux-arm64'],
          groups: [],
        },
        {
          id: 'timeline',
          type: 'steps',
          label: 'Timeline',
          steps: [{ id: 'earlier', label: '12 earlier steps', detail: '29s', tone: 'idle' }],
        },
      ],
    },
  },
  play: async ({ canvas, args }) => {
    const table = canvas.getByRole('table');
    const head = table.querySelector('thead')!;
    const rows = [...table.querySelectorAll('tbody tr')];
    if (matchMedia('(min-width: 640px)').matches) {
      await expect(head).toBeVisible();
      const heading = getComputedStyle(head.querySelector('th')!);
      await expect(parseFloat(heading.paddingTop)).toBeGreaterThanOrEqual(8);
      await expect(heading.paddingTop).toBe(heading.paddingBottom);
      await expect(parseFloat(getComputedStyle(head.querySelector('tr')!).borderBottomWidth)).toBe(
        1,
      );
      const textCenter = (element: Element) => {
        const range = document.createRange();
        range.selectNodeContents(element);
        const box = range.getBoundingClientRect();
        return box.top + box.height / 2;
      };
      const name = textCenter(canvas.getByText('Package macOS universal'));
      for (const cell of [...rows[0].querySelectorAll('td')].slice(1)) {
        await expect(Math.abs(textCenter(cell) - name)).toBeLessThanOrEqual(1);
      }
    } else {
      await expect(head).not.toBeVisible();
      await expect(
        canvas.getByRole('button', { name: 'Select Package Windows x64' }),
      ).toHaveTextContent('failure');
    }
    for (const row of rows.slice(0, -1)) {
      await expect(parseFloat(getComputedStyle(row).borderBottomWidth)).toBe(1);
    }
    await expect(table.scrollWidth).toBeLessThanOrEqual(table.clientWidth);
    if (args.onSelect) {
      const selected = canvas.getByRole('button', { name: 'Select Package macOS universal' });
      await expect(selected).toHaveAttribute('aria-pressed', 'true');
      await expect(getComputedStyle(selected).backgroundColor).toBe(
        getComputedStyle(selected.closest('td')!).backgroundColor,
      );
      await expect(getComputedStyle(rows[0]).backgroundColor).not.toBe(
        getComputedStyle(rows[1]).backgroundColor,
      );
      // A ROW IS A BAND, NOT A TOUCH CELL (user report: on a desk the variants read as a column
      // of boxes). Under a thumb the face still answers at 44px through its invisible slop; under
      // a pointer it stands 32px, the height the head and the parent rows beside it keep.
      const pointer = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
      for (const row of rows) {
        const height = row.getBoundingClientRect().height;
        if (pointer) {
          await expect(height).toBeGreaterThanOrEqual(28);
          await expect(height).toBeLessThanOrEqual(36);
        } else {
          const face = row.querySelector('button')!;
          await expect(parseFloat(getComputedStyle(face, '::after').height)).toBeGreaterThanOrEqual(
            44,
          );
        }
      }
      const activated = canvas.getByRole('button', { name: 'Select Package Windows x64' });
      await userEvent.click(activated);
      await expect(args.onSelect).toHaveBeenCalledWith('jobs', ['windows']);
      await expect(getComputedStyle(activated.closest('tr')!).backgroundColor).toBe(
        getComputedStyle(activated).backgroundColor,
      );
    }
  },
};

export const DesktopJobs: Story = {
  ...releaseJobs,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

export const TabletJobs: Story = {
  ...releaseJobs,
  globals: { viewport: { value: 'tablet', isRotated: false } },
};

export const PhoneJobs: Story = {
  ...releaseJobs,
  globals: { viewport: { value: 'phone', isRotated: false } },
};

export const RecordedJobs: Story = {
  ...DesktopJobs,
  args: { ...releaseJobs.args, isSettled: true, onSelect: undefined },
};
