import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect } from 'storybook/test';

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
  // Regression #220: no blank line before the description, exactly one after it.
  play: async ({ canvas }) => {
    const title = canvas.getByText('Release checks');
    const description = canvas.getByText('Three jobs');
    const node = title.closest('section')!.querySelector('ul > li')!;
    const line = parseFloat(getComputedStyle(description).lineHeight);
    await expect(description.getBoundingClientRect().top).toBe(
      title.getBoundingClientRect().bottom,
    );
    await expect(
      node.getBoundingClientRect().top - description.getBoundingClientRect().bottom,
    ).toBe(line);
    await expect(getComputedStyle(node).paddingTop).toBe('0px');
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
        },
      ],
    },
  },
};
