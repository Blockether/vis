import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent } from 'storybook/test';

import { STORY_SESSION_SEARCH_MATCH } from '../dev/story-data';
import { SearchMessages } from './SearchMessages';

const meta = {
  title: 'Session/Search messages',
  component: SearchMessages,
  parameters: { layout: 'fullscreen' },
  args: {
    title: 'Windows runtime checks',
    match: STORY_SESSION_SEARCH_MATCH,
    query: 'windows',
    isSearching: false,
    onOpen: fn(),
    className: 'h-dvh',
  },
} satisfies Meta<typeof SearchMessages>;

export default meta;
type Story = StoryObj<typeof meta>;

export const MarkdownMatches: Story = {
  play: async ({ canvas, canvasElement, args }) => {
    await expect(canvas.getAllByRole('listitem')).toHaveLength(3);
    await expect(canvasElement.querySelector('h2 mark')).toHaveTextContent('Windows');
    await expect(canvasElement.querySelectorAll('strong mark')).toHaveLength(2);
    await expect(canvasElement.querySelector('a, img')).toBeNull();

    await userEvent.click(canvas.getByRole('button', { name: 'Open' }));
    await expect(args.onOpen).toHaveBeenCalledTimes(1);
    await userEvent.click(canvas.getAllByRole('listitem')[1].querySelector('button')!);
    await expect(args.onOpen).toHaveBeenCalledTimes(2);
  },
};

export const Searching: Story = {
  args: { match: null, isSearching: true },
  play: async ({ canvas }) => {
    await expect(canvas.getByText('Searching messages...')).toBeVisible();
  },
};

export const TitleOnlyMatch: Story = {
  args: {
    match: {
      ...STORY_SESSION_SEARCH_MATCH,
      inTitle: true,
      inRequest: false,
      inReply: false,
      inThinking: false,
      hits: [],
    },
  },
  play: async ({ canvas }) => {
    await expect(canvas.queryByRole('listitem')).toBeNull();
    await expect(canvas.getByText('The title matches. No message matches.')).toBeVisible();
  },
};
