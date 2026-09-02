import type { Meta, StoryObj } from '@storybook/react-vite';
import { useEffect, useState, type ReactNode } from 'react';
import { expect, fn, within } from 'storybook/test';

import { STORY_FLEET_CONNS, storyFleetFetch } from '../dev/story-data';
import { SessionsScreen } from './SessionsScreen';

/**
 * THE SESSION LIST AS IT SHIPS, over a gateway that answers from a fixture.
 *
 * The screen builds its own gateway client, so the boundary a story may replace is the
 * one it already has: `fetch`. It is installed during the first render — the screen
 * reads in an effect, and an effect runs after this — and put back when the story
 * unmounts, so no other story in the gallery inherits a fake fleet.
 */
function StoryFleet({ children }: { children: ReactNode }) {
  const [restore] = useState(() => {
    const previous = globalThis.fetch;
    globalThis.fetch = storyFleetFetch();
    return () => {
      globalThis.fetch = previous;
    };
  });
  useEffect(() => restore, [restore]);
  return <div className="flex h-dvh w-full flex-col bg-page">{children}</div>;
}

const meta = {
  title: 'Screens/Session list',
  component: SessionsScreen,
  parameters: { layout: 'fullscreen' },
  decorators: [
    (Story) => (
      <StoryFleet>
        <Story />
      </StoryFleet>
    ),
  ],
  args: {
    conns: STORY_FLEET_CONNS,
    primary: STORY_FLEET_CONNS[0],
    query: '',
    onQuery: fn(),
    subscriptions: null,
    onOpen: fn(),
    onSearch: fn(),
    isVisible: true,
  },
} satisfies Meta<typeof SessionsScreen>;

export default meta;

type Story = StoryObj<typeof meta>;

/** Four checkouts on one machine, including a paged project on a phone-width rail. */
export const Fleet: Story = {
  decorators: [
    (Story) => (
      <div className="w-[393px]">
        <Story />
      </div>
    ),
  ],
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    await expect(await page.findByText('uberworkspace')).toBeVisible();
    await expect(await page.findByText('svar')).toBeVisible();
    await expect(await page.findByTitle('~/rewrite')).toBeVisible();
    await expect(
      await page.findByRole('navigation', { name: 'Pages of uberworkspace sessions' }),
    ).toBeVisible();
  },
};
