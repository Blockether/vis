import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { STORY_FLEET_CONNS, storyFleetFetch } from '../dev/story-data';
import { SessionsScreen } from './SessionsScreen';

const meta = {
  title: 'Screens/Project hierarchy',
  component: SessionsScreen,
  parameters: { layout: 'fullscreen' },
  decorators: [
    (Story) => (
      <div className="flex h-dvh w-full max-w-120 flex-col bg-page">
        <Story />
      </div>
    ),
  ],
  beforeEach: () => {
    const previous = globalThis.fetch;
    globalThis.fetch = storyFleetFetch();
    return () => {
      globalThis.fetch = previous;
    };
  },
  args: {
    conns: STORY_FLEET_CONNS,
    primary: STORY_FLEET_CONNS[0],
    query: '',
    onQuery: fn(),
    subscriptions: null,
    onOpen: fn(),
    onSearch: fn(),
    isSearchOpen: false,
    onCloseSearch: fn(),
    isVisible: true,
  },
} satisfies Meta<typeof SessionsScreen>;

export default meta;
type Story = StoryObj<typeof meta>;

export const GroupedSessions: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const title = await page.findByText('infrastructure');
    const group = title.closest<HTMLElement>('[data-project-root]')!;
    const header = title.closest('header')!;
    const collapsed = page.queryByRole('button', { name: 'Expand infrastructure' });
    if (collapsed) await userEvent.click(collapsed);
    await within(group).findByText('Rotate the relay signing key');
    await expect(within(header).queryByRole('button', { name: /^Actions for/ })).toBeNull();
    await expect(header.querySelector('[data-swipe-track]')).toBeNull();

    // Regression: repeated project controls and row controls must stay unframed. Each set
    // under the band carries its own menu, so the menus are found in the group.
    const menus = within(group).getAllByRole('button', { name: /^Actions for (groups|sessions) in / });
    await expect(menus).toHaveLength(2);
    const rows = header.nextElementSibling!;
    // THE SET CARRIES THE LINE between the band and its page: the word is ruled on both
    // edges, the first session under it adds none, and an internal row wears the row's own
    // hairline instead of the band's.
    const set = within(rows as HTMLElement).getByText('Sessions').parentElement!;
    await expect(set.parentElement!.querySelectorAll('[data-session-id]').length).toBeGreaterThan(
      1,
    );
    await userEvent.click(page.getByRole('button', { name: 'Collapse infrastructure' }));
    await expect(group.querySelector('[data-session-id]')).toBeNull();
    await expect(within(header).getByRole('button', { name: 'Expand infrastructure' })).toBeEnabled();
    await userEvent.click(page.getByRole('button', { name: 'Expand infrastructure' }));
    await within(group).findByText('Rotate the relay signing key');
  },
};

export const GroupedSessionsDark: Story = {
  ...GroupedSessions,
  tags: ['!test'],
  globals: { theme: 'blockether-dark' },
};
