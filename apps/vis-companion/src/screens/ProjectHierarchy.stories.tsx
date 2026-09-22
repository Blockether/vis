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
    isVisible: true,
  },
} satisfies Meta<typeof SessionsScreen>;

export default meta;
type Story = StoryObj<typeof meta>;

export const GroupedSessions: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const title = await page.findByText('infrastructure', {}, { timeout: 5000 });
    const group = title.closest<HTMLElement>('[data-project-root]')!;
    const header = title.closest('header')!;
    const win = canvasElement.ownerDocument.defaultView!;
    const style = (element: Element) => win.getComputedStyle(element);
    const collapsed = page.queryByRole('button', { name: 'Expand infrastructure' });
    if (collapsed) await userEvent.click(collapsed);
    const sessionTitle = await within(group).findByText(
      'Rotate the relay signing key',
      {},
      { timeout: 5000 },
    );
    const session = sessionTitle.closest<HTMLElement>('[data-session-id]')!;
    const rowHeight = session.getBoundingClientRect().height;
    await expect(within(header).queryByRole('button', { name: /^Actions for/ })).toBeNull();
    await expect(header.querySelector('[data-swipe-track]')).toBeNull();

    // Regression: repeated project pluses and row controls must stay unframed. The project's
    // own plus stands on the set it creates in, under the band, so it is found in the group.
    const create = within(group).getByRole('button', { name: /^New session on / });
    const face = style(create);
    // Regression: the plus must use the header's neutral ink, not the theme's accent.
    await expect(face.color).toBe(style(title).color);
    const box = create.getBoundingClientRect();
    await expect(box.width).toBe(box.height);
    await expect(parseFloat(face.borderRadius)).toBe(0);
    for (const side of ['Top', 'Right', 'Bottom', 'Left'] as const) {
      await expect(parseFloat(face[`border${side}Width`])).toBe(0);
    }

    // Regression: project names and full-width row rules previously had nearly
    // the same visual weight. Hierarchy must survive without relying on hue.
    await expect(Number(style(title).fontWeight)).toBeGreaterThan(
      Number(style(sessionTitle).fontWeight),
    );
    await expect(Number(style(title).fontWeight)).toBeGreaterThanOrEqual(700);
    const rows = header.nextElementSibling!;
    // THE SET CARRIES THE LINE between the band and its page: the word is ruled on both
    // edges, the first session under it adds none, and an internal row wears the row's own
    // hairline instead of the band's.
    const set = within(rows as HTMLElement).getByText('Sessions').parentElement!;
    await expect(set.parentElement!.querySelectorAll('[data-session-id]').length).toBeGreaterThan(
      1,
    );
    await expect(style(set).borderTopWidth).toBe('1px');
    const firstRow = set.nextElementSibling!;
    const internalRow = firstRow.nextElementSibling!;
    await expect(style(internalRow).borderTopColor).not.toBe(style(header).borderTopColor);
    await expect(style(internalRow).borderTopWidth).toBe(style(header).borderTopWidth);
    // The gray band closes once, even while open.
    await expect(style(header).borderBottomWidth).toBe('1px');
    await expect(style(rows).borderTopWidth).toBe('0px');
    await expect(style(firstRow).borderTopWidth).toBe('0px');
    const nextGroup = group.nextElementSibling!;
    await expect(parseFloat(style(nextGroup).paddingTop)).toBeGreaterThan(0);

    // Folding changes the list, not the heading's surface or its actions.
    const background = style(header).backgroundColor;
    await userEvent.click(page.getByRole('button', { name: 'Collapse infrastructure' }));
    await expect(group.querySelector('[data-session-id]')).toBeNull();
    await expect(style(header).backgroundColor).toBe(background);
    await expect(style(header).borderBottomWidth).toBe('1px');
    await expect(within(header).getByRole('button', { name: /^New session/ })).toBeEnabled();
    await userEvent.click(page.getByRole('button', { name: 'Expand infrastructure' }));
    const reopened = await within(group).findByText('Rotate the relay signing key');
    await expect(style(header).borderBottomWidth).toBe('1px');
    await expect(reopened.closest('[data-session-id]')!.getBoundingClientRect().height).toBe(
      rowHeight,
    );
  },
};

export const GroupedSessionsDark: Story = {
  ...GroupedSessions,
  globals: { theme: 'blockether-dark' },
};
