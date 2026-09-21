import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent } from 'storybook/test';

import { STORY_GATEWAYS, STORY_SESSION_ROW, STORY_SESSION_SEARCH_MATCH } from '../dev/story-data';
import { EMPTY_DRAFT_MESSAGE } from '../lib/draft-messages';
import { SessionRow } from './SessionList';

const commands = {
  open: fn(),
  rename: fn(async () => {}),
  requestDelete: fn(),
  toggleStar: fn(),
};

const meta = {
  title: 'Session/Search results',
  component: SessionRow,
  parameters: { layout: 'fullscreen' },
  args: {
    session: STORY_SESSION_ROW,
    group: null,
    draft: EMPTY_DRAFT_MESSAGE,
    conn: STORY_GATEWAYS[0],
    match: STORY_SESSION_SEARCH_MATCH,
    needle: 'windows',
    commands,
    deletion: null,
  },
  render: (args) => (
    <div className="@container">
      <SessionRow {...args} />
      <SessionRow
        {...args}
        session={{ ...args.session, id: 'search-second', favorite_rank: null }}
      />
    </div>
  ),
} satisfies Meta<typeof SessionRow>;

export default meta;
type Story = StoryObj<typeof meta>;

export const MarkdownMatches: Story = {
  play: async ({ canvas, canvasElement, args }) => {
    // Regression, user screenshots: inset preview rules stay lighter than session seams.
    const panels = canvas
      .getAllByRole('list', { name: 'Matching messages' })
      .map((list) => list.parentElement!);
    const [first, second] = panels.map((panel) => panel.parentElement!);
    for (const panel of panels) {
      await expect(getComputedStyle(panel).borderTopWidth).toBe('0px');
      await expect(getComputedStyle(panel).borderBottomWidth).toBe('0px');
      const list = panel.firstElementChild!;
      await expect(getComputedStyle(list).borderTopWidth).toBe('1px');
      await expect(list.getBoundingClientRect().left).toBeGreaterThan(
        panel.getBoundingClientRect().left,
      );
      await expect(list.getBoundingClientRect().right).toBeLessThan(
        panel.getBoundingClientRect().right,
      );
      const hits = [...list.children];
      await expect(getComputedStyle(list).borderTopColor).toBe(
        getComputedStyle(hits[0]).borderBottomColor,
      );
      await expect(getComputedStyle(hits[0]).borderTopWidth).toBe('0px');
      await expect(getComputedStyle(hits[0]).borderBottomWidth).toBe('1px');
      await expect(getComputedStyle(hits.at(-1)!).borderBottomWidth).toBe('0px');
      for (const hit of hits) {
        const excerpt = hit.lastElementChild!;
        const style = getComputedStyle(excerpt);
        await expect(style.webkitLineClamp).toBe('2');
        await expect(excerpt.getBoundingClientRect().height).toBeLessThanOrEqual(
          parseFloat(style.lineHeight) * 2,
        );
      }
    }
    await expect(getComputedStyle(first).borderTopWidth).toBe('0px');
    await expect(getComputedStyle(first).borderBottomWidth).toBe('0px');
    await expect(getComputedStyle(second).borderTopWidth).toBe('1px');
    await expect(first.getBoundingClientRect().bottom).toBe(second.getBoundingClientRect().top);
    await expect(getComputedStyle(second).borderTopColor).not.toBe(
      getComputedStyle(panels[0].firstElementChild!.firstElementChild!).borderBottomColor,
    );
    await expect(canvasElement.querySelectorAll('strong mark')).toHaveLength(4);
    await expect(canvasElement.querySelector('a, img')).toBeNull();
    await expect(canvasElement.scrollWidth).toBeLessThanOrEqual(canvasElement.clientWidth);

    const open = canvasElement.querySelector<HTMLButtonElement>(
      `[data-session-id="${args.session.id}"]`,
    )!;
    await userEvent.click(open);
    await expect(commands.open).toHaveBeenCalledWith(STORY_GATEWAYS[0], STORY_SESSION_ROW.id);
    open.focus();
    await userEvent.keyboard('{Enter}');
    await expect(commands.open).toHaveBeenCalledTimes(2);
  },
};

export const TitleOnlyMatches: Story = {
  args: { match: null, needle: 'design' },
  play: async ({ canvas, canvasElement }) => {
    await expect(canvas.queryByRole('list', { name: 'Matching messages' })).toBeNull();
    await expect(canvasElement.querySelectorAll('[data-session-id]')).toHaveLength(2);
    await expect(canvasElement.querySelectorAll('mark')).toHaveLength(0);
  },
};
