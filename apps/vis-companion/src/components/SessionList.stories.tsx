import { useState } from 'react';
import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, waitFor, within } from 'storybook/test';

import {
  STORY_GATEWAYS,
  STORY_SESSION,
  STORY_SESSION_ROW,
  STORY_SESSION_USAGE,
} from '../dev/story-data';
import { EMPTY_DRAFT_MESSAGE } from '../lib/draft-messages';
import { SessionRow } from './SessionList';

const onOpen = fn();
const commands = {
  open: onOpen,
  rename: fn(async () => {}),
  requestDelete: fn(),
  toggleStar: fn(),
};

const meta = {
  title: 'Session/Navigator row',
  component: SessionRow,
  parameters: { layout: 'fullscreen' },
  decorators: [
    (Story) => (
      <div className="@container">
        <Story />
      </div>
    ),
  ],
  args: {
    session: STORY_SESSION_ROW,
    draft: EMPTY_DRAFT_MESSAGE,
    conn: STORY_GATEWAYS[0],
    match: null,
    needle: '',
    commands,
    deletion: null,
  },
} satisfies Meta<typeof SessionRow>;

export default meta;
type Story = StoryObj<typeof meta>;

export const AwaitingInput: Story = {
  play: async ({ canvas }) => {
    const openSession = canvas.getByText(STORY_SESSION.title).closest('button');
    await expect(openSession).not.toBeNull();
    await userEvent.click(openSession!);
    await expect(onOpen).toHaveBeenCalledWith(STORY_GATEWAYS[0], STORY_SESSION_ROW.id);
  },
};

export const Renaming: Story = {
  play: async ({ canvas, canvasElement }) => {
    const pointer = canvasElement.ownerDocument.defaultView!.matchMedia(
      '(min-width: 640px) and (pointer: fine)',
    ).matches;
    if (pointer) {
      await userEvent.click(
        canvas.getByRole('button', { name: `Actions for ${STORY_SESSION_ROW.title}` }),
      );
      const menu = within(canvasElement.ownerDocument.body).getByRole('dialog');
      await userEvent.click(within(menu).getByRole('button', { name: 'Rename' }));
    } else {
      const actions = canvas.getByRole('group', { name: `${STORY_SESSION_ROW.title} actions` });
      const rename = within(actions).getByRole('button', { name: 'Rename' });
      rename.focus();
      await userEvent.keyboard('{Enter}');
    }
    await expect(
      await canvas.findByRole('textbox', {
        name: `Rename ${STORY_SESSION_ROW.title}`,
      }),
    ).toHaveValue(STORY_SESSION_ROW.title);
    await expect(canvas.getByText(STORY_SESSION_ROW.id)).toBeInTheDocument();
    await expect(canvas.getByText(`${STORY_SESSION_ROW.turn_count} turns`)).toBeInTheDocument();
    await expect(canvas.getByText('INPUT NEEDED')).toBeInTheDocument();
    await expect(
      canvas.getByRole('button', {
        name: `Show details for ${STORY_SESSION_ROW.title}`,
      }),
    ).toBeInTheDocument();
    await expect(
      canvas.queryByRole('group', {
        name: `${STORY_SESSION_ROW.title} actions`,
      }),
    ).not.toBeInTheDocument();
    await expect(canvas.queryByRole('dialog')).not.toBeInTheDocument();
  },
};

/** Production rows with local favorite state standing in for their gateway-owned props. */
export const Favorites: Story = {
  beforeEach: () => {
    const previous = globalThis.fetch;
    globalThis.fetch = async () =>
      new Response(JSON.stringify({ usage: STORY_SESSION_USAGE }), {
        headers: { 'Content-Type': 'application/json' },
      });
    return () => {
      globalThis.fetch = previous;
    };
  },
  render: function FavoriteRows(args) {
    const [favorites, setFavorites] = useState([args.session.id]);
    const rows = [
      { ...args.session, title: 'Review the session list layout' },
      {
        ...args.session,
        id: 'plain-row',
        title: 'A longer session title uses the available width',
      },
    ];
    return (
      <>
        {rows.map((session) => (
          <SessionRow
            {...args}
            key={session.id}
            session={{
              ...session,
              favorite_rank: favorites.includes(session.id) ? 1 : null,
              is_awaiting_input: false,
            }}
            commands={{
              ...args.commands,
              toggleStar: (row, conn) => {
                args.commands.toggleStar(row, conn);
                setFavorites((current) =>
                  current.includes(row.id)
                    ? current.filter((id) => id !== row.id)
                    : [...current, row.id],
                );
              },
            }}
          />
        ))}
      </>
    );
  },
  play: async ({ canvasElement }) => {
    const win = canvasElement.ownerDocument.defaultView!;
    const rows = Array.from(canvasElement.querySelectorAll<HTMLElement>('[data-session-id]'));
    const favorite = (row: HTMLElement) =>
      row.querySelector<HTMLElement>('[data-session-favorite-slot]')!;
    const status = (row: HTMLElement) => row.querySelector<HTMLElement>('[data-session-status]')!;
    const title = (row: HTMLElement) => row.querySelector<HTMLElement>('[title]')!;
    const boxes = () =>
      rows.map((row) => ({
        title: title(row).parentElement!.getBoundingClientRect().toJSON(),
        status: status(row).getBoundingClientRect().toJSON(),
        slot: favorite(row).getBoundingClientRect().toJSON(),
      }));
    const before = boxes();
    for (const row of rows) {
      const track = row.closest<HTMLElement>('[data-swipe-track]')!;
      const disclosure = within(track).getByRole('button', { name: /^Show details/ });
      const leftInset =
        title(row).getBoundingClientRect().left - track.getBoundingClientRect().left;
      const rightInset =
        track.getBoundingClientRect().right -
        disclosure.querySelector('svg')!.getBoundingClientRect().right;
      await expect(leftInset).toBe(16);
      await expect(Math.abs(rightInset - leftInset)).toBeLessThanOrEqual(1);
      await expect(favorite(row).nextElementSibling).toBe(status(row));
      await expect(favorite(row).getBoundingClientRect().right + 8).toBe(
        status(row).getBoundingClientRect().left,
      );
      await expect(title(row).getBoundingClientRect().right).toBeLessThanOrEqual(
        favorite(row).getBoundingClientRect().left,
      );
    }
    const star = favorite(rows[0]).querySelector('svg')!;
    await expect(win.getComputedStyle(star).fill).toBe(win.getComputedStyle(star).stroke);
    await expect(favorite(rows[1]).querySelector('svg')).toBeNull();

    const track = rows[0].closest<HTMLElement>('[data-swipe-track]')!;
    for (const verb of ['Unstar', 'Star']) {
      if (win.matchMedia('(min-width: 640px) and (pointer: fine)').matches) {
        await userEvent.click(within(track).getByRole('button', { name: /^Actions for/ }));
        const menu = within(canvasElement.ownerDocument.body).getByRole('dialog');
        await userEvent.click(within(menu).getByRole('button', { name: verb }));
      } else {
        const actions = within(track).getByRole('group', { name: / actions$/ });
        within(actions).getByRole('button', { name: verb }).focus();
        await userEvent.keyboard('{Enter}');
      }
      await expect(Boolean(favorite(rows[0]).querySelector('svg'))).toBe(verb === 'Star');
      // Closing the touch action drawer scrolls the row back asynchronously.
      await waitFor(() => expect(boxes()).toEqual(before));
    }

    await userEvent.click(within(track).getByRole('button', { name: /^Show details/ }));
    await expect(await within(canvasElement).findByText('≈98%')).toBeVisible();
    await userEvent.click(within(track).getByRole('button', { name: /^Hide details/ }));
    await waitFor(() => expect(boxes()).toEqual(before));
  },
};

export const FavoritesNarrow: Story = {
  ...Favorites,
  decorators: [
    (Story) => (
      <div className="@container w-[393px] max-w-full">
        <Story />
      </div>
    ),
  ],
};
