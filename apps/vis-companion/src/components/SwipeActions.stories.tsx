import type { ReactNode } from 'react';
import { expect, fn, userEvent, within } from 'storybook/test';
import type { Meta, StoryObj } from '@storybook/react-vite';
import { SESSION_VERBS, STORY_SESSION } from '../dev/story-data';
import { PencilIcon, StarIcon, TrashIcon } from './icons';
import { SwipeActions, type SwipeAction } from './SwipeActions';
import { ListRow } from './ui';

/**
 * A ROW'S OWN VERBS, WAITING UNDER ITS RIGHT EDGE UNTIL IT IS SLID.
 *
 * The action cells share the available row width, up to 72px each.
 * Keep captions short. Use `name` for the full accessible action name.
 *
 * Touch slides the row open to show the drawer. A pointer opens a vertical-dot dropdown,
 * reserving only one target beside the row's permanent controls.
 */
const MARKS: Record<string, ReactNode> = {
  star: <StarIcon className="size-4" />,
  rename: <PencilIcon className="size-4" />,
  delete: <TrashIcon className="size-4" />,
};

const onStar = fn();
const actions: SwipeAction[] = SESSION_VERBS.map((verb) => ({
  key: verb.key,
  label: verb.label,
  name: verb.name,
  tone: verb.tone,
  icon: MARKS[verb.key],
  onSelect: verb.key === 'star' ? onStar : () => {},
}));
const meta = {
  title: 'Session list/Swipe actions',
  component: SwipeActions,
  parameters: { layout: 'padded' },
} satisfies Meta<typeof SwipeActions>;

export default meta;

type Story = StoryObj<typeof meta>;

/** Star, rename, delete — the three a session row carries. */
export const SessionRow: Story = {
  args: {
    label: `Actions for ${STORY_SESSION.title}`,
    actions,
    children: <ListRow>{STORY_SESSION.title}</ListRow>,
  },
  play: async ({ canvas, canvasElement }) => {
    const pointer = canvasElement.ownerDocument.defaultView!.matchMedia(
      '(min-width: 640px) and (pointer: fine)',
    ).matches;
    if (pointer) {
      await userEvent.click(canvas.getByRole('button', { name: /^Actions for/ }));
      const menu = within(canvasElement.ownerDocument.body).getByRole('dialog');
      await userEvent.click(within(menu).getByRole('button', { name: 'Star this session' }));
    } else {
      await userEvent.click(canvas.getByRole('button', { name: 'Star this session' }));
    }
    await expect(onStar).toHaveBeenCalledOnce();
  },
};
