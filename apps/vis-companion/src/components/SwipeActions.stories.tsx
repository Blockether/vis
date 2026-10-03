import type { ReactNode } from 'react';
import { expect, fn, userEvent, within } from 'storybook/test';
import type { Meta, StoryObj } from '@storybook/react-vite';
import { SESSION_VERBS, STORY_SESSION } from '../dev/story-data';
import { ArchiveIcon, FolderPlusIcon, ForkIcon, PencilIcon, StarIcon, TrashIcon } from './icons';
import { SwipeActions, type SwipeAction } from './SwipeActions';
import { ListRow } from './ui';

/**
 * A ROW'S OWN VERBS, WAITING UNDER ITS RIGHT EDGE UNTIL IT IS SLID.
 *
 * The action cells share the available row width, up to 72px each.
 * Keep captions short. Use `name` for the full accessible action name.
 *
 * Touch opens the scroll-snap drawer. A pointer opens a vertical-dot dropdown,
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
const staticActions = actions.map((action) => ({ ...action, onSelect: () => {} }));

const meta = {
  title: 'Components/Swipe actions',
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

/** One verb: the cell keeps its 72px, so the strip never reads as a half-open row. */
export const OneVerb: Story = {
  args: {
    label: 'Actions for mini',
    actions: staticActions.slice(0, 1),
    children: <ListRow>mini — not answering since 11:20</ListRow>,
  },
};

/** A selected row keeps its own paper beside the action slot. */
export const SelectedRow: Story = {
  args: {
    label: `Actions for ${STORY_SESSION.title}`,
    actions: staticActions,
    children: <ListRow isSelected>{STORY_SESSION.title}</ListRow>,
  },
};

/** All session actions must fit before the first swipe, including on narrow phones. */
export const FullMenu: Story = {
  parameters: { layout: 'fullscreen' },
  args: {
    label: 'A session with all actions',
    actions: [
      {
        key: 'star', label: 'Star', icon: <StarIcon className="size-4" />,
        tone: 'accent', onSelect: () => {},
      },
      { key: 'rename', label: 'Rename', icon: <PencilIcon className="size-4" />, onSelect: () => {} },
      { key: 'fork', label: 'Fork', icon: <ForkIcon className="size-4" />, onSelect: () => {} },
      { key: 'move', label: 'Move to...', icon: <FolderPlusIcon className="size-4" />, onSelect: () => {} },
      { key: 'archive', label: 'Unarchive', icon: <ArchiveIcon className="size-4" />, onSelect: () => {} },
      {
        key: 'delete', label: 'Delete', icon: <TrashIcon className="size-4" />,
        tone: 'danger', onSelect: () => {},
      },
    ],
    children: <ListRow>Review the session list layout</ListRow>,
  },
  play: async ({ canvas, canvasElement }) => {
    const strip = canvas.getByRole('group', { hidden: true });
    const track = canvasElement.querySelector<HTMLElement>('[data-swipe-track]')!;
    const buttons = Array.from(strip.querySelectorAll('button'));
    await expect(buttons).toHaveLength(6);
    // jsdom has no layout. Browser Storybook also checks the actual cell geometry.
    if (!track.clientWidth || getComputedStyle(strip).display === 'none') return;
    const widths = buttons.map((button) => button.getBoundingClientRect().width);
    const height = track.getBoundingClientRect().height;
    await expect(strip.getBoundingClientRect().width).toBeLessThanOrEqual(track.clientWidth);
    track.scrollTo({ left: track.scrollWidth, behavior: 'instant' });
    await new Promise<void>((resolve) => {
      requestAnimationFrame(() => requestAnimationFrame(() => resolve()));
    });
    await expect(buttons.map((button) => button.getBoundingClientRect().width)).toEqual(widths);
    await expect(track.getBoundingClientRect().height).toBe(height);
    const bounds = track.getBoundingClientRect();
    await expect(buttons[0].getBoundingClientRect().left).toBeGreaterThanOrEqual(bounds.left);
    await expect(buttons.at(-1)!.getBoundingClientRect().right).toBeCloseTo(bounds.right, 0);
  },
};
