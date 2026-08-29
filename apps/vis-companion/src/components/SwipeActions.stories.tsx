import type { ReactNode } from 'react';
import { fn } from 'storybook/test';
import type { Meta, StoryObj } from '@storybook/react-vite';
import { SESSION_VERBS, STORY_SESSION } from '../dev/story-data';
import { PencilIcon, StarIcon, TrashIcon } from './icons';
import { SwipeActions, type SwipeAction } from './SwipeActions';
import { ListRow } from './ui';

/**
 * A ROW'S OWN VERBS, WAITING UNDER ITS RIGHT EDGE UNTIL IT IS SLID.
 *
 * This is the app's ONE row-verb surface, and one drawer is open at a time in the
 * whole app. The strip is drawn here because what it costs is width: three cells
 * at 72px each is most of a phone, so the caption on a cell stays one word and the
 * whole sentence lives in `name`, for a reader who cannot see the row.
 *
 * Slide the row in the frame to read it — the drawer is a scroll track with snap
 * points, not an animation, so it answers a trackpad and a thumb the same way.
 */
const MARKS: Record<string, ReactNode> = {
  star: <StarIcon className="size-4" />,
  rename: <PencilIcon className="size-4" />,
  delete: <TrashIcon className="size-4" />,
};

const actions: SwipeAction[] = SESSION_VERBS.map((verb) => ({
  key: verb.key,
  label: verb.label,
  name: verb.name,
  tone: verb.tone,
  icon: MARKS[verb.key],
  onSelect: fn(),
}));

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
};

/** One verb: the cell keeps its 72px, so the strip never reads as a half-open row. */
export const OneVerb: Story = {
  args: {
    label: 'Actions for mini',
    actions: [actions[0]],
    children: <ListRow>mini — not answering since 11:20</ListRow>,
  },
};

/** A selected row keeps its own paper under the drawer that opens over it. */
export const SelectedRow: Story = {
  args: {
    label: `Actions for ${STORY_SESSION.title}`,
    actions,
    children: <ListRow isSelected>{STORY_SESSION.title}</ListRow>,
  },
};
