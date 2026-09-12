import type { ReactNode } from 'react';
import { expect, fn, userEvent, within } from 'storybook/test';
import type { Meta, StoryObj } from '@storybook/react-vite';
import { SESSION_VERBS, STORY_SESSION } from '../dev/story-data';
import { PencilIcon, StarIcon, TrashIcon } from './icons';
import { SwipeActions, type SwipeAction } from './SwipeActions';
import { ListRow } from './ui';
import {
  HeaderActions,
  NewSessionButton,
  Pager,
  ProjectCrumb,
  SectionHeader,
} from './SessionNavigator';

/**
 * A ROW'S OWN VERBS, WAITING UNDER ITS RIGHT EDGE UNTIL IT IS SLID.
 *
 * This is the app's ONE row-verb surface, and one drawer is open at a time in the
 * whole app. The strip is drawn here because what it costs is width: three cells
 * at 72px each is most of a phone, so the caption on a cell stays one word and the
 * whole sentence lives in `name`, for a reader who cannot see the row.
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

const onCreate = fn();
const onPage = fn();
const onDelete = fn();

/** Regression: desktop hover must not put Delete over the project's pager or +. */
export const ProjectHeader: Story = {
  globals: { viewport: { value: 'desktop', isRotated: false } },
  decorators: [
    (Story) => (
      <div className="@container max-w-sm">
        <SectionHeader>
          <div className="grid min-w-0 flex-1">
            <Story />
          </div>
          <div className="px-3 pb-2 pt-1 sm:px-4">
            <Pager page={1} pageCount={5} label={STORY_SESSION.project} onPage={onPage} />
          </div>
        </SectionHeader>
      </div>
    ),
  ],
  args: {
    label: STORY_SESSION.project,
    actions: [
      {
        key: 'delete',
        label: 'Delete',
        name: `Delete ${STORY_SESSION.project}`,
        icon: MARKS.delete,
        tone: 'danger',
        onSelect: onDelete,
      },
    ],
    children: (
      <div className="flex bg-level-project">
        <ProjectCrumb
          name={STORY_SESSION.project}
          qualifier={STORY_SESSION.where}
          disclosure={null}
        />
      </div>
    ),
    trailing: (
      <div className="flex bg-level-project">
        <HeaderActions align="center">
          <NewSessionButton machine={STORY_SESSION.machine} onPress={onCreate} />
        </HeaderActions>
      </div>
    ),
  },
  play: async ({ canvas, canvasElement }) => {
    const create = canvas.getByRole('button', { name: `New session on ${STORY_SESSION.machine}` });
    const second = canvas.getByRole('button', { name: 'Page 2' });
    const track = create.closest<HTMLElement>('[data-swipe-track]')!;
    const doc = canvasElement.ownerDocument;
    const win = doc.defaultView!;
    if (!win.matchMedia('(min-width: 640px) and (pointer: fine)').matches) {
      await expect(track.scrollWidth).toBeGreaterThan(track.clientWidth);
      return;
    }
    const trigger = canvas.getByRole('button', { name: `Actions for ${STORY_SESSION.project}` });
    const before = create.getBoundingClientRect();
    await expect(track.scrollWidth).toBe(track.clientWidth);
    for (const control of [second, create, trigger]) {
      const box = control.getBoundingClientRect();
      await expect(box.width).toBeGreaterThanOrEqual(28);
      await expect(box.height).toBeGreaterThanOrEqual(28);
      await expect(
        control.contains(doc.elementFromPoint(box.x + box.width / 2, box.y + box.height / 2)),
      ).toBe(true);
    }
    await userEvent.click(second);
    await expect(onPage).toHaveBeenCalledWith(2);
    await userEvent.click(create);
    await expect(onCreate).toHaveBeenCalledOnce();
    await expect(onDelete).not.toHaveBeenCalled();
    trigger.focus();
    await userEvent.keyboard('{Enter}');
    const menu = within(doc.body).getByRole('dialog');
    const remove = within(menu).getByRole('button', { name: `Delete ${STORY_SESSION.project}` });
    await expect(remove).toHaveFocus();
    await userEvent.keyboard('{Escape}');
    await expect(trigger).toHaveFocus();
    await userEvent.click(trigger);
    await userEvent.click(
      within(within(doc.body).getByRole('dialog')).getByRole('button', {
        name: `Delete ${STORY_SESSION.project}`,
      }),
    );
    await expect(onDelete).toHaveBeenCalledOnce();
    await expect(within(doc.body).queryByRole('dialog')).not.toBeInTheDocument();
    await expect(create.getBoundingClientRect().x).toBe(before.x);
  },
};

/** The same band keeps its full-width row and captioned drawer on a phone. */
export const ProjectHeaderTouch: Story = {
  ...ProjectHeader,
  globals: { viewport: { value: 'phone', isRotated: false } },
};
