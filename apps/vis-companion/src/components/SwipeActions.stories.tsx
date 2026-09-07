import type { ReactNode } from 'react';
import { expect, fn, userEvent, waitFor } from 'storybook/test';
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
 * Touch opens the scroll-snap drawer. A pointer reveals a reserved trailing slot,
 * with no sideways scroll and no action painted over the row's own controls.
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
  play: async ({ canvas }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Star this session' }));
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
          <Pager page={1} pageCount={5} label={STORY_SESSION.project} onPage={onPage} />
          <NewSessionButton machine={STORY_SESSION.machine} onPress={onCreate} />
        </HeaderActions>
      </div>
    ),
  },
  play: async ({ canvas, canvasElement }) => {
    const create = canvas.getByRole('button', { name: `New session on ${STORY_SESSION.machine}` });
    const next = canvas.getByRole('button', { name: 'Next page' });
    const remove = canvas.getByRole('button', { name: `Delete ${STORY_SESSION.project}` });
    const strip = remove.parentElement!;
    const track = strip.parentElement!;
    const doc = canvasElement.ownerDocument;
    const win = doc.defaultView!;
    await expect(win.getComputedStyle(track).display).toBe('flex');
    if (!win.matchMedia('(min-width: 640px) and (pointer: fine)').matches) {
      // Touch retains the full-width row and the separate, captioned swipe drawer.
      await expect(track.scrollWidth).toBeGreaterThan(track.clientWidth);
      await expect(remove.getBoundingClientRect().width).toBeGreaterThanOrEqual(44);
      return;
    }
    const before = create.getBoundingClientRect();
    await userEvent.hover(create);
    // userEvent dispatches events; focus also reveals the strip in real browser CSS.
    create.focus();
    await waitFor(() => expect(win.getComputedStyle(strip).opacity).toBe('1'));
    await expect(create.getBoundingClientRect().x).toBe(before.x);
    await expect(create.getBoundingClientRect().width).toBe(before.width);
    await expect(track.scrollWidth).toBe(track.clientWidth);
    await expect(
      track.firstElementChild!.firstElementChild!.getBoundingClientRect().right,
    ).toBeLessThanOrEqual(strip.getBoundingClientRect().left);
    // Hit-test the real pixels, not just DOM clicks which ignore covering siblings.
    for (const control of [next, create, remove]) {
      const box = control.getBoundingClientRect();
      await expect(
        control.contains(doc.elementFromPoint(box.x + box.width / 2, box.y + box.height / 2)),
      ).toBe(true);
    }
    await expect(remove.getBoundingClientRect().width).toBeGreaterThanOrEqual(28);
    await expect(remove.getBoundingClientRect().height).toBeGreaterThanOrEqual(28);
    await expect(remove.querySelector('svg')!.getBoundingClientRect().width).toBeLessThan(
      create.querySelector('svg')!.getBoundingClientRect().width,
    );
    await userEvent.click(next);
    await expect(onPage).toHaveBeenCalledWith(2);
    await userEvent.click(create);
    await expect(onCreate).toHaveBeenCalledOnce();
    await expect(onDelete).not.toHaveBeenCalled();
    await userEvent.tab();
    await expect(remove).toHaveFocus();
    await userEvent.keyboard('{Enter}');
    await expect(onDelete).toHaveBeenCalledOnce();
    remove.blur();
    await userEvent.unhover(create);
    await waitFor(() => expect(win.getComputedStyle(strip).opacity).toBe('0'));
    await expect(win.getComputedStyle(strip).pointerEvents).toBe('none');
  },
};

/** The same band keeps its full-width row and captioned drawer on a phone. */
export const ProjectHeaderTouch: Story = {
  ...ProjectHeader,
  globals: { viewport: { value: 'phone', isRotated: false } },
};
