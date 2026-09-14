import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { STORY_INERT_CLIENT, STORY_PROJECTS } from '../dev/story-data';
import { ManageProjectsSheet } from './ManageProjectsSheet';

/** The inventory opens before the filesystem: what this machine already has is the first answer. */
const meta = {
  title: 'Components/Manage projects sheet',
  component: ManageProjectsSheet,
  args: {
    label: 'tower',
    at: null,
    client: STORY_INERT_CLIENT,
    startAt: STORY_PROJECTS[0].root,
    knownRoots: new Set(STORY_PROJECTS.map((project) => project.root)),
    projects: STORY_PROJECTS,
    onCancel: () => {},
    onChoose: () => {},
    onRemove: () => {},
  },
} satisfies Meta<typeof ManageProjectsSheet>;

export default meta;
type Story = StoryObj<typeof meta>;

const choose = fn();
const close = fn();

/** Existing projects, one current and one settled, with both main exits exercised. */
export const Inventory: Story = {
  args: { onChoose: choose, onCancel: close },
  play: async ({ args, canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    const panel = page.getByRole('dialog', { name: 'Manage projects on tower' });
    const inventory = panel.lastElementChild!;
    // A short project list must fit without either scrollbar, including the trailing actions.
    await expect(inventory.scrollWidth).toBe(inventory.clientWidth);
    await expect(inventory.scrollHeight).toBe(inventory.clientHeight);
    for (const action of page.getAllByRole('button', { name: /^Remove every transcript/ })) {
      const box = action.getBoundingClientRect();
      const row = action.parentElement!.getBoundingClientRect();
      const icon = action.querySelector('svg')!.getBoundingClientRect();
      // Regression: deletion must be an inset circle, not a stretched edge cell.
      await expect(box.width).toBe(box.height);
      await expect(parseFloat(getComputedStyle(action).borderRadius)).toBeGreaterThanOrEqual(
        box.width / 2,
      );
      await expect(row.right - box.right).toBeGreaterThanOrEqual(8);
      await expect(box.top - row.top).toBeGreaterThanOrEqual(8);
      await expect(Math.abs(icon.x + icon.width / 2 - (box.x + box.width / 2))).toBeLessThan(1);
      await expect(Math.abs(icon.y + icon.height / 2 - (box.y + box.height / 2))).toBeLessThan(1);
    }
    await userEvent.click(page.getByRole('button', { name: /^vis/i }));
    await userEvent.click(page.getByRole('button', { name: 'Close projects on tower' }));
    await expect(args.onChoose).toHaveBeenCalledWith(STORY_PROJECTS[0].root);
    await expect(args.onCancel).toHaveBeenCalledOnce();
  },
};

/** The desktop gutter must fit just as it does in the phone sheet. */
export const DesktopInventory: Story = {
  ...Inventory,
  globals: { viewport: { value: 'desktop', isRotated: false } },
  args: { ...Inventory.args, at: { top: 46, left: 34 } },
};

/** Project removal stays in the selected row and never grows into a second line. */
export const DeleteConfirmation: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await userEvent.click(
      page.getByRole('button', {
        name: `Remove every transcript in ${STORY_PROJECTS[0].name}`,
      }),
    );
    const question = page.getByRole('group', {
      name: `Delete ${STORY_PROJECTS[0].name}?`,
    });
    await expect(question.querySelector('p')).toBeNull();
    await expect(page.getByRole('button', { name: 'No, keep' })).toBeVisible();
    await expect(page.getByRole('button', { name: 'Yes, delete' })).toBeVisible();
  },
};
