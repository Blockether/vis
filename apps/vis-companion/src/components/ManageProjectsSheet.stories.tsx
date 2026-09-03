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
    await userEvent.click(page.getByRole('button', { name: /^vis/i }));
    await userEvent.click(page.getByRole('button', { name: 'Close projects on tower' }));
    await expect(args.onChoose).toHaveBeenCalledWith(STORY_PROJECTS[0].root);
    await expect(args.onCancel).toHaveBeenCalledOnce();
  },
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
