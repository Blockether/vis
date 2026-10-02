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

/** The desktop gutter must fit just as it does on a phone. */
export const DesktopInventory: Story = {
  ...Inventory,
  globals: { viewport: { value: 'desktop', isRotated: false } },
  args: { ...Inventory.args, at: { top: 46, left: 34 } },
};

/** Deletion keeps the measured row, the next project and the sheet in place on both pointer faces. */
export const DeleteConfirmation: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    const trash = page.getByRole('button', {
      name: `Remove every transcript in ${STORY_PROJECTS[0].name}`,
    });
    const rowHeight = trash.parentElement!.getBoundingClientRect().height;
    const panel = page.getByRole('dialog', { name: 'Manage projects on tower' });
    const panelHeight = panel.getBoundingClientRect().height;
    const nextProject = page.getByRole('button', { name: /^demo/i });
    const nextOffset = nextProject.getBoundingClientRect().top - panel.getBoundingClientRect().top;
    await userEvent.click(trash);
    const question = page.getByRole('group', {
      name: `Delete ${STORY_PROJECTS[0].name}?`,
    });
    await expect(question.querySelector('p')).toBeNull();
    await expect(page.getByRole('button', { name: 'No, keep' })).toBeVisible();
    await expect(page.getByRole('button', { name: 'Yes, delete' })).toBeVisible();
    await expect(question).toHaveStyle({ minHeight: `${rowHeight}px` });
    // jsdom has no layout; the same story checks geometry and painted edges in the browser.
    if (rowHeight > 0) {
      await expect(question.getBoundingClientRect().height).toBe(rowHeight);
      await expect(panel.getBoundingClientRect().height).toBe(panelHeight);
      await expect(
        nextProject.getBoundingClientRect().top - panel.getBoundingClientRect().top,
      ).toBeCloseTo(nextOffset, 3);
      const edges = getComputedStyle(question, '::after');
      await expect(parseFloat(edges.borderLeftWidth)).toBe(0);
      await expect(parseFloat(edges.borderRightWidth)).toBe(0);
    }
  },
};

/** Breadcrumbs keep the platform's target height without growing their path band. */
export const Browsing: Story = {
  args: {
    isAdding: true,
    startAt: '/home/developer/work/vis/src',
    client: new Proxy(STORY_INERT_CLIENT, {
      get(target, key) {
        if (key !== 'browse') return Reflect.get(target, key);
        return async () => ({
          path: '/home/developer/work/vis',
          parent: '/home/developer/work',
          home: '/home/developer',
          is_truncated: false,
          entries: [],
        });
      },
    }),
  },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await page.findByRole('button', { name: 'vis' });
  },
};

export const BrowsingPointer: Story = {
  ...Browsing,
  tags: ['!test'],
  globals: { viewport: { value: 'desktop', isRotated: false } },
};
