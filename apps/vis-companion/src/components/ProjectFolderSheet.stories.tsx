import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { STORY_INERT_CLIENT, STORY_PROJECT_ROOTS } from '../dev/story-data';
import { ProjectFolderSheet } from './ProjectFolderSheet';
import type { BrowseEntry } from '../lib/types';

/** A client whose folder browser always answers with one folder, empty unless it gets entries. */
const browsing = (path: string, parent: string, entries: BrowseEntry[] = []) =>
  new Proxy(STORY_INERT_CLIENT, {
    get(target, key) {
      if (key !== 'browse') return Reflect.get(target, key);
      return async () => ({ path, parent, home: '/home/developer', is_truncated: false, entries });
    },
  });

/** The folder browser that adds a project, or gives an existing project another folder. */
const meta = {
  title: 'Session list/Projects/Folder picker',
  component: ProjectFolderSheet,
  args: {
    label: 'tower',
    mode: 'add',
    at: null,
    client: browsing('/home/developer', '/home'),
    startAt: STORY_PROJECT_ROOTS[0],
    knownRoots: new Set(STORY_PROJECT_ROOTS),
    onCancel: () => {},
    onChoose: () => {},
  },
} satisfies Meta<typeof ProjectFolderSheet>;

export default meta;
type Story = StoryObj<typeof meta>;

const close = fn();

/** A new project starts on the folder browser, and its band holds the way out. */
export const Adding: Story = {
  args: { onCancel: close },
  play: async ({ args, canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await expect(page.getByText('New project')).toBeVisible();
    await userEvent.click(page.getByRole('button', { name: 'Close new project on tower' }));
    await expect(args.onCancel).toHaveBeenCalledOnce();
  },
};

/** Breadcrumbs keep the platform's target height without growing their path band. */
export const Browsing: Story = {
  args: {
    startAt: '/home/developer/work/vis/src',
    client: browsing('/home/developer/work/vis', '/home/developer/work'),
  },
};

/** A name that no folder here has puts its create button in the field. */
export const Creating: Story = {
  args: {
    startAt: '/home/developer/work/vis',
    client: browsing('/home/developer/work', '/home/developer', [
      { name: 'vis', path: '/home/developer/work/vis', entry_count: 12, is_repo: true, branch: 'main' },
      { name: 'notes', path: '/home/developer/work/notes', entry_count: 3, is_repo: false },
    ]),
  },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await userEvent.type(await page.findByLabelText('Filter folders or type a path'), 'fresh');
    await expect(page.getByRole('button', { name: 'Create project ~/work/fresh' })).toBeVisible();
  },
};
