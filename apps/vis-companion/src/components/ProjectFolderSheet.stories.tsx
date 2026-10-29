import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { STORY_INERT_CLIENT, STORY_PROJECT_ROOTS } from '../dev/story-data';
import { ProjectFolderSheet } from './ProjectFolderSheet';

/** A client whose folder browser always answers with one empty folder. */
const browsing = (path: string, parent: string) =>
  new Proxy(STORY_INERT_CLIENT, {
    get(target, key) {
      if (key !== 'browse') return Reflect.get(target, key);
      return async () => ({ path, parent, home: '/home/developer', is_truncated: false, entries: [] });
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
