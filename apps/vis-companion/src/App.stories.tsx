import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent } from 'storybook/test';
import { Header } from './App';
import { Button, Input } from './components/ui';
import { liveViewFromWire, type LiveView } from './lib/live-view';

const meta = {
  title: 'App/Header',
  component: Header,
  args: { onSearch: fn(), onAppSettings: fn() },
  render: (args) => (
    <>
      <Header {...args} />
      <div className="p-4">
        <Input aria-label="Reference form field" placeholder="Project name" />
      </div>
    </>
  ),
} satisfies Meta<typeof Header>;

export default meta;
type Story = StoryObj<typeof meta>;

/**
 * The glass opens the search dialog, and so does `/` unless a field holds the caret.
 * From inside a field, `C-x s s` opens it (`CommandSurface`).
 */
export const SearchShortcut: Story = {
  play: async ({ args, canvas }) => {
    const glass = canvas.getByRole('button', { name: 'Search sessions' });
    await expect(glass).toHaveAttribute('aria-keyshortcuts', '/');
    await userEvent.click(glass);
    await expect(args.onSearch).toHaveBeenCalledTimes(1);
    await userEvent.keyboard('/');
    await expect(args.onSearch).toHaveBeenCalledTimes(2);

    const field = canvas.getByRole('textbox', { name: 'Reference form field' });
    await userEvent.click(field);
    await userEvent.keyboard('/');
    await expect(field).toHaveValue('/');
    await expect(args.onSearch).toHaveBeenCalledTimes(2);
  },
};

function globalApp(id: string, title: string, icon: string): LiveView {
  const view = liveViewFromWire({ id, title, icon, key: id, scope: 'global', placement: 'attachment', seq: 1, nodes: [{ id: 'break', type: 'divider' }] });
  if (!view) throw new Error('story app must parse');
  return view;
}

/** A program shows global apps: each has its own icon next to the search, and one tap opens it. */
export const WithGlobalApps: Story = {
  args: {
    globalApps: { apps: [globalApp('board', 'Board', 'board'), globalApp('builds', 'Builds', 'chart')], openId: 'board', onOpen: fn() },
  },
  play: async ({ args, canvas }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Open Builds' }));
    await expect(args.globalApps?.onOpen).toHaveBeenCalledTimes(1);
  },
};

/** A dialog with the focus keeps the keyboard: `/` does not open the search past it. */
export const BehindADialog: Story = {
  render: (args) => (
    <>
      <Header {...args} />
      <div role="dialog" aria-label="Rename machine" className="flex gap-2 p-4">
        <Input aria-label="Machine name" placeholder="Machine name" />
        <Button type="button">Save</Button>
      </div>
    </>
  ),
  play: async ({ args, canvas }) => {
    canvas.getByRole('button', { name: 'Save' }).focus();
    await userEvent.keyboard('/');
    await expect(args.onSearch).not.toHaveBeenCalled();
  },
};
