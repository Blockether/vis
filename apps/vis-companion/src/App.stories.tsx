import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent } from 'storybook/test';
import { Header } from './App';
import { Button, Input } from './components/ui';

const meta = {
  title: 'Navigation/Header',
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
 * `Ctrl+/` is no character, so it opens the search from inside the field too.
 */
export const Search: Story = {
  play: async ({ args, canvas }) => {
    const glass = canvas.getByRole('button', { name: 'Search all machines' });
    await expect(glass).toHaveAttribute('aria-keyshortcuts', 'Control+/ /');
    await userEvent.click(glass);
    await expect(args.onSearch).toHaveBeenCalledTimes(1);
    await userEvent.keyboard('/');
    await expect(args.onSearch).toHaveBeenCalledTimes(2);

    const field = canvas.getByRole('textbox', { name: 'Reference form field' });
    await userEvent.click(field);
    await userEvent.keyboard('/');
    await expect(field).toHaveValue('/');
    await expect(args.onSearch).toHaveBeenCalledTimes(2);
    await userEvent.keyboard('{Control>}/{/Control}');
    await expect(args.onSearch).toHaveBeenCalledTimes(3);
    await expect(field).toHaveValue('/');
    // A Windows AltGr layout reports Ctrl+Alt while it only types the slash.
    await userEvent.keyboard('{Control>}{Alt>}/{/Alt}{/Control}');
    await expect(args.onSearch).toHaveBeenCalledTimes(3);
  },
};

export const SearchPointer: Story = {
  ...Search,
  tags: ['!test'],
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

export const SearchDark: Story = {
  ...Search,
  tags: ['!test'],
  globals: { theme: 'blockether-dark' },
};

export const SearchDarkPointer: Story = {
  ...Search,
  tags: ['!test'],
  globals: { theme: 'blockether-dark', viewport: { value: 'desktop', isRotated: false } },
};

/** With no session list behind it there is nothing to search, so the bar offers no glass. */
export const NothingToSearch: Story = {
  render: (args) => <Header {...args} onSearch={null} />,
  play: async ({ canvas }) => {
    await expect(canvas.queryByRole('button', { name: 'Search all machines' })).toBeNull();
    await userEvent.keyboard('/');
    await expect(canvas.getByRole('button', { name: 'Open preferences' })).toBeVisible();
  },
};

/** A dialog with the focus keeps the keyboard: neither `/` nor `Ctrl+/` opens the search past it. */
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
    await userEvent.click(canvas.getByRole('textbox', { name: 'Machine name' }));
    await userEvent.keyboard('{Control>}/{/Control}');
    canvas.getByRole('button', { name: 'Save' }).focus();
    await userEvent.keyboard('/');
    await userEvent.keyboard('{Control>}/{/Control}');
    await expect(args.onSearch).not.toHaveBeenCalled();
  },
};
