import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent } from 'storybook/test';
import { useState } from 'react';
import { Header } from './App';
import { Input } from './components/ui';

const meta = {
  title: 'Navigation/Header',
} satisfies Meta;

export default meta;
type Story = StoryObj<typeof meta>;

function SearchHeader() {
  const [query, setQuery] = useState('');
  const [isSearching, setSearching] = useState(true);
  return (
    <>
      <Header
        query={query}
        onQuery={setQuery}
        isSearching={isSearching}
        onSearch={() => setSearching(true)}
        onCloseSearch={() => {
          setSearching(false);
          setQuery('');
        }}
        onAppSettings={fn()}
      />
      <div className="p-4">
        <Input aria-label="Reference form field" placeholder="Project name" />
      </div>
    </>
  );
}

export const Search: Story = {
  render: () => <SearchHeader />,
  play: async ({ canvas, canvasElement }) => {
    const search = canvas.getByRole('searchbox', {
      name: 'Search sessions on every machine',
    });
    const reference = canvas.getByRole('textbox', { name: 'Reference form field' });
    await search.ownerDocument.fonts.ready;
    const header = canvasElement.querySelector('header')!;
    const frame = header.getBoundingClientRect();
    const inset = window.innerWidth >= 640 ? 16 : 12;
    await expect(header.firstElementChild!.getBoundingClientRect().width).toBe(frame.width);
    await expect(frame.right - search.getBoundingClientRect().right).toBe(inset);
    await expect(search).toHaveFocus();
    // The header cannot introduce another height or type scale for the same input.
    await expect(search.getBoundingClientRect().height).toBe(
      reference.getBoundingClientRect().height,
    );
    await expect(getComputedStyle(search).fontSize).toBe(getComputedStyle(reference).fontSize);
    await userEvent.type(search, 'companion/project');
    await expect(search).toHaveValue('companion/project');
    await userEvent.click(canvas.getByRole('button', { name: 'Clear search' }));
    await expect(search).toHaveValue('');
    await expect(search).toHaveFocus();
    await userEvent.keyboard('{Escape}');
    await expect(canvas.queryByRole('searchbox')).not.toBeInTheDocument();
    // The logo and preferences follow the full-width bar, not a centered content cap.
    const logo = canvas.getByLabelText('Vis').getBoundingClientRect();
    const preferences = canvas.getByRole('button', { name: 'Open preferences' });
    await expect(logo.left - frame.left).toBe(inset);
    await expect(frame.right - preferences.getBoundingClientRect().right).toBe(inset);
    await userEvent.keyboard('/');
    await expect(canvas.getByRole('searchbox')).toHaveFocus();
  },
};

export const SearchPointer: Story = {
  ...Search,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};
