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
    // Regression: the search field sat at the top of the bar while the back chevron
    // was centred in its full-height button. Their visible centres must share a row.
    const field = search.getBoundingClientRect();
    const chevron = canvas
      .getByRole('button', { name: 'Close search' })
      .querySelector('svg')!
      .getBoundingClientRect();
    await expect(chevron.top + chevron.height / 2).toBe(field.top + field.height / 2);
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
    const brand = canvas.getByLabelText('Vis');
    const logo = brand.getBoundingClientRect();
    const preferences = canvas.getByRole('button', { name: 'Open preferences' });
    await expect(logo.left - frame.left).toBe(inset);
    await expect(frame.right - preferences.getBoundingClientRect().right).toBe(inset);
    // The selected wordmark keeps a hard yellow offset in both Blockether palettes.
    const wordmark = canvas.getByText('VIS');
    const face = getComputedStyle(wordmark);
    await expect(face.fontSize).toBe('17px');
    await expect(face.lineHeight).toBe('24px');
    await expect(face.fontWeight).toBe('800');
    await expect(Number.parseFloat(face.letterSpacing)).toBeCloseTo(2.38, 2);
    await expect(face.color).toBe(getComputedStyle(brand).color);
    await expect(face.textShadow).toMatch(/ 2px 2px 0px$/);
    const theme = canvasElement.ownerDocument.documentElement.dataset.theme;
    if (theme === 'blockether-light' || theme === 'blockether-dark') {
      await expect(face.textShadow).toBe('rgb(255, 196, 32) 2px 2px 0px');
    }
    const letters = wordmark.getBoundingClientRect();
    const mark = brand.querySelector('img')!.getBoundingClientRect();
    await expect(letters.top + letters.height / 2).toBe(logo.top + logo.height / 2);
    await expect(mark.top + mark.height / 2 + 2).toBe(letters.top + letters.height / 2);
    await expect(logo.right).toBeLessThan(
      canvas.getByRole('button', { name: 'Search all machines' }).getBoundingClientRect().left,
    );
    await userEvent.keyboard('/');
    await expect(canvas.getByRole('searchbox')).toHaveFocus();
  },
};

export const SearchPointer: Story = {
  ...Search,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

export const SearchDark: Story = {
  ...Search,
  globals: { theme: 'blockether-dark' },
};

export const SearchDarkPointer: Story = {
  ...Search,
  globals: { theme: 'blockether-dark', viewport: { value: 'desktop', isRotated: false } },
};
