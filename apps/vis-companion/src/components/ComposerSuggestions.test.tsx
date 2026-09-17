// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import type { FileSuggestion, SlashCommand } from '../lib/types';
import { ComposerSuggestions, composerSuggestionListId } from './ComposerSuggestions';
import sessionScreenSource from '../screens/SessionScreen.tsx?raw';

const files: FileSuggestion[] = [
  { name: 'src/App.tsx', size: '24 KB', age: '2m', status: 'modified' },
  { name: 'README.md', size: '8 KB', age: '1d', status: 'clean' },
];
const commands: SlashCommand[] = [
  { name: '/help', doc: 'Show the available slash commands.' },
  { name: '/rename', doc: 'Rename this session.' },
];

afterEach(cleanup);

describe('composer suggestions', () => {
  it('renders and selects file mentions through the canonical list', () => {
    const onSelect = vi.fn();
    render(
      <ComposerSuggestions kind="files" items={files} selectedIndex={1} onSelect={onSelect} />,
    );

    expect(screen.getByRole('listbox', { name: 'File mentions' })).toHaveAttribute(
      'id',
      composerSuggestionListId('files'),
    );
    expect(screen.getByRole('option', { name: /README.md/ })).toHaveAttribute(
      'aria-selected',
      'true',
    );
    fireEvent.click(screen.getByRole('option', { name: /src\/App.tsx/ }));
    expect(onSelect).toHaveBeenCalledWith(files[0]);
  });

  it('renders slash commands with the same list semantics', () => {
    const onSelect = vi.fn();
    render(
      <ComposerSuggestions kind="slashes" items={commands} selectedIndex={0} onSelect={onSelect} />,
    );

    expect(screen.getByRole('listbox', { name: 'Slash commands' })).toHaveAttribute(
      'id',
      composerSuggestionListId('slashes'),
    );
    fireEvent.click(screen.getByRole('option', { name: /rename/i }));
    expect(onSelect).toHaveBeenCalledWith(commands[1]);
  });

  // An absolute box is laid out against its ancestor's PADDING box, so the composer
  // footer's gutters do not inset this list: it has to repeat them at every breakpoint to
  // stay as wide as the input. With only the `sm` reading column it stood narrower than a
  // composer widened to `mouse:max-w-6xl`; with no gutters at all it overhung the input.
  it('repeats the composer footer gutters at every breakpoint', () => {
    render(
      <ComposerSuggestions kind="slashes" items={commands} selectedIndex={0} onSelect={vi.fn()} />,
    );

    const frame = screen.getByRole('listbox', { name: 'Slash commands' }).className;
    const footer = sessionScreenSource.match(/shrink-0 border-t border-dialog-edge bg-ink[^`]*/);
    const inset = (classes: string, utility: string) =>
      classes.match(new RegExp(`(?:^|\\s)${utility}-\\[([^\\]]+)\\]`))?.[1];

    expect(footer?.[0], 'the composer footer classes').toMatch(/\bpl-\[/);
    for (const [padding, side] of [
      ['pl', 'left'],
      ['pr', 'right'],
    ]) {
      for (const variant of ['', 'sm:', 'mouse:']) {
        const gutter = inset(footer?.[0] ?? '', `${variant}${padding}`);
        expect(gutter, `${variant}${padding}`).toMatch(/\S/);
        expect(inset(frame, `${variant}${side}`), `${variant}${side}`).toBe(gutter);
      }
    }
  });

  it('does not mount an empty completion surface', () => {
    const { container } = render(
      <ComposerSuggestions kind="files" items={[]} selectedIndex={0} onSelect={vi.fn()} />,
    );
    expect(container).toBeEmptyDOMElement();
  });
});
