import type { Meta, StoryObj } from '@storybook/react-vite';
import type { ComponentProps } from 'react';
import { useState } from 'react';
import { expect, userEvent, waitFor, within } from 'storybook/test';
import { Markdown } from './ChatContent';
import { MarkdownArtifact } from './MarkdownArtifact';

const paragraph =
  'A paragraph reads more evenly when its line breaks are chosen together. Justice considers the whole paragraph, balancing the spaces between words instead of stretching each line independently. The same prose should remain readable when you resize the window or open a document on a narrow phone.';

function ProseExample() {
  const [text, setText] = useState(paragraph);
  return (
    <div className="p-3 text-body text-white">
      <button
        onClick={() =>
          setText(`${paragraph} More words arrive as the response continues to stream.`)
        }
      >
        Continue response
      </button>
      <div data-prose-column style={{ width: 340, maxWidth: '100%' }}>
        <Markdown>{text}</Markdown>
        <Markdown>{`- ${paragraph}\n\n> ${paragraph}`}</Markdown>
      </div>
    </div>
  );
}

const meta = {
  title: 'Components/Justice prose',
  component: ProseExample,
  parameters: { layout: 'fullscreen' },
} satisfies Meta<typeof ProseExample>;
export default meta;
type Story = StoryObj<typeof meta>;

function expectFitted(prose: HTMLElement) {
  const column = prose.getBoundingClientRect();
  const inset = parseFloat(getComputedStyle(prose).paddingLeft);
  const lines = [...prose.children];
  expect(lines.length).toBeGreaterThan(1);
  for (const [index, line] of lines.entries()) {
    const range = document.createRange();
    range.selectNodeContents(line);
    const rect = range.getBoundingClientRect();
    expect(Math.abs(rect.left - column.left - inset)).toBeLessThan(1);
    expect(rect.right).toBeLessThanOrEqual(column.right + 1);
    if (index < lines.length - 1) expect(Math.abs(rect.right - column.right)).toBeLessThan(1);
  }
}

function expectSelection(prose: HTMLElement, text: string) {
  const range = document.createRange();
  range.selectNodeContents(prose);
  const selection = window.getSelection()!;
  selection.removeAllRanges();
  selection.addRange(range);
  expect(selection.toString()).toBe(text);
  selection.removeAllRanges();
  expect(prose.textContent).toBe(text);
}

export const ResponsiveParagraphs: Story = {
  play: async ({ canvasElement }) => {
    await document.fonts.ready;
    const column = canvasElement.querySelector<HTMLElement>('[data-prose-column]')!;
    let prose = column.querySelector('p')!;
    await waitFor(() => expect(prose).toHaveAttribute('data-justice'));
    expectFitted(prose);
    expectSelection(prose, paragraph);
    const narrowLines = prose.children.length;
    column.style.width = '280px';
    await waitFor(() => expect(prose.children.length).toBeGreaterThan(narrowLines));
    expectFitted(prose);
    expectSelection(prose, paragraph);
    const smallerFontLines = prose.children.length;
    column.style.fontSize = '22px';
    await waitFor(() => expect(prose.children.length).toBeGreaterThan(smallerFontLines));
    expectFitted(prose);
    await userEvent.click(within(canvasElement).getByRole('button', { name: 'Continue response' }));
    prose = column.querySelector('p')!;
    const streamed = `${paragraph} More words arrive as the response continues to stream.`;
    await waitFor(() => expect(prose.textContent).toBe(streamed));
    await waitFor(() => expect(prose).toHaveAttribute('data-justice'));
    expectFitted(prose);
    expectSelection(prose, streamed);
    const range = document.createRange();
    range.selectNodeContents(prose);
    const selection = window.getSelection()!;
    selection.addRange(range);
    const selectedMarkup = prose.innerHTML;
    column.style.width = '320px';
    // Let ResizeObserver and its scheduled fit run while the passage is selected.
    await new Promise((resolve) => requestAnimationFrame(() => requestAnimationFrame(resolve)));
    expect(selection.toString()).toBe(streamed);
    expect(prose.innerHTML).toBe(selectedMarkup);
    selection.removeAllRanges();
    await waitFor(() => expectFitted(prose));
    const item = column.querySelector('li')!;
    await waitFor(() => expect(item).toHaveAttribute('data-justice'));
    await waitFor(() => expectFitted(item));
    expectSelection(item, paragraph);
  },
};

export const NativeFallbacks: Story = {
  globals: { theme: 'blockether-dark' },
  render: () => (
    <div className="w-full max-w-sm bg-answer p-3 text-body text-answer-foreground">
      <Markdown
        hardBreaks
      >{`## Headings stay native\n\nRead **bold text**, *emphasis*, [a link](https://example.com) and \`inline code\`.\nKeep this hard break.\n\n日本語の文章はブラウザーが改行します。\n\nمرحبا بكم في هذا النص الذي يحتفظ باتجاهه الأصلي\n\n${'longidentifier'.repeat(30)}`}</Markdown>
    </div>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByRole('link', { name: 'a link' })).toHaveAttribute(
      'href',
      'https://example.com',
    );
    expect(canvas.getByRole('heading', { name: 'Headings stay native' })).toBeVisible();
    expect(canvasElement.querySelector('br')).not.toBeNull();
    await document.fonts.ready;
    for (const prose of canvasElement.querySelectorAll('p')) {
      expect(prose).not.toHaveAttribute('data-justice');
      expect(prose.scrollWidth).toBeLessThanOrEqual(prose.clientWidth + 1);
    }
  },
};

const openingSource = new Blob(
  [
    `# Opening Markdown

${paragraph}

- ${paragraph}

## Next section`,
  ],
  { type: 'text/markdown' },
);
const openingClient = { base: 'http://127.0.0.1:7777' } as ComponentProps<
  typeof MarkdownArtifact
>['client'];

function OpeningArtifact() {
  const [open, setOpen] = useState(false);
  return (
    <div className="p-3 text-body text-white">
      <button onClick={() => setOpen(!open)}>{open ? 'Close Markdown' : 'Open Markdown'}</button>
      <div data-opening-column style={{ width: 600, maxWidth: '100%' }}>
        {open && (
          <MarkdownArtifact
            client={openingClient}
            sid="s1"
            iterationId="i1"
            name="opening.md"
            mediaType="text/markdown"
            source={openingSource}
            chrome={({ body }) => <>{body}</>}
          />
        )}
      </div>
    </div>
  );
}

/** #282 follow-up: opening a document must not reveal a second, justified layout. */
export const StableOpening: Story = {
  render: () => <OpeningArtifact />,
  play: async ({ canvasElement }) => {
    await document.fonts.ready;
    const canvas = within(canvasElement);
    const column = canvasElement.querySelector<HTMLElement>('[data-opening-column]')!;
    for (const width of [600, 300]) {
      column.style.width = `${width}px`;
      const frames: { composed: boolean; geometry: number[][] }[] = [];
      let frame = 0;
      const capture = () => {
        const prose = [...column.querySelectorAll('p, li')];
        if (prose.length > 0) {
          frames.push({
            composed: prose.every((element) => element.hasAttribute('data-justice')),
            geometry: [...column.querySelectorAll('p, li, h2')].map((element) => {
              const box = element.getBoundingClientRect();
              return [box.x, box.y, box.width, box.height];
            }),
          });
        }
        frame = requestAnimationFrame(capture);
      };
      frame = requestAnimationFrame(capture);
      try {
        await userEvent.click(canvas.getByRole('button', { name: 'Open Markdown' }));
        await waitFor(() => {
          expect(frames.length).toBeGreaterThanOrEqual(12);
          expect(frames.at(-1)?.composed).toBe(true);
        });
        expect(frames.every((sample) => sample.composed)).toBe(true);
        for (const sample of frames) expect(sample.geometry).toEqual(frames[0].geometry);
        for (const prose of column.querySelectorAll<HTMLElement>('p, li')) expectFitted(prose);
      } finally {
        cancelAnimationFrame(frame);
      }
      await userEvent.click(canvas.getByRole('button', { name: 'Close Markdown' }));
    }
  },
};
