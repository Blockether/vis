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

/**
 * A line whose gaps would open wider than the ragged limit keeps natural spacing and ends
 * short by design. Glyph advances differ by platform, and so does which line that is.
 */
function isRagged(line: Element) {
  const { wordSpacing, letterSpacing } = (line as HTMLElement).style;
  return parseFloat(wordSpacing) === 0 && parseFloat(letterSpacing) === 0;
}

function expectFitted(prose: HTMLElement) {
  const column = prose.getBoundingClientRect();
  const inset = parseFloat(getComputedStyle(prose).paddingLeft);
  const lines = [...prose.children];
  expect(lines.length).toBeGreaterThan(1);
  expect(lines.slice(0, -1).some((line) => !isRagged(line))).toBe(true);
  for (const [index, line] of lines.entries()) {
    const range = document.createRange();
    range.selectNodeContents(line);
    const rect = range.getBoundingClientRect();
    expect(Math.abs(rect.left - column.left - inset)).toBeLessThan(1);
    expect(rect.right).toBeLessThanOrEqual(column.right + 1);
    if (index < lines.length - 1 && !isRagged(line)) {
      expect(Math.abs(rect.right - column.right)).toBeLessThan(1);
    }
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
    // Prose composes at once and again when its web font finishes loading, so the first
    // composition may still carry the fallback font's advances.
    await waitFor(() => expectFitted(prose), { timeout: 5000 });
    expectSelection(prose, paragraph);
    const narrowLines = prose.children.length;
    column.style.width = '280px';
    await waitFor(() => expect(prose.children.length).toBeGreaterThan(narrowLines));
    expectFitted(prose);
    expectSelection(prose, paragraph);
    const smallerFontLines = prose.children.length;
    // Text sizes change the line height too. Composed lines never wrap, so the new line
    // height is what resizes the passage for its ResizeObserver.
    column.style.fontSize = '22px';
    column.style.lineHeight = '30px';
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
        // The artifact shows a loading line until its source is read; the document ends in h2.
        if (column.querySelector('h2')) {
          const prose = [...column.querySelectorAll('p, li')];
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

const codeLists = `1. Open \`apps/vis-companion/src/components/JustifiedProse.tsx\` and find \`prepareInline\`, \`inlineSlice\` and \`measuredSlice\` before you change anything.
2. Run \`npm test -- --run src/components/JustifiedProse.test.tsx\`, then \`npx vitest --project=storybook --run\` to check the stories.
3. Set \`wordSpacing\`, \`letterSpacing\` and \`fontKerning\` on each \`span\` so \`Range.getBoundingClientRect()\` matches the probe.
4. Keep \`ResizeObserver\`, \`Intl.Segmenter\` and \`document.fonts\` available before \`engine.solve\` runs.

- \`INLINE_CODE_CLASS\` keeps \`px-0.5\`, \`mx-px\` and \`inline-block\` on every \`code\` element in the answer.
- The \`li\` marker uses \`::before\` with \`position: absolute\`, so \`padding-left\` reserves \`2ch\` or \`var(--marker-column)\`.
- \`a\`, \`b\`, \`c\`, \`d\`, \`e\`, \`f\`, \`g\` and \`h\` are short values, while \`src/com/blockether/vis/internal/gateway/state.clj\` is long.
- Read \`deps.edn\`, \`bb.edn\`, \`build.clj\`, \`package.json\`, \`tsconfig.json\` and \`vite.config.ts\` first.`;

/**
 * Count the lines of a composed item that end flush and those left short at natural
 * spacing. Every line starts at the inset and stays inside the column, and no line
 * opens word gaps wider than five spaces.
 */
function countEndings(prose: HTMLElement) {
  const column = prose.getBoundingClientRect();
  const style = getComputedStyle(prose);
  const inset = parseFloat(style.paddingLeft);
  const context = document.createElement('canvas').getContext('2d')!;
  context.font = `${style.fontStyle} ${style.fontWeight} ${style.fontSize} ${style.fontFamily}`;
  const space = context.measureText(' ').width;
  const lines = [...prose.children] as HTMLElement[];
  let flush = 0;
  let ragged = 0;
  for (const [index, line] of lines.entries()) {
    const range = document.createRange();
    range.selectNodeContents(line);
    const rect = range.getBoundingClientRect();
    const spacing = parseFloat(line.style.wordSpacing) || 0;
    expect(Math.abs(rect.left - column.left - inset)).toBeLessThan(1);
    expect(rect.right).toBeLessThanOrEqual(column.right + 1);
    expect(spacing).toBeLessThanOrEqual(4 * space);
    if (index === lines.length - 1) continue;
    if (Math.abs(rect.right - column.right) < 1) flush++;
    else {
      // Only a line kept at natural spacing may end short of the column.
      expect(spacing).toBe(0);
      ragged++;
    }
  }
  return { flush, ragged };
}

/** Lists crowded with inline code compose every item without opening holes. */
export const InlineCodeLists: Story = {
  render: () => (
    <div className="p-3 text-body text-white">
      <div data-list-column style={{ width: 390, maxWidth: '100%' }}>
        <Markdown>{codeLists}</Markdown>
      </div>
    </div>
  ),
  play: async ({ canvasElement }) => {
    await document.fonts.ready;
    const column = canvasElement.querySelector<HTMLElement>('[data-list-column]')!;
    const items = [...column.querySelectorAll<HTMLElement>('li')];
    expect(items).toHaveLength(8);
    for (const width of [390, 340, 300, 260]) {
      column.style.width = `${width}px`;
      await waitFor(() => {
        let flush = 0;
        let ragged = 0;
        for (const item of items) {
          expect(item).toHaveAttribute('data-justice');
          const endings = countEndings(item);
          flush += endings.flush;
          ragged += endings.ragged;
        }
        // Each hole is avoided on its own line; the lists as a whole stay justified.
        expect(ragged).toBeLessThan(flush);
      });
    }
  },
};
