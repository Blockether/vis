import type { Meta, StoryObj } from '@storybook/react-vite';
import type { ComponentProps } from 'react';
import { useState } from 'react';
import { expect, userEvent, waitFor, within } from 'storybook/test';
import { Markdown, ThinkingBand, UserMessage } from './ChatContent';
import { MarkdownArtifact } from './MarkdownArtifact';

const paragraph =
  'A paragraph reads more evenly when no gap between its words opens wide. The browser justifies each line, and a line beside `apps/vis-companion/src/components/JustifiedProse.tsx` or another long path keeps its natural spacing instead. The same prose should remain readable when you resize the window or open a document on a narrow phone.';
const paragraphText = paragraph.replaceAll('`', '');

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
  title: 'Components/Justified prose',
  component: ProseExample,
  parameters: { layout: 'fullscreen' },
} satisfies Meta<typeof ProseExample>;
export default meta;
type Story = StoryObj<typeof meta>;

/**
 * Every line of the passage stays inside its column, and no gap outside inline code opens
 * wider than five natural spaces. Native and composed lines must both pass.
 */
function expectEven(prose: HTMLElement) {
  const column = prose.getBoundingClientRect();
  // A canvas measures the space without a DOM change, which would wake `waitFor` again.
  const style = getComputedStyle(prose);
  const context = document.createElement('canvas').getContext('2d')!;
  context.font = `${style.fontStyle} ${style.fontWeight} ${style.fontSize} ${style.fontFamily}`;
  const space = context.measureText(' ').width;
  const range = document.createRange();
  const walker = document.createTreeWalker(prose, NodeFilter.SHOW_TEXT);
  while (walker.nextNode()) {
    const text = walker.currentNode as Text;
    for (const word of text.data.matchAll(/\S+/g)) {
      range.setStart(text, word.index);
      range.setEnd(text, word.index + word[0].length);
      for (const rect of range.getClientRects()) {
        expect(rect.left).toBeGreaterThanOrEqual(column.left - 1);
        expect(rect.right).toBeLessThanOrEqual(column.right + 1);
      }
    }
    if (text.parentElement?.closest('code')) continue;
    for (const gap of text.data.matchAll(/ +/g)) {
      range.setStart(text, gap.index);
      range.setEnd(text, gap.index + 1);
      const rect = range.getClientRects()[0];
      if (rect) expect(rect.width).toBeLessThanOrEqual(5 * space + 1);
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

/** The passage keeps its box from one frame to the next: no second layout appears. */
async function expectSteady(element: HTMLElement, frames = 12) {
  const boxes: number[][] = [];
  for (let frame = 0; frame < frames; frame++) {
    const box = element.getBoundingClientRect();
    boxes.push([box.x, box.y, box.width, box.height]);
    await new Promise((resolve) => requestAnimationFrame(resolve));
  }
  for (const box of boxes) expect(box).toEqual(boxes[0]);
}

export const ResponsiveParagraphs: Story = {
  // Line geometry needs real browser layout, which the jsdom story run lacks: open this
  // story in Storybook to play it.
  tags: ['!test'],
  play: async ({ canvasElement }) => {
    await document.fonts.ready;
    const column = canvasElement.querySelector<HTMLElement>('[data-prose-column]')!;
    const passages = () => [...column.querySelectorAll<HTMLElement>('p, li')];
    for (const width of [340, 300, 280, 360]) {
      column.style.width = `${width}px`;
      await waitFor(() => passages().forEach(expectEven));
      await expectSteady(column);
    }
    expectSelection(column.querySelector('p')!, paragraphText);
    // Text sizes change the line height too, which resizes the passage for its observer.
    column.style.fontSize = '22px';
    column.style.lineHeight = '30px';
    await waitFor(() => passages().forEach(expectEven));
    await userEvent.click(within(canvasElement).getByRole('button', { name: 'Continue response' }));
    const prose = column.querySelector('p')!;
    const streamed = `${paragraphText} More words arrive as the response continues to stream.`;
    await waitFor(() => expect(prose.textContent).toBe(streamed));
    await waitFor(() => expectEven(prose));
    expectSelection(prose, streamed);
    const range = document.createRange();
    range.selectNodeContents(prose);
    const selection = window.getSelection()!;
    selection.addRange(range);
    const selectedMarkup = prose.innerHTML;
    column.style.width = '320px';
    // Let ResizeObserver and its scheduled reading run while the passage is selected.
    await new Promise((resolve) => requestAnimationFrame(() => requestAnimationFrame(resolve)));
    expect(selection.toString()).toBe(streamed);
    expect(prose.innerHTML).toBe(selectedMarkup);
    selection.removeAllRanges();
    await waitFor(() => passages().forEach(expectEven));
    expectSelection(column.querySelector('li')!, paragraphText);
  },
};

const wideCode =
  'See the change in `c6a8ef275 fix(trace): show the running step below a closed digest`. Its own CI run was cancelled.';

/**
 * Regression, user report: code wider than the column took its own lines, and the short
 * line before it stretched its few gaps across the whole column.
 */
export const CodeWiderThanColumn: Story = {
  // Line geometry needs real browser layout: open this story in Storybook to play it.
  tags: ['!test'],
  render: () => (
    <div className="p-3 text-body text-white">
      <div data-prose-column style={{ width: 340, maxWidth: '100%' }}>
        <Markdown>{wideCode}</Markdown>
      </div>
    </div>
  ),
  play: async ({ canvasElement }) => {
    await document.fonts.ready;
    const column = canvasElement.querySelector<HTMLElement>('[data-prose-column]')!;
    const prose = column.querySelector('p')!;
    for (const width of [340, 300, 420]) {
      column.style.width = `${width}px`;
      await waitFor(() => expectEven(prose));
      await expectSteady(column);
    }
    expectSelection(prose, wideCode.replaceAll('`', ''));
  },
};

const spokenRequest = Array(30)
  .fill(
    'I want to review the request,  keep the original words and check that a long recording remains easy to read.',
  )
  .join(' ');
const longReasoning = spokenRequest.replaceAll('  ', ' ');

/** Native prose fills the column without stretching its final line or changing copied text. */
function expectNativeFitted(prose: HTMLElement) {
  expect(prose).not.toHaveAttribute('data-justified-lines');
  expect(getComputedStyle(prose).textAlign).toBe('justify');
  const column = prose.getBoundingClientRect();
  // Measure words rather than hanging spaces, and include generated hyphen glyphs.
  const text = prose.firstChild!;
  const range = document.createRange();
  const bounds = new Map<number, { left: number; right: number }>();
  for (const word of text.textContent!.matchAll(/\S+/g)) {
    range.setStart(text, word.index);
    range.setEnd(text, word.index + word[0].length);
    for (const rect of range.getClientRects()) {
      if (!rect.width) continue;
      const line = bounds.get(rect.top) ?? { left: rect.left, right: rect.right };
      line.left = Math.min(line.left, rect.left);
      line.right = Math.max(line.right, rect.right);
      bounds.set(rect.top, line);
    }
  }
  const lines = [...bounds.values()];
  expect(lines.length).toBeGreaterThan(1);
  for (const line of lines.slice(0, -1)) {
    expect(Math.abs(line.left - column.left)).toBeLessThan(1);
    expect(Math.abs(line.right - column.right)).toBeLessThan(1);
  }
  expect(lines.at(-1)!.right).toBeLessThanOrEqual(column.right + 1);
}

export const LongSpokenRequest: Story = {
  // jsdom cannot check text alignment or selection; play this story in the browser.
  tags: ['!test'],
  globals: { theme: 'blockether-dark' },
  render: () => (
    <div className="w-full max-w-4xl p-3 text-body text-white">
      <div data-spoken-request>
        <UserMessage>{spokenRequest}</UserMessage>
      </div>
      <div data-long-thinking>
        <ThinkingBand>{longReasoning}</ThinkingBand>
      </div>
      <div data-literal-request>
        <UserMessage>{'  git  status\ngit\tstatus\n```sh\ngit  status\n```'}</UserMessage>
      </div>
    </div>
  ),
  play: async ({ canvasElement }) => {
    await document.fonts.ready;
    const request = canvasElement.querySelector<HTMLElement>('[data-spoken-request] p')!;
    expectNativeFitted(request);
    expectSelection(request, spokenRequest);
    const thinking = canvasElement.querySelector<HTMLElement>('[data-long-thinking]')!;
    thinking.scrollIntoView({ block: 'center' });
    const disclosure = within(thinking).queryByRole('button');
    if (disclosure?.getAttribute('aria-expanded') === 'false') await userEvent.click(disclosure);
    const reasoning = thinking.querySelector<HTMLElement>('p')!;
    expectNativeFitted(reasoning);
    expectSelection(reasoning, longReasoning);
    for (const line of canvasElement.querySelectorAll('[data-literal-request] p')) {
      expect(getComputedStyle(line).textAlign).toBe('left');
      expect(getComputedStyle(line).whiteSpace).toBe('pre-wrap');
    }
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
    for (const prose of canvasElement.querySelectorAll('p')) {
      expect(prose).not.toHaveAttribute('data-justified-lines');
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

/** #282 follow-up: opening a document must not reveal a second layout. */
export const StableOpening: Story = {
  // Line geometry needs real browser layout, which the jsdom story run lacks: open this
  // story in Storybook to play it.
  tags: ['!test'],
  render: () => <OpeningArtifact />,
  play: async ({ canvasElement }) => {
    await document.fonts.ready;
    const canvas = within(canvasElement);
    const column = canvasElement.querySelector<HTMLElement>('[data-opening-column]')!;
    for (const width of [600, 300]) {
      column.style.width = `${width}px`;
      const frames: number[][][] = [];
      let frame = 0;
      const capture = () => {
        // The artifact shows a loading line until its source is read; the document ends in h2.
        if (column.querySelector('h2')) {
          frames.push(
            [...column.querySelectorAll('p, li, h2')].map((element) => {
              const box = element.getBoundingClientRect();
              return [box.x, box.y, box.width, box.height];
            }),
          );
        }
        frame = requestAnimationFrame(capture);
      };
      frame = requestAnimationFrame(capture);
      try {
        await userEvent.click(canvas.getByRole('button', { name: 'Open Markdown' }));
        await waitFor(() => expect(frames.length).toBeGreaterThanOrEqual(12));
        for (const sample of frames) expect(sample).toEqual(frames[0]);
        for (const prose of column.querySelectorAll<HTMLElement>('p, li')) expectEven(prose);
      } finally {
        cancelAnimationFrame(frame);
      }
      await userEvent.click(canvas.getByRole('button', { name: 'Close Markdown' }));
    }
  },
};

const codeLists = `1. Open \`apps/vis-companion/src/components/JustifiedProse.tsx\` and find \`inlineContent\`, \`inlineSlice\` and \`nativeLines\` before you change anything.
2. Run \`npm test -- --run src/components/JustifiedProse.test.tsx\`, then \`npx vitest --project=storybook --run\` to check the stories.
3. Read \`Range.getClientRects()\` for each word so \`ResizeObserver\` and \`IntersectionObserver\` see the same lines.
4. Keep \`document.fonts\` and \`getComputedStyle\` available before \`nativeLines\` runs.

- \`INLINE_CODE_CLASS\` keeps \`px-0.5\`, \`mx-px\` and \`inline-block\` on every \`code\` element in the answer.
- The \`li\` marker uses \`::before\` with \`position: absolute\`, so \`padding-left\` reserves \`2ch\` or \`var(--marker-column)\`.
- \`a\`, \`b\`, \`c\`, \`d\`, \`e\`, \`f\`, \`g\` and \`h\` are short values, while \`src/com/blockether/vis/internal/gateway/state.clj\` is long.
- Read \`deps.edn\`, \`bb.edn\`, \`build.clj\`, \`package.json\`, \`tsconfig.json\` and \`vite.config.ts\` first.`;

/** Lists crowded with inline code keep every gap narrow at each column width. */
export const InlineCodeLists: Story = {
  // Line geometry needs real browser layout, which the jsdom story run lacks: open this
  // story in Storybook to play it.
  tags: ['!test'],
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
      await waitFor(() => items.forEach(expectEven));
      await expectSteady(column);
    }
  },
};
