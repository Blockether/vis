import type { Meta, StoryObj } from '@storybook/react-vite';
import { useState } from 'react';
import { expect, userEvent, waitFor, within } from 'storybook/test';
import { Markdown } from './ChatContent';

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
  title: 'Session/Transcript/Prose',
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
