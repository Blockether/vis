import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, waitFor } from 'storybook/test';

import { NOTE_MARKDOWN } from '../dev/story-data';
import { type DocumentChrome, MarkdownArtifact } from './MarkdownArtifact';

/**
 * A NOTE, READ AS PROSE.
 *
 * A document is always read inside somebody else's chrome, and the reader hands
 * its cells UP: `actions` is the band's verb, `note` is what the band should
 * REPORT, `body` is the column that scrolls. The stand-in below is the frame, not
 * the subject.
 */
const chrome: DocumentChrome = ({ actions, note, body }) => (
  <div className="flex h-[520px] min-h-0 flex-col gap-2">
    <header className="flex min-h-12 items-stretch justify-between gap-3 mouse:min-h-9">
      <span className="flex min-w-0 flex-1 items-center text-meta text-muted">
        {note || 'plan.md'}
      </span>
      <div className="flex shrink-0 items-stretch">{actions}</div>
    </header>
    <div className="min-h-0 flex-1 overflow-auto">{body}</div>
  </div>
);

const meta = {
  title: 'Session/Artifacts/Markdown',
  component: MarkdownArtifact,
  parameters: { layout: 'padded' },
  args: { source: new Blob([NOTE_MARKDOWN], { type: 'text/markdown' }), chrome },
} satisfies Meta<typeof MarkdownArtifact>;

export default meta;

type Story = StoryObj<typeof meta>;

const RICH_PARAGRAPH =
  'A **formatted paragraph** should use the same optimizer as plain prose. Read *the careful explanation*, follow [the reference](https://example.com), and keep `git  status --short` literal. Repeated words, **words**, and well-**known** choices must be measured in their own styles, without changing what you select or quote.';
const RICH_TEXT = `# Artifact typography

${RICH_PARAGRAPH}

- A **formatted list item** also balances its lines without losing the meaning or the original source.

> A quotation with *emphasis* follows the same paragraph layout rules.`;
const RICH_SOURCE = new Blob([RICH_TEXT], { type: 'text/markdown' });

/** Every line stays inside the column, and no gap outside code opens wider than five spaces. */
function fittedParagraph(prose: HTMLElement) {
  const bounds = prose.getBoundingClientRect();
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
        expect(rect.left).toBeGreaterThanOrEqual(bounds.left - 1);
        expect(rect.right).toBeLessThanOrEqual(bounds.right + 1);
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

/** The actual artifact reader, including rich text, code and responsive composition. */
export const RichText: Story = {
  // Line fitting measures text in real browser layout, which the jsdom story run lacks: open
  // this story in Storybook to play it.
  tags: ['!test'],
  globals: { theme: 'blockether-dark' },
  render: () => (
    <div data-artifact-column style={{ width: 600, maxWidth: '100%' }}>
      <MarkdownArtifact source={RICH_SOURCE} chrome={chrome} />
    </div>
  ),
  play: async ({ canvas, canvasElement }) => {
    await document.fonts.ready;
    const column = canvasElement.querySelector<HTMLElement>('[data-artifact-column]')!;
    await waitFor(() => fittedParagraph(column.querySelector('p')!));
    const prose = column.querySelector('p')!;
    const original = prose.textContent;
    fittedParagraph(prose);
    const wideHeight = prose.getBoundingClientRect().height;
    column.style.width = '300px';
    await waitFor(() => {
      fittedParagraph(prose);
      expect(prose.getBoundingClientRect().height).toBeGreaterThan(wideHeight);
    });
    const link = canvas.getByRole('link', { name: 'the reference' });
    expect(link).toHaveAttribute('href', 'https://example.com');
    const code = prose.querySelectorAll('code');
    // Code may wrap after a space, which then stays between the lines, outside the boxes.
    const literal = document.createRange();
    literal.setStartBefore(code[0]);
    literal.setEndAfter(code[code.length - 1]);
    expect(literal.toString()).toBe('git  status --short');
    for (const part of code) {
      expect(getComputedStyle(part).wordSpacing).toBe('0px');
      expect(parseFloat(getComputedStyle(part).letterSpacing) || 0).toBe(0);
    }
    const range = document.createRange();
    range.selectNodeContents(prose);
    const selection = window.getSelection()!;
    selection.removeAllRanges();
    selection.addRange(range);
    expect(selection.toString()).toBe(original);
    const selectedMarkup = prose.innerHTML;
    column.style.width = '500px';
    await new Promise((resolve) => requestAnimationFrame(() => requestAnimationFrame(resolve)));
    expect(prose.innerHTML).toBe(selectedMarkup);
    expect(selection.toString()).toBe(original);
    selection.removeAllRanges();
    await waitFor(() => fittedParagraph(prose));
    expect(prose.textContent).toBe(original);
    await waitFor(() => fittedParagraph(column.querySelector('li')!));
    await waitFor(() => fittedParagraph(column.querySelector('blockquote p')!));
  },
};
