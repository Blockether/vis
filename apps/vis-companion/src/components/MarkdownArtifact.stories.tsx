import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, waitFor } from 'storybook/test';

import { LOG_TEXT, NOTE_ANNOTATED, NOTE_MARKDOWN } from '../dev/story-data';
import { type DocumentChrome, MarkdownAnnotator, MarkdownArtifact } from './MarkdownArtifact';
import type { GatewayClient } from '../lib/gateway';
import { quoteOf, renderAnnotated } from '../lib/markdown-annotations';

/**
 * A NOTE, READ AS PROSE AND TALKED BACK TO.
 *
 * Select a passage, say what you think, and the remark is kept in the note itself
 * under one `## Comments` heading — so a commented document is just a document,
 * and the fixture here is the file's own format.
 *
 * A document is always read inside somebody else's chrome, and the annotator
 * hands its cells UP: `actions` is the band's verb, `note` is what the band
 * should REPORT, `body` is the column that scrolls. The stand-in below is the one
 * `MarkdownArtifact.test.tsx` uses, for the same reason: it is the frame, not the
 * subject.
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
  title: 'Components/Markdown annotator',
  component: MarkdownAnnotator,
  parameters: { layout: 'padded' },
  args: { text: NOTE_MARKDOWN, chrome, onSave: fn(async () => 2) },
} satisfies Meta<typeof MarkdownAnnotator>;

export default meta;

type Story = StoryObj<typeof meta>;

/** The note as prose: headings as headings, through the transcript's own renderer. */
export const Note: Story = {
  play: async ({ canvas, args }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Comment on the whole document' }));
    await userEvent.type(canvas.getByRole('textbox', { name: 'Comment' }), 'Worth a second pass.');
    await userEvent.click(canvas.getByRole('button', { name: 'Add comment' }));
    await userEvent.click(canvas.getByRole('button', { name: 'Save changes' }));
    await expect(args.onSave).toHaveBeenCalledTimes(1);
    await expect(args.onSave).toHaveBeenCalledWith(expect.stringContaining('Worth a second pass.'));
    await expect(canvas.getByRole('status')).toHaveTextContent('Saved as v2');
  },
};

/** The same note after two remarks — one about a passage, one about the whole file. */
export const Commented: Story = {
  args: { text: NOTE_ANNOTATED },
};

/** A `.log`: the same annotator, reading the file verbatim instead of rendering it. */
export const Plain: Story = {
  args: { text: LOG_TEXT, plain: true },
};

/** Regression: selecting and commenting must not reflow the document. */
export const StableHighlight: Story = {
  args: {
    text: '# Delivery plan\n\nThis passage wraps on a phone and must keep exactly the same layout when selected or commented.\n\n- Keep the next step in place.',
  },
  play: async ({ canvas, canvasElement }) => {
    const passage = canvas.getByRole('paragraph');
    expect(passage).toHaveTextContent(
      'This passage wraps on a phone and must keep exactly the same layout when selected or commented.',
    );
    // Take the baseline after fonts and paragraph composition have settled.
    await document.fonts.ready;
    await waitFor(() => expect(passage).toHaveAttribute('data-justice'));
    const blocks = [...canvasElement.querySelectorAll('h1, p, li')];
    await waitFor(() => expect(canvasElement.querySelector('li')).toHaveAttribute('data-justice'));
    const geometry = () =>
      blocks.map((block) => {
        const box = block.getBoundingClientRect();
        return [box.width, box.height];
      });
    const before = geometry();
    const content = passage.innerHTML;
    const check = () => {
      expect(geometry()).toEqual(before);
      expect(passage.innerHTML).toBe(content);
      expect(getComputedStyle(passage).textDecorationLine).toBe('none');
      expect(getComputedStyle(passage).boxShadow).toBe('none');
      expect(passage.style.backgroundColor).not.toBe('');
    };
    await userEvent.click(passage);
    check();
    await userEvent.type(canvas.getByRole('textbox', { name: 'Comment' }), 'Keep this layout.');
    await userEvent.click(canvas.getByRole('button', { name: 'Add comment' }));
    check();
  },
};

const RICH_PARAGRAPH =
  'A **formatted paragraph** should use the same optimizer as plain prose. Read *the careful explanation*, follow [the reference](https://example.com), and keep `git  status --short` literal. Repeated words, **words**, and well-**known** choices must be measured in their own styles, without changing what you select or quote.';
const RICH_TEXT = `# Artifact typography

${RICH_PARAGRAPH}

- A **formatted list item** also balances its lines without losing the meaning or the original source.

> A quotation with *emphasis* follows the same paragraph layout rules.`;
const RICH_SOURCE = new Blob([RICH_TEXT], { type: 'text/markdown' });
const readerClient = { base: 'http://127.0.0.1:7777' } as GatewayClient;

function fittedParagraph(prose: HTMLElement) {
  expect(prose).toHaveAttribute('data-justice');
  const bounds = prose.getBoundingClientRect();
  const style = getComputedStyle(prose);
  const left = bounds.left + parseFloat(style.paddingLeft);
  const right = bounds.right - parseFloat(style.paddingRight);
  const lines = [...prose.children];
  expect(lines.length).toBeGreaterThan(1);
  for (const [index, line] of lines.entries()) {
    const range = document.createRange();
    range.selectNodeContents(line);
    const rect = range.getBoundingClientRect();
    expect(Math.abs(rect.left - left)).toBeLessThan(1);
    expect(rect.right).toBeLessThanOrEqual(right + 1);
    if (index < lines.length - 1) expect(Math.abs(rect.right - right)).toBeLessThan(1);
  }
}

/** The actual artifact reader, including rich text, code and responsive composition. */
export const RichReadOnlyArtifact: Story = {
  globals: { theme: 'blockether-dark' },
  render: () => (
    <div data-artifact-column style={{ width: 600, maxWidth: '100%' }}>
      <MarkdownArtifact
        client={readerClient}
        sid="s1"
        iterationId="i1"
        name="typography.md"
        mediaType="text/markdown"
        source={RICH_SOURCE}
        chrome={chrome}
      />
    </div>
  ),
  play: async ({ canvas, canvasElement }) => {
    await document.fonts.ready;
    const column = canvasElement.querySelector<HTMLElement>('[data-artifact-column]')!;
    await waitFor(() => expect(column.querySelector('p')).toHaveAttribute('data-justice'));
    const prose = column.querySelector('p')!;
    const original = prose.textContent;
    fittedParagraph(prose);
    const wideLines = prose.children.length;
    column.style.width = '300px';
    await waitFor(() => {
      fittedParagraph(prose);
      expect(prose.children.length).toBeGreaterThan(wideLines);
    });
    const link = canvas.getByRole('link', { name: 'the reference' });
    expect(link).toHaveAttribute('href', 'https://example.com');
    expect(prose.querySelectorAll('code')).toHaveLength(1);
    const code = prose.querySelector('code')!;
    expect(code.textContent).toBe('git  status --short');
    expect(getComputedStyle(code).wordSpacing).toBe('0px');
    expect(parseFloat(getComputedStyle(code).letterSpacing) || 0).toBe(0);
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
    expect(canvas.queryByRole('button', { name: 'Comment on the whole document' })).toBeNull();
  },
};

/** Formatting and optimized line spans remain transparent to artifact comments. */
export const RichArtifactComments: Story = {
  globals: { theme: 'blockether-dark' },
  args: { text: RICH_TEXT, onSave: fn(async () => 2) },
  render: (args) => (
    <div style={{ width: 420, maxWidth: '100%' }}>
      <MarkdownAnnotator {...args} />
    </div>
  ),
  play: async ({ canvas, canvasElement, args }) => {
    await document.fonts.ready;
    const prose = canvasElement.querySelector('p')!;
    await waitFor(() => fittedParagraph(prose));
    const original = prose.textContent;
    const markup = prose.innerHTML;
    await userEvent.click(prose.querySelector('strong')!);
    expect(prose.innerHTML).toBe(markup);
    const quote = canvasElement.querySelector('textarea[aria-label="Comment"]')!;
    expect(quote).toBeVisible();
    await userEvent.type(
      canvas.getByRole('textbox', { name: 'Comment' }),
      'Keep these line breaks.',
    );
    await userEvent.click(canvas.getByRole('button', { name: 'Add comment' }));
    expect(prose.innerHTML).toBe(markup);
    expect(prose.textContent).toBe(original);
    await userEvent.click(canvas.getByRole('button', { name: 'Save changes' }));
    expect(args.onSave).toHaveBeenCalledWith(
      renderAnnotated(RICH_TEXT, [
        { quote: quoteOf(original ?? ''), body: 'Keep these line breaks.' },
      ]),
    );
  },
};

const PLAN_TEXT =
  '# Session search\n\n**Feature:** session-search\n**Status:** ready\n\n## Spec\nFind a session by title without leaving the current conversation.\n\n## Implementation plan\n1. Search titles end to end; verify empty results and keyboard selection.\n2. Restore the selected session and its scroll position.\n\n## Open questions\nNone.';
const planning = { filename: 'PLAN-session-search.md', version: 3, onSend: fn(async () => {}) };
const specification = { text: PLAN_TEXT, planning, onSave: fn(async () => 4) };

/** One approval starts implementation of the viewed specification. */
export const PlanReady: Story = {
  args: specification,
  play: async ({ canvas, args, canvasElement }) => {
    await expect(canvas.queryByRole('button', { name: 'Save changes' })).toBeNull();
    await expect(canvas.queryByRole('button', { name: 'Send for revision' })).toBeNull();
    const workflow = canvas.getByRole('region', { name: 'Specification workflow' });
    await expect(workflow.querySelectorAll('button')).toHaveLength(1);
    const prose = canvasElement.querySelector('h1')!;
    await expect(workflow.getBoundingClientRect().top).toBeGreaterThan(
      prose.getBoundingClientRect().bottom,
    );
    await userEvent.click(canvas.getByRole('button', { name: 'Approve and start' }));
    await expect(args.planning!.onSend).toHaveBeenCalledTimes(1);
    await expect(args.planning!.onSend).toHaveBeenCalledWith('approve', 3);
    await expect(canvas.getByRole('status')).toHaveTextContent('Implementation requested for v3');
  },
};

/** A review round is sent only after the reader finishes adding remarks. */
export const SpecificationReviewRound: Story = {
  args: { ...specification, planning: { ...planning, onSend: fn(async () => {}) } },
  play: async ({ canvas, args }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Comment on the whole document' }));
    await userEvent.type(
      canvas.getByRole('textbox', { name: 'Comment' }),
      'Include archived sessions.',
    );
    await expect(canvas.queryByRole('button', { name: 'Approve and start' })).toBeNull();
    await userEvent.click(canvas.getByRole('button', { name: 'Add comment' }));
    await expect(args.planning!.onSend).not.toHaveBeenCalled();
    await expect(args.onSave).not.toHaveBeenCalled();
    await expect(canvas.getByRole('status')).toHaveTextContent('1 unresolved comment');
    await userEvent.click(canvas.getByRole('button', { name: 'Send for revision' }));
    await expect(args.onSave).toHaveBeenCalledTimes(1);
    await expect(args.onSave).toHaveBeenCalledWith(
      expect.stringContaining('Include archived sessions.'),
    );
    await expect(args.planning!.onSend).toHaveBeenCalledTimes(1);
    await expect(args.planning!.onSend).toHaveBeenCalledWith('revise', 4);
  },
};

export const SpecificationDraft: Story = {
  args: { ...specification, text: PLAN_TEXT.replace('ready', 'draft') },
};

/** Remarks must be resolved before approval becomes available. */
export const PlanCommented: Story = {
  args: {
    ...specification,
    text: `${PLAN_TEXT}\n\n## Comments\n\n- **Whole document** — Include archived sessions.\n`,
  },
  play: async ({ canvas, args }) => {
    await expect(canvas.queryByRole('button', { name: 'Approve and start' })).toBeNull();
    await expect(canvas.queryByRole('button', { name: 'Save changes' })).toBeNull();
    await userEvent.click(canvas.getByRole('button', { name: 'Send for revision' }));
    await expect(args.planning!.onSend).toHaveBeenCalledWith('revise', 3);
  },
};

/** Accepted specifications use the same single approval-and-start action. */
export const PlanAccepted: Story = {
  args: { ...specification, text: PLAN_TEXT.replace('ready', 'accepted') },
};

export const PlanSendFailure: Story = {
  args: {
    ...specification,
    planning: {
      ...planning,
      onSend: fn(async () => {
        throw new Error('Connection lost. Retry when connected.');
      }),
    },
  },
  play: async ({ canvas }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Approve and start' }));
    await expect(canvas.getByRole('status')).toHaveTextContent('Connection lost');
    await expect(canvas.getByRole('button', { name: 'Approve and start' })).toBeEnabled();
  },
};
