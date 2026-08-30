import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent } from 'storybook/test';

import { LOG_TEXT, NOTE_ANNOTATED, NOTE_MARKDOWN } from '../dev/story-data';
import { type DocumentChrome, MarkdownAnnotator } from './MarkdownArtifact';

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
    <div className="flex items-center justify-between gap-3">
      <span className="text-meta text-muted">{note || 'plan.md'}</span>
      {actions}
    </div>
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
    await userEvent.click(canvas.getByRole('button', { name: 'Save' }));
    await expect(args.onSave).toHaveBeenCalledTimes(1);
    await expect(args.onSave).toHaveBeenCalledWith(
      expect.stringContaining('Worth a second pass.'),
    );
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
