import type { Meta, StoryObj } from '@storybook/react-vite';
import { LOG_TEXT } from '../dev/story-data';
import { TextBody } from './TextArtifact';

/**
 * A DOCUMENT THE MODEL WROTE, READ IN THE TRANSCRIPT.
 *
 * The same component answers two questions through layout: a note renders as a
 * document, while a log stays pre-wrapped and keeps its authored line breaks. Both
 * use the app's one JetBrains Mono face.
 *
 * `raw` is the third state and the one worth looking at: a note still being
 * written is shown as its SOURCE inline, because rendering a half-finished plan in
 * the transcript turns the turn that made it into a wall of headings. Opening it
 * is what renders it.
 */
const meta = {
  title: 'Session/Artifacts/Text',
  component: TextBody,
  parameters: { layout: 'padded' },
} satisfies Meta<typeof TextBody>;

export default meta;

type Story = StoryObj<typeof meta>;

/** Machine output stays machine-faced, and long lines wrap rather than scroll away. */
export const LongLines: Story = {
  args: { text: LOG_TEXT, name: 'gateway.log', mime: 'text/plain' },
};
