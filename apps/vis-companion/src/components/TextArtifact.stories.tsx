import type { Meta, StoryObj } from '@storybook/react-vite';
import { LOG_TEXT, NOTE_MARKDOWN, PODCAST_SRT, PODCAST_VTT } from '../dev/story-data';
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
  title: 'Components/Text artifact',
  component: TextBody,
  parameters: { layout: 'padded' },
} satisfies Meta<typeof TextBody>;

export default meta;

type Story = StoryObj<typeof meta>;

/** Opened: markdown, rendered as the document it is. */
export const Note: Story = {
  args: { text: NOTE_MARKDOWN, name: 'composer.md', mime: 'text/markdown' },
};

/** Inline: the same bytes as source, which is what a draft is. */
export const NoteSource: Story = {
  args: { text: NOTE_MARKDOWN, name: 'composer.md', mime: 'text/markdown', raw: true },
};

/** Machine output stays machine-faced, and long lines wrap rather than scroll away. */
export const Log: Story = {
  args: { text: LOG_TEXT, name: 'gateway.log', mime: 'text/plain' },
};

/** Subtitles read as a script: the start time, the speaker once per turn, then the words. */
export const Subtitles: Story = {
  args: { text: PODCAST_VTT, name: 'episode.vtt', mime: 'text/vtt' },
};

/** SubRip has no speakers, so each cue is its time and its words. */
export const SubtitlesSrt: Story = {
  args: { text: PODCAST_SRT, name: 'episode.srt', mime: 'application/x-subrip' },
};

/** An empty artifact is still a frame: the paper is what says "nothing here yet". */
export const Empty: Story = {
  args: { text: '', name: 'empty.log', mime: 'text/plain' },
};
