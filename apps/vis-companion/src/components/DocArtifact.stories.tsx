import type { Meta, StoryObj } from '@storybook/react-vite';

import { STORY_DOC_URL } from '../dev/story-data';
import { DocFrame } from './DocArtifact';

/**
 * AN ARTIFACT, QUARANTINED.
 *
 * The page paints live — markup, CSS and script — because the sandbox already
 * says what it may touch ({@link docSandbox}), so there is nothing left for a
 * strip above the frame to ask about. What the gallery is looking at is the FRAME
 * around a stranger's document: the paper it sits on, the edge, and the fact that
 * an opened document always fills its box.
 *
 * The bytes are a `data:` URL rather than an attachment's object URL, so the
 * story downloads nothing and two frames of it compare.
 */
const meta = {
  title: 'Components/Doc frame',
  component: DocFrame,
  parameters: { layout: 'padded' },
  decorators: [
    (Story) => (
      <div className="flex h-[520px] flex-col">
        <Story />
      </div>
    ),
  ],
  args: { url: STORY_DOC_URL, mime: 'text/html', name: 'coverage.html' },
} satisfies Meta<typeof DocFrame>;

export default meta;

type Story = StoryObj<typeof meta>;

/** A page artifact, opened. */
export const Opened: Story = {};
