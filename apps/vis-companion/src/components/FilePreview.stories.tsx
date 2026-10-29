import type { Meta, StoryObj } from '@storybook/react-vite';

import { STORY_FILE_WINDOW, storyPreviewClient } from '../dev/story-data';
import { FilePreview } from './FilePreview';

/**
 * The file a pressed path names, read where the session is read. The sheet stands on
 * the line the press pointed at, and the machine that holds the tree is one band
 * button away — the phone that pressed has no editor to open.
 */
const meta = {
  title: 'Session/Artifacts/File preview',
  component: FilePreview,
  parameters: { layout: 'fullscreen' },
  args: {
    client: storyPreviewClient(STORY_FILE_WINDOW),
    sid: 'session-story',
    path: STORY_FILE_WINDOW.path,
    onClose: () => {},
  },
} satisfies Meta<typeof FilePreview>;

export default meta;
type Story = StoryObj<typeof meta>;

/** A press that carried a line: that line is marked, and the window is built around it. */
export const MarkedLine: Story = { args: { line: STORY_FILE_WINDOW.line } };
