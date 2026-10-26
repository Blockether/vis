import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import {
  PODCAST_VTT,
  STORY_ARTIFACT_HISTORY,
  STORY_ARTIFACTS,
  STORY_INERT_CLIENT,
} from '../dev/story-data';
import type { SessionArtifact } from '../lib/artifacts';
import { ArtifactsSheet } from './ArtifactsSheet';

/** The session's produced files, indexed without eagerly fetching their bytes. */
const meta = {
  title: 'Components/Artifacts sheet',
  component: ArtifactsSheet,
  args: {
    client: { appViews: async () => [] } as unknown as typeof STORY_INERT_CLIENT,
    sid: 'session-story',
    artifacts: STORY_ARTIFACTS,
    onClose: () => {},
  },
} satisfies Meta<typeof ArtifactsSheet>;

export default meta;
type Story = StoryObj<typeof meta>;

const close = fn();

/** Three non-previewable files prove the index is not secretly an image gallery. */
export const Files: Story = {
  args: { onClose: close },
  play: async ({ args, canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await userEvent.click(page.getByRole('button', { name: 'Close artifacts' }));
    await expect(args.onClose).toHaveBeenCalledOnce();
  },
};

/** A rewritten artifact keeps its history control inside the newest tile. */
export const History: Story = { args: { artifacts: [STORY_ARTIFACT_HISTORY] } };

/** A filter with no matches says so inside the same full sheet. */
export const Empty: Story = { args: { artifacts: [] } };

const note: SessionArtifact = {
  key: 'i1:0',
  kind: 'doc',
  name: 'plan.md',
  media: 'MD',
  mediaType: 'text/markdown',
  size: 30,
  sizeLabel: '30 B',
  turn: 1,
  iterationId: 'i1',
  index: 0,
  version: 1,
};

/** Opening a document keeps its band flush with the top of the full-screen surface. */
export const OpenedDocument: Story = {
  args: {
    client: {
      attachmentUrl: async () => 'blob:plan',
      attachmentBlob: async () =>
        new Blob(['# Plan\n\nRead without a margin.'], { type: 'text/markdown' }),
      retainAttachment: () => () => {},
      appViews: async () => [],
    } as unknown as typeof STORY_INERT_CLIENT,
    artifacts: [note],
    initialArtifact: note,
  },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await page.findByRole('heading', { name: 'Plan', level: 1 });

    await userEvent.click(page.getByRole('button', { name: 'Close plan.md' }));
    await expect(page.queryByRole('dialog', { name: 'plan.md' })).toBeNull();
  },
};

const episode: SessionArtifact = {
  key: 'i2:0',
  kind: 'doc',
  name: 'episode.vtt',
  media: 'VTT',
  mediaType: 'text/vtt',
  size: PODCAST_VTT.length,
  sizeLabel: '1KB',
  turn: 2,
  iterationId: 'i2',
  index: 0,
  version: 1,
};

/** The subtitle file's bytes, served the way the gateway client serves them. */
const subtitleClient = {
  attachmentUrl: async () => `data:text/vtt;charset=utf-8,${encodeURIComponent(PODCAST_VTT)}`,
  attachmentBlob: async () => new Blob([PODCAST_VTT], { type: 'text/vtt' }),
  retainAttachment: () => () => {},
  appViews: async () => [],
} as unknown as typeof STORY_INERT_CLIENT;

/** A subtitle tile peeks at its first cues, `0:00 Maya: …`, not at timing lines. */
export const SubtitleTile: Story = {
  args: { client: subtitleClient, artifacts: [episode, ...STORY_ARTIFACTS] },
};

/** Opened subtitles read as a timed script, one turn for each speaker. */
export const OpenedSubtitles: Story = {
  args: { client: subtitleClient, artifacts: [episode], initialArtifact: episode },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await page.findByRole('list', { name: 'episode.vtt' });
  },
};
