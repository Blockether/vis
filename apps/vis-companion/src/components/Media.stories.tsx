import type { Meta, StoryObj } from '@storybook/react-vite';
import { PICTURE_SUMMARY, RECORDING_TRANSCRIPT, STORY_PICTURES } from '../dev/story-data';
import { mediaTileContentClass } from '../lib/media-frame';
import { MediaGrid, MediaRecording, MediaTile, RecordingPlayer } from './Media';

/**
 * WHAT A TURN CARRIED, ON THE PAPER THE TRANSCRIPT GIVES IT.
 *
 * One picture stands on a plate with its name under it; several become a grid
 * whose names collapse into one line, because a caption per 183px thumbnail is two
 * rows of chrome around the picture. A recording has no picture at all, so it is a
 * ROW: the player, and the words the gateway heard, folded.
 *
 * The pictures here are inline SVG, so the sheet draws the FRAME — the reserved
 * box, the edge, the caption rhythm — with nothing to fetch and nothing to wait
 * for. What a real photograph does inside that frame is `object-contain`, and that
 * is the same class the app hands it.
 */
const meta = {
  title: 'Session/Artifacts/Media',
  parameters: { layout: 'padded' },
} satisfies Meta;

export default meta;

type Story = StoryObj<typeof meta>;

/** Three pictures: the names leave the tiles and come back as one line. */
export const Grid: Story = {
  render: () => (
    <MediaGrid summary={PICTURE_SUMMARY}>
      {STORY_PICTURES.map((one) => (
        <MediaTile key={one.name}>
          <img src={one.src} alt="" className={mediaTileContentClass} />
        </MediaTile>
      ))}
    </MediaGrid>
  ),
};

/** A memo with words: the transcription folds under the player, quoted. */
export const Recording: Story = {
  render: () => (
    <MediaRecording name="memo-2.m4a" meta="M4A · 412KB" transcription={RECORDING_TRANSCRIPT}>
      <RecordingPlayer />
    </MediaRecording>
  ),
};
