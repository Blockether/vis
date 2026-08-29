import type { Meta, StoryObj } from '@storybook/react-vite';
import {
  PICTURE_SUMMARY,
  RECORDING_TRANSCRIPT,
  STORY_PICTURES,
} from '../dev/story-data';
import { mediaContentClass, mediaTileContentClass } from '../lib/media-frame';
import { MediaGrid, MediaPlate, MediaRecording, MediaTile } from './Media';

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
  title: 'Components/Media',
  parameters: { layout: 'padded' },
} satisfies Meta;

export default meta;

type Story = StoryObj<typeof meta>;

const [first, second, third] = STORY_PICTURES;

/** One picture: the plate, and the name it was sent under. */
export const Plate: Story = {
  render: () => (
    <MediaPlate name={first.name} meta={first.meta}>
      <img src={first.src} alt="" className={mediaContentClass} />
    </MediaPlate>
  ),
};

/** A name longer than the plate: it truncates, and the weight keeps its column. */
export const PlateLongName: Story = {
  render: () => (
    <MediaPlate name={second.name} meta={second.meta}>
      <img src={second.src} alt="" className={mediaContentClass} />
    </MediaPlate>
  ),
};

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
    <MediaRecording
      name="memo-2.m4a"
      meta="M4A · 412KB"
      transcription={RECORDING_TRANSCRIPT}
    >
      <audio controls preload="metadata" className="h-11 w-full" />
    </MediaRecording>
  ),
};

/** Nobody spoke, and the row says so instead of showing an empty band. */
export const RecordingSilent: Story = {
  render: () => (
    <MediaRecording name="memo-3.m4a" meta="M4A · 96KB" transcriptionStatus="silent">
      <audio controls preload="metadata" className="h-11 w-full" />
    </MediaRecording>
  ),
};

/** No name at all: an anonymous clip is the plate alone, with no caption strip. */
export const PlateUncaptioned: Story = {
  render: () => (
    <MediaPlate>
      <img src={third.src} alt="" className={mediaContentClass} />
    </MediaPlate>
  ),
};
