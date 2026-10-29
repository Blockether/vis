// @vitest-environment jsdom
import { fireEvent, render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';

import { mediaFrameClass } from '../lib/media-frame';
import { MediaPlate, MediaRecording, RecordingPlayer, clipTeaserSrc, mediaMeta, mediaSummary } from './Media';

/** The frame element of a rendered plate/tile, class list and all. */
const frame = (html: string) => /<div class="([^"]*)"/u.exec(html)?.[1] ?? '';

// Regression, iOS scroll jump: the box a picture occupies is decided before
// anyone knows what is in it, so the pulse, the picture and the failure notice
// all have to reserve the SAME rectangle. It used to be spelled at the call
// site once per state; now it is spelled by the plate, once.
describe('MediaPlate', () => {
  // User report: a wide or tall screenshot stood in the 4:3 reservation between two
  // bands of empty mat. When its size is known before it decodes, the frame takes the
  // picture's own ratio, the figure its width, and the frame keeps every class.
  it("fits a known picture's frame and figure to the picture, and nothing else", () => {
    const plate = (size?: { width: number; height: number }) =>
      renderToStaticMarkup(
        <MediaPlate name="shot.png" meta="PNG · 8B" size={size}>
          <div />
        </MediaPlate>,
      );
    expect(frame(plate({ width: 851, height: 332 }))).toBe(mediaFrameClass);
    expect(plate({ width: 851, height: 332 })).toContain('style="aspect-ratio:851 / 332"');
    const tall = plate({ width: 390, height: 844 });
    expect(tall).toContain('style="aspect-ratio:390 / 844"');
    expect(tall).toContain('style="width:min(100%, 390px, calc(60svh * 390 / 844))"');
    expect(plate()).not.toContain('style=');
  });
});

describe("the gallery's own line", () => {
  it('counts what it holds', () => {
    expect(mediaSummary([{ size: 1 }])).toBe('1 image · 1B');
    expect(mediaSummary([{ size: 1024 }, { size: 1024 }])).toBe('2 images · 2.0KB');
  });

  // A weight that only some of the pictures reported is a WRONG number, not a
  // smaller one, so the line simply does not claim one.
  it('claims a weight only when every picture reported one', () => {
    expect(mediaSummary([{ size: 1024 }, {}])).toBe('2 images');
  });
});

describe("a plate's caption", () => {
  it('names the format from the file, then its weight', () => {
    expect(mediaMeta({ filename: 'shot.png', size: 8 })).toBe('PNG · 8B');
    expect(mediaMeta({ media_type: 'image/webp' })).toBe('WEBP');
  });
});

// A recording is the one artifact with nothing to paint. Given the plate it
// would reserve a 4:3 box around silence and caption it like a picture that
// failed to decode.
describe('MediaRecording', () => {
  // Regression: the speech engine timestamps every token it decodes, and the row
  // threw the timings away — the words stood still while the audio moved, and the
  // only way back to a sentence was dragging the scrubber until it turned up.
  it('follows the player through timed lines and seeks to the one pressed', async () => {
    const { container } = render(
      <MediaRecording
        name="memo.m4a"
        transcription="buy milk and call back"
        transcriptionSegments={[
          { start: 0, end: 2, text: 'buy milk' },
          { start: 2, end: 4, text: 'and call back' },
        ]}
      >
        <RecordingPlayer src="blob:memo" />
      </MediaRecording>,
    );
    await userEvent.click(screen.getByText('TRANSCRIPTION'));
    const first = screen.getByRole('button', { name: 'buy milk' });
    const second = screen.getByRole('button', { name: 'and call back' });
    expect(first).toHaveAttribute('aria-current', 'true');

    const audio = container.querySelector('audio');
    if (audio) {
      audio.currentTime = 2.5;
      fireEvent.timeUpdate(audio);
    }
    expect(second).toHaveAttribute('aria-current', 'true');
    expect(first).not.toHaveAttribute('aria-current');

    await userEvent.click(second);
    expect(audio?.currentTime).toBe(2);
  });
});

// The platform's own `<audio controls>` is another program's widget on this
// app's paper: a grey rounded pill, its own typeface, its own AirPlay and
// overflow buttons, and its own idea of contrast. What a listener uses is a
// button, a scrubber and a clock.
describe('RecordingPlayer', () => {

  // An hour-long recording is the case that made this necessary, and `61:15` is
  // not a time anybody reads.
  it('clocks a long recording in hours, and flips the one button as it runs', () => {
    const view = render(<RecordingPlayer src="blob:memo" />);
    const audio = view.container.querySelector('audio');
    expect(audio).toBeInTheDocument();
    if (!audio) return;
    Object.defineProperty(audio, 'duration', { value: 3675, configurable: true });

    fireEvent.loadedMetadata(audio);
    expect(screen.getByText('0:00 / 1:01:15')).toBeInTheDocument();

    fireEvent.play(audio);
    expect(screen.getByLabelText('Pause')).toBeInTheDocument();
    fireEvent.pause(audio);
    expect(screen.getByLabelText('Play')).toBeInTheDocument();
  });
});

// Regression, issue vis_session_id#83d1d828-d2a1-45b2-bbdc-4a5fea1ec354: iOS painted a
// clip as a grey plate until it played; a seek to its first frame paints that frame.
describe('clipTeaserSrc', () => {
  it('opens a clip on its first frame without breaking inline or named sources', () => {
    expect(clipTeaserSrc('blob:vis/1')).toBe('blob:vis/1#t=0.001');
    expect(clipTeaserSrc('https://gateway.example.com/clip.mp4')).toBe(
      'https://gateway.example.com/clip.mp4#t=0.001',
    );
    expect(clipTeaserSrc('data:video/mp4;base64,AAAA')).toBe('data:video/mp4;base64,AAAA');
    expect(clipTeaserSrc('blob:vis/1#t=4')).toBe('blob:vis/1#t=4');
  });
});
