// @vitest-environment jsdom
import { fireEvent, render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';

import {
  mediaFrameClass,
  mediaGridClass,
  mediaPendingClass,
  mediaTileFrameClass,
} from '../lib/media-frame';
import {
  MediaGrid,
  MediaPlate,
  MediaRecording,
  MediaTile,
  RecordingPlayer,
  clipTeaserSrc,
  mediaMeta,
  mediaSummary,
} from './Media';

/** The frame element of a rendered plate/tile, class list and all. */
const frame = (html: string) => /<div class="([^"]*)"/u.exec(html)?.[1] ?? '';

// Regression, iOS scroll jump: the box a picture occupies is decided before
// anyone knows what is in it, so the pulse, the picture and the failure notice
// all have to reserve the SAME rectangle. It used to be spelled at the call
// site once per state; now it is spelled by the plate, once.
describe('MediaPlate', () => {
  const withChild = (child: string) =>
    frame(
      renderToStaticMarkup(
        <MediaPlate name="shot.png" meta="PNG · 8B">
          <div className={child} />
        </MediaPlate>,
      ),
    );

  it('reserves the same box whatever it is showing', () => {
    expect(withChild(mediaPendingClass)).toBe(mediaFrameClass);
    expect(withChild('some-picture')).toBe(mediaFrameClass);
    expect(withChild('some-failure-notice')).toBe(mediaFrameClass);
  });

  it('docks the name and the format under the mat', () => {
    const html = renderToStaticMarkup(
      <MediaPlate name="shot.png" meta="PNG · 8B">
        <div />
      </MediaPlate>,
    );

    expect(html).toContain('<figcaption');
    expect(html).toContain('shot.png');
    expect(html).toContain('PNG · 8B');
  });

  it('carries no caption strip at all when there is no name', () => {
    expect(
      renderToStaticMarkup(
        <MediaPlate>
          <div />
        </MediaPlate>,
      ),
    ).not.toContain('<figcaption');
  });
});

describe('MediaGrid', () => {
  it('lays its tiles out in the gallery and says what they are', () => {
    const html = renderToStaticMarkup(
      <MediaGrid summary="2 images · 16B">
        <MediaTile>
          <div />
        </MediaTile>
        <MediaTile>
          <div />
        </MediaTile>
      </MediaGrid>,
    );

    expect(html).toContain(mediaGridClass);
    expect(html.match(new RegExp(mediaTileFrameClass, 'gu'))).toHaveLength(2);
    expect(html).toContain('2 images · 16B');
  });
});

// Regression, user report ("margin bottom from this component is not the same
// like margin top"): a media block sits in the transcript's own `gap-2.5` stack,
// and it used to spell a gap on BOTH edges — so the picture had 10px above it and
// 18-20px below, and the block read as if it belonged to the paragraph under it.
// The stack owns the trailing gap; the block only tops itself up to it, and never
// on the very first row.
describe("a media block's own edges", () => {
  const root = (html: string) => /^<\w+ class="([^"]*)"/u.exec(html)?.[1] ?? '';

  const edges = [
    [
      'MediaPlate',
      renderToStaticMarkup(
        <MediaPlate>
          <div />
        </MediaPlate>,
      ),
    ],
    [
      'MediaGrid',
      renderToStaticMarkup(
        <MediaGrid summary="1 image · 8B">
          <MediaTile>
            <div />
          </MediaTile>
        </MediaGrid>,
      ),
    ],
  ] as const;

  for (const [name, html] of edges) {
    it(`tops ${name} up to the stack's gap and never pads under it`, () => {
      const classes = root(html).split(/\s+/u);
      expect(classes.filter((one) => /^m[by]?-/u.test(one))).toEqual([]);
    });
  }
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
  const html = renderToStaticMarkup(
    <MediaRecording name="memo.m4a" meta="M4A · 8B">
      <audio controls />
    </MediaRecording>,
  );

  it('reserves no picture box', () => {
    expect(html).not.toContain(mediaFrameClass);
    expect(html).not.toContain(mediaTileFrameClass);
  });

  it('names the recording under the player it hands over', () => {
    expect(html).toContain('<audio');
    expect(html).toContain('<figcaption');
    expect(html).toContain('memo.m4a');
    expect(html).toContain('M4A · 8B');
  });

  // The words a memo carries are the only thing a reader can SKIM, and the same
  // string the model was given — but a minute of speech is a paragraph, so the row
  // stays one line until it is asked.
  it('folds the transcription away behind its own band', () => {
    const withWords = renderToStaticMarkup(
      <MediaRecording name="memo.m4a" meta="M4A · 8B" transcription="buy milk and call back">
        <audio controls />
      </MediaRecording>,
    );
    expect(withWords).toContain('TRANSCRIPTION');
    expect(withWords).toContain('aria-expanded="false"');
    expect(withWords).not.toContain('buy milk and call back');
  });

  it('shows no band at all when nobody has asked for a transcript', () => {
    expect(html).not.toContain('TRANSCRIPTION');
    expect(html).not.toContain('aria-expanded');
  });

  // Regression, issue: a recording whose transcription failed looked exactly like one
  // nobody had transcribed yet — an empty space under the player, and a model that was
  // handed the filename alone.
  it('says what the words are doing when there are none to show', () => {
    const band = (status: string) =>
      renderToStaticMarkup(
        <MediaRecording name="memo.m4a" meta="M4A · 8B" transcriptionStatus={status}>
          <audio controls />
        </MediaRecording>,
      );
    expect(band('pending')).toContain('TRANSCRIBING…');
    expect(band('unavailable')).toContain('NO TRANSCRIPTION');
    expect(band('silent')).toContain('NO SPEECH');
    // A status is a caption, never a control: there is nothing to open.
    expect(band('pending')).not.toContain('aria-expanded');
  });

  it('prefers the words themselves to whatever the status still says', () => {
    const settled = renderToStaticMarkup(
      <MediaRecording
        name="memo.m4a"
        transcription="buy milk and call back"
        transcriptionStatus="pending"
      >
        <audio controls />
      </MediaRecording>,
    );
    expect(settled).toContain('TRANSCRIPTION');
    expect(settled).not.toContain('TRANSCRIBING…');
  });

  // A mic glyph beside a player is the same claim twice: the control is already,
  // visibly, audio. It only ate column width from the scrubber.
  it('puts no icon beside the player', () => {
    expect(html).not.toContain('<svg');
  });

  // Speech is not code. Opened, the words read as an italic QUOTATION of the audio
  // above them without distorting the spacing between words.
  it('quotes the opened transcript in naturally spaced italic', async () => {
    render(
      <MediaRecording name="memo.m4a" transcription="buy milk and call back">
        <audio controls />
      </MediaRecording>,
    );
    await userEvent.click(screen.getByText('TRANSCRIPTION'));
    const words = screen.getByText(/buy milk and call back/);
    expect(words.textContent).toBe('“buy milk and call back”');
    expect(words.className).toContain('italic');
  });

  it('ignores a transcript that is only whitespace', () => {
    const blank = renderToStaticMarkup(
      <MediaRecording name="memo.m4a" transcription="   ">
        <audio controls />
      </MediaRecording>,
    );
    expect(blank).not.toContain('TRANSCRIPTION');
  });

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

  // Nothing placed the words in the audio — an older recording, or an engine that
  // only returns a string. The band is still the quotation it always was.
  it('quotes the whole transcript when there are no timings', async () => {
    render(
      <MediaRecording name="memo.m4a" transcription="buy milk and call back">
        <RecordingPlayer src="blob:memo" />
      </MediaRecording>,
    );
    await userEvent.click(screen.getByText('TRANSCRIPTION'));
    expect(screen.getByText(/buy milk and call back/).textContent).toBe(
      '“buy milk and call back”',
    );
    expect(screen.queryByRole('button', { name: 'buy milk' })).toBeNull();
  });
});

// The platform's own `<audio controls>` is another program's widget on this
// app's paper: a grey rounded pill, its own typeface, its own AirPlay and
// overflow buttons, and its own idea of contrast. What a listener uses is a
// button, a scrubber and a clock.
describe('RecordingPlayer', () => {
  it("hands the row this app's own controls instead of the platform's", () => {
    const html = renderToStaticMarkup(<RecordingPlayer src="blob:memo" />);

    expect(html).toContain('<audio');
    expect(html).not.toMatch(/<audio[^>]*controls/u);
    expect(html).toContain('aria-label="Play"');
    expect(html).toContain('type="range"');
  });

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

  // Metadata that has not landed reports `NaN`, and a scrubber cannot be drawn
  // against it — the bar stays inert rather than claiming a position.
  it('waits for the length before it offers seeking', () => {
    const view = render(<RecordingPlayer />);
    const scrubber = view.container.querySelector('input[type="range"]');

    expect(scrubber).toBeDisabled();
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
