import { describe, expect, it } from 'vitest';
import { captionsFor, parseSubtitles, subtitleSeconds } from './subtitles';
import type { IterationAttachment } from './types';

const VTT = `WEBVTT

NOTE produced by vis-podcaster

00:00:00.300 --> 00:00:15.200
<v Maya>Welcome to Long Story Short &amp; friends.

00:00:15.550 --> 00:00:25.470
<v.host Daniel>Guilty as charged.
Hi, Maya.
`;

const SRT = `1\r\n00:01:02,500 --> 00:01:04,000\r\n<i>Hello</i> there.\r\n\r\n2\r\n00:01:05,000 --> 00:01:06,000\r\n\r\n`;

const file = (filename: string, media_type: string, index = 0): IterationAttachment =>
  ({ filename, media_type, index, iteration_id: 'it-1' }) as IterationAttachment;

describe('subtitles', () => {
  it('reads WebVTT cues with their speakers and skips notes', () => {
    expect(parseSubtitles(VTT)).toEqual([
      { start: 0.3, end: 15.2, speaker: 'Maya', text: 'Welcome to Long Story Short & friends.' },
      { start: 15.55, end: 25.47, speaker: 'Daniel', text: 'Guilty as charged.\nHi, Maya.' },
    ]);
  });

  it('reads SRT cues, strips markup and drops a cue without words', () => {
    expect(parseSubtitles(SRT)).toEqual([{ start: 62.5, end: 64, text: 'Hello there.' }]);
  });

  it('accepts short stamps and refuses text that is not a time', () => {
    expect(subtitleSeconds('01:02.5')).toBe(62.5);
    expect(subtitleSeconds('1:00:00.000')).toBe(3600);
    expect(subtitleSeconds('soon')).toBeNull();
  });

  it('pairs a recording with its subtitles by name, else only without doubt', () => {
    const mp3 = file('episode.mp3', 'audio/mpeg');
    const vtt = file('episode.vtt', 'text/vtt', 1);
    const other = file('notes.vtt', 'text/vtt', 2);
    expect(captionsFor(mp3, [mp3, other, vtt])).toBe(vtt);
    expect(captionsFor(mp3, [mp3, other])).toBe(other);
    expect(captionsFor(mp3, [mp3, other, file('trailer.srt', 'application/x-subrip', 3)])).toBeUndefined();
    expect(captionsFor(mp3, [mp3, file('second.mp3', 'audio/mpeg', 4), other])).toBeUndefined();
  });
});
