import type { IterationAttachment, TranscriptionSegment } from './types';
import { isSubtitleMedia } from './artifacts';

/**
 * Subtitles (WebVTT and SRT) as timed lines.
 *
 * A podcast or a meeting recording often comes with a subtitle file. The app reads
 * that file as the lines of a transcript, and a recording in the same step plays
 * with those lines, the same as a recording that the gateway transcribed itself.
 */

/** One cue: its time span, its words and, when the file names one, the speaker. */
export interface SubtitleCue extends TranscriptionSegment {
  speaker?: string;
}

const STAMP = /^(?:(\d+):)?(\d{1,2}):(\d{2})[.,](\d{1,3})$/;

/** `00:01:02.500` or `00:01:02,500` (SRT) as seconds, or null. */
export function subtitleSeconds(stamp: string): number | null {
  const match = STAMP.exec(stamp.trim());
  if (!match) return null;
  const [, hours, minutes, seconds, fraction] = match;
  return (
    Number(hours ?? 0) * 3600 +
    Number(minutes) * 60 +
    Number(seconds) +
    Number(fraction.padEnd(3, '0')) / 1000
  );
}

const ENTITIES: Record<string, string> = {
  '&amp;': '&',
  '&lt;': '<',
  '&gt;': '>',
  '&nbsp;': ' ',
  '&quot;': '"',
  '&#39;': "'",
};

function cueText(lines: string[]): { speaker?: string; text: string } {
  const raw = lines.join('\n');
  const voice = /<v(?:\.[^\s>]*)?\s+([^>]+)>/.exec(raw);
  const text = raw
    .replace(/<[^>]*>/g, '')
    .replace(/&(?:amp|lt|gt|nbsp|quot|#39);/g, (entity) => ENTITIES[entity] ?? entity)
    .trim();
  const speaker = voice?.[1].trim();
  return speaker ? { speaker, text } : { text };
}

/**
 * The cues of a WebVTT or SRT file, in file order. Header, `NOTE`, `STYLE` and
 * `REGION` blocks have no timing line and are skipped, as is a cue without words.
 */
export function parseSubtitles(source: string): SubtitleCue[] {
  const blocks = source
    .replace(/^\uFEFF/, '')
    .replace(/\r\n?/g, '\n')
    .split(/\n[ \t]*\n/);
  const cues: SubtitleCue[] = [];
  for (const block of blocks) {
    const lines = block.split('\n');
    const at = lines.findIndex((line) => line.includes('-->'));
    if (at < 0) continue;
    const [from, rest = ''] = lines[at].split('-->');
    const start = subtitleSeconds(from);
    const end = subtitleSeconds(rest.trim().split(/\s+/)[0] ?? '');
    if (start === null || end === null) continue;
    const { speaker, text } = cueText(lines.slice(at + 1));
    if (!text) continue;
    cues.push(speaker ? { start, end, text, speaker } : { start, end, text });
  }
  return cues;
}

/** The cues as the player's timed lines; a named speaker leads the line. */
export function subtitleSegments(cues: SubtitleCue[]): TranscriptionSegment[] {
  return cues.map(({ start, end, text, speaker }) => ({
    start,
    end,
    text: speaker ? `${speaker}: ${text}` : text,
  }));
}

function stem(name: string | undefined): string {
  const base = (name ?? '').trim().toLowerCase();
  const dot = base.lastIndexOf('.');
  return dot > 0 ? base.slice(0, dot) : base;
}

/**
 * The subtitle file that belongs to `recording`, among the attachments of its step:
 * the one with the same name stem, else the only subtitle file beside the only
 * recording. Undefined when no file matches without doubt.
 */
export function captionsFor(
  recording: IterationAttachment,
  attachments: IterationAttachment[],
): IterationAttachment | undefined {
  const subtitles = attachments.filter((entry) =>
    isSubtitleMedia(entry.media_type, entry.filename),
  );
  const named = subtitles.find((entry) => stem(entry.filename) === stem(recording.filename));
  if (named) return named;
  const recordings = attachments.filter((entry) =>
    (entry.media_type ?? '').startsWith('audio/'),
  );
  return subtitles.length === 1 && recordings.length === 1 ? subtitles[0] : undefined;
}
