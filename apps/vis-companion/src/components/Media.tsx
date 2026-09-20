import {
  createContext,
  useContext,
  useRef,
  useState,
  type ChangeEvent,
  type CSSProperties,
  type ReactNode,
  type RefObject,
} from 'react';

import { artifactMedia, attachmentBytes } from '../lib/artifacts';
import {
  mediaCaptionClass,
  mediaFrameClass,
  mediaGridClass,
  mediaTileFrameClass,
} from '../lib/media-frame';
import { ImageGallery } from '../lib/gallery';
import type { TranscriptionSegment } from '../lib/types';
import { PauseIcon, PlayIcon } from './icons';
import { Disclosure, PROSE } from './ui';

/**
 * ONE picture on its own plate: the reserved frame from `lib/media-frame` with
 * the caption strip docked under it.
 *
 * The frame is a WRAPPER, never a class list handed to whatever paints inside
 * it. `ExpandableImage`'s trigger spells `border-0 bg-transparent` on itself,
 * so the rail that put `border border-code-edge bg-code` on that same element
 * lost its border to Tailwind's emission order — which is why a picture the
 * human sent had no frame while the artifact the model produced two rows below
 * it did. The face lives here and at no call site.
 */
export function MediaPlate({
  name,
  meta,
  children,
}: {
  /** The caption's file name. Without one the plate carries no caption at all. */
  name?: string;
  /** The caption's right half, e.g. `PNG · 287KB`. */
  meta?: string;
  children: ReactNode;
}) {
  return (
    // The gap over a picture is the transcript's own rhythm, spelled ONCE: the
    // block that opens a step takes the stack's gap and adds none of its own,
    // or the whitespace above a gallery and the whitespace below it stop
    // matching.
    <figure className="mt-2.5 min-w-0 first:mt-0">
      <div className={mediaFrameClass}>{children}</div>
      {name ? (
        <figcaption className={mediaCaptionClass}>
          <span className="min-w-0 flex-1 truncate">{name}</span>
          {meta ? <span className="shrink-0 uppercase tracking-wider">{meta}</span> : null}
        </figcaption>
      ) : null}
    </figure>
  );
}

/**
 * What a recording's row says when it has NO words — the app's spelling of the
 * gateway's `transcription_status`.
 *
 * A player with nothing under it is the one thing this must never mean twice: a memo
 * still being transcribed, a memo nothing could read, and a memo nobody spoke into
 * are three different facts, and the reader is owed which one it is.
 */
const TRANSCRIPTION_STATUS_LABEL: Record<string, string> = {
  pending: 'TRANSCRIBING…',
  unavailable: 'NO TRANSCRIPTION',
  silent: 'NO SPEECH',
};

/**
 * The needle a recording's player and its own words SHARE.
 *
 * The player arrives as `children`, so the card that paints the transcript cannot
 * reach into it — the two meet here instead. The card owns the `<audio>` handle and
 * the current position; the player fills the handle and publishes the position as it
 * advances; a line of the transcript seeks by writing `currentTime`. A player
 * standing on its own reads the default below and behaves exactly as it did before.
 */
type RecordingTimeline = {
  /** Where the needle is, in seconds. */
  position: number;
  /** The card's `<audio>` handle, or null when no card is listening. */
  audioRef: RefObject<HTMLAudioElement | null> | null;
  /** The player's report that the needle moved. */
  onPosition: (seconds: number) => void;
};

const RecordingTimelineContext = createContext<RecordingTimeline>({
  position: 0,
  audioRef: null,
  onPosition: () => undefined,
});

/**
 * `mm:ss`, or `h:mm:ss` once a recording passes the hour — a meeting is measured
 * in hours, and `72:14` is not a time anybody reads.
 */
function clockTime(seconds: number): string {
  if (!Number.isFinite(seconds) || seconds <= 0) return '0:00';
  const whole = Math.floor(seconds);
  const secs = String(whole % 60).padStart(2, '0');
  const mins = Math.floor(whole / 60) % 60;
  const hours = Math.floor(whole / 3600);
  return hours > 0 ? `${hours}:${String(mins).padStart(2, '0')}:${secs}` : `${mins}:${secs}`;
}

/**
 * The control that plays ONE recording, in this app's own face.
 *
 * `<audio controls>` hands the row to the PLATFORM, and the platform paints a grey
 * lozenge with its own typeface, its own AirPlay and overflow buttons and its own
 * idea of contrast — a widget from another program parked on the transcript's own
 * paper. The element itself stays, because it is what decodes and plays; it is
 * hidden behind the three things a listener actually uses: a play/pause button at
 * thumb size, a scrubber that fills with the accent ink as it advances, and the
 * clock. Nothing else — every further glyph is column width taken from the only
 * part anybody drags.
 *
 * The scrubber is a real `input[type=range]`, so a keyboard and a screen reader
 * get seeking for free; only its face is ours.
 */
export function RecordingPlayer({
  src,
  onError,
}: {
  /** Where the bytes are. Absent while the URL is still being fetched. */
  src?: string;
  /** The bytes would not decode — the caller paints its own failure line. */
  onError?: () => void;
}) {
  const timeline = useContext(RecordingTimelineContext);
  const ownRef = useRef<HTMLAudioElement>(null);
  // Inside a recording card the CARD owns the element and the clock, so a sentence
  // pressed in the transcript moves this scrubber too. Alone, the player owns both.
  const audioRef = timeline.audioRef ?? ownRef;
  const [isPlaying, setIsPlaying] = useState(false);
  const [ownElapsed, setOwnElapsed] = useState(0);
  const [duration, setDuration] = useState(0);
  const elapsed = timeline.audioRef ? timeline.position : ownElapsed;
  // A stream still being fetched reports `Infinity` or `NaN` for its length, and
  // a scrubber cannot be drawn against either: until the metadata lands the bar
  // stays empty and inert rather than jumping to a made-up position.
  const total = Number.isFinite(duration) && duration > 0 ? duration : 0;
  const position = total > 0 ? Math.min(elapsed, total) : 0;

  const moveTo = (seconds: number) => {
    setOwnElapsed(seconds);
    timeline.onPosition(seconds);
  };

  const toggle = () => {
    const audio = audioRef.current;
    if (!audio) return;
    try {
      if (audio.paused) void Promise.resolve(audio.play()).catch(() => undefined);
      else audio.pause();
    } catch {
      // A platform that refuses playback leaves the button exactly where it was.
    }
  };

  const seek = (event: ChangeEvent<HTMLInputElement>) => {
    const audio = audioRef.current;
    const next = Number(event.target.value);
    if (!audio || !Number.isFinite(next)) return;
    audio.currentTime = next;
    moveTo(next);
  };

  return (
    <div className="flex min-w-0 items-center gap-3">
      <audio
        ref={audioRef}
        src={src}
        preload="metadata"
        className="hidden"
        onLoadedMetadata={(event) => setDuration(event.currentTarget.duration)}
        onDurationChange={(event) => setDuration(event.currentTarget.duration)}
        onTimeUpdate={(event) => moveTo(event.currentTarget.currentTime)}
        onPlay={() => setIsPlaying(true)}
        onPause={() => setIsPlaying(false)}
        onEnded={() => {
          setIsPlaying(false);
          moveTo(0);
        }}
        onError={onError}
      />
      <button
        type="button"
        onClick={toggle}
        aria-label={isPlaying ? 'Pause' : 'Play'}
        className="flex size-11 shrink-0 items-center justify-center border border-code-edge bg-thinking-surface text-accent-ink mouse:size-9 mouse:hover:text-accent"
      >
        {isPlaying ? <PauseIcon className="size-3.5" /> : <PlayIcon className="size-3.5" />}
      </button>
      <input
        type="range"
        min={0}
        max={total || 1}
        step="any"
        value={position}
        onChange={seek}
        disabled={total <= 0}
        aria-label="Seek"
        style={{ '--played': `${total > 0 ? (position / total) * 100 : 0}%` } as CSSProperties}
        className="h-1 w-full min-w-0 flex-1 cursor-pointer appearance-none bg-[linear-gradient(to_right,var(--color-accent-ink)_var(--played),var(--color-code-edge)_var(--played))] disabled:cursor-default [&::-moz-range-thumb]:size-3 [&::-moz-range-thumb]:border-0 [&::-moz-range-thumb]:bg-accent-ink [&::-webkit-slider-thumb]:size-3 [&::-webkit-slider-thumb]:appearance-none [&::-webkit-slider-thumb]:bg-accent-ink"
      />
      <span className="shrink-0 font-mono text-chip tabular-nums text-footer-muted">
        {clockTime(position)} / {clockTime(total)}
      </span>
    </div>
  );
}

/**
 * ONE recording as a ROW: the platform's own player, the file name under it, and —
 * when something could read the audio — its TRANSCRIPTION, folded away.
 *
 * A voice memo has no picture, so the reserved 4:3 box a still or a clip stands in
 * would be a frame around silence — and a poster frame that cannot be started is a
 * picture that lies. What identifies a recording is its NAME; what the reader wants
 * is the control that starts it, at the width of the column and at a height a thumb
 * can hit.
 *
 * The transcript is FOLDED because of what it is: the same words the reader can hear,
 * and the same words the model was given. Open, it answers "what does this say?"
 * without a playback; shut, a two-minute memo stays one row. It is a `Disclosure` at
 * band weight, so it opens exactly like the THINKING band and the tool step above it
 * rather than inventing a fourth chevron for the same question.
 *
 * The words are painted as SPEECH QUOTED, not as code: curly quotes around them,
 * italic, and wrapped to the column. A memo is somebody talking, and what the row
 * owes the reader is that it is a quotation of the audio directly above it — the mono
 * code face said "machine output" about a sentence a person said. There is no mic
 * beside the player either: the control already announces itself as audio, and a glyph
 * that repeats the widget next to it only takes column width from the scrubber.
 *
 * When the words arrive as TIMED LINES, the band FOLLOWS the audio: the line being
 * spoken is lit as the needle reaches it, and pressing a line jumps the recording
 * there and plays on from it. It is the same question asked in both directions —
 * "where am I in these words?" and "play me this sentence" — and it costs nothing
 * beyond the timestamps the speech engine already produced.
 */
export function MediaRecording({
  name,
  meta,
  transcription,
  transcriptionSegments,
  transcriptionStatus,
  children,
}: {
  /** The caption's file name. Without one the row carries no caption at all. */
  name?: string;
  /** The caption's right half, e.g. `M4A · 412KB`. */
  meta?: string;
  /**
   * What the recording SAYS, transcribed once by the gateway's own speech engine on
   * the turn that carried it. Absent means nothing read it — no engine, or audio it
   * could not decode — and the row shows the player alone rather than an empty band.
   */
  transcription?: string;
  /**
   * The same words CUT INTO TIMED LINES, when the engine could place them in the
   * audio. The band then follows playback instead of standing still, and each line
   * is somewhere to jump to.
   */
  transcriptionSegments?: TranscriptionSegment[];
  /**
   * WHY there are none, when there are none: `pending` while the speech engine is
   * still working, `unavailable` when this machine could not read the recording,
   * `silent` when it read the whole thing and nobody spoke.
   */
  transcriptionStatus?: string;
  children: ReactNode;
}) {
  const audioRef = useRef<HTMLAudioElement>(null);
  const [position, setPosition] = useState(0);
  const [isTranscriptOpen, setIsTranscriptOpen] = useState(false);
  const transcript = transcription?.trim() ?? '';
  const lines = (transcriptionSegments ?? []).filter((line) => line.text.trim().length > 0);
  const spoken = transcript.length > 0 || lines.length > 0;
  const statusLabel = spoken ? '' : (TRANSCRIPTION_STATUS_LABEL[transcriptionStatus ?? ''] ?? '');
  // The line being SPOKEN is the last one that has STARTED: a needle caught in the
  // breath between two sentences keeps the sentence it just finished lit rather
  // than blinking off, and a player parked before the first word lights nothing.
  const activeLine = lines.reduce(
    (found, line, index) => (line.start <= position ? index : found),
    -1,
  );

  const playFrom = (seconds: number) => {
    const audio = audioRef.current;
    setPosition(seconds);
    if (!audio) return;
    try {
      audio.currentTime = seconds;
      void Promise.resolve(audio.play()).catch(() => undefined);
    } catch {
      // A platform that refuses playback still moved the needle there.
    }
  };

  return (
    <figure className="mt-2.5 min-w-0 first:mt-0">
      {/* ONE card: the player, and the name strip DOCKED under it on the same
          paper. `mediaCaptionClass` cuts its own top border away because a
          plate's caption sits straight on the picture frame — a recording has a
          transcription band between the two, so borrowing it left the name
          floating in a three-sided box under a second box. */}
      <div className="min-w-0 border border-code-edge bg-code">
        <div className="min-w-0 p-2">
          <RecordingTimelineContext.Provider
            value={{ position, audioRef, onPosition: setPosition }}
          >
            {children}
          </RecordingTimelineContext.Provider>
        </div>
        {name || meta || statusLabel ? (
          <figcaption className="flex min-w-0 items-center gap-2 border-t border-code-edge bg-thinking-surface px-2 py-1 font-mono text-chip text-footer-muted">
            <span className="min-w-0 flex-1 truncate">{name}</span>
            {/* Its OWN word beside the format, never joined to it: what the
                reader is waiting for is not a property of the container. */}
            {statusLabel ? (
              <span className="shrink-0 uppercase tracking-wider text-dialog-hint">
                {statusLabel}
              </span>
            ) : null}
            {meta ? <span className="shrink-0 uppercase tracking-wider">{meta}</span> : null}
          </figcaption>
        ) : null}
      </div>
      {spoken ? (
        <div className="min-w-0">
          <Disclosure
            isOpen={isTranscriptOpen}
            tone="step"
            bleed
            onClick={() => setIsTranscriptOpen(!isTranscriptOpen)}
          >
            TRANSCRIPTION
          </Disclosure>
          {isTranscriptOpen ? (
            lines.length > 0 ? (
              <div className="min-w-0 border-l-2 border-code-edge bg-code px-3 py-2">
                {lines.map((line, index) => (
                  <button
                    key={`${line.start}-${index}`}
                    type="button"
                    onClick={() => playFrom(line.start)}
                    aria-current={index === activeLine ? 'true' : undefined}
                    className={`block w-full min-w-0 cursor-pointer whitespace-pre-wrap break-words border-0 bg-transparent py-0.5 text-left text-meta italic ${PROSE} ${index === activeLine ? 'text-accent-ink' : 'text-dialog-hint mouse:hover:text-accent'}`}
                  >
                    {line.text}
                  </button>
                ))}
              </div>
            ) : (
              <p
                className={`min-w-0 whitespace-pre-wrap break-words border-l-2 border-code-edge bg-code px-3 py-2 text-meta italic text-dialog-hint ${PROSE}`}
              >
                {`“${transcript}”`}
              </p>
            )
          ) : null}
        </div>
      ) : null}
    </figure>
  );
}

/**
 * Several pictures as a gallery.
 *
 * The names leave the tiles and come back as ONE line under the grid: a caption
 * per tile is two rows of chrome around a 183px thumbnail, and the name of each
 * picture is already in the viewer that a tap opens. The line says what the
 * whole group is instead — `3 images · 1.2MB`.
 *
 * The grid is also the GALLERY: every picture in it registers with
 * {@link ImageGallery}, so opening one tile can walk to the others with the
 * arrow keys instead of closing the viewer once per picture.
 */
export function MediaGrid({ summary, children }: { summary?: string; children: ReactNode }) {
  return (
    <div className="mt-2.5 min-w-0 first:mt-0">
      <div className={mediaGridClass}>
        <ImageGallery>{children}</ImageGallery>
      </div>
      {summary ? (
        <p className="mt-1 min-w-0 truncate font-mono text-chip text-footer-muted">{summary}</p>
      ) : null}
    </div>
  );
}

/** ONE cell of a {@link MediaGrid}: the plate's own paper and edge, square, and
 *  reserved before its bytes land for exactly the same reason the plate is. */
export function MediaTile({ children }: { children: ReactNode }) {
  return <div className={mediaTileFrameClass}>{children}</div>;
}

/**
 * `3 images · 1.2MB` — what a gallery is, said once under it.
 *
 * The weight is claimed only when every picture reported one: a partial total
 * is a wrong number, not a smaller one.
 */
export function mediaSummary(pictures: { size?: number }[]): string {
  const things = `${pictures.length} ${pictures.length === 1 ? 'image' : 'images'}`;
  const total = pictures.every((picture) => typeof picture.size === 'number')
    ? attachmentBytes(pictures.reduce((sum, picture) => sum + (picture.size ?? 0), 0))
    : '';
  return total ? `${things} · ${total}` : things;
}

/** `PNG · 287KB` — the right half of a plate's caption, spelled the same way
 *  for a picture the human sent and one the model produced. */
export function mediaMeta(item: { filename?: string; media_type?: string; size?: number }): string {
  return [artifactMedia(item), attachmentBytes(item.size)].filter(Boolean).join(' · ');
}
