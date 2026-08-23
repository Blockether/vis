import { useState, type ReactNode } from "react";

import { artifactMedia, attachmentBytes } from "../lib/artifacts";
import {
  mediaCaptionClass,
  mediaFrameClass,
  mediaGridClass,
  mediaTileFrameClass,
} from "../lib/media-frame";
import { ImageGallery } from "../lib/gallery";
import { Disclosure, PROSE } from "./ui";

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
          {meta ? (
            <span className="shrink-0 uppercase tracking-wider opacity-70">
              {meta}
            </span>
          ) : null}
        </figcaption>
      ) : null}
    </figure>
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
 * italic, and justified to the column. A memo is somebody talking, and what the row
 * owes the reader is that it is a quotation of the audio directly above it — the mono
 * code face said "machine output" about a sentence a person said. There is no mic
 * beside the player either: the control already announces itself as audio, and a glyph
 * that repeats the widget next to it only takes column width from the scrubber.
 */
export function MediaRecording({
  name,
  meta,
  transcription,
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
  children: ReactNode;
}) {
  const [isTranscriptOpen, setIsTranscriptOpen] = useState(false);
  const transcript = transcription?.trim() ?? "";
  return (
    <figure className="mt-2.5 min-w-0 first:mt-0">
      <div className="min-w-0 border border-code-edge bg-code p-2">
        <div className="min-w-0">{children}</div>
      </div>
      {transcript ? (
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
            <p className={`min-w-0 whitespace-pre-wrap break-words border-l-2 border-code-edge bg-code px-3 py-2 text-meta italic text-dialog-hint ${PROSE}`}>
              {`“${transcript}”`}
            </p>
          ) : null}
        </div>
      ) : null}
      {name ? (
        <figcaption className={mediaCaptionClass}>
          <span className="min-w-0 flex-1 truncate">{name}</span>
          {meta ? (
            <span className="shrink-0 uppercase tracking-wider opacity-70">
              {meta}
            </span>
          ) : null}
        </figcaption>
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
export function MediaGrid({
  summary,
  children,
}: {
  summary?: string;
  children: ReactNode;
}) {
  return (
    <div className="mt-2.5 min-w-0 first:mt-0">
      <div className={mediaGridClass}>
        <ImageGallery>{children}</ImageGallery>
      </div>
      {summary ? (
        <p className="mt-1 min-w-0 truncate font-mono text-chip text-footer-muted">
          {summary}
        </p>
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
  const things = `${pictures.length} ${
    pictures.length === 1 ? "image" : "images"
  }`;
  const total = pictures.every((picture) => typeof picture.size === "number")
    ? attachmentBytes(
        pictures.reduce((sum, picture) => sum + (picture.size ?? 0), 0),
      )
    : "";
  return total ? `${things} · ${total}` : things;
}

/** `PNG · 287KB` — the right half of a plate's caption, spelled the same way
 *  for a picture the human sent and one the model produced. */
export function mediaMeta(item: {
  filename?: string;
  media_type?: string;
  size?: number;
}): string {
  return [artifactMedia(item), attachmentBytes(item.size)]
    .filter(Boolean)
    .join(" · ");
}
