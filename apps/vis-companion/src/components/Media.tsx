import type { ReactNode } from "react";

import { artifactMedia, attachmentBytes } from "../lib/artifacts";
import {
  mediaCaptionClass,
  mediaFrameClass,
  mediaGridClass,
  mediaTileFrameClass,
} from "../lib/media-frame";
import { ImageGallery } from "../lib/gallery";

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
