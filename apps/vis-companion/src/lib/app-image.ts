/**
 * What the `image` node of an app needs to decide before it paints: the source of
 * a picture, the box that keeps its place, and how a comparison shows.
 */

import type { LivePicture } from './live-view';

/** How a comparison shows the old and the new picture. */
export type ImageCompareMode = 'side' | 'swipe';

/** The comparison modes, in the order of the toggle. */
export const IMAGE_COMPARE_MODES: readonly ImageCompareMode[] = ['side', 'swipe'];

/** The words of each mode on its control. */
export const IMAGE_COMPARE_WORDS: Record<ImageCompareMode, string> = {
  side: 'Side by side',
  swipe: 'Swipe',
};

/** The picture as a data URL that an `img` can show, or nothing without data. */
export function pictureSource(picture: LivePicture): string | undefined {
  return picture.data ? `data:${picture.media_type};base64,${picture.data}` : undefined;
}

/** What a picture without data still tells: format, size in pixels and bytes. */
export function pictureFacts(picture: LivePicture): string {
  const format = picture.media_type.replace('image/', '').toUpperCase();
  const size = picture.width && picture.height ? `${picture.width} × ${picture.height}` : null;
  const bytes =
    picture.bytes === undefined
      ? null
      : picture.bytes < 1024
        ? `${picture.bytes} B`
        : picture.bytes < 1024 * 1024
          ? `${(picture.bytes / 1024).toFixed(1)} KB`
          : `${(picture.bytes / (1024 * 1024)).toFixed(1)} MB`;
  return [format, size, bytes].filter(Boolean).join(' · ');
}

/** The CSS aspect ratio of a picture of known size, so the box keeps its place. */
export function pictureRatio(picture: Pick<LivePicture, 'width' | 'height'>): string | undefined {
  return picture.width && picture.height ? `${picture.width} / ${picture.height}` : undefined;
}

/**
 * The aspect ratio of the swipe box, where both pictures lie on top of each
 * other: the new picture's ratio, else the old one's. The other picture fits
 * inside it. Without a known size the box takes the height of the new picture.
 */
export function swipeRatio(after: LivePicture, before: LivePicture): string | undefined {
  return pictureRatio(after) ?? pictureRatio(before);
}

/** The swipe line in percent of the box width, from 0 (all new) to 100 (all old). */
export function swipePosition(value: number): number {
  if (!Number.isFinite(value)) return 50;
  return Math.min(100, Math.max(0, Math.round(value)));
}
