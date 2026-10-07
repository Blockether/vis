import { describe, expect, it } from 'vitest';

import {
  inlinePictureSize,
  mediaCaptionClass,
  mediaContentClass,
  mediaFrameClass,
  mediaGridClass,
  mediaGroupLayout,
  mediaTileContentClass,
  mediaTileFrameClass,
  plateFit,
} from './media-frame';

// Regression, issue: scrolling an iOS transcript full of screenshots jumped.
// A produced-image tile reserved a 96 px pulse while its object URL loaded and
// then became whatever the picture measured (up to 60svh), and a user bubble's
// pasted image reserved nothing at all until it decoded. Each of those swaps
// above the fold shoved the reader's line down — and the scroll corrector
// stands down while a finger is on the glass, so nobody put it back.
describe('transcript media frame', () => {
  it('reserves the box from the column and the viewport, never from the decoded picture', () => {
    expect(mediaFrameClass).toContain('w-full');
    expect(mediaFrameClass).toMatch(/aspect-/u);
    // `w-auto`/`h-auto` are exactly the "ask the image how big it is" sizings
    // that made the box move once the bytes landed.
    expect(mediaFrameClass).not.toMatch(/\b[wh]-auto\b/u);
  });

  it('contains the media inside the reserved box instead of letting it push', () => {
    expect(mediaContentClass).toContain('h-full');
    expect(mediaContentClass).toContain('w-full');
    expect(mediaContentClass).toContain('object-contain');
  });
});

// Regression, user report ("the contain object makes it look awful — the image
// filename is ON the image instead of under it as a label"): the picture was
// shoved to `object-left` inside an unframed 4:3 box, so a tall screenshot left
// a wide empty half with the bare caption floating under it.
describe('the media plate', () => {
  it("centres the picture on the frame's own mat", () => {
    expect(mediaContentClass).toContain('object-center');
    expect(mediaContentClass).not.toContain('object-left');
  });

  it('frames the mat so the letterbox is paper, not a gap', () => {
    expect(mediaFrameClass).toContain('border');
    expect(mediaFrameClass).toContain('bg-code');
    expect(mediaFrameClass).toContain('overflow-hidden');
  });

  it('docks the name under the mat as a label sharing that frame', () => {
    expect(mediaCaptionClass).toContain('border');
    expect(mediaCaptionClass).toContain('border-t-0');
    expect(mediaCaptionClass).toContain('bg-thinking-surface');
  });
});

// ONE picture is a plate; several are a gallery. Four dropped screenshots used
// to be four 60svh plates stacked down the column, which is a wall to scroll
// past rather than something to look at.
describe('the media gallery', () => {
  it('plates a lone picture and grids the rest', () => {
    expect(mediaGroupLayout(0)).toBe('plate');
    expect(mediaGroupLayout(1)).toBe('plate');
    expect(mediaGroupLayout(2)).toBe('grid');
    expect(mediaGroupLayout(9)).toBe('grid');
  });

  it('reserves a tile exactly as it reserves a plate', () => {
    expect(mediaTileFrameClass).toContain('w-full');
    expect(mediaTileFrameClass).toMatch(/aspect-/u);
    expect(mediaTileFrameClass).toContain('overflow-hidden');
    expect(mediaTileFrameClass).not.toMatch(/\b[wh]-auto\b/u);
  });

  it("wears the plate's own paper and edge, at gallery size", () => {
    expect(mediaTileFrameClass).toContain('border-code-edge');
    expect(mediaTileFrameClass).toContain('bg-code');
  });

  it('fills a tile instead of matting it', () => {
    expect(mediaTileContentClass).toContain('object-cover');
    expect(mediaContentClass).toContain('object-contain');
  });

  // An iPad is a wide TOUCH device: width may add a column, and only a mouse
  // may take the tighter one. Nothing here pins a smaller box.
  it('lets width add columns and never shrink a hit box', () => {
    expect(mediaGridClass).toContain('grid-cols-2');
    expect(mediaGridClass).toContain('sm:grid-cols-3');
    expect(mediaGridClass).toContain('mouse:grid-cols-4');
    expect(mediaGridClass).not.toMatch(/\bsm:(?:min-)?[wh]-/u);
  });
});

// Regression, user report with a screenshot: a wide screenshot sat under a band of
// empty mat on its plate. WebKit resolved the zoom trigger's `h-full` against the
// 4:3 height before the 60svh cap, so the picture was centred in a box taller than
// its frame, and the frame cut that box off at the bottom.
describe('the plate frame child', () => {
  it('lays the child over the whole frame instead of a percentage height', () => {
    expect(mediaFrameClass).toContain('relative');
    expect(mediaFrameClass).toContain('*:absolute');
    expect(mediaFrameClass).toContain('*:inset-0');
  });
});

describe('the plate fit', () => {
  // User report: a wide or tall screenshot sat in bands of empty mat inside the 4:3 frame.
  it('gives the frame the picture ratio, wide or tall', () => {
    expect(plateFit({ width: 851, height: 332 })?.aspectRatio).toBe('851 / 332');
    expect(plateFit({ width: 390, height: 844 })?.aspectRatio).toBe('390 / 844');
  });

  it('keeps the plate inside the column, its own pixels and the height cap', () => {
    expect(plateFit({ width: 390, height: 844 })?.width).toBe('min(100%, 390px, calc(60svh * 390 / 844))');
  });

  it('keeps the reserved 4:3 for a picture of unknown size', () => {
    expect(plateFit(null)).toBeUndefined();
    expect(plateFit(undefined)).toBeUndefined();
  });
});

const base64 = (...parts: number[][]) => {
  let binary = '';
  for (const byte of parts.flat()) binary += String.fromCharCode(byte);
  return btoa(binary);
};
const ascii = (value: string) => [...value].map((char) => char.charCodeAt(0));
const be16 = (value: number) => [value >> 8, value & 0xff];
const be32 = (value: number) => [...be16(value >>> 16), ...be16(value & 0xffff)];
const le16 = (value: number) => be16(value).reverse();
const le32 = (value: number) => be32(value).reverse();

const png = (width: number, height: number) =>
  base64(
    [0x89, ...ascii('PNG\r\n\x1a\n')],
    be32(13),
    ascii('IHDR'),
    be32(width),
    be32(height),
    [8, 6, 0, 0, 0],
  );

/** One APP1 EXIF segment whose first directory holds only the orientation. */
const exif = (orientation: number, little: boolean) => {
  const u16 = little ? le16 : be16;
  const u32 = little ? le32 : be32;
  const entry = [...u16(0x0112), ...u16(3), ...u32(1), ...u16(orientation), 0, 0];
  const tiff = [...ascii(little ? 'II' : 'MM'), ...u16(42), ...u32(8), ...u16(1), ...entry, ...u32(0)];
  const body = [...ascii('Exif\0\0'), ...tiff];
  return [0xff, 0xe1, ...be16(body.length + 2), ...body];
};

/** A JPEG header: the segments given, a quantization table, the frame header and the scan. */
const jpeg = (width: number, height: number, ...segments: number[][]) =>
  base64(
    [0xff, 0xd8],
    ...segments,
    [0xff, 0xdb, ...be16(67), 0, ...new Array<number>(64).fill(1)],
    [0xff, 0xc0, ...be16(17), 8, ...be16(height), ...be16(width), 3, 1, 0x22, 0, 2, 0x11, 1, 3, 0x11, 1],
    [0xff, 0xda],
  );

// An inline picture's plate takes its ratio from the header, before any decode.
describe('inline picture size', () => {
  it('reads a PNG header, bare or as a data URL', () => {
    expect(inlinePictureSize(png(851, 332))).toEqual({ width: 851, height: 332 });
    expect(inlinePictureSize(`data:image/png;base64,${png(851, 332)}`)).toEqual({
      width: 851,
      height: 332,
    });
  });

  it('reads a GIF header', () => {
    const gif = base64(ascii('GIF89a'), le16(640), le16(200), [0, 0, 0]);
    expect(inlinePictureSize(gif)).toEqual({ width: 640, height: 200 });
  });

  it('walks a JPEG to its frame header, however far other segments push it', () => {
    const icc = [0xff, 0xe2, ...be16(40_002), ...new Array<number>(40_000).fill(0)];
    expect(inlinePictureSize(jpeg(1600, 900))).toEqual({ width: 1600, height: 900 });
    expect(inlinePictureSize(jpeg(1600, 900, icc))).toEqual({ width: 1600, height: 900 });
  });

  it('swaps the axes of a JPEG that its EXIF turns a quarter, as the browser paints it', () => {
    const turned = { width: 2268, height: 4032 };
    const upright = { width: 4032, height: 2268 };
    expect(inlinePictureSize(jpeg(4032, 2268, exif(6, false)))).toEqual(turned);
    expect(inlinePictureSize(jpeg(4032, 2268, exif(8, true)))).toEqual(turned);
    expect(inlinePictureSize(jpeg(4032, 2268, exif(1, true)))).toEqual(upright);
    expect(inlinePictureSize(jpeg(4032, 2268, exif(3, false)))).toEqual(upright);
  });

  it('answers null for another format, a short or empty header, or text that is not base64', () => {
    const webp = base64(ascii('RIFF'), [0, 0, 0, 0], ascii('WEBPVP8 '), new Array<number>(12).fill(0));
    expect(inlinePictureSize(webp)).toBeNull();
    expect(inlinePictureSize('iVBORw0KGgo=')).toBeNull();
    expect(inlinePictureSize(png(0, 332))).toBeNull();
    expect(inlinePictureSize('%%%% not base64 %%%%')).toBeNull();
  });
});
