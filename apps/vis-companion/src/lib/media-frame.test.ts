import { describe, expect, it } from 'vitest';

import {
  inlinePictureSize,
  plateFit,
} from './media-frame';

describe('the plate fit', () => {

  it('keeps the plate inside the column, its own pixels and the height cap', () => {
    expect(plateFit({ width: 390, height: 844 })?.width).toBe('min(100%, 390px, calc(60svh * 390 / 844))');
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
