import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { inflateSync } from 'node:zlib';

import { describe, expect, it } from 'vitest';

// Regression: the Vis mark on Android was a washed-out ghost of the one on iOS.
// Every tracked launcher PNG had been flattened onto white at about a third
// opacity AND kept that coverage in its alpha channel, so the adaptive
// foreground composited over the white background layer at roughly 12%: the
// navy pupil painted pale grey (176,182,197) where iOS paints (28,45,87). The
// status bar was worse — Firebase refuses an adaptive icon there ("Adaptive
// icons cannot be used in notifications"), so every push carried Android's
// stock bell instead of the eye.

const here = dirname(fileURLToPath(import.meta.url));
const res = join(here, '..', 'native-assets', 'android', 'res');
const iosMaster = join(here, '..', 'native-assets', 'ios', 'AppIcon-512@2x.png');

/**
 * The whole image, one pixel per entry. The suite carries no image dependency,
 * and only real pixels can tell branded art from a ghost of it.
 */
const decodePng = (file) => {
  const buf = readFileSync(file);
  let head;
  const parts = [];
  for (let at = 8; at + 8 <= buf.length; ) {
    const length = buf.readUInt32BE(at);
    const type = buf.toString('ascii', at + 4, at + 8);
    const body = buf.subarray(at + 8, at + 8 + length);
    if (type === 'IHDR') {
      head = {
        width: body.readUInt32BE(0),
        height: body.readUInt32BE(4),
        depth: body[8],
        colour: body[9],
        interlace: body[12],
      };
    } else if (type === 'IDAT') parts.push(body);
    at += 12 + length;
  }
  const { width, height, depth, colour, interlace } = head;
  // 0 grey, 2 RGB, 4 grey+alpha, 6 RGBA — every shape an icon exporter emits.
  const channels = { 0: 1, 2: 3, 4: 2, 6: 4 }[colour];
  if (depth !== 8 || interlace !== 0 || !channels) {
    throw new Error(
      `${file}: expected an 8-bit non-interlaced PNG, got depth ${depth} colour ${colour}`,
    );
  }
  const raw = inflateSync(Buffer.concat(parts));
  const stride = width * channels;
  const data = Buffer.alloc(height * stride);
  for (let y = 0, at = 0; y < height; y += 1) {
    const filter = raw[at];
    at += 1;
    for (let i = 0; i < stride; i += 1) {
      const left = i >= channels ? data[y * stride + i - channels] : 0;
      const up = y > 0 ? data[(y - 1) * stride + i] : 0;
      const upLeft = y > 0 && i >= channels ? data[(y - 1) * stride + i - channels] : 0;
      let value = raw[at + i];
      if (filter === 1) value += left;
      else if (filter === 2) value += up;
      else if (filter === 3) value += (left + up) >> 1;
      else if (filter === 4) {
        const guess = left + up - upLeft;
        const dl = Math.abs(guess - left);
        const du = Math.abs(guess - up);
        const dul = Math.abs(guess - upLeft);
        value += dl <= du && dl <= dul ? left : du <= dul ? up : upLeft;
      } else if (filter !== 0) throw new Error(`${file}: unknown PNG filter ${filter}`);
      data[y * stride + i] = value & 0xff;
    }
    at += stride;
  }
  const pixel = (x, y) => {
    const i = y * stride + x * channels;
    return channels >= 3
      ? [data[i], data[i + 1], data[i + 2], channels === 4 ? data[i + 3] : 255]
      : [data[i], data[i], data[i], channels === 2 ? data[i + 1] : 255];
  };
  return { width, height, pixel };
};

const every = function* (img) {
  for (let y = 0; y < img.height; y += 1) {
    for (let x = 0; x < img.width; x += 1) yield [x, y, img.pixel(x, y)];
  }
};

/** The darkest fully opaque pixel — the ink the eye is drawn in. */
const ink = (img) => {
  let best = [255, 255, 255];
  for (const [, , [r, g, b, a]] of every(img)) {
    if (a >= 250 && r + g + b < best[0] + best[1] + best[2]) best = [r, g, b];
  }
  return best;
};

/** Share of the painted pixels that are solid, and the mark's extent. */
const coverage = (img) => {
  let painted = 0;
  let solid = 0;
  let [x0, y0, x1, y1] = [img.width, img.height, 0, 0];
  for (const [x, y, [, , , a]] of every(img)) {
    if (a <= 8) continue;
    painted += 1;
    if (a >= 250) solid += 1;
    x0 = Math.min(x0, x);
    y0 = Math.min(y0, y);
    x1 = Math.max(x1, x + 1);
    y1 = Math.max(y1, y + 1);
  }
  const centre = [(x0 + x1) / 2, (y0 + y1) / 2];
  let radius = 0;
  for (const [x, y, [, , , a]] of every(img)) {
    if (a <= 8) continue;
    radius = Math.max(radius, Math.hypot(x + 0.5 - centre[0], y + 0.5 - centre[1]));
  }
  return { solidShare: solid / painted, centre, radius };
};

const near = (got, want, slack) => got.every((channel, i) => Math.abs(channel - want[i]) <= slack);

const densities = ['mdpi', 'hdpi', 'xhdpi', 'xxhdpi', 'xxxhdpi'];
/** Adaptive foreground canvas, legacy launcher bitmap, status-bar icon — per density. */
const sizes = {
  mdpi: [108, 48, 24],
  hdpi: [162, 72, 36],
  xhdpi: [216, 96, 48],
  xxhdpi: [324, 144, 72],
  xxxhdpi: [432, 192, 96],
};
const mipmap = (density, name) => join(res, `mipmap-${density}`, `${name}.png`);

const brand = ink(decodePng(iosMaster));

describe('Android launcher icon', () => {
  it('ships every density at its adaptive canvas size', () => {
    for (const density of densities) {
      const [canvas, legacy] = sizes[density];
      for (const [name, want] of [
        ['ic_launcher_foreground', canvas],
        ['ic_launcher', legacy],
        ['ic_launcher_round', legacy],
      ]) {
        const img = decodePng(mipmap(density, name));
        expect([name, img.width, img.height]).toEqual([name, want, want]);
      }
    }
  });

  it('centres the mark inside the 66dp safe zone every launcher mask keeps', () => {
    for (const density of densities) {
      const [canvas] = sizes[density];
      const dp = canvas / 108;
      const { centre, radius } = coverage(decodePng(mipmap(density, 'ic_launcher_foreground')));
      expect(Math.abs(centre[0] / dp - 54)).toBeLessThanOrEqual(1);
      expect(Math.abs(centre[1] / dp - 54)).toBeLessThanOrEqual(1);
      expect(radius / dp).toBeLessThanOrEqual(33);
      expect(radius / dp).toBeGreaterThan(24);
    }
  });

  it('cuts the round variant round and leaves the foreground to the white layer', () => {
    const round = decodePng(mipmap('xxxhdpi', 'ic_launcher_round'));
    expect(round.pixel(2, 2)[3]).toBe(0);
    expect(round.pixel(96, 96)[3]).toBe(255);
    const foreground = decodePng(mipmap('xxxhdpi', 'ic_launcher_foreground'));
    expect(foreground.pixel(2, 2)[3]).toBe(0);
  });
});

describe('Android notification icon', () => {
  it('is a monochrome silhouette, because the system keeps only its alpha', () => {
    for (const density of densities) {
      const img = decodePng(join(res, `drawable-${density}`, 'ic_stat_vis.png'));
      expect([density, img.width, img.height]).toEqual([
        density,
        sizes[density][2],
        sizes[density][2],
      ]);
      let solid = 0;
      let clear = 0;
      for (const [, , [r, g, b, a]] of every(img)) {
        if (a > 0) expect([r, g, b]).toEqual([255, 255, 255]);
        if (a >= 250) solid += 1;
        if (a === 0) clear += 1;
      }
      expect(solid).toBeGreaterThan(0);
      expect(clear).toBeGreaterThan(0);
    }
  });

});
