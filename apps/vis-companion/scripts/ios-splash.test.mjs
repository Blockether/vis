import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';

/**
 * The iOS launch screen, as `scripts/ios-prepare.mjs` stamps it.
 *
 * `ios/` is generated and gitignored, so what ships is whatever this script writes over
 * Capacitor's scaffold — and the scaffold's launch screen is Capacitor's own logo,
 * aspect-FILLED from a single 2732² image. This reads the tracked assets and the script's
 * text, so somebody else's mark cannot come back through a `cap add ios`.
 */

const here = dirname(fileURLToPath(import.meta.url));
const root = join(here, '..');
const source = readFileSync(join(here, 'ios-prepare.mjs'), 'utf8');

/** Width, height and colour type straight out of a PNG's IHDR — no image library in this repo. */
const header = (path) => {
  const png = readFileSync(path);
  expect(png.subarray(1, 4).toString('ascii')).toBe('PNG');
  return { width: png.readUInt32BE(16), height: png.readUInt32BE(20), colourType: png[25] };
};

/** The page colour the web layer paints, as the generated theme declares it. */
const pageColour = () => {
  const found = /--bg:\s*#([0-9a-f]{6})/i.exec(
    readFileSync(join(root, 'src', 'lib', 'themes.generated.css'), 'utf8'),
  );
  expect(found).not.toBeNull();
  return [1, 3, 5].map((at) => Number.parseInt(found[1].slice(at - 1, at + 1), 16));
};

describe('ios launch screen', () => {
  // 64 pt tall at every scale: the size `App.tsx`'s `<Splash/>` paints the same mark at
  // (`h-16`), so the launch image and the first web frame are one picture.
  it.each([
    ['Splash-mark.png', 64],
    ['Splash-mark@2x.png', 128],
    ['Splash-mark@3x.png', 192],
  ])('%s is the mark at %i px tall', (name, height) => {
    const png = header(join(root, 'native-assets', 'ios', name));
    expect(png.height).toBe(height);
    expect(png.colourType).toBe(6); // RGBA: the background colour shows through around it
  });

  it('paints the colour the web layer paints a frame later', () => {
    const colours = source.slice(
      source.indexOf('const splashColorContents'),
      source.indexOf('const splashColorOk'),
    );
    const components = ['red', 'green', 'blue'].map((channel) => {
      const found = new RegExp(`"${channel}": "([\\d.]+)"`).exec(colours);
      expect(found).not.toBeNull();
      return Math.round(Number(found[1]) * 255);
    });
    expect(components).toEqual(pageColour());
  });

});
