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
  const found = /--bg:\s*#([0-9a-f]{6})/i.exec(readFileSync(join(root, 'src', 'lib', 'themes.generated.css'), 'utf8'));
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

  it('registers all three scales in the asset catalog', () => {
    for (const scale of ['1x', '2x', '3x']) expect(source).toContain(`"scale": "${scale}"`);
    for (const file of ['splash-mark.png', 'splash-mark@2x.png', 'splash-mark@3x.png']) {
      expect(source).toContain(`"filename": "${file}"`);
    }
  });

  it('drops the stock 2732 px splash rather than leaving it beside ours', () => {
    expect(source).toContain("startsWith('splash-2732')");
    expect(source).toContain('for (const name of splashStale) rmSync(join(splashDir, name));');
  });

  // Regression: with `UILaunchStoryboardName`, SplashBoard composes the storyboard into one
  // bitmap per orientation and refuses over a budget — measured on an iPhone 17 Pro:
  // XBLaunchStoryboardErrorDomain Code=6, "Estimated size (29900800) is over limit
  // (25000000)" — and the app then launches on BLACK. `UILaunchScreen` renders natively.
  it('declares the launch screen the modern way and removes the storyboard key', () => {
    expect(source).toContain('<key>UILaunchScreen</key>');
    expect(source).toContain('<key>UIImageName</key>');
    expect(source).toContain('<string>Splash</string>');
    expect(source).toContain('<key>UIColorName</key>');
    expect(source).toContain('<string>SplashBackground</string>');
    expect(source).toContain('launchStoryboardStale');
    expect(source).toContain("preparedPlist = currentPlist.replace(launchStoryboardEntry, '')");
  });

  it('paints the colour the web layer paints a frame later', () => {
    const colours = source.slice(source.indexOf('const splashColorContents'), source.indexOf('const splashColorOk'));
    const components = ['red', 'green', 'blue'].map((channel) => {
      const found = new RegExp(`"${channel}": "([\\d.]+)"`).exec(colours);
      expect(found).not.toBeNull();
      return Math.round(Number(found[1]) * 255);
    });
    expect(components).toEqual(pageColour());
  });

  // The Xcode target lists the storyboard as a resource, so the bundle carries one either
  // way; it must not be the one with somebody else's logo in it.
  it('leaves no foreign logo in the storyboard the target still ships', () => {
    const board = source.slice(source.indexOf('const launchBoardSource'), source.indexOf('const launchBoardOk'));
    expect(board).toContain('contentMode="center"');
    expect(board).not.toContain('scaleAspectFill');
    expect(board).toContain('image="Splash"');
    expect(board).toContain('<image name="Splash" width="72" height="64"/>');
    const colour = /<color key="backgroundColor" red="([\d.]+)" green="([\d.]+)" blue="([\d.]+)"/.exec(board);
    expect(colour).not.toBeNull();
    expect(colour.slice(1, 4).map((value) => Math.round(Number(value) * 255))).toEqual(pageColour());
  });

  it('--check refuses a project still showing Capacitor’s splash', () => {
    const check = source.slice(source.indexOf('if (check) {'), source.indexOf('if (!appIconOk)'));
    expect(check).toContain('splashOk');
    expect(check).toContain('launch screen still shows');
    expect(source).toContain('const splashOk = splashFilesOk && splashColorOk && launchBoardOk;');
  });
});
