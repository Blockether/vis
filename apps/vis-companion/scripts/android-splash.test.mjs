import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';

/**
 * The launch screen and the notification channel, as `scripts/android-prepare.mjs` stamps them.
 *
 * Both are things you only see on a device, and both were WRONG in a shipped build: the app
 * opened on Capacitor's blue logo over the platform's near-white, and every alert was filed
 * under Firebase's unnamed "Miscellaneous" channel at default importance. These read the
 * tracked assets and the script's own text, so a regression fails here instead of in a store
 * build nobody re-screenshots.
 */

const here = dirname(fileURLToPath(import.meta.url));
const root = join(here, '..');
const source = readFileSync(join(here, 'android-prepare.mjs'), 'utf8');
const resources = join(root, 'native-assets', 'android', 'res');

/** Width, height and colour type straight out of a PNG's IHDR — no image library in this repo. */
const header = (path) => {
  const png = readFileSync(path);
  expect(png.subarray(1, 4).toString('ascii')).toBe('PNG');
  return {
    width: png.readUInt32BE(16),
    height: png.readUInt32BE(20),
    colourType: png[25],
  };
};

// 64 dp tall: the size `App.tsx`'s `<Splash/>` paints the same mark at (`h-16`), so the
// launch window and the first web frame show one picture at one size.
const densities = [
  ['mdpi', 72, 64],
  ['hdpi', 108, 96],
  ['xhdpi', 143, 128],
  ['xxhdpi', 215, 192],
  ['xxxhdpi', 287, 256],
];

describe('android launch screen assets', () => {
  it.each(densities)('drawable-%s carries the mark at %ix%i', (density, width, height) => {
    const png = header(join(resources, `drawable-${density}`, 'splash_mark.png'));
    expect([png.width, png.height]).toEqual([width, height]);
    // 6 = RGBA: the mark is cut out, so the layer-list's colour shows through around it.
    expect(png.colourType).toBe(6);
  });

  // One mark, both platforms: the iOS asset catalog and these are the same bytes, so a
  // redraw that reaches only one store is impossible rather than merely unlikely.
  it.each([
    ['Splash-mark.png', 'drawable-mdpi'],
    ['Splash-mark@2x.png', 'drawable-xhdpi'],
    ['Splash-mark@3x.png', 'drawable-xxhdpi'],
  ])('%s is byte-identical to %s', (ios, density) => {
    const iosBytes = readFileSync(join(root, 'native-assets', 'ios', ios));
    const androidBytes = readFileSync(join(resources, density, 'splash_mark.png'));
    expect(androidBytes.equals(iosBytes)).toBe(true);
  });

  it('draws the mark centred at its own size, never stretched', () => {
    const layers = readFileSync(join(resources, 'drawable', 'splash.xml'), 'utf8');
    expect(layers).toContain('<layer-list');
    expect(layers).toContain('@color/vis_splash');
    expect(layers).toContain('@drawable/splash_mark');
    expect(layers).toContain('android:gravity="center"');
    // Capacitor's stock splash was one bitmap scaled to the window; that is the distortion.
    expect(layers).not.toContain('tileMode');
  });

  it('paints the colour the web layer paints a frame later', () => {
    const colours = readFileSync(join(resources, 'values', 'vis_splash_color.xml'), 'utf8');
    const page = /--bg:\s*(#[0-9a-f]{6})/i.exec(
      readFileSync(join(root, 'src', 'lib', 'themes.generated.css'), 'utf8'),
    );
    expect(page).not.toBeNull();
    expect(colours).toContain(`<color name="vis_splash">${page[1]}</color>`);
  });

  // Regression: aapt2 refuses an XML comment containing a double hyphen, and the build died
  // on `values/vis_splash_color.xml` naming the CSS token by its own dashed spelling.
  it.each(['values/vis_splash_color.xml', 'drawable/splash.xml'])('%s has XML-legal comments', (relative) => {
    const xml = readFileSync(join(resources, relative), 'utf8');
    for (const comment of xml.match(/<!--[\s\S]*?-->/g) ?? []) {
      expect(comment.slice(4, -3)).not.toContain('--');
    }
  });

  it('removes the stock splash bitmaps and brands both launch systems', () => {
    // Below Android 12 the theme's window background IS the splash; from 12 the platform
    // draws the launcher icon over `windowSplashScreenBackground` and ignores the drawable.
    expect(source).toContain('stockSplashBitmaps');
    expect(source).toContain('rmSync(join(androidResources, relative))');
    expect(source).toContain('<item name="windowSplashScreenBackground">@color/vis_splash</item>');
    expect(source).toContain('android:windowSplashScreenBackground');
  });

  it('--check refuses a project still showing the stock splash', () => {
    const check = source.slice(source.indexOf("if (has('check'))"), source.indexOf('// ── Launch screen'));
    expect(check).toContain('stockSplashBitmaps()');
    expect(check).toContain('launchThemeOk()');
  });
});

describe('android push channel', () => {
  // Android 8+ posts nothing without a channel, so an app that declares none hands every
  // alert to Firebase's `fcm_fallback_notification_channel`: "Miscellaneous", default
  // importance, no description, no heads-up banner.
  it('stamps Firebase default channel id', () => {
    expect(source).toContain('com.google.firebase.messaging.default_notification_channel_id');
  });

  it('takes the id from the app instead of repeating it', () => {
    const push = readFileSync(join(root, 'src', 'lib', 'push.ts'), 'utf8');
    const id = /export const PUSH_CHANNEL_ID = '([a-z0-9_]+)'/.exec(push)?.[1];
    expect(id).toBeTruthy();
    // One truth: the script reads that declaration, and carries no second copy of the id.
    expect(source).toContain("PUSH_CHANNEL_ID = '([a-z0-9_]+)'");
    expect(source).not.toContain(`'${id}'`);
  });
});
