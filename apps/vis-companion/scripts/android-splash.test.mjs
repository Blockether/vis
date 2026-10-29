import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import { brandLaunchTheme } from './android-splash.mjs';

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
  it.each(['values/vis_splash_color.xml', 'drawable/splash.xml'])(
    '%s has XML-legal comments',
    (relative) => {
      const xml = readFileSync(join(resources, relative), 'utf8');
      for (const comment of xml.match(/<!--[\s\S]*?-->/g) ?? []) {
        expect(comment.slice(4, -3)).not.toContain('--');
      }
    },
  );

  // Regression: lintDebug rejected the API-31 platform attribute with minSdk 26.
  // Preparation must handle both a fresh scaffold and an already-branded generated project.
  it.each([
    '',
    '<item name="android:windowSplashScreenBackground">@color/vis_splash</item>',
    '<item name="windowSplashScreenBackground">@color/old</item>',
    '<item name="windowSplashScreenBackground">@color/vis_splash</item><item name="android:windowSplashScreenBackground">@color/vis_splash</item>',
  ])('uses only the AndroidX splash attribute, idempotently: %s', (previousColour) => {
    const otherTheme =
      '<style name="AppTheme"><item name="android:background">@color/other</item></style>';
    const styles = `<resources>${otherTheme}<style name="AppTheme.NoActionBarLaunch" parent="Theme.SplashScreen">
        ${previousColour}
        <item name="android:background">@drawable/splash</item>
    </style></resources>`;
    const branded = brandLaunchTheme(styles);
    expect(branded).not.toContain('android:windowSplashScreenBackground');
    expect(branded.match(/name="windowSplashScreenBackground"/g)).toHaveLength(1);
    expect(branded).toContain('<item name="windowSplashScreenBackground">@color/vis_splash</item>');
    expect(branded).toContain('parent="Theme.SplashScreen"');
    expect(branded).toContain('<item name="android:background">@drawable/splash</item>');
    expect(branded).toContain(otherTheme);
    expect(brandLaunchTheme(branded)).toBe(branded);
  });

  it('refuses a scaffold without the launch theme instead of silently skipping branding', () => {
    expect(() => brandLaunchTheme('<resources/>')).toThrow('no AppTheme.NoActionBarLaunch');
  });
});

