// @vitest-environment jsdom
// The document itself is the artifact under test: the stamp has to be IN the
// page that ships, not in a module that loads after it.
import html from '../../index.html?raw';
import { beforeEach, describe, expect, it } from 'vitest';
import { applyTheme, paintStoredTheme, resolveTheme } from './theme';
import { DEFAULT_THEME, THEMES } from './themes.generated';

/** The key `storage.ts` mirrors into localStorage beside the durable Preferences copy. */
const THEME_PREF_KEY = 'vis.themePref';

// Theming used to be a GATEWAY call: every launch read `/v1/theme` off whichever
// machine this device happened to be paired with, and six machines answered six
// catalogs for data that never moves. Every palette is now generated from the
// Clojure themes and SHIPPED as static CSS (the stylesheet itself is pinned by
// `com.blockether.vis.dev.companion-themes-test`), so painting one is setting an
// attribute — nothing is fetched, and no machine can repaint this device.
describe('the shipped theme catalog', () => {
  it('names every palette exactly once, with a chrome colour to match', () => {
    expect(THEMES.length).toBeGreaterThan(1);
    expect(new Set(THEMES.map((theme) => theme.id)).size).toBe(THEMES.length);
    for (const theme of THEMES) {
      expect(theme.label.trim()).not.toBe('');
      expect(theme.mode).toMatch(/^(light|dark)$/);
      expect(theme.chrome).toMatch(/^#[0-9a-f]{6}$/);
    }
  });

  it('leads with the default palette, the one painted before any preference is read', () => {
    expect(THEMES[0]).toEqual(DEFAULT_THEME);
  });

  it('resolves a stored id, and falls back to the default for anything else', () => {
    const dark = THEMES.find((theme) => theme.mode === 'dark');
    expect(dark).toBeDefined();
    expect(resolveTheme(dark!.id)).toEqual(dark);
    expect(resolveTheme('a-theme-that-was-uninstalled')).toEqual(DEFAULT_THEME);
  });
});

describe('applying a theme', () => {
  it('is one attribute, the colour scheme and the browser chrome — no inline palette', () => {
    const meta = document.createElement('meta');
    meta.name = 'theme-color';
    document.head.append(meta);
    const dark = THEMES.find((theme) => theme.mode === 'dark')!;

    applyTheme(dark);

    expect(document.documentElement.dataset.theme).toBe(dark.id);
    expect(document.documentElement.style.colorScheme).toBe('dark');
    expect(meta.content).toBe(dark.chrome);
    // The palette itself lives in the stylesheet: nothing is written per variable.
    expect(document.documentElement.style.getPropertyValue('--bg')).toBe('');

    applyTheme(DEFAULT_THEME);
    expect(document.documentElement.dataset.theme).toBe(DEFAULT_THEME.id);
    expect(document.documentElement.style.colorScheme).toBe(DEFAULT_THEME.mode);
    expect(meta.content).toBe(DEFAULT_THEME.chrome);
    meta.remove();
  });
});

// The report: coming back to the app from the background showed a flash instead
// of the splash. The palette was read through the native bridge and applied from
// an effect, so every load — and iOS reloads the page whenever it recycled the
// backgrounded webview — painted the LIGHT default (`:root` in the generated
// stylesheet) under the splash before flipping to the stored one.
describe('painting the stored palette before React renders', () => {
  beforeEach(() => {
    localStorage.clear();
    delete document.documentElement.dataset.theme;
    document.documentElement.style.colorScheme = '';
  });

  it('paints the stored palette synchronously, with no bridge call to await', () => {
    const dark = THEMES.find((theme) => theme.mode === 'dark')!;
    localStorage.setItem(THEME_PREF_KEY, dark.id);

    expect(paintStoredTheme()).toEqual(dark);
    expect(document.documentElement.dataset.theme).toBe(dark.id);
    expect(document.documentElement.style.colorScheme).toBe('dark');
  });

  it('migrates the old light/dark preference instead of flashing the default', () => {
    localStorage.setItem(THEME_PREF_KEY, 'dark');
    expect(paintStoredTheme().id).toBe('blockether-dark');
  });

  it('falls back to the default for an uninstalled id and for no preference', () => {
    localStorage.setItem(THEME_PREF_KEY, 'a-theme-that-was-uninstalled');
    expect(paintStoredTheme()).toEqual(DEFAULT_THEME);

    localStorage.clear();
    expect(paintStoredTheme()).toEqual(DEFAULT_THEME);
  });
});

// The bundle still has to load and evaluate before `paintStoredTheme()` runs, so
// the FIRST frame is stamped by an inline script in `index.html`. It reads the
// same mirrored key; this pins the two together, because a rename on either side
// brings the white sheet back and nothing else would notice.
describe('the boot stamp in index.html', () => {
  const inline = /<script>([\s\S]*?)<\/script>/.exec(html)?.[1] ?? '';

  it('runs before the module bundle', () => {
    expect(inline.trim()).not.toBe('');
    expect(html.indexOf('<script>')).toBeLessThan(html.indexOf('type="module"'));
  });

  it('stamps the stored id — the same key storage.ts mirrors — during parse', () => {
    delete document.documentElement.dataset.theme;
    const dark = THEMES.find((theme) => theme.mode === 'dark')!;
    localStorage.setItem(THEME_PREF_KEY, dark.id);

    new Function('document', 'localStorage', inline)(document, localStorage);

    expect(document.documentElement.dataset.theme).toBe(dark.id);
  });

  it('leaves the default standing when nothing is stored', () => {
    delete document.documentElement.dataset.theme;
    localStorage.clear();

    new Function('document', 'localStorage', inline)(document, localStorage);

    expect(document.documentElement.dataset.theme).toBeUndefined();
  });
});
