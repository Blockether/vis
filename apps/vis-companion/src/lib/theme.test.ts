// @vitest-environment jsdom
// The document itself is the artifact under test: the stamp has to be IN the
// page that ships, not in a module that loads after it.
import html from '../../index.html?raw';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { followSystemTheme, paintStoredTheme, paintThemePref, resolveTheme } from './theme';
import { AUTO_THEME_PREF, AUTO_THEMES, DEFAULT_THEME, THEMES } from './themes.generated';

/** The key `storage.ts` mirrors into localStorage beside the durable Preferences copy. */
const THEME_PREF_KEY = 'vis.themePref';

/** A system color scheme that the test controls. `set` sends the `change` event of the media query. */
function systemScheme(isDark: boolean) {
  const listeners = new Set<() => void>();
  vi.stubGlobal('matchMedia', (query: string) => ({
    get matches() {
      return isDark && query.includes('prefers-color-scheme: dark');
    },
    media: query,
    addEventListener: (_type: string, listener: () => void) => listeners.add(listener),
    removeEventListener: (_type: string, listener: () => void) => listeners.delete(listener),
  }));
  return {
    set(next: boolean) {
      isDark = next;
      for (const listener of [...listeners]) listener();
    },
  };
}

// Theming used to be a GATEWAY call: every launch read `/v1/theme` off whichever
// machine this device happened to be paired with, and six machines answered six
// catalogs for data that never moves. Every palette is now generated from the
// Clojure themes and SHIPPED as static CSS (the stylesheet itself is pinned by
// `com.blockether.vis.dev.companion-themes-test`), so painting one is setting an
// attribute — nothing is fetched, and no machine can repaint this device.
describe('the shipped theme catalog', () => {

  it('resolves a stored id, and falls back to the default for anything else', () => {
    const dark = THEMES.find((theme) => theme.mode === 'dark');
    expect(dark?.mode).toBe('dark');
    expect(resolveTheme(dark!.id)).toEqual(dark);
    expect(resolveTheme('a-theme-that-was-uninstalled')).toEqual(DEFAULT_THEME);
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
  });

  it('migrates the old light/dark preference instead of flashing the default', () => {
    localStorage.setItem(THEME_PREF_KEY, 'dark');
    expect(paintStoredTheme().id).toBe('blockether-dark');
  });
});

// The request: the theme follows the system unless the person chose one.
describe('the auto theme', () => {
  beforeEach(() => {
    localStorage.clear();
    delete document.documentElement.dataset.theme;
  });
  afterEach(() => {
    vi.unstubAllGlobals();
  });

  it('paints the palette of the system when no theme is stored', () => {
    systemScheme(true);
    expect(paintStoredTheme()).toEqual(AUTO_THEMES.dark);
    expect(document.documentElement.dataset.theme).toBe(AUTO_THEMES.dark.id);
  });

  // Regression: the desktop shell reads an inline `color-scheme` on the root as a theme
  // override and pins the appearance of the app. After "Blockether Light", then "Auto",
  // the app stayed light on a dark system until a restart.
  it('writes no inline color scheme, so a desktop shell does not pin the system', () => {
    systemScheme(true);
    paintThemePref('blockether-light');
    expect(document.documentElement.style.colorScheme).toBe('');
    paintThemePref(AUTO_THEME_PREF);
    expect(document.documentElement.style.colorScheme).toBe('');
    expect(document.documentElement.dataset.theme).toBe(AUTO_THEMES.dark.id);
  });

  it('paints again when the system changes, and stops for an explicit theme', () => {
    const system = systemScheme(false);
    const stop = followSystemTheme();
    const root = document.documentElement;
    try {
      paintThemePref('auto');
      expect(root.dataset.theme).toBe(AUTO_THEMES.light.id);
      system.set(true);
      expect(root.dataset.theme).toBe(AUTO_THEMES.dark.id);

      const chosen = THEMES.find((theme) => theme.id === 'solarized-light')!;
      paintThemePref(chosen.id);
      system.set(false);
      system.set(true);
      expect(root.dataset.theme).toBe(chosen.id);
    } finally {
      stop();
    }
  });

  it('follows the system for a stored id that the catalog does not hold', () => {
    const system = systemScheme(true);
    const stop = followSystemTheme();
    try {
      localStorage.setItem(THEME_PREF_KEY, 'a-theme-that-was-uninstalled');
      expect(paintStoredTheme()).toEqual(AUTO_THEMES.dark);
      system.set(false);
      expect(document.documentElement.dataset.theme).toBe(AUTO_THEMES.light.id);
    } finally {
      stop();
    }
  });
});

// The bundle still has to load and evaluate before `paintStoredTheme()` runs, so
// the FIRST frame is stamped by an inline script in `index.html`. It reads the
// same mirrored key; this pins the two together, because a rename on either side
// brings the white sheet back and nothing else would notice.
describe('the boot stamp in index.html', () => {
  const inline = /<script>([\s\S]*?)<\/script>/.exec(html)?.[1] ?? '';

  it('stamps the stored id — the same key storage.ts mirrors — during parse', () => {
    delete document.documentElement.dataset.theme;
    const dark = THEMES.find((theme) => theme.mode === 'dark')!;
    localStorage.setItem(THEME_PREF_KEY, dark.id);

    new Function('document', 'localStorage', inline)(document, localStorage);

    expect(document.documentElement.dataset.theme).toBe(dark.id);
  });

  it('stamps the palette of the system when no theme is stored', () => {
    delete document.documentElement.dataset.theme;
    localStorage.clear();
    systemScheme(true);
    try {
      new Function('document', 'localStorage', inline)(document, localStorage);
      expect(document.documentElement.dataset.theme).toBe(AUTO_THEMES.dark.id);
    } finally {
      vi.unstubAllGlobals();
    }
  });

  // The browser paints its own chrome from this element before the bundle runs.
  it('gives the browser chrome the color of the system palette', () => {
    const meta = document.createElement('meta');
    meta.name = 'theme-color';
    meta.content = AUTO_THEMES.light.chrome;
    document.head.append(meta);
    localStorage.clear();
    systemScheme(true);
    try {
      new Function('document', 'localStorage', inline)(document, localStorage);
      expect(meta.content).toBe(AUTO_THEMES.dark.chrome);
    } finally {
      meta.remove();
      vi.unstubAllGlobals();
    }
  });
});
