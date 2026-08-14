// @vitest-environment jsdom
import { describe, expect, it } from 'vitest';
import { applyTheme, resolveTheme } from './theme';
import { DEFAULT_THEME, THEMES } from './themes.generated';

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
