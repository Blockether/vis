import { loadThemePrefSync } from './storage';
import { DEFAULT_THEME, THEMES, type ThemeChoice } from './themes.generated';

/**
 * Painting a theme is setting ONE attribute.
 *
 * Every palette ships as static CSS (`themes.generated.css`, rendered from the
 * Clojure themes), so there is nothing to fetch, nothing to cache and no
 * per-variable write: `[data-theme='<id>']` already holds the whole palette.
 */
export function applyTheme(theme: ThemeChoice): void {
  const root = document.documentElement;
  if (root.dataset.theme !== theme.id) root.dataset.theme = theme.id;
  if (root.style.colorScheme !== theme.mode) root.style.colorScheme = theme.mode;

  const themeMeta = document.querySelector<HTMLMetaElement>('meta[name="theme-color"]');
  if (themeMeta && themeMeta.content !== theme.chrome) themeMeta.content = theme.chrome;
}

/** The palette a stored preference names, or the default when it names none of them. */
export function resolveTheme(pref: string): ThemeChoice {
  return THEMES.find((theme) => theme.id === pref) ?? DEFAULT_THEME;
}

/**
 * Paint the stored palette BEFORE React renders, without asking the bridge.
 *
 * `index.html` stamps `data-theme` from the same mirrored key during parse, so
 * the very first frame is already the right paper; this is the same decision
 * made against the shipped catalog — an id no longer in it resolves to the
 * default, and the colour scheme and the browser chrome follow it here.
 */
export function paintStoredTheme(): ThemeChoice {
  const theme = resolveTheme(loadThemePrefSync());
  applyTheme(theme);
  return theme;
}
