import { currentBrand } from './brand';
import { loadThemePrefSync } from './storage';
import {
  AUTO_THEME_PREF,
  AUTO_THEMES,
  DEFAULT_THEME,
  THEMES,
  type ThemeChoice,
} from './themes.generated';

/** The web standard for the color scheme of the system: browser, desktop shell and mobile webview. */
const DARK_SCHEME = '(prefers-color-scheme: dark)';

/** The variables that the brand theme put on the root: a later paint without them removes them. */
let brandColors: string[] = [];

/** The preference of the last paint. A change of the system paints again only for `auto`. */
let paintedPref: string | null = null;

/** The palette for the color scheme that the system reports now. */
function systemTheme(): ThemeChoice {
  const isDark = globalThis.matchMedia?.(DARK_SCHEME).matches ?? false;
  return AUTO_THEMES[isDark ? 'dark' : 'light'];
}

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
  // No inline `color-scheme`: the stylesheet block of the palette holds it. The desktop
  // shell reads an inline value as an override and pins the appearance of the app. Then
  // `prefers-color-scheme` stops following the system, and so does the auto theme.

  // A brand theme is its base palette with some variables replaced on the root.
  const brand = currentBrand().theme;
  const colors = brand?.base === theme.id ? (brand.colors ?? {}) : {};
  for (const name of brandColors) if (!(name in colors)) root.style.removeProperty(name);
  for (const [name, color] of Object.entries(colors)) root.style.setProperty(name, color);
  brandColors = Object.keys(colors);

  const chrome = colors['--bg'] ?? theme.chrome;
  const themeMeta = document.querySelector<HTMLMetaElement>('meta[name="theme-color"]');
  if (themeMeta && themeMeta.content !== chrome) themeMeta.content = chrome;
}

/**
 * The palette a stored preference names, or the default when it names none of them.
 * `auto` names the palette for the color scheme of the system.
 * A brand theme is the only palette: the preference does not count while it exists.
 */
export function resolveTheme(pref: string): ThemeChoice {
  const brand = currentBrand().theme;
  const id = brand?.base ?? pref;
  const theme =
    id === AUTO_THEME_PREF
      ? systemTheme()
      : (THEMES.find((choice) => choice.id === id) ?? DEFAULT_THEME);
  return brand?.label ? { ...theme, label: brand.label } : theme;
}

/** Paint the palette of a preference, and remember the preference for `followSystemTheme`. */
export function paintThemePref(pref: string): ThemeChoice {
  paintedPref = pref;
  const theme = resolveTheme(pref);
  applyTheme(theme);
  return theme;
}

/**
 * Paint again when the system changes its color scheme while the preference is `auto`.
 * The media query sends the change, so nothing polls. Returns the function that stops it.
 */
export function followSystemTheme(): () => void {
  const query = globalThis.matchMedia?.(DARK_SCHEME);
  if (!query) return () => {};
  const repaint = () => {
    if (paintedPref === AUTO_THEME_PREF) paintThemePref(paintedPref);
  };
  query.addEventListener?.('change', repaint);
  return () => query.removeEventListener?.('change', repaint);
}

/**
 * Paint the stored palette BEFORE React renders, without asking the bridge.
 *
 * `index.html` stamps `data-theme` from the same mirrored key during parse, so
 * the very first frame is already the right paper; this is the same decision
 * made against the shipped catalog — an id no longer in it resolves to the
 * default, and the browser chrome follows it here.
 */
export function paintStoredTheme(): ThemeChoice {
  return paintThemePref(loadThemePrefSync());
}
