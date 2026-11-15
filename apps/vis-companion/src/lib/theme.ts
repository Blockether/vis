import { currentBrand } from './brand';
import { loadThemePrefSync } from './storage';
import { DEFAULT_THEME, THEMES, type ThemeChoice } from './themes.generated';

/** The variables that the brand theme put on the root: a later paint without them removes them. */
let brandColors: string[] = [];

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
 * A brand theme is the only palette: the preference does not count while it exists.
 */
export function resolveTheme(pref: string): ThemeChoice {
  const brand = currentBrand().theme;
  const id = brand?.base ?? pref;
  const theme = THEMES.find((choice) => choice.id === id) ?? DEFAULT_THEME;
  return brand?.label ? { ...theme, label: brand.label } : theme;
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
