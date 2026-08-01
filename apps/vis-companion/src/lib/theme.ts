import type { GatewayTheme, ThemePref } from './types';
import { BUNDLED_LIGHT, BUNDLED_THEMES } from './palettes';

const appliedThemeVars = new Map<string, string>();

type AppTheme = Pick<GatewayTheme, 'id' | 'mode' | 'css_vars'>;

/** Paint one of the companion's bundled, app-local palettes. */
export function applyTheme(theme: AppTheme): void {
  const root = document.documentElement;
  const next = new Map<string, string>();
  for (const [name, value] of Object.entries(theme.css_vars)) {
    if (name.startsWith('--')) next.set(name, value);
  }

  for (const name of appliedThemeVars.keys()) {
    if (!next.has(name)) root.style.removeProperty(name);
  }
  for (const [name, value] of next) {
    if (appliedThemeVars.get(name) !== value) root.style.setProperty(name, value);
  }
  appliedThemeVars.clear();
  for (const [name, value] of next) appliedThemeVars.set(name, value);

  if (root.dataset.theme !== theme.id) root.dataset.theme = theme.id;
  if (root.style.colorScheme !== theme.mode) root.style.colorScheme = theme.mode;

  const chromeColor = theme.css_vars['--bg'];
  const themeMeta = document.querySelector<HTMLMetaElement>('meta[name="theme-color"]');
  if (chromeColor && themeMeta && themeMeta.content !== chromeColor) themeMeta.content = chromeColor;
}

/** Resolve a paint-ready palette without contacting a gateway. */
export function resolveLocalTheme(pref: ThemePref): GatewayTheme {
  const selected = BUNDLED_THEMES.find((theme) => theme.mode === pref) ?? BUNDLED_LIGHT;
  return {
    ...selected,
    css_vars: selected.css_vars ?? {},
    themes: BUNDLED_THEMES,
  };
}
