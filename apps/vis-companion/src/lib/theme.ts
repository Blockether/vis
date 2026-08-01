import type { GatewayTheme, ThemePref, ThemeSummary } from './types';
import { BUNDLED_LIGHT, BUNDLED_THEMES } from './palettes';

const appliedThemeVars = new Map<string, string>();

type AppTheme = Pick<GatewayTheme, 'id' | 'mode' | 'css_vars'>;

/** Paint one of the companion's bundled or gateway-advertised app-local palettes. */
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

/** Union gateway catalogs by stable id, preferring the first complete palette found. */
export function dedupeThemes(...catalogs: readonly ThemeSummary[][]): ThemeSummary[] {
  const themes = new Map<string, ThemeSummary>();
  for (const catalog of catalogs) {
    for (const theme of catalog) {
      if (!theme.id.trim()) continue;
      const prior = themes.get(theme.id);
      if (!prior || (!prior.css_vars && theme.css_vars)) themes.set(theme.id, theme);
    }
  }
  return [...themes.values()];
}

/** Resolve a paint-ready palette without waiting for a gateway. */
export function resolveLocalTheme(
  pref: ThemePref,
  cached: ThemeSummary | null = null,
): GatewayTheme {
  const themes = dedupeThemes(cached ? [cached] : [], BUNDLED_THEMES);
  const selected = themes.find((theme) => theme.id === pref) ?? BUNDLED_LIGHT;
  return {
    ...selected,
    css_vars: selected.css_vars ?? {},
    themes,
  };
}
