import type { GatewayTheme } from './types';

/** Apply the daemon's browser-ready projection of the active TUI palette. */

const appliedThemeVars = new Set<string>();

/** Apply the daemon's browser-ready projection of the active TUI palette. */
export function applyGatewayTheme(theme: GatewayTheme): void {
  const root = document.documentElement;

  for (const name of appliedThemeVars) root.style.removeProperty(name);
  appliedThemeVars.clear();

  for (const [name, value] of Object.entries(theme.css_vars ?? {})) {
    if (!name.startsWith('--')) continue;
    root.style.setProperty(name, value);
    appliedThemeVars.add(name);
  }

  root.dataset.theme = theme.id;
  root.style.colorScheme = theme.mode;

  const chromeColor = theme.css_vars?.['--bg'];
  const themeMeta = document.querySelector<HTMLMetaElement>('meta[name="theme-color"]');
  if (chromeColor && themeMeta) themeMeta.content = chromeColor;
}
