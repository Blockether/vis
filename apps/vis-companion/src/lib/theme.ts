import type { GatewayTheme, ThemePref, ThemeSummary } from './types';
import { BUNDLED_LIGHT, BUNDLED_THEMES } from './palettes';

const appliedThemeVars = new Map<string, string>();

/**
 * Apply the daemon's browser-ready projection of the active TUI palette.
 * Diff-based and idempotent: it removes only vars that disappeared and sets only
 * vars that changed, never remove-then-add — so a repeated identical apply is a
 * no-op and can't flash the `:root` fallback (the toggle-click flicker).
 */
export function applyGatewayTheme(theme: GatewayTheme): void {
  const root = document.documentElement;

  const next = new Map<string, string>();
  for (const [name, value] of Object.entries(theme.css_vars ?? {})) {
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

  const chromeColor = theme.css_vars?.['--bg'];
  const themeMeta = document.querySelector<HTMLMetaElement>('meta[name="theme-color"]');
  if (chromeColor && themeMeta && themeMeta.content !== chromeColor) themeMeta.content = chromeColor;
}

/**
 * Resolve which palette to paint from the gateway's theme payload and the
 * app-local preference. The gateway stays the source of truth for the set of
 * themes and their exact colors; the app only *chooses* among them locally, so
 * a companion can pin light without touching the shared TUI/gateway theme.
 */
export function resolveTheme(gateway: GatewayTheme, pref: ThemePref): GatewayTheme {
  const active: ThemeSummary = {
    id: gateway.id,
    display_name: gateway.display_name,
    mode: gateway.mode,
    css_vars: gateway.css_vars,
  };
  // Gateway themes are the source of truth; bundled palettes are the offline /
  // back-compat floor so a chosen light/dark always has colors to paint even
  // when an older gateway omits per-theme css_vars.
  const all: ThemeSummary[] = [active, ...(gateway.themes ?? []), ...BUNDLED_THEMES];

  const pick = (predicate: (t: ThemeSummary) => boolean): ThemeSummary | undefined =>
    all.find((t) => predicate(t) && !!t.css_vars);

  let chosen: ThemeSummary | undefined;
  if (pref === 'gateway') chosen = active;
  else if (pref === 'light' || pref === 'dark') chosen = pick((t) => t.mode === pref);
  else chosen = pick((t) => t.id === pref);

  const target = chosen ?? active;
  return {
    ...gateway,
    id: target.id,
    display_name: target.display_name,
    mode: target.mode,
    css_vars: target.css_vars ?? gateway.css_vars,
  };
}

/**
 * Resolve a paint-ready palette from ONLY the app-local preference, with no
 * gateway payload. Used on first mount so the app renders the chosen theme
 * (default light) immediately — before any gateway connects and regardless of
 * a stale browser-cached stylesheet. `gateway`/unknown prefs fall back to light
 * until the real gateway theme loads and re-resolves.
 */
export function resolveLocalTheme(pref: ThemePref): GatewayTheme {
  const byMode = (m: 'light' | 'dark') => BUNDLED_THEMES.find((t) => t.mode === m);
  const byId = BUNDLED_THEMES.find((t) => t.id === pref);
  const target =
    pref === 'dark' ? byMode('dark') : pref === 'light' ? byMode('light') : byId ?? byMode('light');
  const chosen = target ?? BUNDLED_LIGHT;
  return {
    id: chosen.id,
    display_name: chosen.display_name,
    mode: chosen.mode,
    css_vars: chosen.css_vars ?? {},
    themes: BUNDLED_THEMES,
  };
}
