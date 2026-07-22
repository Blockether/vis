import type { GatewayTheme } from './types';

/** Apply the daemon's browser-ready projection of the active TUI palette. */
export function applyGatewayTheme(theme: GatewayTheme): void {
  const root = document.documentElement;
  for (const [name, value] of Object.entries(theme.css_vars ?? {})) {
    if (name.startsWith('--')) root.style.setProperty(name, value);
  }
  root.dataset.theme = theme.id;
  root.style.colorScheme = theme.mode;
}
