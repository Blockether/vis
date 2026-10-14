/**
 * THE HOST THE iOS BUILD IS RUNNING ON — the one fact a web view cannot measure.
 *
 * "Designed for iPad" on an Apple-silicon Mac is iOS WebKit in a Mac window: it
 * matches `(pointer: coarse)` under a trackpad, reports five touch points, and
 * UIKit still posts `keyboardWillShow` for a software keyboard it never draws.
 * Every heuristic the web layer has says "iPad, keys coming", and the shell
 * reserved a third of the window for keys nobody could see. `ProcessInfo`'s
 * `isiOSAppOnMac` is the truth, and only native code can read it, so the iOS
 * project carries a one-verb `VisHost` plugin (stamped by `scripts/ios-prepare.mjs`)
 * and this module asks it ONCE at launch. Readers ask synchronously afterwards:
 * a keyboard decision is made in a focus handler, not in a promise.
 */
import { Capacitor, registerPlugin } from '@capacitor/core';
import { desktopInvoke } from './desktop';

interface VisHostPlugin {
  info(): Promise<{ isMac: boolean }>;
}

let isMac = false;

/** Is the native iOS app, whose status bar is hidden before the web view loads? */
export function isIosNativeApp(): boolean {
  return Capacitor.getPlatform() === 'ios' && Capacitor.isNativePlatform();
}

/** Is this iOS build a Mac window? `false` until `loadHost` has answered. */
export function isIosAppOnMac(): boolean {
  return isMac;
}

/** Ask the native host once; a build without the plugin, or any other platform, stays `false`. */
export async function loadHost(): Promise<void> {
  if (Capacitor.getPlatform() !== 'ios' || !Capacitor.isPluginAvailable('VisHost')) return;
  try {
    isMac = (await registerPlugin<VisHostPlugin>('VisHost').info()).isMac === true;
  } catch {
    /* an app stamped before the plugin existed answers "not implemented" */
  }
}

/**
 * The window that holds this bundle: the Pake `desktop` app, a Capacitor `native` app, or a
 * browser tab on the `web`. Only the desktop app is a window of its own, so window-like
 * styling (a frame, rounded corners) uses the `desktop:` variant and never reaches a tab.
 */
export type ShellHost = 'desktop' | 'native' | 'web';

export function shellHost(): ShellHost {
  if (desktopInvoke()) return 'desktop';
  if (Capacitor.isNativePlatform()) return 'native';
  return 'web';
}

/** Mark the document with its host before the first render; `index.css` reads `data-host`. */
export function stampShellHost(root: HTMLElement = document.documentElement): ShellHost {
  const host = shellHost();
  if (root.dataset.host !== host) root.dataset.host = host;
  return host;
}
