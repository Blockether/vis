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
import { Capacitor, registerPlugin } from "@capacitor/core";

interface VisHostPlugin {
  info(): Promise<{ isMac: boolean }>;
}

let isMac = false;

/** Is this iOS build a Mac window? `false` until `loadHost` has answered. */
export function isIosAppOnMac(): boolean {
  return isMac;
}

/** Ask the native host once; a build without the plugin, or any other platform, stays `false`. */
export async function loadHost(): Promise<void> {
  if (
    Capacitor.getPlatform() !== "ios" ||
    !Capacitor.isPluginAvailable("VisHost")
  )
    return;
  try {
    isMac =
      (await registerPlugin<VisHostPlugin>("VisHost").info()).isMac === true;
  } catch {
    /* an app stamped before the plugin existed answers "not implemented" */
  }
}
