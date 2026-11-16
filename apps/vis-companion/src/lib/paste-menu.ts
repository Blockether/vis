/**
 * The paste menu of the phone system, shown at an element of the page.
 *
 * The Paste verb of the app reads the clipboard in code: iOS can then ask for a
 * permission, and the verb has the look of the app, not of the phone. The native
 * `PasteMenu` plugin shows the system menu in its place. Its Paste goes to the focused
 * field as a normal paste event, so the page reads no clipboard.
 */
import { Capacitor, registerPlugin } from '@capacitor/core';

/** A rectangle in CSS pixels of the visible page. */
type Rect = { x: number; y: number; width: number; height: number };

type PasteMenuPlugin = {
  /** A call while the menu shows closes it. */
  show: (rect: Rect) => Promise<void>;
  /** Puts the menu that shows at a new rectangle. */
  move: (rect: Rect) => Promise<void>;
  hide: () => Promise<void>;
};

let plugin: PasteMenuPlugin | undefined;
const pasteMenu = () => (plugin ??= registerPlugin<PasteMenuPlugin>('PasteMenu'));

/** True in an app build that has the plugin. A browser gives a page no such menu to open. */
export const hasSystemPasteMenu = (): boolean =>
  Capacitor.isNativePlatform() && Capacitor.isPluginAvailable('PasteMenu');

function rectOf(element: Element): Rect {
  const box = element.getBoundingClientRect();
  // A box is measured from the layout viewport. The native view shows the visual viewport.
  const view = window.visualViewport;
  const scale = view?.scale ?? 1;
  return {
    x: (box.left - (view?.offsetLeft ?? 0)) * scale,
    y: (box.top - (view?.offsetTop ?? 0)) * scale,
    width: box.width * scale,
    height: box.height * scale,
  };
}

/** Shows the system menu at `element`. It rejects on a phone system that has no such menu. */
export function showSystemPasteMenu(element: Element): Promise<void> {
  return pasteMenu().show(rectOf(element));
}

/** Puts the system menu at the place that `element` has now. */
export function moveSystemPasteMenu(element: Element): void {
  pasteMenu()
    .move(rectOf(element))
    .catch((error: unknown) => console.warn('paste menu: move failed', error));
}

export function hideSystemPasteMenu(): void {
  pasteMenu()
    .hide()
    .catch((error: unknown) => console.warn('paste menu: hide failed', error));
}
