/**
 * Link handling for the desktop app, which is this same bundle inside a Pake (Tauri) window.
 */
type HostInvoke = (command: string, payload?: Record<string, unknown>) => Promise<unknown>;
type DesktopHost = Window & { __TAURI__?: { core?: { invoke?: HostInvoke } } };

/**
 * Pake grants the page a command channel: `shell:allow-open` for the system browser and the
 * notification plugin for system alerts, and they are the only routes from this window to the
 * machine around it. Its presence is also how the bundle recognizes that it is running as the
 * desktop app: the browser build and the tests never see it.
 */
export function desktopInvoke(): HostInvoke | undefined {
  if (typeof window === 'undefined') return;
  const invoke = (window as DesktopHost).__TAURI__?.core?.invoke;
  return typeof invoke === 'function' ? invoke : undefined;
}

/**
 * Open a URL away from the app. In a browser that is a new tab; inside the desktop window
 * `window.open` is Pake's own rewrite rather than a browser, so the shell channel carries it.
 */
export const openExternalUrl = (url: string): void => {
  const invoke = desktopInvoke();
  // A refused open leaves the screen as it was, with its instructions and manual return.
  if (invoke) void invoke('plugin:shell|open', { path: url }).catch(() => {});
  else window.open(url, '_blank', 'noopener,noreferrer');
};

let claimed = false;

function claimExternalLink(event: MouseEvent): void {
  // Read per click rather than at install: on the web this listener stays inert.
  if (!desktopInvoke()) return;
  if (event.defaultPrevented || event.button !== 0) return;
  const anchor = event.target instanceof Element ? event.target.closest('a') : null;
  const href = anchor?.getAttribute('href');
  if (!href) return;
  let url: URL;
  try {
    url = new URL(href, window.location.href);
  } catch {
    return;
  }
  // `attachment://` and every other app scheme belongs to the component that drew the link.
  if (url.protocol !== 'http:' && url.protocol !== 'https:') return;
  // An in-app route is a render, not a visit to another site.
  if (url.origin === window.location.origin) return;
  event.preventDefault();
  event.stopImmediatePropagation();
  openExternalUrl(url.href);
}

/**
 * Claim link clicks before Pake can read them. Pake installs its OWN capture listener on
 * `document` at `DOMContentLoaded`: it sends a sign-in URL to the CURRENT window, so the
 * provider's page replaces Vis, and it pushes any href it cannot classify at the shell, which
 * refuses unknown schemes. Either way it stops the event, and the React handler on an
 * `attachment://` link never runs. A module script executes before `DOMContentLoaded`, so the
 * listener registered here runs first: external web links reach the system browser, and
 * everything the app draws for itself is left alone and hidden from Pake.
 */
export function captureLinkClicks(): void {
  if (claimed || typeof document === 'undefined') return;
  claimed = true;
  document.addEventListener('click', claimExternalLink, true);
}

let claimedContextMenu = false;

/** The fields whose own menu is the only route a mouse has to Cut, Copy and Paste. */
const EDITABLE = 'input, textarea, [contenteditable=""], [contenteditable="true"]';

function claimContextMenu(event: MouseEvent): void {
  // Read per event rather than at install: in a browser the right button is the
  // browser's, and this listener stays inert there.
  if (!desktopInvoke()) return;
  if (event.defaultPrevented) return;
  if (event.target instanceof Element && event.target.closest(EDITABLE)) return;
  event.preventDefault();
}

/**
 * Take the right button away from the webview inside the desktop window.
 *
 * The menu Pake's webview drops is the one a BROWSER owns — reload, go back, look at
 * the page source — and none of those verbs mean anything in an app the reader paired
 * to a gateway. The app draws its own menus instead: a right-click on a session row
 * opens the very menu its `⋯` holds (`SwipeActions`). Text fields keep the system's
 * editing menu, because it is the only way a mouse reaches the clipboard there.
 *
 * Pake's own injected listener is not what this refuses: it draws a small menu of its
 * own for a link or an image and hands every other element back to the browser's
 * default — and that default is the menu the reader was seeing over a session row.
 */
export function captureContextMenu(): void {
  if (claimedContextMenu || typeof document === 'undefined') return;
  claimedContextMenu = true;
  document.addEventListener('contextmenu', claimContextMenu);
}
