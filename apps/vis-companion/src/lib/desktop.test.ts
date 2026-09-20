// @vitest-environment jsdom
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import { captureContextMenu, captureLinkClicks, openExternalUrl } from './desktop';

// The desktop app is this bundle inside a Pake (Tauri) window, and Pake reads every click
// through its own capture listener: a sign-in URL replaces Vis in the current window, and an
// href it cannot classify — `attachment://` — is pushed at the shell, which refuses the scheme
// after the event has already been stopped. This bundle therefore claims clicks first.
type DesktopWindow = Window & { __TAURI__?: { core: { invoke: ReturnType<typeof vi.fn> } } };

/** The Pake desktop window: one shell command channel reaching the system browser. */
function stubDesktopShell() {
  const invoke = vi.fn().mockResolvedValue(undefined);
  (window as DesktopWindow).__TAURI__ = { core: { invoke } };
  return invoke;
}

const pakeListeners: EventListener[] = [];

/** Pake's listener arrives at `DOMContentLoaded`, after the module script installed ours. */
function stubPakeCapture() {
  const pake = vi.fn();
  document.addEventListener('click', pake, true);
  pakeListeners.push(pake);
  return pake;
}

/** Whatever the app itself put on the link: a router, a preview, an attachment reader. */
function stubAppHandler() {
  const handler = vi.fn((event: Event) => event.preventDefault());
  document.addEventListener('click', handler);
  pakeListeners.push(handler);
  return handler;
}

function linkTo(href: string) {
  const anchor = document.createElement('a');
  anchor.setAttribute('href', href);
  anchor.textContent = 'Open';
  document.body.append(anchor);
  return anchor;
}

function click(anchor: HTMLAnchorElement) {
  const event = new MouseEvent('click', { bubbles: true, cancelable: true, button: 0 });
  anchor.dispatchEvent(event);
  return event;
}

beforeEach(() => {
  vi.spyOn(window, 'open').mockReturnValue(null);
  captureLinkClicks();
  captureContextMenu();
});

afterEach(() => {
  for (const listener of pakeListeners) document.removeEventListener('click', listener, true);
  for (const listener of pakeListeners) document.removeEventListener('click', listener);
  pakeListeners.length = 0;
  document.body.innerHTML = '';
  delete (window as DesktopWindow).__TAURI__;
  vi.restoreAllMocks();
});

it('sends an external link to the system browser before Pake reads it', () => {
  const invoke = stubDesktopShell();
  const pake = stubPakeCapture();

  const event = click(linkTo('https://example.com/docs'));

  expect(invoke).toHaveBeenCalledWith('plugin:shell|open', { path: 'https://example.com/docs' });
  expect(event.defaultPrevented).toBe(true);
  expect(pake).not.toHaveBeenCalled();
});

it('keeps a sign-in page out of the app window', () => {
  const invoke = stubDesktopShell();
  const pake = stubPakeCapture();

  click(linkTo('https://accounts.google.com/o/oauth2/auth?client_id=vis'));

  expect(invoke).toHaveBeenCalledWith('plugin:shell|open', {
    path: 'https://accounts.google.com/o/oauth2/auth?client_id=vis',
  });
  expect(pake).not.toHaveBeenCalled();
});

it('leaves an attachment link to the handler that drew it', () => {
  const invoke = stubDesktopShell();
  const app = stubAppHandler();

  click(linkTo('attachment://8e3a587d-232c-497d-a290-7d16cfcf0e02'));

  expect(invoke).not.toHaveBeenCalled();
  expect(app).toHaveBeenCalledOnce();
});

it('leaves an in-app route to the app', () => {
  const invoke = stubDesktopShell();
  const app = stubAppHandler();

  click(linkTo('/sessions/8e3a587d'));

  expect(invoke).not.toHaveBeenCalled();
  expect(app).toHaveBeenCalledOnce();
});

it('does not touch a link in a browser, where the window is a browser window', () => {
  const app = stubAppHandler();

  click(linkTo('https://example.com/docs'));

  expect(window.open).not.toHaveBeenCalled();
  expect(app).toHaveBeenCalledOnce();
});

it('opens a URL through the shell channel, or a browser tab without one', () => {
  openExternalUrl('https://example.com/docs');
  expect(window.open).toHaveBeenCalledWith(
    'https://example.com/docs',
    '_blank',
    'noopener,noreferrer',
  );

  const invoke = stubDesktopShell();
  openExternalUrl('https://example.com/docs');
  expect(invoke).toHaveBeenCalledWith('plugin:shell|open', { path: 'https://example.com/docs' });
});

/** The right button, as a mouse sends it. */
function rightClick(target: Element) {
  const event = new MouseEvent('contextmenu', { bubbles: true, cancelable: true, button: 2 });
  target.dispatchEvent(event);
  return event;
}

function appendTo(body: HTMLElement, tag: string) {
  const element = document.createElement(tag);
  body.append(element);
  return element;
}

// The webview's menu offers a browser's verbs — reload, go back, view source — inside an
// app that is not a browser. The row under the cursor answers with its own menu instead
// (`SwipeActions`), so the system one is refused first.
it('drops the browser menu inside the desktop window', () => {
  stubDesktopShell();

  expect(rightClick(appendTo(document.body, 'div')).defaultPrevented).toBe(true);
});

it('keeps the editing menu a text field needs for the clipboard', () => {
  stubDesktopShell();

  expect(rightClick(appendTo(document.body, 'textarea')).defaultPrevented).toBe(false);
  expect(rightClick(appendTo(document.body, 'input')).defaultPrevented).toBe(false);
});

it('leaves the right button to a browser, where the window is a browser window', () => {
  expect(rightClick(appendTo(document.body, 'div')).defaultPrevented).toBe(false);
});
