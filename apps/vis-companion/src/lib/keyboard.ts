import { Capacitor } from '@capacitor/core';
import { Keyboard } from '@capacitor/keyboard';
import { hasHardwarePointer } from './pointer';
import { isAppForeground, isSoftKeyboardUp } from './viewport';

/**
 * Blur before opening a native media sheet and restore focus once after dismissal. This
 * prevents UIKit's own responder restoration from racing a JavaScript repair.
 */

/** The sheet is still dismissing when the plugin promise settles; a focus landed
 *  mid-animation is dropped before the webview is first responder again. */
const SHEET_DISMISS_MS = 150;

/**
 * Gap between verification looks.
 *
 * WebKit can drop a focus that arrived too early, which would leave the composer
 * keyboard-less, so the raise is checked and retried. Deliberately generous:
 * `isSoftKeyboardUp` trails the real keyboard, and a look taken right after a focus
 * that DID work would cycle focus under a keyboard that is already coming up.
 */
const KEYBOARD_RETRY_MS = 500;

/** Focus attempts in total, the first one included. */
const KEYBOARD_FOCUS_ATTEMPTS = 3;

interface Caret {
  start: number | null;
  end: number | null;
}

export interface KeyboardHoldOptions {
  /** Defers the refocus. Injected by tests; production waits out the dismissal. */
  schedule?: (run: () => void, delayMs: number) => void;
  /** Asks the platform for the keyboard once focus is back. */
  showSoftKeyboard?: () => void;
  /** True while the software keyboard is on screen. */
  isKeyboardOpen?: () => boolean;
}

function caretOf(element: HTMLElement): Caret {
  const field = element as Partial<HTMLTextAreaElement>;
  return {
    start: typeof field.selectionStart === 'number' ? field.selectionStart : null,
    end: typeof field.selectionEnd === 'number' ? field.selectionEnd : null,
  };
}

function restoreCaret(element: HTMLElement, caret: Caret): void {
  const field = element as Partial<HTMLTextAreaElement>;
  if (caret.start === null || caret.end === null || typeof field.setSelectionRange !== 'function') {
    return;
  }
  // A field whose value shrank while the sheet was open must not throw here.
  try {
    field.setSelectionRange(caret.start, caret.end);
  } catch {
    // Selection is a nicety; the keyboard is the point.
  }
}

/**
 * iOS raises the keyboard from the programmatic focus alone — the bridge disables
 * WebKit's user-interaction requirement. Android's WebView does not: focus without
 * an explicit `showSoftInput` leaves the IME closed, and the plugin's `show()` is
 * Android-only.
 */
function requestSoftKeyboard(): void {
  if (Capacitor.getPlatform() !== 'android') return;
  void Keyboard.show().catch(() => undefined);
}

function afterSheetDismissal(run: () => void, delayMs: number): void {
  if (typeof setTimeout !== 'function') {
    run();
    return;
  }
  setTimeout(run, delayMs);
}

/**
 * Keeps the currently focused field — and therefore the software keyboard — in
 * place while a pointer press hands control to another composer action.
 */
export function keepKeyboard(event: { preventDefault(): void }): void {
  event.preventDefault();
}

/**
 * Takes the keyboard down with `element`'s focus and returns the thunk that puts
 * both back. Call the thunk in a `finally`, whether the sheet delivered media or was
 * cancelled. It is a no-op when the keyboard was down to begin with — a user who
 * opened the picker from a closed composer must not be ambushed by one — and calling
 * it twice restores once.
 *
 * The thunk never fights the platform: it focuses only while the keyboard is down,
 * so a keyboard the OS did bring back on its own is left strictly alone.
 */
export function holdKeyboardAcrossSheet(
  element: HTMLElement | null | undefined,
  options: KeyboardHoldOptions = {},
): () => void {
  const held = !!element && element.ownerDocument?.activeElement === element;
  const caret = held && element ? caretOf(element) : null;
  let restored = false;

  // Down on our terms, before the sheet takes it: with no focused field left, the
  // dismissal has nothing to restore and nothing to collide with the raise below.
  if (held && element) element.blur();

  return () => {
    if (!held || !element || restored) return;
    restored = true;
    const schedule = options.schedule ?? afterSheetDismissal;
    const show = options.showSoftKeyboard ?? requestSoftKeyboard;
    const isKeyboardOpen = options.isKeyboardOpen ?? isSoftKeyboardUp;
    let attempts = 0;

    const raise = () => {
      // Away: a programmatic blur or focus while the app is off screen is exactly
      // what UIKit cannot answer — the keyboard queue holds the main thread until
      // the watchdog kills the app (TestFlight builds 4861 and 5275). The composer
      // is not worth that, and the shell restores focus on the app's own lifecycle
      // events anyway.
      if (!isAppForeground()) return;
      if (element.ownerDocument?.visibilityState === 'hidden') return;
      // Up already — this focus worked, or the platform brought it back by itself.
      // Touching focus now is precisely what the user reads as a flicker.
      if (isKeyboardOpen()) return;
      attempts += 1;
      // Something may have refocused the composer without a keyboard behind it (a
      // focus WebKit dropped mid-dismissal). Only a real focus CHANGE raises one.
      if (element.ownerDocument?.activeElement === element) element.blur();
      element.focus({ preventScroll: true });
      if (caret) restoreCaret(element, caret);
      show();
      if (attempts < KEYBOARD_FOCUS_ATTEMPTS) schedule(raise, KEYBOARD_RETRY_MS);
    };

    schedule(raise, SHEET_DISMISS_MS);
  };
}

/**
 * Does a bare Enter SEND the message?
 *
 * With a hardware keyboard it does, and Shift+Enter makes the new line. On a phone
 * the Return key IS the new-line key: the on-screen keyboard has no modifier to
 * hold, so a bare Enter that submits makes a multi-line message impossible to type
 * and fires the turn on the first paragraph break.
 *
 * Read it PER KEYSTROKE, never once at mount: a keyboard is folded onto an iPad
 * mid-session, and the answer has to change with it.
 */
export function isEnterSendKeyboard(matches: boolean = hasHardwarePointer()): boolean {
  return matches;
}
