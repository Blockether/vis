import { Capacitor } from '@capacitor/core';
import { Keyboard } from '@capacitor/keyboard';
import { isSoftKeyboardUp } from './viewport';

/**
 * Keeping the software keyboard across a native sheet.
 *
 * Opening the photo/video picker or the camera hands first responder to a native
 * view controller, and the software keyboard goes down with it. The composer never
 * lost DOM focus, so the webview sees no focus change and has no reason to raise the
 * keyboard again: the user is left mid-sentence staring at a keyboard-less screen.
 *
 * Leaving the composer focused across the sheet and repairing afterwards is exactly
 * what does NOT work. When the sheet DELIVERED media, UIKit hands first responder
 * back and raises the keyboard by itself, and Capacitor's `keyboardWillShow` only
 * reaches JS after that keyboard is already moving (see `useVisualViewportShell`) —
 * so a repair timed against `isSoftKeyboardUp` can fire underneath a keyboard on its
 * way up. The blur drives it back down and the focus drags it up again: the
 * down/up/down/up flicker reported after attaching a photo to a half-written
 * message.
 *
 * So the keyboard is taken down on OUR terms instead. Blurring the composer BEFORE
 * the sheet opens leaves UIKit nothing to restore — the sheet's own presentation
 * covers that hide — and the way back is one deliberate focus. One down, one up,
 * identical whether the sheet delivered a photo or was cancelled, and never a race
 * against a platform that answers late.
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
 * On a hardware keyboard it does, and Shift+Enter makes the new line. On a phone
 * or a tablet the Return key IS the new-line key: the on-screen keyboard has no
 * modifier to hold, so a bare Enter that submits makes a multi-line message
 * impossible to type and fires the turn on the first paragraph break.
 */
export function isEnterSendPlatform(
  platform: string = Capacitor.getPlatform(),
): boolean {
  return platform === 'web';
}

/**
 * Puts the software keyboard away with the field that raised it.
 *
 * Sending is the end of writing, so the keyboard goes down with the message —
 * on the phone it used to stay up over the answer the user just asked for. The
 * blur is what the webview understands; `Keyboard.hide()` covers the native
 * shell, where the composer can keep DOM focus while UIKit still shows the IME.
 * A no-op with a hardware keyboard, where nothing is covering anything.
 */
export function dismissSoftKeyboard(
  element: HTMLElement | null | undefined,
): void {
  if (isEnterSendPlatform()) return;
  if (element && element.ownerDocument?.activeElement === element) element.blur();
  if (Capacitor.isNativePlatform()) void Keyboard.hide().catch(() => undefined);
}
