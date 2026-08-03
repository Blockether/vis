import { Capacitor } from '@capacitor/core';
import { Keyboard } from '@capacitor/keyboard';
import { isSoftKeyboardUp } from './viewport';

/**
 * Keeping the software keyboard across a native sheet.
 *
 * Opening the photo/video picker or the camera hands first responder to a native
 * view controller, and the software keyboard goes down with it. When the user
 * cancels, nothing brings it back: the composer textarea never lost DOM focus, so
 * the webview sees no focus change and has no reason to raise the keyboard again.
 * The user is left mid-sentence staring at a keyboard-less screen and has to tap
 * the composer to carry on.
 *
 * The cure is a deliberate focus CHANGE once the sheet is gone — blur, then focus
 * — with the caret put back exactly where it was.
 */

/** The sheet is still dismissing when the plugin promise settles; a focus landed
 *  mid-animation is dropped before the webview is first responder again. */
const SHEET_DISMISS_MS = 150;

/** Gap between later looks at the keyboard, once the first window has passed. */
const KEYBOARD_WATCH_MS = 100;

/**
 * Looks taken before the keyboard counts as gone for good.
 *
 * A sheet that DELIVERED media is not the cancel case: iOS hands first responder
 * back to the webview and raises the keyboard by itself, a few hundred ms after
 * the plugin promise settles. Blurring on top of that drives the keyboard down
 * again and the focus that follows drags it back up — the open/close/open flicker
 * seen after attaching a picture to a half-written message. So watch first, and
 * only repair a keyboard that never came back.
 */
const KEYBOARD_WATCH_LOOKS = 3;

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
 * Captures whether `element` currently owns the keyboard and returns the thunk that
 * puts it back. Call the thunk in a `finally`, whether the sheet delivered media or
 * was cancelled. It is a no-op when the keyboard was down to begin with — a user who
 * opened the picker from a closed composer must not be ambushed by one — and calling
 * it twice restores once.
 *
 * The thunk WATCHES before it acts: a keyboard that the OS brought back on its own
 * is left strictly alone, because cycling focus underneath it is what the user sees
 * as the keyboard closing and reopening.
 */
export function holdKeyboardAcrossSheet(
  element: HTMLElement | null | undefined,
  options: KeyboardHoldOptions = {},
): () => void {
  const held = !!element && element.ownerDocument?.activeElement === element;
  const caret = held && element ? caretOf(element) : null;
  let restored = false;

  return () => {
    if (!held || !element || restored) return;
    restored = true;
    const schedule = options.schedule ?? afterSheetDismissal;
    const show = options.showSoftKeyboard ?? requestSoftKeyboard;
    const isKeyboardOpen = options.isKeyboardOpen ?? isSoftKeyboardUp;
    let looks = 0;

    const watch = () => {
      // Back on its own: the sheet returned first responder and the OS is already
      // raising the keyboard. There is nothing to repair, and touching focus here
      // would take it down and bring it up again in front of the user.
      if (isKeyboardOpen()) return;
      looks += 1;
      if (looks < KEYBOARD_WATCH_LOOKS) {
        schedule(watch, KEYBOARD_WATCH_MS);
        return;
      }
      // The element is still `document.activeElement`, so plain `focus()` is a
      // no-op to the engine. Only a real focus change raises the keyboard.
      if (element.ownerDocument?.activeElement === element) element.blur();
      element.focus({ preventScroll: true });
      if (caret) restoreCaret(element, caret);
      show();
    };

    schedule(watch, SHEET_DISMISS_MS);
  };
}
