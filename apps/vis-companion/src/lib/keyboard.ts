import { Capacitor } from '@capacitor/core';
import { Keyboard } from '@capacitor/keyboard';

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

interface Caret {
  start: number | null;
  end: number | null;
}

export interface KeyboardHoldOptions {
  /** Defers the refocus. Injected by tests; production waits out the dismissal. */
  schedule?: (run: () => void) => void;
  /** Asks the platform for the keyboard once focus is back. */
  showSoftKeyboard?: () => void;
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

function afterSheetDismissal(run: () => void): void {
  if (typeof setTimeout !== 'function') {
    run();
    return;
  }
  setTimeout(run, SHEET_DISMISS_MS);
}

/**
 * Captures whether `element` currently owns the keyboard and returns the thunk that
 * puts it back. Call the thunk in a `finally`, whether the sheet delivered media or
 * was cancelled. It is a no-op when the keyboard was down to begin with — a user who
 * opened the picker from a closed composer must not be ambushed by one — and calling
 * it twice restores once.
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
    schedule(() => {
      // The element is still `document.activeElement`, so plain `focus()` is a
      // no-op to the engine. Only a real focus change raises the keyboard.
      if (element.ownerDocument?.activeElement === element) element.blur();
      element.focus({ preventScroll: true });
      if (caret) restoreCaret(element, caret);
      show();
    });
  };
}
