import { describe, expect, it, vi } from 'vitest';
import { holdKeyboardAcrossSheet, isEnterSendPlatform } from './keyboard';

let platform = 'web';

vi.mock('@capacitor/core', () => ({
  Capacitor: {
    getPlatform: () => platform,
    isNativePlatform: () => platform !== 'web',
  },
}));
vi.mock('@capacitor/keyboard', () => ({ Keyboard: { show: () => Promise.resolve() } }));

interface Composer {
  element: HTMLTextAreaElement;
  document: { activeElement: unknown };
  log: string[];
}

/** A textarea reduced to what the hold touches: focus, blur, caret, owner document. */
function composer(options: { focused?: boolean; caret?: [number, number] } = {}): Composer {
  const log: string[] = [];
  const document = { activeElement: null as unknown };
  const [start, end] = options.caret ?? [4, 4];
  const element = {
    ownerDocument: document,
    selectionStart: start,
    selectionEnd: end,
    blur() {
      log.push('blur');
      document.activeElement = null;
    },
    focus(init?: { preventScroll?: boolean }) {
      log.push(`focus:${init?.preventScroll === true}`);
      document.activeElement = element;
    },
    setSelectionRange(from: number, to: number) {
      log.push(`caret:${from}-${to}`);
    },
  };
  if (options.focused !== false) document.activeElement = element;
  return { element: element as unknown as HTMLTextAreaElement, document, log };
}

/** A clock the test steps by hand, so a retry loop can be watched one look at a time. */
function stepper() {
  const pending: Array<() => void> = [];
  return {
    schedule: (run: () => void) => void pending.push(run),
    step: () => pending.shift()?.(),
    drain: () => {
      while (pending.length) pending.shift()?.();
    },
  };
}

describe('holdKeyboardAcrossSheet', () => {
  // The keyboard goes down on the app's terms, not the sheet's: with no field
  // focused, the dismissal has nothing to restore and nothing to race.
  it('takes the keyboard down with the composer before the sheet opens', () => {
    const { element, log, document } = composer();

    holdKeyboardAcrossSheet(element, {
      schedule: () => undefined,
      showSoftKeyboard: () => undefined,
      isKeyboardOpen: () => false,
    });

    expect(log).toEqual(['blur']);
    expect(document.activeElement).toBeNull();
  });

  it('refocuses with the caret so the keyboard comes back after the sheet', () => {
    const { element, log, document } = composer({ caret: [7, 9] });
    const showSoftKeyboard = vi.fn();
    const clock = stepper();

    const restore = holdKeyboardAcrossSheet(element, {
      schedule: clock.schedule,
      showSoftKeyboard,
      isKeyboardOpen: () => false,
    });
    restore();
    clock.step();

    expect(log).toEqual(['blur', 'focus:true', 'caret:7-9']);
    expect(showSoftKeyboard).toHaveBeenCalledTimes(1);
    expect(document.activeElement).toBe(element);
  });

  it('waits for the sheet to finish dismissing before refocusing', () => {
    const { element, log } = composer();
    const clock = stepper();

    holdKeyboardAcrossSheet(element, {
      schedule: clock.schedule,
      showSoftKeyboard: () => undefined,
      isKeyboardOpen: () => false,
    })();

    expect(log).toEqual(['blur']);
    clock.step();
    expect(log).toEqual(['blur', 'focus:true', 'caret:4-4']);
  });

  // WebKit drops a focus that lands while the sheet is still dismissing, which
  // would leave the composer keyboard-less. The retry is what saves it — and it
  // stops the moment the keyboard is up, so it can never cycle one that is.
  it('retries the focus until the keyboard is up', () => {
    const { element, log } = composer();
    const clock = stepper();
    let open = false;

    holdKeyboardAcrossSheet(element, {
      schedule: clock.schedule,
      showSoftKeyboard: () => undefined,
      isKeyboardOpen: () => open,
    })();

    clock.step();
    expect(log).toEqual(['blur', 'focus:true', 'caret:4-4']);
    clock.step();
    const twice = ['blur', 'focus:true', 'caret:4-4', 'blur', 'focus:true', 'caret:4-4'];
    expect(log).toEqual(twice);

    open = true;
    clock.drain();
    expect(log).toEqual(twice);
  });

  it('leaves a closed keyboard closed', () => {
    const { element, log } = composer({ focused: false });
    const showSoftKeyboard = vi.fn();

    holdKeyboardAcrossSheet(element, {
      schedule: (run) => run(),
      showSoftKeyboard,
      isKeyboardOpen: () => false,
    })();

    expect(log).toEqual([]);
    expect(showSoftKeyboard).not.toHaveBeenCalled();
  });

  it('tolerates a missing composer', () => {
    const showSoftKeyboard = vi.fn();
    expect(() =>
      holdKeyboardAcrossSheet(null, { schedule: (run) => run(), showSoftKeyboard })(),
    ).not.toThrow();
    expect(showSoftKeyboard).not.toHaveBeenCalled();
  });

  it('restores once however often the thunk is called', () => {
    const { element, log } = composer();
    const clock = stepper();

    const restore = holdKeyboardAcrossSheet(element, {
      schedule: clock.schedule,
      showSoftKeyboard: () => undefined,
      isKeyboardOpen: () => false,
    });
    restore();
    restore();
    clock.step();

    expect(log).toEqual(['blur', 'focus:true', 'caret:4-4']);
  });

  it('still restores focus when the caret no longer fits the shrunken value', () => {
    const { element, log } = composer();
    const failing = element as unknown as { setSelectionRange: () => void };
    failing.setSelectionRange = () => {
      throw new RangeError('index out of range');
    };
    const showSoftKeyboard = vi.fn();
    const clock = stepper();

    expect(() => {
      holdKeyboardAcrossSheet(element, {
        schedule: clock.schedule,
        showSoftKeyboard,
        isKeyboardOpen: () => false,
      })();
      clock.step();
    }).not.toThrow();

    expect(log).toEqual(['blur', 'focus:true']);
    expect(showSoftKeyboard).toHaveBeenCalledTimes(1);
  });

  // Delivering media makes iOS hand first responder back and raise the keyboard
  // itself; a blind blur/focus on top of that IS the flicker.
  it('leaves a keyboard the platform already handed back alone', () => {
    const { element, log } = composer();
    const showSoftKeyboard = vi.fn();

    holdKeyboardAcrossSheet(element, {
      schedule: (run) => run(),
      showSoftKeyboard,
      isKeyboardOpen: () => true,
    })();

    expect(log).toEqual(['blur']);
    expect(showSoftKeyboard).not.toHaveBeenCalled();
  });
});

/**
 * A miniature iPhone: the software keyboard follows the focused editable element,
 * a native sheet owns the screen while it is up, and UIKit hands first responder
 * back to whatever the webview still has focused once the sheet is gone.
 *
 * The flag `holdKeyboardAcrossSheet` reads is NOT the keyboard: Capacitor's
 * `keyboardWillShow` crosses the bridge after UIKit has already started moving it
 * (see `useVisualViewportShell`), so `isKeyboardOpen` trails the real thing.
 */
function iphone(options: { flagLagMs?: number } = {}) {
  const flagLagMs = options.flagLagMs ?? 150;
  const log: string[] = [];
  const timers: Array<{ at: number; seq: number; run: () => void }> = [];
  const ownerDocument = { activeElement: null as unknown };
  let now = 0;
  let seq = 0;
  let keyboardUp = false;
  let reportedUp = false;
  let sheetOpen = false;

  const schedule = (run: () => void, delayMs: number) => {
    timers.push({ at: now + delayMs, seq: (seq += 1), run });
  };
  const advance = (ms: number) => {
    const until = now + ms;
    for (;;) {
      timers.sort((a, b) => a.at - b.at || a.seq - b.seq);
      const next = timers[0];
      if (!next || next.at > until) break;
      timers.shift();
      now = next.at;
      next.run();
    }
    now = until;
  };
  const setKeyboard = (up: boolean) => {
    if (up === keyboardUp) return;
    keyboardUp = up;
    log.push(up ? 'up' : 'down');
    schedule(() => void (reportedUp = up), flagLagMs);
  };

  const element = {
    ownerDocument,
    selectionStart: 12,
    selectionEnd: 12,
    blur() {
      if (ownerDocument.activeElement !== element) return;
      ownerDocument.activeElement = null;
      if (!sheetOpen) setKeyboard(false);
    },
    focus() {
      ownerDocument.activeElement = element;
      if (!sheetOpen) setKeyboard(true);
    },
    setSelectionRange() {},
  };

  return {
    composer: element as unknown as HTMLTextAreaElement,
    log,
    schedule,
    advance,
    isKeyboardOpen: () => reportedUp,
    type() {
      element.focus();
    },
    openSheet() {
      sheetOpen = true;
      setKeyboard(false);
    },
    /** Dismissal done: UIKit restores the keyboard iff the webview still owns a field. */
    closeSheet(restoreMs = 250) {
      sheetOpen = false;
      schedule(() => {
        if (ownerDocument.activeElement === element) setKeyboard(true);
      }, restoreMs);
    },
  };
}

// Regression, reported from an iOS build: write a message, attach a photo, and the
// keyboard goes "down then up then down then up". Delivering media makes UIKit put
// the keyboard back by itself, but `isKeyboardOpen` still reads false while it is
// rising, so the timed repair blurs the composer underneath a keyboard already on
// its way up and then drags it back — two visible cycles for one attachment.
describe('holdKeyboardAcrossSheet on a phone that answers late', () => {
  it('costs one keyboard cycle when the sheet delivers a photo', () => {
    const ios = iphone();
    ios.type();

    const restore = holdKeyboardAcrossSheet(ios.composer, {
      schedule: ios.schedule,
      showSoftKeyboard: () => undefined,
      isKeyboardOpen: ios.isKeyboardOpen,
    });
    ios.openSheet();
    ios.advance(4000);
    ios.closeSheet();
    restore();
    ios.advance(4000);

    expect(ios.log).toEqual(['up', 'down', 'up']);
  });

  it('brings the keyboard back after a cancelled sheet', () => {
    const ios = iphone();
    ios.type();

    const restore = holdKeyboardAcrossSheet(ios.composer, {
      schedule: ios.schedule,
      showSoftKeyboard: () => undefined,
      isKeyboardOpen: ios.isKeyboardOpen,
    });
    ios.openSheet();
    ios.advance(2000);
    // Cancel: iOS restores nothing, whatever the webview still has focused.
    ios.closeSheet(Number.POSITIVE_INFINITY);
    restore();
    ios.advance(4000);

    expect(ios.log).toEqual(['up', 'down', 'up']);
  });
});

// Regression: on iOS/Android a bare Return submitted the message, so a multi-line
// message could not be typed at all — the on-screen keyboard has no Shift to hold.
describe('isEnterSendPlatform', () => {
  it('sends on Enter only where a hardware keyboard has a Shift to hold', () => {
    expect(isEnterSendPlatform('web')).toBe(true);
    expect(isEnterSendPlatform('ios')).toBe(false);
    expect(isEnterSendPlatform('android')).toBe(false);
  });
});
