import { describe, expect, it, vi } from 'vitest';
import { holdKeyboardAcrossSheet } from './keyboard';

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

describe('holdKeyboardAcrossSheet', () => {
  it('blurs and refocuses so the keyboard comes back after a cancelled sheet', () => {
    const { element, log, document } = composer({ caret: [7, 9] });
    const showSoftKeyboard = vi.fn();

    const restore = holdKeyboardAcrossSheet(element, {
      schedule: (run) => run(),
      showSoftKeyboard,
    });
    // The native sheet never moved DOM focus, so a plain focus() would be ignored.
    expect(log).toEqual([]);

    restore();

    expect(log).toEqual(['blur', 'focus:true', 'caret:7-9']);
    expect(showSoftKeyboard).toHaveBeenCalledTimes(1);
    expect(document.activeElement).toBe(element);
  });

  it('waits for the sheet to finish dismissing before refocusing', () => {
    const { element, log } = composer();
    const pending: Array<() => void> = [];

    holdKeyboardAcrossSheet(element, {
      schedule: (run) => void pending.push(run),
      showSoftKeyboard: () => undefined,
    })();

    expect(log).toEqual([]);
    pending.forEach((run) => run());
    expect(log).toEqual(['blur', 'focus:true', 'caret:4-4']);
  });

  it('leaves a closed keyboard closed', () => {
    const { element, log } = composer({ focused: false });
    const showSoftKeyboard = vi.fn();

    holdKeyboardAcrossSheet(element, { schedule: (run) => run(), showSoftKeyboard })();

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

    const restore = holdKeyboardAcrossSheet(element, {
      schedule: (run) => run(),
      showSoftKeyboard: () => undefined,
    });
    restore();
    restore();

    expect(log).toEqual(['blur', 'focus:true', 'caret:4-4']);
  });

  it('still restores focus when the caret no longer fits the shrunken value', () => {
    const { element, log } = composer();
    const failing = element as unknown as { setSelectionRange: () => void };
    failing.setSelectionRange = () => {
      throw new RangeError('index out of range');
    };
    const showSoftKeyboard = vi.fn();

    expect(() =>
      holdKeyboardAcrossSheet(element, { schedule: (run) => run(), showSoftKeyboard })(),
    ).not.toThrow();

    expect(log).toEqual(['blur', 'focus:true']);
    expect(showSoftKeyboard).toHaveBeenCalledTimes(1);
  });
});
