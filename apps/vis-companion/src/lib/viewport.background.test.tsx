// @vitest-environment jsdom
import { useRef } from 'react';
import { act, render, screen } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import {
  isAppForeground,
  useKeyboardInset,
  useVisualViewportShell,
  watchKeyboardInset,
} from './viewport';

const native = vi.hoisted(() => ({
  keyboard: new Map<string, (info: { keyboardHeight: number }) => void>(),
  app: new Map<string, (state?: { isActive: boolean }) => void>(),
}));

vi.mock('@capacitor/core', () => ({
  Capacitor: { getPlatform: () => 'ios' },
}));

vi.mock('@capacitor/keyboard', () => ({
  Keyboard: {
    addListener: (event: string, listener: (info: { keyboardHeight: number }) => void) => {
      native.keyboard.set(event, listener);
      return Promise.resolve({ remove: () => void native.keyboard.delete(event) });
    },
    setScroll: () => Promise.resolve(),
  },
}));

vi.mock('@capacitor/app', () => ({
  App: {
    addListener: (event: string, listener: (state?: { isActive: boolean }) => void) => {
      native.app.set(event, listener);
      return Promise.resolve({ remove: () => void native.app.delete(event) });
    },
  },
}));

function ViewportProbe() {
  const shell = useRef<HTMLDivElement>(null);
  const inset = useKeyboardInset();
  useVisualViewportShell(shell);
  return (
    <div ref={shell} data-testid="shell">
      <textarea aria-label="Message" />
      <span data-testid="inset">{inset}</span>
    </div>
  );
}

beforeEach(() => {
  vi.useFakeTimers();
  native.keyboard.clear();
  native.app.clear();
  window.innerWidth = 390;
  window.innerHeight = 844;
  Object.defineProperty(window, 'visualViewport', {
    configurable: true,
    value: {
      width: 390,
      height: 844,
      offsetTop: 0,
      addEventListener: () => undefined,
      removeEventListener: () => undefined,
    },
  });
  vi.stubGlobal('requestAnimationFrame', () => 1);
  vi.stubGlobal('cancelAnimationFrame', () => undefined);
});

afterEach(() => {
  vi.useRealTimers();
  vi.unstubAllGlobals();
});

describe('the native shell after backgrounding', () => {
  // Regression, user report (paraphrased: with the keyboard open, a trip to another
  // app and back showed the session try to open the keyboard, drop it again and
  // make the composer jump). The return refocused the composer, which pinned the
  // shell for a predicted keyboard that iOS never raised, then unpinned it.
  it('comes back with the keyboard down and the composer still', () => {
    render(<ViewportProbe />);
    const shell = screen.getByTestId('shell');
    const composer = screen.getByRole('textbox', { name: 'Message' });
    // Each inset with the shell height its listener sees: the pin is already written.
    const insets: string[] = [];
    const stopWatching = watchKeyboardInset((inset) => insets.push(`${inset}@${shell.style.height}`));
    composer.focus();
    act(() => native.keyboard.get('keyboardWillShow')?.({ keyboardHeight: 300 }));
    expect(shell).toHaveStyle({ height: '544px' });
    // A box that reaches under the keyboard hears the inset in the same task.
    expect(insets.at(-1)).toBe('300@544px');

    act(() => native.app.get('appStateChange')?.({ isActive: false }));
    // The shell is back at full height before the app is on screen again.
    expect(shell.style.height).toBe('');
    expect(insets.at(-1)).toBe('0@');
    expect(screen.getByTestId('inset')).toHaveTextContent('0');

    const focus = vi.spyOn(composer, 'focus');
    act(() => native.app.get('appStateChange')?.({ isActive: true }));
    for (let elapsed = 0; elapsed < 1500; elapsed += 50) {
      act(() => vi.advanceTimersByTime(50));
      expect(shell.style.height).toBe('');
    }
    expect(focus).not.toHaveBeenCalled();
    expect(document.activeElement).not.toBe(composer);
    stopWatching();
  });

  // Regression: TestFlight builds 5358 and 5468 blocked in
  // didProgrammaticallyClearFocusedElement / UIKeyboardTaskQueue. Capacitor's
  // iOS resume is willEnterForeground, not didBecomeActive: it must not blur.
  it('waits for didBecomeActive before dropping the stale focus, once', () => {
    render(<ViewportProbe />);
    const composer = screen.getByRole('textbox', { name: 'Message' });
    composer.focus();
    act(() => native.keyboard.get('keyboardWillShow')?.({ keyboardHeight: 300 }));
    act(() => native.app.get('appStateChange')?.({ isActive: false }));
    const blur = vi.spyOn(composer, 'blur');
    const focus = vi.spyOn(composer, 'focus');

    act(() => native.app.get('resume')?.());
    act(() => vi.advanceTimersByTime(500));
    expect(isAppForeground()).toBe(false);
    expect(blur).not.toHaveBeenCalled();

    act(() => native.app.get('appStateChange')?.({ isActive: true }));
    act(() => vi.advanceTimersByTime(500));
    expect(isAppForeground()).toBe(true);
    expect(blur).toHaveBeenCalledTimes(1);
    expect(focus).not.toHaveBeenCalled();
  });

  // Regression, Vis session 78b0c0b5-f5ba-453f-97ee-af0a85f72d25: iOS ended
  // keyboard editing while the WebView was suspended, but stale DOM focus and the
  // old keyboard-height pin returned as an empty band where the keyboard had been.
  it('drops the empty band without reopening the keyboard', () => {
    render(<ViewportProbe />);
    const shell = screen.getByTestId('shell');
    const composer = screen.getByRole('textbox', { name: 'Message' });
    composer.focus();

    act(() => native.keyboard.get('keyboardWillShow')?.({ keyboardHeight: 300 }));
    expect(shell).toHaveStyle({ height: '544px' });

    act(() => native.app.get('appStateChange')?.({ isActive: true }));

    expect(shell.style.height).toBe('');
    expect(document.activeElement).not.toBe(composer);

    act(() => vi.advanceTimersByTime(500));
    expect(document.activeElement).not.toBe(composer);
  });

  // Regression, TestFlight build 5275: leaving the foreground blurred the composer
  // from JS. WebKit reported that programmatic focus clear to the UI process during
  // the background scene update, UIKit turned it into a keyboard task no thread was
  // left to run, and the watchdog killed Vis with 0x8BADF00D after ten seconds.
  it('never touches DOM focus while the app leaves the foreground', () => {
    render(<ViewportProbe />);
    const composer = screen.getByRole('textbox', { name: 'Message' });
    const blur = vi.spyOn(composer, 'blur');
    const focus = vi.spyOn(composer, 'focus');
    composer.focus();

    act(() => native.keyboard.get('keyboardWillShow')?.({ keyboardHeight: 300 }));
    expect(document.activeElement).toBe(composer);

    act(() => native.app.get('appStateChange')?.({ isActive: false }));
    act(() => vi.advanceTimersByTime(500));
    expect(blur).not.toHaveBeenCalled();
    expect(focus).toHaveBeenCalledTimes(1);
    expect(document.activeElement).toBe(composer);
  });
});

// Regression, user report (paraphrased: on the phone the box for typing the group's
// name cannot be seen): a menu hanging under a control low on the window ran into the
// raised keyboard, so the field it opened for sat under the keys. The shell already
// knows how tall the covered band is; it now publishes it, and `Menu`'s panel stands
// on exactly that much (BLO-167).
describe('the band a raised keyboard covers', () => {
  it('publishes the covered height to whoever is pinned to the bottom edge', () => {
    render(<ViewportProbe />);
    const inset = screen.getByTestId('inset');
    expect(inset).toHaveTextContent('0');

    act(() => screen.getByRole('textbox', { name: 'Message' }).focus());
    act(() => native.keyboard.get('keyboardWillShow')?.({ keyboardHeight: 300 }));

    expect(screen.getByTestId('shell')).toHaveStyle({ height: '544px' });
    expect(inset).toHaveTextContent('300');
  });
});
