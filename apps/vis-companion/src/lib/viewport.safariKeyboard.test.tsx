// @vitest-environment jsdom
import { act, useRef } from 'react';
import { render, screen } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { useVisualViewportShell } from './viewport';

vi.mock('@capacitor/core', () => ({
  Capacitor: { getPlatform: () => 'web', isNativePlatform: () => false },
}));

function ViewportProbe() {
  const shell = useRef<HTMLDivElement>(null);
  useVisualViewportShell(shell);
  return (
    <div ref={shell} data-testid="shell">
      <textarea aria-label="Message" />
    </div>
  );
}

const listeners = new Map<string, () => void>();

/** A visual viewport that the keyboard shortened and the browser scrolled. */
function viewport(top: { offsetTop: number; pageTop: number; scrollY: number }) {
  window.innerWidth = 390;
  window.innerHeight = 844;
  Object.defineProperty(window, 'scrollY', { configurable: true, value: top.scrollY });
  Object.defineProperty(window, 'visualViewport', {
    configurable: true,
    value: {
      width: 390,
      height: 500,
      offsetTop: top.offsetTop,
      pageTop: top.pageTop,
      addEventListener: (event: string, listener: () => void) => void listeners.set(event, listener),
      removeEventListener: (event: string) => void listeners.delete(event),
    },
  });
}

beforeEach(() => {
  listeners.clear();
  vi.stubGlobal('requestAnimationFrame', (callback: FrameRequestCallback) => {
    callback(0);
    return 1;
  });
  vi.stubGlobal('cancelAnimationFrame', () => undefined);
});

afterEach(() => {
  vi.unstubAllGlobals();
  Object.defineProperty(window, 'scrollY', { configurable: true, value: 0 });
});

describe('a web browser keyboard that scrolls the page', () => {
  // Regression, user report (paraphrased: in iOS Safari the input and the whole UI
  // jump when the keyboard opens): WebKit reports the visual viewport position in
  // `scrollY` too, so `offsetTop + scrollY` counted the scroll twice and pushed the
  // shell down under an empty band above the header.
  it('pins the shell to the visual viewport top once in WebKit', () => {
    viewport({ offsetTop: 190, pageTop: 190, scrollY: 190 });
    render(<ViewportProbe />);

    act(() => listeners.get('resize')?.());

    expect(screen.getByTestId('shell').style.transform).toBe('translateY(190px)');
  });

  it('adds the layout scroll where the browser keeps the two apart', () => {
    viewport({ offsetTop: 120, pageTop: 190, scrollY: 70 });
    render(<ViewportProbe />);

    act(() => listeners.get('resize')?.());

    expect(screen.getByTestId('shell').style.transform).toBe('translateY(190px)');
  });
});

describe('focusing a field in iOS Safari', () => {
  // Regression, user report (paraphrased: entering the input still jumps, smaller,
  // before it settles): Safari scrolled the page while its keyboard slid up, and the
  // shell followed only when the visual viewport reported the end of the slide.
  it('shortens the shell in the focus frame, then keeps the measured keyboard', () => {
    vi.stubGlobal('navigator', { ...navigator, userAgent: 'Mozilla/5.0 (iPhone; CPU iPhone OS 18_0 like Mac OS X)', maxTouchPoints: 5 });
    vi.stubGlobal('matchMedia', (query: string) => ({
      matches: false,
      media: query,
      addEventListener: () => undefined,
      removeEventListener: () => undefined,
    }));
    viewport({ offsetTop: 0, pageTop: 0, scrollY: 0 });
    Object.assign(window.visualViewport!, { height: 844 });
    render(<ViewportProbe />);
    const shell = screen.getByTestId('shell');
    const composer = screen.getByRole('textbox', { name: 'Message' });

    act(() => composer.focus());
    expect(shell.style.height).toBe('549px');

    Object.assign(window.visualViewport!, { height: 480 });
    act(() => listeners.get('resize')?.());
    expect(shell.style.height).toBe('480px');

    act(() => composer.blur());
    Object.assign(window.visualViewport!, { height: 844 });
    act(() => listeners.get('resize')?.());
    expect(shell.style.height).toBe('');

    act(() => composer.focus());
    expect(shell.style.height).toBe('480px');
  });
});

describe('Safari reveal scroll after the keyboard is up', () => {
  // Regression, device trace from the user report (paraphrased: entering the input
  // still jumps a little before it settles): Safari scrolled the page 172 px about
  // 200 ms after the keyboard, the shell followed one painted frame late, and a
  // reset at 350 ms left an empty band above the header for several frames.
  it('follows the scroll in the same event and does not undo it', () => {
    vi.useFakeTimers();
    vi.stubGlobal('navigator', { ...navigator, userAgent: 'Mozilla/5.0 (iPhone; CPU iPhone OS 18_7 like Mac OS X)', maxTouchPoints: 5 });
    vi.stubGlobal('matchMedia', (query: string) => ({
      matches: false,
      media: query,
      addEventListener: () => undefined,
      removeEventListener: () => undefined,
    }));
    const frames: FrameRequestCallback[] = [];
    vi.stubGlobal('requestAnimationFrame', (callback: FrameRequestCallback) => frames.push(callback));
    const flush = () => frames.splice(0).forEach((callback) => callback(0));
    const scrollTo = vi.fn();
    vi.stubGlobal('scrollTo', scrollTo);
    viewport({ offsetTop: 0, pageTop: 0, scrollY: 0 });
    Object.assign(window.visualViewport!, { height: 474 });
    window.innerHeight = 796;
    render(<ViewportProbe />);
    const shell = screen.getByTestId('shell');
    const composer = screen.getByRole('textbox', { name: 'Message' });
    act(() => {
      composer.focus();
      flush();
    });
    expect(shell.style.height).toBe('474px');

    Object.assign(window.visualViewport!, { offsetTop: 172, pageTop: 172 });
    Object.defineProperty(window, 'scrollY', { configurable: true, value: 172 });
    act(() => window.dispatchEvent(new Event('scroll')));
    expect(shell.style.transform).toBe('translateY(172px)');

    act(() => {
      vi.advanceTimersByTime(400);
      flush();
    });
    expect(scrollTo).not.toHaveBeenCalled();
    expect(shell.style.transform).toBe('translateY(172px)');

    // Keyboard down: Safari zeroes `scrollY` first and keeps the stale `pageTop`
    // and visual height for about 500 ms. The header moved 172 px down meanwhile.
    act(() => {
      composer.blur();
      vi.advanceTimersByTime(0);
    });
    expect(shell.style.transform).toBe('');
    expect(shell.style.height).toBe('');
    Object.defineProperty(window, 'scrollY', { configurable: true, value: 0 });
    act(() => {
      window.dispatchEvent(new Event('scroll'));
      flush();
    });
    expect(shell.style.transform).toBe('');
    expect(shell.style.height).toBe('');
    vi.useRealTimers();
  });
});

describe('focusing a field in iOS Safari without a reveal pan', () => {
  // Regression, user report (paraphrased: the session header still moves down when
  // the keyboard opens): Safari pans the page to reveal a tapped field, and script
  // cannot follow the animated pan. The device trace showed pans up to 322 px.
  const iosSafari = () => {
    vi.useFakeTimers();
    vi.stubGlobal('requestAnimationFrame', (callback: FrameRequestCallback) => {
      callback(0);
      return 1;
    });
    vi.stubGlobal('navigator', { ...navigator, userAgent: 'Mozilla/5.0 (iPhone; CPU iPhone OS 18_7 like Mac OS X)', maxTouchPoints: 5 });
    vi.stubGlobal('matchMedia', (query: string) => ({
      matches: false,
      media: query,
      addEventListener: () => undefined,
      removeEventListener: () => undefined,
    }));
    // A real phone keeps `screen` fixed while Safari changes `innerHeight`.
    vi.spyOn(window.screen, 'width', 'get').mockReturnValue(390);
    vi.spyOn(window.screen, 'height', 'get').mockReturnValue(844);
    viewport({ offsetTop: 0, pageTop: 0, scrollY: 0 });
    Object.assign(window.visualViewport!, { height: 796 });
    window.innerHeight = 796;
    render(<ViewportProbe />);
    return screen.getByRole('textbox', { name: 'Message' });
  };
  const touch = (target: Element, type: 'touchstart' | 'touchend', y: number) => {
    const point = { clientX: 100, clientY: y, target } as unknown as Touch;
    const event = new TouchEvent(type, {
      bubbles: true,
      cancelable: true,
      touches: type === 'touchstart' ? [point] : [],
      changedTouches: [point],
    });
    target.dispatchEvent(event);
    return event;
  };

  it('focuses a tapped field itself, without a browser scroll', () => {
    const composer = iosSafari();
    const focus = vi.spyOn(composer, 'focus');
    touch(composer, 'touchstart', 700);
    const tap = touch(composer, 'touchend', 702);
    expect(tap.defaultPrevented).toBe(true);
    expect(focus).toHaveBeenCalledWith({ preventScroll: true });
    expect(document.activeElement).toBe(composer);

    // A tap on the focused field keeps the native caret placement.
    touch(composer, 'touchstart', 700);
    expect(touch(composer, 'touchend', 700).defaultPrevented).toBe(false);
    act(() => {
      composer.blur();
      vi.advanceTimersByTime(0);
    });

    // A drag that starts on the field is a scroll, not a tap.
    touch(composer, 'touchstart', 700);
    expect(touch(composer, 'touchend', 640).defaultPrevented).toBe(false);
    expect(document.activeElement).not.toBe(composer);
    vi.useRealTimers();
  });

  it('learns the whole keyboard while Safari shortens innerHeight', () => {
    const composer = iosSafari();
    const shell = screen.getByTestId('shell');
    act(() => composer.focus());

    // Safari also shortens `innerHeight` while the keys are up; the learned height
    // must still be the whole keyboard.
    Object.assign(window.visualViewport!, { height: 474 });
    window.innerHeight = 624;
    act(() => listeners.get('resize')?.());
    expect(shell.style.height).toBe('474px');

    act(() => {
      composer.blur();
      vi.advanceTimersByTime(0);
    });
    expect(shell.style.height).toBe('');

    Object.assign(window.visualViewport!, { height: 796 });
    window.innerHeight = 796;
    act(() => listeners.get('resize')?.());
    act(() => composer.focus());
    expect(shell.style.height).toBe('474px');
    vi.useRealTimers();
  });
});
