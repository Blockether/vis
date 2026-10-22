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
