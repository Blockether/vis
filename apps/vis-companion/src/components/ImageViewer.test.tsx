// @vitest-environment jsdom
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { ImageViewer } from './ImageViewer';

// The viewer is now a COMPOSITION: the geometry comes from `lib/zoom-pan`, the
// strokes from `AnnotationLayer`, the sheets from `lib/image-share`. These
// tests hold the seams together — that the readout really reflects the shared
// zoom maths, and that the pen the whole app shares is the pen this dialog
// mounts.

globalThis.IS_REACT_ACT_ENVIRONMENT = true;

let root: Root;
let host: HTMLDivElement;

beforeEach(() => {
  host = document.createElement('div');
  document.body.append(host);
  root = createRoot(host);
  HTMLCanvasElement.prototype.getContext = vi.fn(() => null) as never;
  act(() =>
    root.render(
      <ImageViewer
        src="blob:picture"
        name="chart.png"
        onClose={() => undefined}
      />,
    ),
  );
});

afterEach(() => {
  act(() => root.unmount());
  host.remove();
});

function control(label: string): HTMLButtonElement {
  const button = document.querySelector<HTMLButtonElement>(
    `button[aria-label="${label}"]`,
  );
  if (!button) throw new Error(`no control labelled ${label}`);
  return button;
}

function named(text: string): HTMLButtonElement {
  const button = [...document.querySelectorAll('button')].find(
    (b) => b.textContent === text,
  );
  if (!button) throw new Error(`no button named ${text}`);
  return button;
}

describe('ImageViewer', () => {
  it('opens as a modal named after the picture, fitted at 100%', () => {
    const dialog = document.querySelector('[role="dialog"]');
    expect(dialog?.getAttribute('aria-label')).toBe('chart.png image viewer');
    expect(control('Reset zoom').textContent).toBe('100%');
  });

  // The readout is written straight to the DOM rather than through state — a
  // pinch that re-rendered React on every frame stutters on exactly the devices
  // that pinch — so this is the only proof the shared maths reached the screen.
  it('zooms through the shared geometry and resets back to fitted', () => {
    act(() => control('Zoom in').click());
    expect(control('Reset zoom').textContent).toBe('135%');
    act(() => control('Zoom out').click());
    expect(control('Reset zoom').textContent).toBe('100%');
    act(() => control('Zoom in').click());
    act(() => control('Reset zoom').click());
    expect(control('Reset zoom').textContent).toBe('100%');
  });

  it('mounts the shared annotation layer, inert until Draw is pressed', () => {
    const canvas = document.querySelector('canvas');
    expect(canvas?.getAttribute('aria-label')).toBe('Image annotation layer');
    expect(canvas?.getAttribute('data-annotation')).toBe('idle');
    expect(document.querySelector('[aria-label="Drawing tools"]')).toBeNull();

    act(() => named('Draw').click());
    expect(named('Done').getAttribute('aria-pressed')).toBe('true');
    expect(
      document.querySelector('canvas')?.getAttribute('data-annotation'),
    ).toBe('active');
    // Five inks, Undo and Clear: the same strip a document page gets.
    const tools = document.querySelector('[aria-label="Drawing tools"]');
    expect(tools?.querySelectorAll('button[aria-pressed]')).toHaveLength(5);

    act(() => named('Done').click());
    expect(document.querySelector('[aria-label="Drawing tools"]')).toBeNull();
  });

  // The promise printed under the buttons has to match the button that is
  // actually there: without `onApply` the picture can only leave by copy/share.
  it('says what drawing is FOR, and only offers Apply when there is somewhere to apply it', () => {
    expect(
      document.querySelector('[aria-live="polite"]')?.textContent,
    ).toContain('to zoom');
    act(() => named('Draw').click());
    expect(document.querySelector('[aria-live="polite"]')?.textContent).toBe(
      'Draw on the image, then copy or share it.',
    );

    act(() =>
      root.render(
        <ImageViewer
          src="blob:picture"
          name="chart.png"
          onClose={() => undefined}
          onApply={() => undefined}
          applyLabel="Attach to message"
        />,
      ),
    );
    expect(named('Attach to message')).toBeTruthy();
  });
});

// Regression: the CSS transition meant for button/reset snaps also applied
// while a finger was dragging the picture every frame, fighting the direct
// pointer-driven transform and reading as pinch/pan stutter and lag.
it('suspends the snap transition while a pinch or pan is live, and restores it on lift', () => {
  const surface = document.querySelector<HTMLDivElement>('[role="dialog"] .cursor-grab');
  const transformed = document.querySelector<HTMLDivElement>('.origin-center');
  if (!surface || !transformed) throw new Error('viewer surface not found');
  Element.prototype.setPointerCapture = vi.fn();

  expect(transformed.style.transitionDuration).toBe('');

  act(() => {
    surface.dispatchEvent(
      new PointerEvent('pointerdown', { pointerId: 1, clientX: 0, clientY: 0, bubbles: true }),
    );
  });
  expect(transformed.style.transitionDuration).toBe('0ms');

  act(() => {
    surface.dispatchEvent(
      new PointerEvent('pointermove', { pointerId: 1, clientX: 10, clientY: 0, bubbles: true }),
    );
  });
  expect(transformed.style.transitionDuration).toBe('0ms');

  act(() => {
    surface.dispatchEvent(
      new PointerEvent('pointerup', { pointerId: 1, clientX: 10, clientY: 0, bubbles: true }),
    );
  });
  expect(transformed.style.transitionDuration).toBe('');
});
