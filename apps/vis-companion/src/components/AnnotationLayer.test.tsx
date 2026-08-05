// @vitest-environment jsdom
import { act, createRef, type RefObject } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import {
  AnnotationLayer,
  PenToolbar,
  type AnnotationSurface,
} from './AnnotationLayer';

// The pen used to live inside the image viewer and nowhere else, so annotating
// a PDF page meant opening a full-screen picture dialog. These tests hold the
// layer to being usable ON ITS OWN: mount it over anything, drive it by ref.

declare global {
  // eslint-disable-next-line no-var
  var IS_REACT_ACT_ENVIRONMENT: boolean;
}
globalThis.IS_REACT_ACT_ENVIRONMENT = true;

let host: HTMLDivElement;
let root: Root;

beforeEach(() => {
  host = document.createElement('div');
  document.body.append(host);
  root = createRoot(host);
  // jsdom has no 2d context and no pointer capture; the layer must survive both,
  // because a stroke is DATA and the painting is only its shadow.
  HTMLCanvasElement.prototype.getContext = vi.fn(() => null) as never;
  Element.prototype.setPointerCapture = vi.fn();
});

afterEach(() => {
  act(() => root.unmount());
  host.remove();
});

function mount(node: React.ReactNode) {
  act(() => root.render(node));
}

function layer(): HTMLCanvasElement {
  const canvas = host.querySelector('canvas');
  if (!canvas) throw new Error('no annotation layer');
  canvas.getBoundingClientRect = () =>
    ({
      left: 0,
      top: 0,
      width: canvas.width,
      height: canvas.height,
    }) as DOMRect;
  return canvas;
}

function pointer(
  canvas: HTMLCanvasElement,
  type: string,
  x: number,
  y: number,
) {
  const event = new MouseEvent(type, { bubbles: true, clientX: x, clientY: y });
  Object.defineProperty(event, 'isPrimary', { value: true });
  Object.defineProperty(event, 'pointerId', { value: 1 });
  act(() => {
    canvas.dispatchEvent(event);
  });
}

function draw(canvas: HTMLCanvasElement, points: [number, number][]) {
  const [first, ...rest] = points;
  if (!first) return;
  pointer(canvas, 'pointerdown', first[0], first[1]);
  for (const [x, y] of rest) pointer(canvas, 'pointermove', x, y);
  pointer(canvas, 'pointerup', 0, 0);
}

function surface(ref: RefObject<AnnotationSurface | null>): AnnotationSurface {
  if (!ref.current) throw new Error('layer did not publish a surface');
  return ref.current;
}

describe('AnnotationLayer', () => {
  it('is inert until it is switched on, so the picture below stays draggable', () => {
    const onStrokesChange = vi.fn();
    mount(
      <AnnotationLayer
        active={false}
        color="--err"
        onStrokesChange={onStrokesChange}
      />,
    );
    const canvas = layer();
    expect(canvas.className).toContain('pointer-events-none');
    draw(canvas, [
      [1, 1],
      [5, 5],
    ]);
    expect(onStrokesChange).not.toHaveBeenCalled();
  });

  it('records one stroke per gesture and reports the count', () => {
    const onStrokesChange = vi.fn();
    const ref = createRef<AnnotationSurface>();
    mount(
      <AnnotationLayer
        ref={ref}
        active
        color="--err"
        onStrokesChange={onStrokesChange}
      />,
    );
    const canvas = layer();
    act(() => surface(ref).fit(200, 100));
    draw(canvas, [
      [10, 10],
      [20, 30],
    ]);
    draw(canvas, [[40, 40]]);
    expect(onStrokesChange).toHaveBeenLastCalledWith(2);
    expect(canvas.width).toBe(200);
    expect(canvas.height).toBe(100);
  });

  it('undoes the last stroke and clears every one', () => {
    const onStrokesChange = vi.fn();
    const ref = createRef<AnnotationSurface>();
    mount(
      <AnnotationLayer
        ref={ref}
        active
        color="--fg"
        onStrokesChange={onStrokesChange}
      />,
    );
    const canvas = layer();
    act(() => surface(ref).fit(50, 50));
    draw(canvas, [[1, 1]]);
    draw(canvas, [[2, 2]]);
    act(() => surface(ref).undo());
    expect(onStrokesChange).toHaveBeenLastCalledWith(1);
    act(() => surface(ref).clear());
    expect(onStrokesChange).toHaveBeenLastCalledWith(0);
  });

  // Resizing a canvas wipes it, so the strokes have to go with it — keeping them
  // would leave marks that no longer sit on anything.
  it('drops the drawing when it is fitted to a different picture', () => {
    const onStrokesChange = vi.fn();
    const ref = createRef<AnnotationSurface>();
    mount(
      <AnnotationLayer
        ref={ref}
        active
        color="--ok"
        onStrokesChange={onStrokesChange}
      />,
    );
    act(() => surface(ref).fit(50, 50));
    draw(layer(), [[1, 1]]);
    expect(onStrokesChange).toHaveBeenLastCalledWith(1);
    act(() => surface(ref).fit(80, 20));
    expect(onStrokesChange).toHaveBeenLastCalledWith(0);
  });

  it('hands its own canvas back for flattening', () => {
    const ref = createRef<AnnotationSurface>();
    mount(<AnnotationLayer ref={ref} active color="--err" />);
    expect(surface(ref).canvas()).toBe(layer());
  });
});

describe('PenToolbar', () => {
  it('offers every ink, says which one is chosen, and is touch-sized', () => {
    const onColor = vi.fn();
    mount(
      <PenToolbar
        color="--fg"
        onColor={onColor}
        strokeCount={0}
        onUndo={() => undefined}
        onClear={() => undefined}
      />,
    );
    const swatches = [...host.querySelectorAll('button[aria-pressed]')];
    expect(swatches).toHaveLength(5);
    expect(
      swatches.filter((b) => b.getAttribute('aria-pressed') === 'true'),
    ).toHaveLength(1);
    for (const swatch of swatches) {
      expect(swatch.getAttribute('aria-label')).toMatch(/pen$/u);
      expect(swatch.className).toContain('min-h-11');
      expect(swatch.className).not.toContain('sm:min-h');
    }
    act(() => (swatches[0] as HTMLButtonElement).click());
    expect(onColor).toHaveBeenCalledWith('--err');
  });

  // Undo and Clear on an untouched picture promise something they cannot do.
  it('disables the ways back until there is something to take back', () => {
    const onUndo = vi.fn();
    mount(
      <PenToolbar
        color="--err"
        onColor={() => undefined}
        strokeCount={0}
        onUndo={onUndo}
        onClear={() => undefined}
      />,
    );
    const back = [...host.querySelectorAll('button')].filter((b) =>
      ['Undo', 'Clear'].includes(b.textContent ?? ''),
    );
    expect(back).toHaveLength(2);
    for (const button of back)
      expect((button as HTMLButtonElement).disabled).toBe(true);

    mount(
      <PenToolbar
        color="--err"
        onColor={() => undefined}
        strokeCount={2}
        onUndo={onUndo}
        onClear={() => undefined}
      />,
    );
    const undo = [...host.querySelectorAll('button')].find(
      (b) => b.textContent === 'Undo',
    );
    expect((undo as HTMLButtonElement).disabled).toBe(false);
    act(() => (undo as HTMLButtonElement).click());
    expect(onUndo).toHaveBeenCalled();
  });
});
