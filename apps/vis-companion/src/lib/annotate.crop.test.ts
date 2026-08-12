// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from 'vitest';
import { flattenAnnotations } from './annotate';

// Flattening is where a trim actually cuts: `visiblePart` says WHICH pixels,
// this says that exactly those pixels — and the strokes over them — are what
// gets written, at the original's own resolution rather than the screen's.

type Draw = number[];

function stubCanvas(): { draws: Draw[]; output: HTMLCanvasElement } {
  const draws: Draw[] = [];
  const context = {
    drawImage: (_source: unknown, ...rest: number[]) => draws.push(rest),
  };
  const output = {
    width: 0,
    height: 0,
    getContext: () => context,
    toBlob: (done: (blob: Blob) => void) => done(new Blob(['png'])),
  };
  vi.spyOn(document, 'createElement').mockReturnValue(
    output as unknown as HTMLElement,
  );
  return { draws, output: output as unknown as HTMLCanvasElement };
}

function picture(width: number, height: number): HTMLImageElement {
  return { naturalWidth: width, naturalHeight: height } as HTMLImageElement;
}

afterEach(() => vi.restoreAllMocks());

describe('flattening a picture', () => {
  it('writes the whole picture when nothing was trimmed', async () => {
    const { draws, output } = stubCanvas();
    await flattenAnnotations(picture(800, 600), null);
    expect(output.width).toBe(800);
    expect(output.height).toBe(600);
    expect(draws).toEqual([[0, 0, 800, 600, 0, 0, 800, 600]]);
  });

  // The trim is cut from the ORIGINAL: a quarter of a 4000px photo is a
  // 2000px photo, not a 400px one because that is how wide the phone was.
  it('cuts the trimmed region at the original resolution', async () => {
    const { draws, output } = stubCanvas();
    await flattenAnnotations(picture(4000, 3000), null, {
      x: 0.25,
      y: 0.5,
      width: 0.5,
      height: 0.25,
    });
    expect([output.width, output.height]).toEqual([2000, 750]);
    expect(draws).toEqual([[1000, 1500, 2000, 750, 0, 0, 2000, 750]]);
  });

  // A mark drawn before the trim survives it: the layer is fitted to the
  // picture's own pixels, so the same rectangle cuts both.
  it('cuts the annotation layer by the same rectangle', async () => {
    const { draws } = stubCanvas();
    const layer = { width: 4000, height: 3000 } as HTMLCanvasElement;
    await flattenAnnotations(picture(4000, 3000), layer, {
      x: 0.25,
      y: 0.5,
      width: 0.5,
      height: 0.25,
    });
    expect(draws).toHaveLength(2);
    expect(draws[1]).toEqual([1000, 1500, 2000, 750, 0, 0, 2000, 750]);
  });

  it('refuses a picture that has not loaded', () => {
    stubCanvas();
    expect(() => flattenAnnotations(picture(0, 0), null)).toThrow(
      'Image is not ready',
    );
  });
});
