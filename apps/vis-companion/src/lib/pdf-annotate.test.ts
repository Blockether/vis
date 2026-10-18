// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { openPdf } from './pdf-annotate';

// pdf.js is the browser's job; these tests hold what the app keeps of an opened document.
const pdfjs = vi.hoisted(() => ({
  getPage: vi.fn(async (_page: number) => ({
    getViewport: ({ scale }: { scale: number }) => ({ width: 100 * scale, height: 150 * scale }),
    render: () => ({ promise: Promise.resolve() }),
  })),
  destroy: vi.fn(async () => undefined),
  parsed: 0,
}));
vi.mock('pdfjs-dist', () => ({
  GlobalWorkerOptions: {},
  getDocument: () => {
    pdfjs.parsed += 1;
    return {
      promise: Promise.resolve({ numPages: 3, getPage: pdfjs.getPage }),
      destroy: pdfjs.destroy,
    };
  },
}));
vi.mock('pdfjs-dist/build/pdf.worker.min.mjs?url', () => ({ default: 'worker.js' }));

beforeEach(() => {
  pdfjs.parsed = 0;
  pdfjs.getPage.mockClear();
  pdfjs.destroy.mockClear();
  // jsdom draws nothing: a context and an encoder are all a page needs here.
  vi.spyOn(HTMLCanvasElement.prototype, 'getContext').mockReturnValue(
    {} as CanvasRenderingContext2D,
  );
  vi.spyOn(HTMLCanvasElement.prototype, 'toDataURL').mockReturnValue('data:image/png;base64,');
});

afterEach(() => {
  vi.restoreAllMocks();
});

describe('an opened PDF', () => {
  // Regression, user report: a page turn lagged, because every turn parsed the whole
  // document again before it could draw one page.
  it('parses the document once and draws each page once per density', async () => {
    const doc = await openPdf(new ArrayBuffer(4));
    expect(doc.pageCount).toBe(3);

    const first = await doc.render(2, 2);
    expect(first).toEqual({ src: 'data:image/png;base64,', width: 200, height: 300 });
    expect(doc.render(2, 2)).toBe(doc.render(2, 2));
    await doc.render(3, 2);
    await doc.render(2, 4);
    expect(pdfjs.parsed).toBe(1);
    expect(pdfjs.getPage.mock.calls.map(([page]) => page)).toEqual([2, 3, 2]);
  });

  it('forgets the pages it is not told to keep, and lets go of the document', async () => {
    const doc = await openPdf(new ArrayBuffer(4));
    await doc.render(1, 2);
    await doc.render(3, 2);
    doc.keep([2, 3]);
    await doc.render(1, 2);
    await doc.render(3, 2);
    expect(pdfjs.getPage.mock.calls.map(([page]) => page)).toEqual([1, 3, 1]);

    doc.close();
    expect(pdfjs.destroy).toHaveBeenCalledTimes(1);
  });

  it('draws a failed page again on the next ask', async () => {
    const doc = await openPdf(new ArrayBuffer(4));
    pdfjs.getPage.mockRejectedValueOnce(new Error('broken page'));
    await expect(doc.render(1, 2)).rejects.toThrow('broken page');
    await expect(doc.render(1, 2)).resolves.toEqual({
      src: 'data:image/png;base64,',
      width: 200,
      height: 300,
    });
  });
});
