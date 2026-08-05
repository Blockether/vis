import { describe, expect, it } from 'vitest';
import { CAPTURE_WIDTH, MAX_CAPTURE_PIXELS } from './doc-capture';
import { clampPage, pdfPageScale } from './pdf-pages';

describe('page numbers', () => {
  it('stays inside the document', () => {
    expect(clampPage(1, 12)).toBe(1);
    expect(clampPage(7, 12)).toBe(7);
    expect(clampPage(99, 12)).toBe(12);
    expect(clampPage(0, 12)).toBe(1);
    expect(clampPage(-3, 12)).toBe(1);
  });

  it('treats a document of unknown length as one page', () => {
    expect(clampPage(4, 0)).toBe(1);
    expect(clampPage(4, Number.NaN)).toBe(1);
  });
});

// A PDF point is 1/72", so an A4 page is only 595 units wide: rendered 1:1 it is
// a blurry thing to draw on, and the pen would be wider than the text.
describe('page magnification', () => {
  const pixels = (width: number, height: number, scale: number) =>
    Math.ceil(width * scale) * Math.ceil(height * scale);

  it('brings A4 up to the capture width', () => {
    expect(pdfPageScale(595, 842, CAPTURE_WIDTH, 1)).toBeCloseTo(CAPTURE_WIDTH / 595, 5);
    expect(595 * pdfPageScale(595, 842, CAPTURE_WIDTH, 1)).toBeCloseTo(CAPTURE_WIDTH, 5);
  });

  it('doubles that again on a retina screen', () => {
    const scale = pdfPageScale(595, 842, CAPTURE_WIDTH, 2);
    expect(scale).toBeCloseTo((CAPTURE_WIDTH / 595) * 2, 5);
    expect(pixels(595, 842, scale)).toBeLessThanOrEqual(MAX_CAPTURE_PIXELS);
  });

  it('shrinks a poster instead of blowing the pixel budget', () => {
    const scale = pdfPageScale(1684, 20000, CAPTURE_WIDTH, 2);
    expect(scale).toBeGreaterThan(0);
    expect(pixels(1684, 20000, scale)).toBeLessThanOrEqual(MAX_CAPTURE_PIXELS + 20_000);
  });

  it('never returns zero for a page it cannot measure', () => {
    expect(pdfPageScale(0, 0, CAPTURE_WIDTH, 1)).toBeGreaterThan(0);
    expect(pdfPageScale(Number.NaN, Number.NaN, CAPTURE_WIDTH, 1)).toBeGreaterThan(0);
  });
});
