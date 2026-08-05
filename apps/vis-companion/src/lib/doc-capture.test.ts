import { describe, expect, it } from 'vitest';
import { docSandbox } from '../components/DocArtifact';
import {
  CAPTURE_RESET_CSS,
  MAX_CAPTURE_HEIGHT,
  MAX_CAPTURE_PIXELS,
  SANITIZE_CONFIG,
  captureHeight,
  captureScale,
  documentBaseName,
  pageCaptureFilename,
  viewCaptureFilename,
} from './doc-capture';

// The model never receives the PDF or the HTML page itself, so a capture's NAME
// is the only thing that says what it is a picture of. `report-p3.png` answers
// "which page did the human draw on"; `capture.png` would not.
describe('capture filenames', () => {
  it('carries the page number of a PDF page', () => {
    expect(pageCaptureFilename('report.pdf', 3)).toBe('report-p3.png');
    expect(pageCaptureFilename('Q3 report.pdf', 12)).toBe('Q3-report-p12.png');
  });

  it('never invents a page zero', () => {
    expect(pageCaptureFilename('report.pdf', 0)).toBe('report-p1.png');
    expect(pageCaptureFilename('report.pdf', -4)).toBe('report-p1.png');
    expect(pageCaptureFilename('report.pdf', 2.7)).toBe('report-p2.png');
  });

  it('says capture, not page, for an artifact that has no pages', () => {
    expect(viewCaptureFilename('page.html')).toBe('page-capture.png');
  });

  it('always ends up with a usable png name', () => {
    expect(documentBaseName('')).toBe('document');
    expect(documentBaseName('.hidden')).toBe('document');
    expect(documentBaseName('a/b\\c.html')).toBe('a-b-c');
    expect(viewCaptureFilename('../../etc/passwd')).toBe('etc-passwd-capture.png');
  });
});

describe('capture geometry', () => {
  it('paints the document at its own height, one screenful at the least', () => {
    expect(captureHeight(2480)).toBe(2480);
    expect(captureHeight(0)).toBe(720);
    expect(captureHeight(Number.NaN)).toBe(720);
    expect(captureHeight(1000.2)).toBe(1001);
  });

  it('refuses to grow without a bound', () => {
    expect(captureHeight(500_000)).toBe(MAX_CAPTURE_HEIGHT);
  });

  it('is crisp on a retina screen', () => {
    expect(captureScale(1024, 1400, 2)).toBe(2);
    expect(captureScale(1024, 1400, 1)).toBe(1);
    expect(captureScale(1024, 1400, 4)).toBe(2);
  });

  // A whole page at 0.6x still answers "look at this"; half a page never does,
  // so the budget shrinks the picture instead of cropping it.
  it('shrinks a very long page rather than truncating it', () => {
    const width = 1024;
    const height = MAX_CAPTURE_HEIGHT;
    const scale = captureScale(width, height, 2);
    expect(scale).toBeLessThan(1);
    expect(width * scale * height * scale).toBeLessThanOrEqual(MAX_CAPTURE_PIXELS + 1);
    expect(scale).toBeGreaterThan(0);
  });
});

// The artifact is READ behind an empty sandbox: opaque origin, no scripts. The
// capture cannot use that frame at all — html2canvas-pro never completes on an
// element inside a child frame — so the copy it paints lives in the app's own
// document and earns the same silence a different way: nothing executable
// survives the sanitiser, and a shadow root keeps the artifact's CSS from
// matching a single node of the app.
describe('the capture copy cannot act', () => {
  it('refuses every tag that would be a live actor in this origin', () => {
    for (const tag of [
      'script',
      'iframe',
      'frame',
      'object',
      'embed',
      'base',
      'noscript',
    ]) {
      expect(SANITIZE_CONFIG.FORBID_TAGS).toContain(tag);
    }
    expect(SANITIZE_CONFIG.FORBID_ATTR).toContain('srcdoc');
    expect(SANITIZE_CONFIG.ALLOW_UNKNOWN_PROTOCOLS).toBe(false);
  });

  it('keeps the whole document, because html and body are part of the picture', () => {
    expect(SANITIZE_CONFIG.WHOLE_DOCUMENT).toBe(true);
    expect(SANITIZE_CONFIG.RETURN_DOM).toBe(true);
  });

  it('drops the app cascade at the shadow boundary and restores document layout', () => {
    expect(CAPTURE_RESET_CSS).toMatch(/:host\s*\{[^}]*all:\s*initial/u);
    expect(CAPTURE_RESET_CSS).toMatch(/\bhtml\s*\{/u);
    expect(CAPTURE_RESET_CSS).toMatch(/\bbody\s*\{/u);
  });
});

describe('the reading frame stays origin-less', () => {
  it('never hands the artifact the app origin', () => {
    expect(docSandbox('text/html')).not.toContain('allow-same-origin');
    expect(docSandbox('application/pdf')).not.toContain('allow-same-origin');
  });
});
