/**
 * PDF pages as pictures, one page at a time.
 *
 * The model never receives the PDF itself (`vis_attach` clamps it to
 * `audience: "user"`), so the only way to show it a page is to RENDER that page
 * and attach the image — which is also what makes drawing on a report possible
 * at all. pdf.js paints into a canvas, so unlike an HTML capture there is no
 * WebKit foreignObject trap here; the page number is carried out with the bytes
 * because `pageCaptureFilename` puts it in the attachment's name.
 *
 * pdf.js is loaded on DEMAND: it is by far the heaviest dependency in the app,
 * and a transcript full of documents nobody opened must not pay for it.
 */
import { CAPTURE_WIDTH, MAX_CAPTURE_PIXELS, captureScale } from './doc-capture';
import { canvasPngBlob } from './image-file';

// Bundled and fetched as a URL, not inlined: the worker is a separate script by
// construction, and Vite rewrites this to the emitted asset for both the web
// build and the Capacitor bundle.
import workerUrl from 'pdfjs-dist/build/pdf.worker.min.mjs?url';

export interface PdfPages {
  pageCount: number;
  /** PNG bytes for one 1-based page, at roughly `CAPTURE_WIDTH` CSS pixels. */
  renderPage: (page: number) => Promise<Blob>;
  /** Release the parsed document and its worker. */
  close: () => void;
}

/** Keep a page number inside the document, whatever the caller believes. */
export function clampPage(page: number, pageCount: number): number {
  const total = Math.max(1, Math.trunc(pageCount) || 1);
  const wanted = Math.trunc(page) || 1;
  return Math.min(Math.max(1, wanted), total);
}

/**
 * How much to magnify a page so it lands near the capture width — a PDF point
 * is 1/72", so an A4 page is only 595 units wide and rendering it 1:1 gives a
 * blurry picture to draw on. The pixel budget still wins on a poster-sized page.
 */
export function pdfPageScale(
  pageWidth: number,
  pageHeight: number,
  targetWidth: number = CAPTURE_WIDTH,
  devicePixelRatio = 1,
  maxPixels: number = MAX_CAPTURE_PIXELS,
): number {
  const width = pageWidth > 0 ? pageWidth : targetWidth;
  const height = pageHeight > 0 ? pageHeight : targetWidth;
  const fit = targetWidth / width;
  const dense = captureScale(
    width * fit,
    height * fit,
    devicePixelRatio,
    maxPixels,
  );
  return Math.max(0.1, fit * dense);
}

/** Parse a PDF from an object URL and hand back a per-page renderer. */
export async function openPdfPages(
  url: string,
  { targetWidth = CAPTURE_WIDTH }: { targetWidth?: number } = {},
): Promise<PdfPages> {
  const pdfjs = await import('pdfjs-dist');
  pdfjs.GlobalWorkerOptions.workerSrc = workerUrl;
  // The loading TASK owns the worker, so it is what has to be destroyed: closing
  // only the document would leave a worker thread per opened artifact behind.
  const task = pdfjs.getDocument({ url });
  const doc = await task.promise;
  return {
    pageCount: doc.numPages,
    close: () => {
      void task.destroy();
    },
    renderPage: async (page: number) => {
      const target = await doc.getPage(clampPage(page, doc.numPages));
      const unscaled = target.getViewport({ scale: 1 });
      const viewport = target.getViewport({
        scale: pdfPageScale(
          unscaled.width,
          unscaled.height,
          targetWidth,
          window.devicePixelRatio,
        ),
      });
      const canvas = document.createElement('canvas');
      canvas.width = Math.max(1, Math.ceil(viewport.width));
      canvas.height = Math.max(1, Math.ceil(viewport.height));
      if (!canvas.getContext('2d'))
        throw new Error('This page cannot be rendered here');
      // A PDF page is transparent where it is white, so it needs paper under it:
      // without one the capture arrives as black pixels with black ink on them.
      await target.render({ canvas, viewport, background: '#ffffff' }).promise;
      return canvasPngBlob(canvas);
    },
  };
}
