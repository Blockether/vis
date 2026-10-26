/**
 * READING A PDF INSIDE THE APP.
 *
 * The browser paints a PDF inside a viewer that the app cannot size. So a page
 * is RASTERISED (`openPdf`) and the reader fits that picture to the overlay.
 */
type PdfLib = typeof import('pdfjs-dist');

let pdfjs: Promise<PdfLib> | null = null;

/** pdf.js and its worker are ~1 MB: nothing loads until a page is read. */
async function library(): Promise<PdfLib> {
  pdfjs ??= (async () => {
    const lib = await import('pdfjs-dist');
    const worker = await import('pdfjs-dist/build/pdf.worker.min.mjs?url');
    lib.GlobalWorkerOptions.workerSrc = worker.default;
    return lib;
  })();
  return pdfjs;
}

export type RenderedPage = {
  /** The page as a PNG data URL. */
  src: string;
  /** The raster's size in pixels, so the reader can fit the page before it decodes. */
  width: number;
  height: number;
};

/** One opened document. Close it when its bytes are replaced or the reader closes. */
export interface PdfPages {
  readonly pageCount: number;
  /**
   * One 1-based page, rasterised at a readable density and decoded. A page asked for
   * again at the same density is the same promise, so a page drawn ahead of a turn
   * shows at once.
   */
  render(pageNumber: number, scale?: number): Promise<RenderedPage>;
  /** Forget every drawn page but these: a long document never stays in memory whole. */
  keep(pageNumbers: readonly number[]): void;
  close(): void;
}

/**
 * The largest raster drawn, in pixels. A WebKit canvas past about 16.7 million pixels
 * draws nothing, and a phone pays for every pixel in memory and in PNG encoding.
 */
const MAX_RASTER_PIXELS = 12_000_000;

/**
 * Parse a document once for all of its pages. A page turn then only draws a page:
 * parsing the whole file again on every turn was most of the wait.
 */
export async function openPdf(bytes: ArrayBuffer): Promise<PdfPages> {
  const lib = await library();
  // pdf.js transfers (and detaches) the buffer it is handed.
  const task = lib.getDocument({ data: bytes.slice(0) });
  const doc = await task.promise;
  const pageCount = doc.numPages;
  const drawn = new Map<string, { page: number; raster: Promise<RenderedPage> }>();
  const pageAt = (pageNumber: number) => Math.min(Math.max(pageNumber, 1), pageCount);

  const draw = async (pageNumber: number, scale: number): Promise<RenderedPage> => {
    const page = await doc.getPage(pageNumber);
    const base = page.getViewport({ scale: 1 });
    const most = Math.sqrt(MAX_RASTER_PIXELS / Math.max(1, base.width * base.height));
    const viewport = page.getViewport({ scale: Math.min(scale, most) });
    const canvas = document.createElement('canvas');
    canvas.width = Math.ceil(viewport.width);
    canvas.height = Math.ceil(viewport.height);
    const context = canvas.getContext('2d');
    if (!context) throw new Error('no 2d context');
    await page.render({ canvas, canvasContext: context, viewport }).promise;
    const src = canvas.toDataURL('image/png');
    // A fresh data URL paints as an EMPTY frame while it decodes, so a page is handed
    // out decoded, and the page being left stays up until then.
    try {
      const ready = new Image();
      ready.src = src;
      await ready.decode();
    } catch {
      // No decoder on this platform — the <img> paints it regardless.
    }
    return { src, width: canvas.width, height: canvas.height };
  };

  return {
    pageCount,
    render(pageNumber, scale = 2) {
      const page = pageAt(pageNumber);
      const key = `${page}@${scale}`;
      const known = drawn.get(key);
      if (known) return known.raster;
      const raster = draw(page, scale);
      drawn.set(key, { page, raster });
      // A failed page is drawn again on the next ask.
      raster.catch(() => drawn.delete(key));
      return raster;
    },
    keep(pageNumbers) {
      for (const [key, { page }] of drawn) {
        if (!pageNumbers.includes(page)) drawn.delete(key);
      }
    },
    close() {
      drawn.clear();
      void task.destroy();
    },
  };
}
