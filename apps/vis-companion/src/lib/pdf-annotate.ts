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
 * A PDF that the app reads in parts (#451). The file of a reference stays on the disk of
 * the gateway machine and can be hundreds of megabytes, so it is never one download.
 */
export interface PdfRangeSource {
  /** The size of the file in bytes. */
  readonly length: number;
  /** The bytes from `begin` up to, but not including, `end`. */
  read(begin: number, end: number): Promise<ArrayBuffer>;
}

/** The size of one read of a {@link PdfRangeSource}. */
export const PDF_RANGE_CHUNK = 2 ** 17;

/**
 * THE DOCUMENT ASKS FOR THE PARTS THAT IT NEEDS.
 *
 * pdf.js reads the cross-reference table at the end of the file and then only the
 * objects of the pages that it draws. The app does each read itself: the gateway does
 * not expose `Accept-Ranges` to a page of another origin, so the reader of pdf.js would
 * fall back to one download of the whole file. A read that fails ends the document, so
 * the reader shows an error and does not wait for ever.
 */
export async function rangeTask(
  lib: Pick<PdfLib, 'getDocument' | 'PDFDataRangeTransport'>,
  source: PdfRangeSource,
): Promise<{ task: ReturnType<PdfLib['getDocument']>; failed: Promise<never> }> {
  const first = new Uint8Array(await source.read(0, Math.min(source.length, PDF_RANGE_CHUNK)));
  // pdf.js waits for a part that never comes, and destroying the task does not end that
  // wait. So the first failed read rejects `failed`, and the reader races its work with it.
  let fail: (cause: unknown) => void = () => undefined;
  const failed = new Promise<never>((_, reject) => {
    fail = reject;
  });
  failed.catch(() => undefined);
  class Transport extends lib.PDFDataRangeTransport {
    override requestDataRange(begin: number, end: number) {
      source.read(begin, end).then(
        (chunk) => this.onDataRange(begin, new Uint8Array(chunk)),
        (cause: unknown) => fail(cause),
      );
    }
  }
  const range = new Transport(source.length, first, first.byteLength === source.length);
  const task = lib.getDocument({
    range,
    rangeChunkSize: PDF_RANGE_CHUNK,
    // Only the parts that a page needs; never the rest of the file in the background.
    disableAutoFetch: true,
    disableStream: true,
  });
  return { task, failed };
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
export async function openPdf(source: ArrayBuffer | PdfRangeSource): Promise<PdfPages> {
  const lib = await library();
  const { task, failed } =
    source instanceof ArrayBuffer
      ? // pdf.js transfers (and detaches) the buffer it is handed.
        { task: lib.getDocument({ data: source.slice(0) }), failed: null }
      : await rangeTask(lib, source);
  // A part of a range source that cannot be read ends the wait for it.
  const guard = <T>(work: Promise<T>): Promise<T> => (failed ? Promise.race([work, failed]) : work);
  let doc: Awaited<typeof task.promise>;
  try {
    doc = await guard(task.promise);
  } catch (cause) {
    void task.destroy();
    throw cause;
  }
  const pageCount = doc.numPages;
  const drawn = new Map<string, { page: number; raster: Promise<RenderedPage> }>();
  const pageAt = (pageNumber: number) => Math.min(Math.max(pageNumber, 1), pageCount);

  const draw = async (pageNumber: number, scale: number): Promise<RenderedPage> => {
    const page = await guard(doc.getPage(pageNumber));
    const base = page.getViewport({ scale: 1 });
    const most = Math.sqrt(MAX_RASTER_PIXELS / Math.max(1, base.width * base.height));
    const viewport = page.getViewport({ scale: Math.min(scale, most) });
    const canvas = document.createElement('canvas');
    canvas.width = Math.ceil(viewport.width);
    canvas.height = Math.ceil(viewport.height);
    const context = canvas.getContext('2d');
    if (!context) throw new Error('no 2d context');
    await guard(page.render({ canvas, canvasContext: context, viewport }).promise);
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
