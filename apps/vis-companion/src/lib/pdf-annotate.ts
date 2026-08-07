/**
 * DRAWING ON A PDF, WITHOUT LEAVING THE PDF.
 *
 * A picture can be drawn on directly — the pen writes on the very pixels that
 * are saved back. A PDF cannot: the browser paints it inside a viewer we are
 * not allowed to touch. So a page is RASTERISED for the pen (`renderPdfPage`),
 * the flattened result is stamped back onto that page (`stampPdfPage`), and
 * what is saved is still a PDF under the same filename — which is what makes
 * the drawing the artifact's NEXT VERSION rather than a picture beside it.
 */
import { PDFDocument } from "pdf-lib";

type PdfLib = typeof import("pdfjs-dist");

let pdfjs: Promise<PdfLib> | null = null;

/** pdf.js and its worker are ~1 MB: nothing loads until a page is drawn on. */
async function library(): Promise<PdfLib> {
  pdfjs ??= (async () => {
    const lib = await import("pdfjs-dist");
    const worker = await import("pdfjs-dist/build/pdf.worker.min.mjs?url");
    lib.GlobalWorkerOptions.workerSrc = worker.default;
    return lib;
  })();
  return pdfjs;
}

export type RenderedPage = {
  /** The page as a PNG data URL, ready for the image viewer's pen. */
  src: string;
  pageCount: number;
};

/** Rasterise one 1-based page at a readable density. */
export async function renderPdfPage(
  bytes: ArrayBuffer,
  pageNumber: number,
  scale = 2,
): Promise<RenderedPage> {
  const lib = await library();
  // pdf.js transfers (and detaches) the buffer it is handed.
  const doc = await lib.getDocument({ data: bytes.slice(0) }).promise;
  const pageCount = doc.numPages;
  const page = await doc.getPage(Math.min(Math.max(pageNumber, 1), pageCount));
  const viewport = page.getViewport({ scale });
  const canvas = document.createElement("canvas");
  canvas.width = Math.ceil(viewport.width);
  canvas.height = Math.ceil(viewport.height);
  const context = canvas.getContext("2d");
  if (!context) throw new Error("no 2d context");
  await page.render({ canvas, canvasContext: context, viewport }).promise;
  return { src: canvas.toDataURL("image/png"), pageCount };
}

/**
 * Put the drawn-on raster back where it came from: the stamp covers exactly the
 * page it was rendered from, so page size, count and every other page survive.
 */
export async function stampPdfPage(
  pdfBytes: ArrayBuffer,
  pageNumber: number,
  pngBytes: ArrayBuffer,
): Promise<Uint8Array> {
  const doc = await PDFDocument.load(pdfBytes);
  const page = doc.getPage(
    Math.min(Math.max(pageNumber, 1), doc.getPageCount()) - 1,
  );
  const png = await doc.embedPng(pngBytes);
  const { width, height } = page.getSize();
  page.drawImage(png, { x: 0, y: 0, width, height });
  return doc.save();
}
