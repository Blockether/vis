/**
 * A document the model may never read, turned into a picture it can.
 *
 * `vis_attach` clamps a PDF or an HTML page to `audience: "user"`, so its bytes
 * never reach the provider. That is the right default — a 40-page report is
 * noise — but it left the human with no way to say "look at THIS". So the app
 * RASTERISES what is on screen: one PDF page, or the whole HTML artifact,
 * becomes a PNG that goes through the existing image viewer (zoom, pen, undo)
 * and is attached to the next message as an ordinary image. The filename is the
 * provenance — `report-p3.png` is how the model knows which page it is looking
 * at — and the untouched document stays exactly where it was.
 *
 * Capturing HTML is the delicate half, and the frame the artifact is READ in
 * cannot be the frame it is painted from. Measured in Chromium against
 * html2canvas-pro 2.3.3: rendering an element that lives in a CHILD FRAME'S
 * document never completes — with an empty sandbox the painter's own clone frame
 * inherits the ban on scripts and its load handshake never fires, and even a
 * plain same-origin frame paints either nothing or, once the frame carries
 * `width`/`height` attributes, the wrong document entirely. Painting the same
 * markup from the app's OWN document takes ~70ms and is pixel-correct.
 *
 * So the capture never reuses the reading frame. It re-reads the artifact's
 * bytes, sanitises them with DOMPurify — no `<script>`, no `on*` handler, no
 * nested frame or object, so nothing in there can execute even though it now
 * sits in this origin — and mounts the surviving `<html>` element in a SHADOW
 * ROOT on an off-screen host. The shadow root is what keeps the promise
 * that an artifact's styles never reach the app: its `<style>` rules cannot
 * match anything outside it, `:host { all: initial }` stops the app's own type
 * and colour from leaking in, and `html`/`body` selectors still match because
 * those elements are carried across intact. Then html2canvas-pro paints the
 * host, at the artifact's own full height. The root is `open` for one reason:
 * a `closed` one hides `host.shadowRoot`, and the painter walks the tree it can
 * see — a closed root photographs as an empty box.
 *
 * The paint goes through html2canvas-pro rather than the one-line
 * `<foreignObject>` trick because WebKit silently drops foreignObject content
 * when the SVG is drawn into a canvas (webkit.org/b/156176): on iOS — half of
 * this product — that shortcut returns a blank picture instead of an error.
 */
import DOMPurify, { type Config } from 'dompurify';

/** CSS width the artifact is laid out at before it is painted. */
export const CAPTURE_WIDTH = 1024;

/**
 * A capture is an ATTACHMENT, and the gateway resizes stills on the way to the
 * provider anyway, so pixels past this budget cost upload time and buy nothing.
 */
export const MAX_CAPTURE_PIXELS = 8_000_000;

/** A scrolling artifact can be arbitrarily long; the picture cannot. */
export const MAX_CAPTURE_HEIGHT = 8_000;

/**
 * What is allowed to survive into this origin. The artifact was read behind an
 * empty sandbox; the capture copy earns the same silence by having nothing
 * executable left in it — DOMPurify drops every script and every `on*` handler,
 * and these tags are refused on top because a nested frame or plugin would be a
 * live actor in the app's own origin (and html2canvas cannot paint one anyway).
 */
export const SANITIZE_CONFIG = {
  WHOLE_DOCUMENT: true,
  RETURN_DOM: true,
  FORBID_TAGS: ['script', 'iframe', 'frame', 'object', 'embed', 'base', 'noscript'],
  FORBID_ATTR: ['srcdoc', 'ping', 'formaction'],
  ALLOW_UNKNOWN_PROTOCOLS: false,
} satisfies Config;

/**
 * The one stylesheet the app puts in the shadow root, first so the artifact's
 * own rules override it: the host is reset out of the app's cascade, and the
 * carried `html`/`body` elements are given the block layout a top-level
 * document would have had.
 */
export const CAPTURE_RESET_CSS =
  ':host{all:initial;display:block}html{display:block;width:100%}body{display:block}';

/** How long the artifact's bytes get before the capture gives up. */
const FETCH_TIMEOUT_MS = 20_000;

/** `report.pdf` → `report`: filesystem-safe, extension dropped, never empty. */
export function documentBaseName(name: string): string {
  const base = (name || '')
    .replace(/\.[^./\\]+$/u, '')
    .replace(/[^a-zA-Z0-9._-]+/gu, '-')
    .replace(/^[-.]+|-+$/gu, '');
  return base || 'document';
}

/**
 * The name a captured PDF page is attached under. The page number is IN the
 * filename because that is the only thing the model receives about it — a bare
 * `capture.png` would leave "which page is this" unanswerable.
 */
export function pageCaptureFilename(name: string, page: number): string {
  const index = Math.max(1, Math.trunc(page) || 1);
  return `${documentBaseName(name)}-p${index}.png`;
}

/** The name a captured HTML artifact is attached under: it has no pages. */
export function viewCaptureFilename(name: string): string {
  return `${documentBaseName(name)}-capture.png`;
}

/** The full document height, floored at one screenful and capped at a sane one. */
export function captureHeight(contentHeight: number, max: number = MAX_CAPTURE_HEIGHT): number {
  if (!Number.isFinite(contentHeight) || contentHeight <= 0) return Math.min(720, max);
  return Math.min(Math.ceil(contentHeight), max);
}

/**
 * Device pixels per CSS pixel for the capture: crisp on a phone, but shrunk
 * below 1 rather than truncated when a very long page would blow the budget —
 * a whole page at 0.6× still answers "look at this", half a page never does.
 */
export function captureScale(
  width: number,
  height: number,
  devicePixelRatio = 1,
  maxPixels: number = MAX_CAPTURE_PIXELS,
): number {
  const area = Math.max(1, width) * Math.max(1, height);
  const wanted = Math.min(Math.max(devicePixelRatio || 1, 1), 2);
  const fits = Math.sqrt(maxPixels / area);
  return Math.max(0.1, Math.min(wanted, fits));
}

/** PNG bytes for a canvas, as a rejected promise rather than a null blob. */
export function canvasPngBlob(canvas: HTMLCanvasElement): Promise<Blob> {
  return new Promise((resolve, reject) => {
    canvas.toBlob((blob) => {
      if (blob) resolve(blob);
      else reject(new Error('Could not prepare the picture'));
    }, 'image/png');
  });
}

/** A background is needed under transparent markup, or the PNG reads as black. */
export function paperColor(element: Element | null | undefined): string {
  for (const candidate of [element?.querySelector('body'), element]) {
    if (!candidate) continue;
    const color = candidate.ownerDocument?.defaultView?.getComputedStyle(candidate)
      .backgroundColor;
    if (color && !/^(transparent|rgba\(0,\s*0,\s*0,\s*0\))$/u.test(color)) return color;
  }
  return '#ffffff';
}

/**
 * The artifact's markup with everything that could act stripped out of it.
 * Exported because "the capture copy cannot execute" is the security claim of
 * this module, and a claim is only worth what a test can put a `<script>` and
 * an `onerror` through.
 */
export function sanitizeArtifactHtml(html: string): HTMLElement {
  return DOMPurify.sanitize(html, SANITIZE_CONFIG) as unknown as HTMLElement;
}

function nextPaint(): Promise<void> {
  return new Promise((resolve) => {
    const done = () => resolve();
    // A hidden or throttled tab may never animate; the capture still has to end.
    window.setTimeout(done, 250);
    requestAnimationFrame(() => requestAnimationFrame(done));
  });
}

async function artifactHtml(url: string): Promise<string> {
  const abort = new AbortController();
  const timer = window.setTimeout(() => abort.abort(), FETCH_TIMEOUT_MS);
  try {
    const response = await fetch(url, { signal: abort.signal });
    if (!response.ok) throw new Error(`The document could not be loaded (${response.status})`);
    return await response.text();
  } finally {
    window.clearTimeout(timer);
  }
}

/**
 * Paint an HTML artifact into PNG bytes, at its own full height.
 *
 * `url` is the attachment's own URL — the same bytes the visible frame shows,
 * read a second time so that reading and painting never share a document.
 */
export async function captureHtmlDocument(
  url: string,
  { width = CAPTURE_WIDTH }: { width?: number } = {},
): Promise<Blob> {
  const html = await artifactHtml(url);
  const host = document.createElement('div');
  host.setAttribute('aria-hidden', 'true');
  host.style.cssText = `position:fixed;left:-20000px;top:0;width:${width}px;background:#ffffff;pointer-events:none;`;
  const shadow = host.attachShadow({ mode: 'open' });
  const reset = document.createElement('style');
  reset.textContent = CAPTURE_RESET_CSS;
  shadow.append(reset);
  const clean = sanitizeArtifactHtml(html);
  shadow.append(clean);
  document.body.append(host);
  try {
    await nextPaint();
    const height = captureHeight(
      Math.max(clean.scrollHeight, host.getBoundingClientRect().height),
    );
    host.style.height = `${height}px`;
    await nextPaint();
    const { default: html2canvas } = await import('html2canvas-pro');
    const canvas = await html2canvas(host, {
      backgroundColor: paperColor(clean),
      width,
      height,
      windowWidth: width,
      windowHeight: height,
      scale: captureScale(width, height, window.devicePixelRatio),
      useCORS: true,
      logging: false,
    });
    return await canvasPngBlob(canvas);
  } finally {
    host.remove();
  }
}
