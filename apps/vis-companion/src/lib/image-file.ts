/**
 * One picture as a FILE: what it is called, and how its bytes are read.
 *
 * The name is a contract, not a detail. A capture is the ONLY thing the model
 * ever receives about a document it may not read, so `report-p3.png` is what
 * says which page a human drew on; `capture.png` would leave that
 * unanswerable. Everything here is pure apart from the two byte readers, which
 * is why naming can be stated in tests instead of demonstrated in a browser.
 *
 * It lives on its own because THREE callers had grown their own copy: the
 * document capture, the attachment intake and the image viewer each carried a
 * slug regex, a `toBlob` wrapper and a data-URL reader of their own.
 */

/** `report.pdf` -> `report`: filesystem-safe, extension dropped, never empty. */
export function fileBaseName(name: string, fallback = 'document'): string {
  const base = (name || '')
    .replace(/\.[^./\\]+$/u, '')
    .replace(/[^a-zA-Z0-9._-]+/gu, '-')
    .replace(/^[-.]+|-+$/gu, '');
  return base || fallback;
}

/**
 * The name a captured PDF page is attached under. The page number is IN the
 * filename because that is the only thing the model receives about it.
 */
export function pageCaptureFilename(name: string, page: number): string {
  const index = Math.max(1, Math.trunc(page) || 1);
  return `${fileBaseName(name)}-p${index}.png`;
}

/** The name a captured HTML artifact is attached under: it has no pages. */
export function viewCaptureFilename(name: string): string {
  return `${fileBaseName(name)}-capture.png`;
}

/**
 * The name a picture leaves the viewer under. An edit is always a PNG — the
 * strokes are flattened into it — whatever the original was called.
 */
export function editedFilename(name: string): string {
  return `${fileBaseName(name, 'vis-image')}.png`;
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

/** A blob as a `data:` URL — what Capacitor's clipboard and filesystem take. */
export function blobAsDataUrl(blob: Blob): Promise<string> {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();
    reader.onerror = () =>
      reject(reader.error ?? new Error('Could not read this file'));
    reader.onload = () => resolve(String(reader.result));
    reader.readAsDataURL(blob);
  });
}

/**
 * Did the human dismiss a system sheet rather than hit a failure? Every native
 * sheet — picker, camera, share — reports a cancel as a throw, and a cancel is
 * a decision to respect, never an error to show.
 */
export function sheetDismissed(cause: unknown): boolean {
  const message = cause instanceof Error ? cause.message : String(cause);
  return /cancel|dismiss|abort/iu.test(message);
}
